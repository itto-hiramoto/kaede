use std::{
    collections::{BTreeSet, VecDeque},
    rc::Rc,
    slice::Iter,
};

use crate::{
    error::CodegenError,
    mangle::{mangle_name, mangle_udt_name},
    stmt::{build_block, build_normal_let, build_statement},
    tcx::{
        EnumInfo, EnumVariantInfo, GenericArgTable, GenericKind, ReturnType, SymbolTable,
        SymbolTableValue,
    },
    value::{has_signed, Value},
    CompileUnitCtx,
};

use inkwell::{
    module::Linkage,
    types::{BasicTypeEnum, StructType},
    values::{BasicValue, BasicValueEnum, IntValue, PointerValue},
    IntPredicate,
};
use kaede_ast::{
    expr::{
        Args, ArrayLiteral, Binary, BinaryKind, Break, Else, Expr, ExprKind, FnCall, If, Indexing,
        LogicalNot, Loop, Match, MatchArm, MatchArmList, Return, StringLiteral, StructLiteral,
        TupleLiteral,
    },
    stmt::{Block, Stmt, StmtKind},
    top::{Fn, FnDecl, GenericFnInstance, TopLevel, TopLevelKind, Visibility},
};
use kaede_span::Span;
use kaede_symbol::{Ident, Symbol};
use kaede_ast_type::{
    change_mutability_dup, is_same_type, make_fundamental_type, wrap_in_ref, FundamentalType,
    FundamentalTypeKind, GenericArgs, Mutability, PointerType, ReferenceType, Ty, TyKind,
    UserDefinedType,
};

pub fn mangle_generic_fn_name(
    cucx: &mut CompileUnitCtx,
    name: Symbol,
    generic_args: &GenericArgs,
) -> Symbol {
    let prefix = mangle_name(cucx, name);

    let mut suffix = String::new();

    generic_args.types.iter().for_each(|arg| {
        suffix.push('.');
        suffix.push_str(arg.kind.to_string().as_str());
    });

    Symbol::from(format!("{}{}", prefix.as_str(), suffix))
}

struct EnumUnpack<'ctx> {
    /// A::B(x)
    ///      ^
    name: Ident,
    /// match x
    ///       ^
    enum_ty: StructType<'ctx>,
    enum_value: Value<'ctx>,
    variant_ty: Rc<Ty>,
    span: Span,
}

/// `If` where `Expr` is changed to `Value`
struct ValuedIf<'ctx> {
    cond: Value<'ctx>,
    then: Rc<Block>,
    else_: Option<Box<ValuedElse<'ctx>>>,
    span: Span,

    /// Used to initialize variant parameters of match
    enum_unpack: Option<EnumUnpack<'ctx>>,
}

impl<'ctx> ValuedIf<'ctx> {
    fn add_else_at_last(&mut self, node: ValuedElse<'ctx>) {
        if let Some(else_) = self.else_.as_mut() {
            if let ValuedElse::If(if_) = else_.as_mut() {
                return if_.add_else_at_last(node);
            }
        }

        self.else_ = Some(Box::new(node));
    }
}

enum ValuedElse<'ctx> {
    If(ValuedIf<'ctx>),
    Block(Rc<Block>),
}

/// Unit value if the end of the block is not an expression
pub fn build_block_expression<'ctx>(
    cucx: &mut CompileUnitCtx<'ctx>,
    block: &Block,
) -> anyhow::Result<Value<'ctx>> {
    if block.body.is_empty() {
        return Ok(Value::new_unit());
    }

    cucx.tcx.push_symbol_table(SymbolTable::new());

    let mut idx: usize = 0;

    let last_stmt = loop {
        if idx + 1 == block.body.len() {
            // Last element
            break &block.body[idx];
        }

        build_statement(cucx, &block.body[idx])?;

        idx += 1;
    };

    let value = match &last_stmt.kind {
        StmtKind::Expr(e) => build_expression(cucx, e)?,

        // The end of the block is not an expression
        _ => {
            build_statement(cucx, last_stmt)?;
            Value::new_unit()
        }
    };

    cucx.tcx.pop_symbol_table();

    Ok(value)
}

type DecomposedEnumVariantPattern<'pat> =
    ((Option<Vec<Ident>>, Ident), &'pat Ident, Option<&'pat Args>);

pub fn build_expression<'ctx>(
    cucx: &mut CompileUnitCtx<'ctx>,
    node: &Expr,
) -> anyhow::Result<Value<'ctx>> {
    let mut builder = ExprBuilder::new(cucx);

    builder.build(node)
}

pub fn build_str_indexing<'ctx>(
    cucx: &mut CompileUnitCtx<'ctx>,
    tuple: PointerValue<'ctx>,
    index: u32,
    tuple_ty: &Rc<Ty>,
    span: Span,
) -> anyhow::Result<Value<'ctx>> {
    build_tuple_indexing_internal(
        cucx,
        tuple,
        tuple_ty,
        index,
        // Str isn't wrapped in a pointer (Not generated from GC).
        // So, number of indexes is 1.
        &[cucx.context().i32_type().const_int(index as u64, false)],
        span,
    )
}

pub fn build_tuple_indexing<'ctx>(
    cucx: &mut CompileUnitCtx<'ctx>,
    tuple: PointerValue<'ctx>,
    index: u32,
    tuple_ty: &Rc<Ty>,
    span: Span,
) -> anyhow::Result<Value<'ctx>> {
    build_tuple_indexing_internal(
        cucx,
        tuple,
        tuple_ty,
        index,
        // Tuple is wrapped in a pointer (Generated from GC).
        // So, number of indexes is 2.
        &[
            cucx.context().i32_type().const_zero(),
            cucx.context().i32_type().const_int(index as u64, false),
        ],
        span,
    )
}

pub fn build_tuple_indexing_internal<'ctx>(
    cucx: &mut CompileUnitCtx<'ctx>,
    tuple: PointerValue<'ctx>,
    tuple_ty: &Rc<Ty>,
    index: u32,
    ordered_indexes: &[IntValue<'ctx>], // Foo[0].F => [0, Foo's offset]
    span: Span,
) -> anyhow::Result<Value<'ctx>> {
    let llvm_tuple_ty = cucx.conv_to_llvm_type(tuple_ty, span)?;

    let gep = unsafe {
        cucx.builder
            .build_in_bounds_gep(llvm_tuple_ty, tuple, ordered_indexes, "")?
    };

    let elem_ty = match tuple_ty.kind.as_ref() {
        TyKind::Tuple(types) => match types.get(index as usize) {
            Some(ty) => ty.clone(),

            None => {
                return Err(CodegenError::IndexOutOfRange {
                    index: index as u64,
                    span,
                }
                .into())
            }
        },

        // (*i8, u64)
        _ if tuple_ty.is_str() => match index {
            0 => Rc::new(Ty {
                kind: TyKind::Pointer(PointerType {
                    pointee_ty: make_fundamental_type(FundamentalTypeKind::I8, Mutability::Not)
                        .into(),
                })
                .into(),
                mutability: Mutability::Not,
            }),

            1 => Rc::new(make_fundamental_type(
                FundamentalTypeKind::U64,
                Mutability::Not,
            )),

            _ => {
                return Err(CodegenError::IndexOutOfRange {
                    index: index as u64,
                    span,
                }
                .into())
            }
        },

        kind => unreachable!("{:?}", kind),
    };

    let llvm_elem_ty = cucx.conv_to_llvm_type(&elem_ty, span)?;

    Ok(Value::new(
        cucx.builder.build_load(llvm_elem_ty, gep, "")?,
        Ty {
            kind: elem_ty.kind.clone(),
            mutability: tuple_ty.mutability,
        }
        .into(),
    ))
}

pub fn create_gc_struct<'ctx>(
    cucx: &mut CompileUnitCtx<'ctx>,
    struct_llvm_ty: BasicTypeEnum<'ctx>,
    inits: &[BasicValueEnum<'ctx>],
) -> anyhow::Result<PointerValue<'ctx>> {
    let mallocd = cucx.gc_malloc(struct_llvm_ty)?;

    for (index, init) in inits.iter().enumerate() {
        let gep = unsafe {
            cucx.builder.build_in_bounds_gep(
                struct_llvm_ty,
                mallocd,
                &[
                    cucx.context().i32_type().const_zero(),
                    cucx.context().i32_type().const_int(index as u64, false),
                ],
                "",
            )?
        };

        cucx.builder.build_store(gep, *init)?;
    }

    Ok(mallocd)
}

struct ExprBuilder<'a, 'ctx> {
    cucx: &'a mut CompileUnitCtx<'ctx>,
}

impl<'a, 'ctx> ExprBuilder<'a, 'ctx> {
    fn new(cucx: &'a mut CompileUnitCtx<'ctx>) -> Self {
        Self { cucx }
    }

    /// Generate expression code
    fn build(&mut self, node: &Expr) -> anyhow::Result<Value<'ctx>> {
        Ok(match &node.kind {
            ExprKind::Block(block) => build_block_expression(self.cucx, block)?,

            ExprKind::Int(int) => Value::new(
                int.as_llvm_int(self.cucx.context()).into(),
                Rc::new(int.get_type()),
            ),

            ExprKind::StringLiteral(node) => self.string_literal(node)?,

            ExprKind::StructLiteral(node) => self.struct_literal(node)?,

            ExprKind::ArrayLiteral(node) => self.array_literal(node)?,

            ExprKind::TupleLiteral(node) => self.tuple_literal(node)?,

            ExprKind::Ident(name) => self.ident_expr(name)?,

            ExprKind::ExternalIdent(_) => todo!(),

            ExprKind::GenericIdent(_) => todo!("Error"),

            ExprKind::Binary(node) => self.binary_op(node)?,

            ExprKind::LogicalNot(node) => self.logical_not(node)?,

            ExprKind::FnCall(node) => self.fn_call(node)?,

            // Boolean literals
            ExprKind::True => self.boolean_literal(true),
            ExprKind::False => self.boolean_literal(false),

            ExprKind::Indexing(node) => self.array_indexing(node)?,

            ExprKind::Break(node) => self.break_(node)?,

            ExprKind::Return(node) => self.return_(node)?,

            ExprKind::Loop(node) => self.loop_(node)?,

            ExprKind::If(node) => self.if_(node)?,

            ExprKind::Match(node) => self.match_(node)?,

            ExprKind::Ty(_) => Value::new_unit(), /* Do nothing! */
        })
    }

    /// A::B to ("A", "B", None)
    /// A::B(a, b, c) to ("A", "B", Some(["a", "b", "c"]))
    fn decompose_enum_variant_pattern<'pat>(
        &self,
        pattern: &'pat Expr,
    ) -> DecomposedEnumVariantPattern<'pat> {
        let (module_name_and_enum_name, variant_name_and_param) = match &pattern.kind {
            ExprKind::Binary(b) => match b.kind {
                BinaryKind::ScopeResolution => match &b.lhs.kind {
                    ExprKind::Ident(i) => ((None, *i), &b.rhs),
                    ExprKind::ExternalIdent(ei) => {
                        ((Some(ei.external_modules.clone()), ei.ident), &b.rhs)
                    }
                    _ => unreachable!(),
                },

                _ => unreachable!(),
            },

            _ => unreachable!(),
        };

        let (variant_name, param) = match &variant_name_and_param.kind {
            ExprKind::Ident(ident) => (ident, None),
            ExprKind::FnCall(fncall) => (&fncall.callee, Some(&fncall.args)),
            _ => unreachable!(),
        };

        (module_name_and_enum_name, variant_name, param)
    }

    fn match_(&mut self, node: &Match) -> anyhow::Result<Value<'ctx>> {
        let value = self.build(&node.target)?;

        let value_ty = value.get_type();

        // Prepare for external types
        let (value_ty, bkup) = if let TyKind::External(ety) = value_ty.kind.as_ref() {
            (
                ety.get_base_type(),
                Some(
                    self.cucx
                        .modules_for_mangle
                        .change_for_external(ety.get_module_names_recursively()),
                ),
            )
        } else {
            (value_ty, None)
        };

        let result = match value_ty.kind.as_ref() {
            TyKind::Reference(refty) => self.build_match_on_reference(node, &value, refty),
            TyKind::Fundamental(fty) => self.build_match_on_fundamental_value(node, &value, fty),

            kind => todo!("Unsupported enum target: {}", kind),
        };

        if let Some(bkup) = bkup {
            self.cucx.modules_for_mangle.change_for_internal(bkup);
        }

        result
    }

    fn check_exhaustiveness_for_match_on_int(
        &mut self,
        target_type: &FundamentalType,
        arms: &MatchArmList,
        span: Span,
    ) -> anyhow::Result<()> {
        if arms.wildcard.is_some() {
            return Ok(());
        }

        assert!(target_type.is_int_or_bool());

        if target_type.kind == FundamentalTypeKind::Bool {
            let mut has_true = false;
            let mut has_false = false;

            for arm in arms.non_wildcard_iter() {
                match arm.pattern.kind {
                    ExprKind::True => has_true = true,
                    ExprKind::False => has_false = true,
                    _ => {
                        return Err(CodegenError::MismatchedTypes {
                            types: (
                                target_type.kind.to_string(),
                                self.build(&arm.pattern)?.get_type().kind.to_string(),
                            ),
                            span: arm.pattern.span,
                        }
                        .into())
                    }
                }
            }

            if has_true && has_false {
                return Ok(());
            }

            if has_true {
                return Err(CodegenError::NonExhaustivePatterns {
                    non_exhaustive_patterns: "`false`".to_owned(),
                    span,
                }
                .into());
            }

            if has_false {
                return Err(CodegenError::NonExhaustivePatterns {
                    non_exhaustive_patterns: "`true`".to_owned(),
                    span,
                }
                .into());
            }

            return Err(CodegenError::NonExhaustivePatterns {
                non_exhaustive_patterns: "`true` and `false`".to_owned(),
                span,
            }
            .into());
        }

        // Non-bool integer
        Err(CodegenError::NonExhaustivePatterns {
            non_exhaustive_patterns: "`_`".to_owned(),
            span,
        }
        .into())
    }

    fn add_wildcard(&self, if_: &mut ValuedIf<'ctx>, wildcard: &MatchArm) {
        assert!(wildcard.is_wildcard());

        let wildcard = Block {
            body: vec![Stmt {
                kind: StmtKind::Expr(wildcard.code.clone()),
                span: wildcard.code.span,
            }],
            span: wildcard.code.span,
        };

        if_.add_else_at_last(ValuedElse::Block(Rc::new(wildcard)));
    }

    fn build_match_on_fundamental_value(
        &mut self,
        node: &Match,
        target: &Value<'ctx>,
        fty: &FundamentalType,
    ) -> anyhow::Result<Value<'ctx>> {
        if fty.is_int_or_bool() {
            self.check_exhaustiveness_for_match_on_int(fty, &node.arms, node.span)?;

            let mut valued_if = self
                .conv_match_arms_on_int_to_if(target, node.arms.non_wildcard_iter(), node.span)?
                .unwrap();

            if let Some(wc) = &node.arms.wildcard {
                self.add_wildcard(&mut valued_if, wc);
            }

            return self.build_if(&valued_if, true);
        }

        todo!()
    }

    /// The return value is `Option` because of the recursion termination condition, so there is no problem if the caller `unwrap`s it
    fn conv_match_arms_on_int_to_if(
        &mut self,
        target: &Value<'ctx>,
        mut arms: Iter<MatchArm>,
        span: Span,
    ) -> anyhow::Result<Option<ValuedIf<'ctx>>> {
        let current_arm = match arms.next() {
            Some(c) => c,
            None => return Ok(None),
        };

        let cond = {
            let arm_value = self.build(&current_arm.pattern)?;

            if !arm_value.get_type().kind.is_int_or_bool() {
                return Err(CodegenError::MismatchedTypes {
                    types: (
                        target.get_type().kind.to_string(),
                        arm_value.get_type().kind.to_string(),
                    ),
                    span: current_arm.pattern.span,
                }
                .into());
            }

            self.build_int_equal(
                target.get_value().into_int_value(),
                arm_value.get_value().into_int_value(),
            )?
        };

        let then = Block {
            body: vec![Stmt {
                kind: StmtKind::Expr(current_arm.code.clone()),
                span: current_arm.code.span,
            }],
            span: current_arm.code.span,
        };

        let else_ = self
            .conv_match_arms_on_int_to_if(target, arms, span)?
            .map(|if_| Box::new(ValuedElse::If(if_)));

        Ok(Some(ValuedIf {
            cond,
            enum_unpack: None,
            then: then.into(),
            else_,
            span,
        }))
    }

    fn check_exhaustiveness_for_match_on_enum(
        &self,
        enum_info: &EnumInfo,
        arms: &MatchArmList,
        span: Span,
    ) -> anyhow::Result<()> {
        if arms.wildcard.is_some() {
            return Ok(());
        }

        let variants = &enum_info.variants;

        let mut pattern_variant_names = BTreeSet::new();

        for arm in arms.non_wildcard_iter() {
            let ((module_names, enum_name), variant_name, _) =
                self.decompose_enum_variant_pattern(&arm.pattern);

            if let Some(module_names) = module_names {
                let enum_info_external_modules = enum_info.is_external.as_ref().unwrap();
                for (idx, mn) in module_names.iter().enumerate() {
                    if mn.as_str() != enum_info_external_modules[idx].as_str() {
                        todo!("Error");
                    }
                }
            }

            if enum_info.name != enum_name.symbol() {
                todo!("Error");
            }

            if !variants.contains_key(&variant_name.symbol()) {
                return Err(CodegenError::NoVariant {
                    variant_name: variant_name.symbol(),
                    parent_name: enum_name.symbol(),
                    span: variant_name.span(),
                }
                .into());
            }

            if !pattern_variant_names.insert(variant_name.as_str()) {
                // There were multiple identical patterns
                return Err(CodegenError::UnreachablePattern {
                    span: enum_name.span(),
                }
                .into());
            }
        }

        let variant_names = variants.keys().map(|k| k.as_str()).collect::<BTreeSet<_>>();

        let dif = variant_names
            .difference(&pattern_variant_names)
            .collect::<Vec<_>>();

        if !dif.is_empty() {
            return Err(CodegenError::NonExhaustivePatterns {
                non_exhaustive_patterns: dif
                    .into_iter()
                    .map(|p| format!("`{}::{}`", enum_info.name.as_str(), p))
                    .collect::<Vec<_>>()
                    .join(" and "),
                span,
            }
            .into());
        }

        Ok(())
    }

    fn build_match_on_reference(
        &mut self,
        node: &Match,
        target: &Value<'ctx>,
        refty: &ReferenceType,
    ) -> anyhow::Result<Value<'ctx>> {
        let base_ty_of_ref = refty.get_base_type();

        // Prepare for external types
        let (base_ty, bkup) = if let TyKind::External(ety) = base_ty_of_ref.kind.as_ref() {
            let bkup = self
                .cucx
                .modules_for_mangle
                .change_for_external(ety.get_module_names_recursively());
            (ety.get_base_type(), Some(bkup))
        } else {
            (base_ty_of_ref, None)
        };

        let udt = match base_ty.kind.as_ref() {
            TyKind::UserDefined(udt) => udt,
            _ => unimplemented!(),
        };

        // For when the target was a generic user-defined type.
        self.cucx.conv_to_llvm_type(&base_ty, node.target.span)?;

        let mangled_name = mangle_udt_name(self.cucx, udt);

        let enum_info = match &*self
            .cucx
            .tcx
            .lookup_symbol(mangled_name, node.span)?
            .borrow()
        {
            SymbolTableValue::Enum(enum_info) => enum_info.clone(),
            _ => {
                return Err(CodegenError::Undeclared {
                    name: udt.name.symbol(),
                    span: node.span,
                }
                .into())
            }
        };

        self.check_exhaustiveness_for_match_on_enum(&enum_info, &node.arms, node.span)?;

        let result = self.build_match_on_enum(&enum_info, target, &node.arms, node.span);

        if let Some(bkup) = bkup {
            self.cucx.modules_for_mangle.change_for_internal(bkup);
        }

        result
    }

    fn build_match_on_enum(
        &mut self,
        enum_info: &EnumInfo<'ctx>,
        target: &Value<'ctx>,
        arms: &MatchArmList,
        span: Span,
    ) -> anyhow::Result<Value<'ctx>> {
        let target_offset = self.load_enum_variant_offset_from_value(enum_info.ty, target)?;

        let mut valued_if = self
            .conv_match_arms_on_enum_to_if(
                enum_info,
                target,
                &target_offset,
                arms.non_wildcard_iter(),
                span,
            )?
            .unwrap();

        if let Some(wc) = &arms.wildcard {
            self.add_wildcard(&mut valued_if, wc);
        }

        self.build_if(&valued_if, true)
    }

    /// The return value is `Option` because of the recursion termination condition, so there is no problem if the caller `unwrap`s it
    fn conv_match_arms_on_enum_to_if(
        &mut self,
        enum_info: &EnumInfo<'ctx>,
        target: &Value<'ctx>,
        target_offset: &Value<'ctx>,
        mut arms: Iter<MatchArm>,
        span: Span,
    ) -> anyhow::Result<Option<ValuedIf<'ctx>>> {
        let current_arm = match arms.next() {
            Some(c) => c,
            None => return Ok(None),
        };

        let (cond, param_name, pattern_variant_info) = {
            let (pattern_variant_info, params) =
                self.derive_variant_from_enum_variant_pattern(enum_info, &current_arm.pattern)?;

            let param_name = params.map(|x| match &x.0.front().unwrap().kind {
                ExprKind::Ident(ident) => ident,
                _ => todo!("Error"),
            });

            (
                self.build_int_equal(
                    target_offset.get_value().into_int_value(),
                    self.cucx
                        .context()
                        .i32_type()
                        .const_int(pattern_variant_info.offset as u64, false),
                )?,
                param_name,
                pattern_variant_info,
            )
        };

        let then = Block {
            body: vec![Stmt {
                kind: StmtKind::Expr(current_arm.code.clone()),
                span: current_arm.code.span,
            }],
            span: current_arm.code.span,
        };

        let else_ = self
            .conv_match_arms_on_enum_to_if(enum_info, target, target_offset, arms, span)?
            .map(|if_| Box::new(ValuedElse::If(if_)));

        let enum_unpack = if let Some(param_name) = param_name {
            match &pattern_variant_info.ty {
                Some(ty) => Some(EnumUnpack {
                    name: *param_name,
                    enum_ty: enum_info.ty,
                    enum_value: target.clone(),
                    variant_ty: change_mutability_dup(ty.clone(), target.get_type().mutability),
                    span,
                }),
                None => {
                    return Err(CodegenError::UnitVariantCannotUnpack {
                        unit_variant_name: format!(
                            "{}::{}",
                            enum_info.name.as_str(),
                            pattern_variant_info.name.as_str()
                        )
                        .into(),
                        span: current_arm.pattern.span,
                    }
                    .into())
                }
            }
        } else {
            None
        };

        Ok(Some(ValuedIf {
            cond,
            enum_unpack,
            then: then.into(),
            else_,
            span,
        }))
    }

    fn load_enum_variant_offset_from_value(
        &mut self,
        enum_ty: StructType<'ctx>,
        value: &Value<'ctx>,
    ) -> anyhow::Result<Value<'ctx>> {
        let gep = unsafe {
            self.cucx.builder.build_in_bounds_gep(
                enum_ty,
                value.get_value().into_pointer_value(),
                &[
                    self.cucx.context().i32_type().const_zero(),
                    self.cucx.context().i32_type().const_zero(),
                ],
                "",
            )?
        };

        Ok(Value::new(
            self.cucx
                .builder
                .build_load(self.cucx.context().i32_type(), gep, "")?,
            make_fundamental_type(FundamentalTypeKind::I32, Mutability::Not).into(),
        ))
    }

    fn derive_variant_from_enum_variant_pattern<'pat, 'e>(
        &self,
        enum_info: &'e EnumInfo,
        pattern: &'pat Expr,
    ) -> anyhow::Result<(&'e EnumVariantInfo, Option<&'pat Args>)> {
        let ((module_names, enum_name), variant_name, param) =
            self.decompose_enum_variant_pattern(pattern);

        if let Some(module_names) = module_names {
            let enum_info_external_modules = enum_info.is_external.as_ref().unwrap();
            for (idx, mn) in module_names.iter().enumerate() {
                if mn.as_str() != enum_info_external_modules[idx].as_str() {
                    todo!("Error");
                }
            }
        }

        if enum_info.name != enum_name.symbol() {
            todo!("Error");
        }

        Ok((
            self.get_enum_variant_info_from_name(
                enum_info,
                (variant_name.symbol(), variant_name.span()),
            )?,
            param,
        ))
    }

    fn get_enum_variant_info_from_name<'e>(
        &self,
        enum_info: &'e EnumInfo,
        variant_name: (Symbol, Span),
    ) -> anyhow::Result<&'e EnumVariantInfo> {
        let variant = match enum_info.variants.get(&variant_name.0) {
            Some(item) => item,
            None => {
                return Err(CodegenError::NoVariant {
                    variant_name: variant_name.0,
                    parent_name: enum_info.name,
                    span: variant_name.1,
                }
                .into())
            }
        };

        Ok(variant)
    }

    fn break_(&self, node: &Break) -> anyhow::Result<Value<'ctx>> {
        match self.cucx.loop_break_bb_stk.last() {
            Some(bb) => {
                self.cucx.builder.build_unconditional_branch(*bb)?;
                Ok(Value::new_never())
            }

            None => Err(CodegenError::BreakOutsideOfLoop { span: node.span }.into()),
        }
    }

    fn return_(&mut self, node: &Return) -> anyhow::Result<Value<'ctx>> {
        match &node.val {
            Some(val) => {
                let value = build_expression(self.cucx, val)?;
                self.cucx.builder.build_return(Some(&value.get_value()))?
            }

            None => self.cucx.builder.build_return(None)?,
        };

        Ok(Value::new_never())
    }

    fn loop_(&mut self, node: &Loop) -> anyhow::Result<Value<'ctx>> {
        let parent = self.cucx.get_current_fn();

        let body_bb = self.cucx.context().append_basic_block(parent, "loopbody");

        let cont_bb = self.cucx.context().append_basic_block(parent, "loopcont");

        // Setup for break statement
        self.cucx.loop_break_bb_stk.push(cont_bb);

        // Build body block
        self.cucx.builder.build_unconditional_branch(body_bb)?;
        self.cucx.builder.position_at_end(body_bb);
        build_block(self.cucx, &node.body)?;

        // Loop!
        if self.cucx.no_terminator() {
            self.cucx.builder.build_unconditional_branch(body_bb)?;
        }

        self.cucx.builder.position_at_end(cont_bb);

        Ok(Value::new_never())
    }

    fn conv_to_valued_if(&mut self, orig: &If) -> anyhow::Result<ValuedIf<'ctx>> {
        let cond = build_expression(self.cucx, &orig.cond)?;

        let else_ = match &orig.else_ {
            Some(els) => Some(match els.as_ref() {
                Else::If(if_) => ValuedElse::If(self.conv_to_valued_if(if_)?).into(),
                Else::Block(block) => ValuedElse::Block(block.clone()).into(),
            }),
            None => None,
        };

        Ok(ValuedIf {
            cond,
            enum_unpack: None,
            then: orig.then.clone(),
            else_,
            span: orig.span,
        })
    }

    fn if_(&mut self, node: &If) -> anyhow::Result<Value<'ctx>> {
        let valued_if = self.conv_to_valued_if(node)?;
        self.build_if(&valued_if, false)
    }

    /// If unreachable_else is true and there is no else, build unreachable
    fn build_if(&mut self, node: &ValuedIf, unreachable_else: bool) -> anyhow::Result<Value<'ctx>> {
        let if_value = self.unsafe_build_if(node, unreachable_else)?;

        // All control paths will be 'never'
        if if_value.is_never_ty() {
            return Err(CodegenError::NeverIfExpr { span: node.span }.into());
        }

        Ok(if_value)
    }

    /// Do not use this function! (Use `build_if` instead)
    ///
    /// Because this function is recursive, it is difficult to handle some errors, so it is wrapped in `build_if`
    ///
    /// Therefore, 'unsafe' in the function name is not memory unsafe!
    fn unsafe_build_if(
        &mut self,
        node: &ValuedIf,
        unreachable_else: bool,
    ) -> anyhow::Result<Value<'ctx>> {
        let span = node.span;

        let parent = self.cucx.get_current_fn();
        let zero_const = self.cucx.context().bool_type().const_zero();

        let cond = self.cucx.builder.build_int_compare(
            IntPredicate::NE,
            node.cond.get_value().into_int_value(),
            zero_const,
            "ifcond",
        )?;

        let then_bb = self.cucx.context().append_basic_block(parent, "then");
        let else_bb = self.cucx.context().append_basic_block(parent, "else");
        let cont_bb = self.cucx.context().append_basic_block(parent, "ifcont");

        self.cucx
            .builder
            .build_conditional_branch(cond, then_bb, else_bb)?;

        // Build then block
        self.cucx.builder.position_at_end(then_bb);

        // Enum unpack (optional)
        if let Some(enum_unpack) = &node.enum_unpack {
            // Push symbol table for enum unpack.
            self.cucx.tcx.push_symbol_table(SymbolTable::new());

            self.build_enum_unpack(enum_unpack)?;
        }

        // Build then code
        let then_val = build_block_expression(self.cucx, &node.then)?;

        // Pop symbol table for enum unpack.
        if node.enum_unpack.is_some() {
            self.cucx.tcx.pop_symbol_table();
        }

        // Since there can be no more than one terminator per block
        if self.cucx.no_terminator() {
            self.cucx.builder.build_unconditional_branch(cont_bb)?;
        }

        let then_bb = self.cucx.builder.get_insert_block().unwrap();

        // Build else block
        self.cucx.builder.position_at_end(else_bb);

        // Build else code
        let else_val = match &node.else_ {
            Some(else_) => match else_.as_ref() {
                ValuedElse::If(if_) => self.unsafe_build_if(if_, unreachable_else)?,
                ValuedElse::Block(block) => build_block_expression(self.cucx, block)?,
            },

            _ => {
                // Check if it is used as an expression
                if self.cucx.is_ifmatch_stmt {
                    Value::new_unit()
                } else if unreachable_else {
                    // Build unreachable
                    self.cucx.builder.build_unreachable()?;
                    Value::new_never()
                } else {
                    return Err(CodegenError::IfMustHaveElseUsedAsExpr { span }.into());
                }
            }
        };

        let else_bb = self.cucx.builder.get_insert_block().unwrap();

        // Since there can be no more than one terminator per block
        if self.cucx.no_terminator() {
            self.cucx.builder.build_unconditional_branch(cont_bb)?;
        }

        self.cucx.builder.position_at_end(cont_bb);

        if self.cucx.is_ifmatch_stmt {
            // Not used as a expression
            return Ok(Value::new_unit());
        }

        if !is_same_type(&then_val.get_type(), &else_val.get_type()) {
            return Err(CodegenError::IfAndElseHaveIncompatibleTypes {
                types: (
                    then_val.get_type().kind.to_string(),
                    else_val.get_type().kind.to_string(),
                ),
                span,
            }
            .into());
        }

        // Either then_val or else_val could be never
        let phi_ty = if then_val.is_never_ty() {
            return Ok(else_val);
        } else if else_val.is_never_ty() {
            return Ok(then_val);
        } else {
            then_val.get_type()
        };

        let llvm_phi_ty = self.cucx.conv_to_llvm_type(&phi_ty, span)?;
        let phi = self.cucx.builder.build_phi(llvm_phi_ty, "iftmp")?;

        phi.add_incoming(&[
            (&then_val.get_value(), then_bb),
            (&else_val.get_value(), else_bb),
        ]);

        Ok(Value::new(phi.as_basic_value(), phi_ty))
    }

    fn build_enum_unpack(&mut self, enum_unpack: &EnumUnpack) -> anyhow::Result<()> {
        let variant_ty_llvm = self
            .cucx
            .conv_to_llvm_type(&enum_unpack.variant_ty, enum_unpack.span)?;

        // { i32, [N x i8] }
        //        ^^^^^^^^
        let gep = unsafe {
            self.cucx.builder.build_in_bounds_gep(
                enum_unpack.enum_ty,
                enum_unpack.enum_value.get_value().into_pointer_value(),
                &[
                    self.cucx.context().i32_type().const_zero(),
                    self.cucx.context().i32_type().const_int(1_u64, false),
                ],
                "",
            )?
        };

        let variant_value = Value::new(
            self.cucx.builder.build_load(variant_ty_llvm, gep, "")?,
            enum_unpack.variant_ty.clone(),
        );

        build_normal_let(
            self.cucx,
            &enum_unpack.name,
            enum_unpack.variant_ty.mutability,
            Some(variant_value),
            Rc::new(Ty::new_inferred(Mutability::Not)),
            enum_unpack.span,
        )?;

        Ok(())
    }

    fn struct_literal(&mut self, node: &StructLiteral) -> anyhow::Result<Value<'ctx>> {
        // Prepare for external types
        let bkup = if !node.external_modules.is_empty() {
            Some(
                self.cucx
                    .modules_for_mangle
                    .change_for_external(node.external_modules.clone()),
            )
        } else {
            None
        };

        let mangled_name = mangle_udt_name(self.cucx, &node.struct_ty);

        let struct_ty = Ty {
            kind: TyKind::UserDefined(node.struct_ty.clone()).into(),
            mutability: Mutability::Not,
        };

        // If this is a generic type, the type is generated here.
        let struct_llvm_ty = self.cucx.conv_to_llvm_type(&struct_ty, node.span)?;

        let symbol_kind = self.cucx.tcx.lookup_symbol(mangled_name, node.span)?;
        let borrowed_symbol_kind = symbol_kind.borrow();
        let struct_info = match &*borrowed_symbol_kind {
            SymbolTableValue::Struct(info) => info,

            _ => {
                return Err(CodegenError::Undeclared {
                    name: node.struct_ty.name.symbol(),
                    span: node.struct_ty.name.span(),
                }
                .into());
            }
        };

        let struct_ty = Rc::new(Ty {
            kind: struct_ty.kind.clone(),
            mutability: struct_ty.mutability,
        });

        let struct_ty = wrap_in_ref(struct_ty, Mutability::Mut).into();

        // Wrapping with external types.
        let struct_ty = if let Some(externals) = &struct_info.is_external {
            Ty::wrap_in_externals(struct_ty, externals)
        } else {
            struct_ty
        };

        let mut values = Vec::new();

        for value in node.values.iter() {
            let field_info = &struct_info.fields[&value.0.symbol()];

            let value = build_expression(self.cucx, &value.1)?;

            // To sort by offset, store offset
            values.push((field_info.offset, value.get_value()));
        }

        // Sort in ascending order based on offset
        values.sort_by(|a, b| a.0.cmp(&b.0));

        // Remove offsets
        let inits: Vec<_> = values.iter().map(|e| e.1).collect();

        let value = Value::new(
            create_gc_struct(self.cucx, struct_llvm_ty, &inits)?.into(),
            struct_ty,
        );

        if let Some(bkup) = bkup {
            self.cucx.modules_for_mangle.change_for_internal(bkup);
        }

        Ok(value)
    }

    fn tuple_literal(&mut self, node: &TupleLiteral) -> anyhow::Result<Value<'ctx>> {
        let element_values = {
            let mut v = Vec::new();
            for e in node.elements.iter() {
                v.push(build_expression(self.cucx, e)?);
            }
            v
        };

        let tuple_ty = Ty {
            kind: TyKind::Tuple(element_values.iter().map(|v| v.get_type()).collect()).into(),
            mutability: Mutability::Not,
        };

        let tuple_llvm_ty = self.cucx.conv_to_llvm_type(&tuple_ty, node.span)?;

        Ok(Value::new(
            create_gc_struct(
                self.cucx,
                tuple_llvm_ty,
                element_values
                    .iter()
                    .map(|v| v.get_value())
                    .collect::<Vec<_>>()
                    .as_slice(),
            )?
            .into(),
            wrap_in_ref(tuple_ty.into(), Mutability::Mut).into(),
        ))
    }

    fn array_literal(&mut self, node: &ArrayLiteral) -> anyhow::Result<Value<'ctx>> {
        assert!(!node.elements.is_empty());

        let mut elems = Vec::new();

        // Compile elements
        for elem in node.elements.iter() {
            elems.push(build_expression(self.cucx, elem)?);
        }

        let array_ty = Rc::new(Ty {
            kind: TyKind::Array((elems[0].get_type(), elems.len() as u32)).into(),
            mutability: Mutability::Not,
        });

        let array_llvm_ty = self.cucx.conv_to_llvm_type(&array_ty, node.span)?;

        let mallocd = self.cucx.gc_malloc(array_llvm_ty)?;

        for (idx, elem) in elems.iter().enumerate() {
            let gep = unsafe {
                self.cucx.builder.build_in_bounds_gep(
                    array_llvm_ty,
                    mallocd,
                    &[
                        self.cucx.context().i32_type().const_zero(),
                        self.cucx.context().i32_type().const_int(idx as u64, false),
                    ],
                    "",
                )?
            };

            self.cucx.builder.build_store(gep, elem.get_value())?;
        }

        Ok(Value::new(
            mallocd.into(),
            Ty {
                kind: TyKind::Reference(ReferenceType {
                    refee_ty: array_ty.clone(),
                })
                .into(),
                mutability: Mutability::Mut,
            }
            .into(),
        ))
    }

    fn array_indexing(&mut self, node: &Indexing) -> anyhow::Result<Value<'ctx>> {
        // A raw array cannot be passed, but a pointer(reference) to an array
        let array_ref = build_expression(self.cucx, &node.operand)?;

        let array_ref_ty = array_ref.get_type();

        let (array_ty, elem_ty) = {
            match array_ref_ty.kind.as_ref() {
                TyKind::Reference(rty) => {
                    if let TyKind::Array((elem_ty, _)) = rty.refee_ty.kind.as_ref() {
                        (rty.refee_ty.clone(), elem_ty.clone())
                    } else {
                        todo!("Error");
                    }
                }

                TyKind::Generic(gty) => match self.cucx.tcx.lookup_generic_arg(gty.name.symbol()) {
                    Some(ga) => match ga.kind.as_ref() {
                        TyKind::Reference(rty) => match rty.refee_ty.kind.as_ref() {
                            TyKind::Array((elem_ty, _)) => (rty.refee_ty.clone(), elem_ty.clone()),
                            _ => todo!("Error"),
                        },
                        _ => todo!("Error"),
                    },
                    None => todo!("Error"),
                },

                _ => unreachable!(),
            }
        };

        let array_llvm_ty = self
            .cucx
            .conv_to_llvm_type(&array_ty, node.span)?
            .into_array_type();

        let ptr_to_array = array_ref.get_value().into_pointer_value();

        let index = build_expression(self.cucx, &node.index)?;

        // Calculate the address of the index-th element
        let gep = unsafe {
            self.cucx.builder.build_in_bounds_gep(
                array_llvm_ty,
                ptr_to_array,
                &[
                    self.cucx.context().i32_type().const_zero(),
                    index.get_value().into_int_value(),
                ],
                "",
            )?
        };

        Ok(Value::new(
            self.cucx
                .builder
                .build_load(array_llvm_ty.get_element_type(), gep, "")?,
            Ty {
                kind: elem_ty.kind.clone(),
                mutability: array_ref_ty.mutability,
            }
            .into(),
        ))
    }

    fn logical_not(&mut self, node: &LogicalNot) -> anyhow::Result<Value<'ctx>> {
        let operand = build_expression(self.cucx, &node.operand)?;

        let zero = self
            .cucx
            .conv_to_llvm_type(&operand.get_type(), node.span)?
            .const_zero();

        // Compared to zero, it would be equivalent to 'logical not'
        Ok(Value::new(
            self.cucx
                .builder
                .build_int_compare(
                    IntPredicate::EQ,
                    operand.get_value().into_int_value(),
                    zero.into_int_value(),
                    "",
                )?
                .into(),
            Rc::new(make_fundamental_type(
                FundamentalTypeKind::Bool,
                Mutability::Not,
            )),
        ))
    }

    fn boolean_literal(&self, value: bool) -> Value<'ctx> {
        Value::new(
            self.cucx
                .context()
                .bool_type()
                .const_int(value as u64, false)
                .into(),
            Rc::new(make_fundamental_type(
                FundamentalTypeKind::Bool,
                Mutability::Not,
            )),
        )
    }

    fn string_literal(&mut self, node: &StringLiteral) -> anyhow::Result<Value<'ctx>> {
        let global_s = self
            .cucx
            .builder
            .build_global_string_ptr(node.syb.as_str(), "str")?;

        let str_ty = Rc::new(make_fundamental_type(
            FundamentalTypeKind::Str,
            Mutability::Not,
        ));

        let str_llvm_ty = self.cucx.conv_to_llvm_type(&str_ty, node.span).unwrap();

        let p = create_gc_struct(
            self.cucx,
            str_llvm_ty,
            &[
                global_s.as_basic_value_enum(),
                self.cucx
                    .context()
                    .i64_type()
                    .const_int(node.syb.as_str().len() as u64, false)
                    .into(),
            ],
        )?;

        Ok(Value::new(
            p.into(),
            wrap_in_ref(str_ty, Mutability::Not).into(),
        ))
    }

    fn ident_expr(&mut self, ident: &Ident) -> anyhow::Result<Value<'ctx>> {
        let symbol_kind = self.cucx.tcx.lookup_symbol(ident.symbol(), ident.span())?;
        let (ptr, ty) = match &*symbol_kind.borrow() {
            SymbolTableValue::Variable(var) => (var.0, var.1.clone()),
            _ => {
                return Err(CodegenError::Undeclared {
                    name: ident.symbol(),
                    span: ident.span(),
                }
                .into())
            }
        };

        let llvm_ty = self.cucx.conv_to_llvm_type(&ty, ident.span())?;

        Ok(Value::new(
            self.cucx.builder.build_load(llvm_ty, ptr, "")?,
            ty.clone(),
        ))
    }

    fn binary_op(&mut self, node: &Binary) -> anyhow::Result<Value<'ctx>> {
        use BinaryKind::*;

        match node.kind {
            Access => self.access(node),

            ScopeResolution => self.scope_resolution(node),

            Cast => self.cast(node),

            _ => self.binary_arithmetic_op(node),
        }
    }

    fn cast(&mut self, node: &Binary) -> anyhow::Result<Value<'ctx>> {
        assert!(matches!(node.kind, BinaryKind::Cast));

        let cast_ty = match &node.rhs.kind {
            ExprKind::Ty(ty) => ty,
            _ => todo!("Error"),
        };

        let value = build_expression(self.cucx, &node.lhs)?;

        if value.get_type().kind.is_int_or_bool() && cast_ty.kind.is_int_or_bool() {
            self.build_int_cast(value, cast_ty.clone(), node.lhs.span)
        } else if matches!(
            value.get_type().kind.as_ref(),
            TyKind::Reference(_) | TyKind::Pointer(_)
        ) && matches!(
            cast_ty.kind.as_ref(),
            TyKind::Reference(_) | TyKind::Pointer(_)
        ) {
            self.build_ptr_cast(value, cast_ty.clone(), node.lhs.span)
        } else {
            todo!("Error");
        }
    }

    fn build_int_cast(
        &mut self,
        value: Value<'ctx>,
        cast_ty: Rc<Ty>,
        span: Span,
    ) -> anyhow::Result<Value<'ctx>> {
        assert!(value.get_type().kind.is_int_or_bool());
        assert!(cast_ty.kind.is_int_or_bool());

        let cast_llvm_ty = self.cucx.conv_to_llvm_type(&cast_ty, span)?;

        Ok(Value::new(
            self.cucx
                .builder
                .build_int_cast_sign_flag(
                    value.get_value().into_int_value(),
                    cast_llvm_ty.into_int_type(),
                    cast_ty.kind.is_signed(),
                    "",
                )?
                .as_basic_value_enum(),
            cast_ty,
        ))
    }

    fn build_ptr_cast(
        &mut self,
        value: Value<'ctx>,
        cast_ty: Rc<Ty>,
        span: Span,
    ) -> anyhow::Result<Value<'ctx>> {
        let cast_llvm_ty = self.cucx.conv_to_llvm_type(&cast_ty, span)?;

        Ok(Value::new(
            self.cucx
                .builder
                .build_pointer_cast(
                    value.get_value().into_pointer_value(),
                    cast_llvm_ty.into_pointer_type(),
                    "",
                )?
                .as_basic_value_enum(),
            cast_ty,
        ))
    }

    fn scope_resolution(&mut self, node: &Binary) -> anyhow::Result<Value<'ctx>> {
        assert!(matches!(node.kind, BinaryKind::ScopeResolution));

        let (left, modules_bkup, generic_args) = match &node.lhs.kind {
            ExprKind::Ident(ident) => (ident, None, None),
            ExprKind::ExternalIdent(eident) => {
                // Prepare for external types
                let bkup = self
                    .cucx
                    .modules_for_mangle
                    .change_for_external(eident.external_modules.clone());
                (&eident.ident, Some(bkup), eident.generic_args.clone())
            }
            ExprKind::GenericIdent(gident) => (&gident.0, None, Some(gident.1.clone())),
            _ => todo!(),
        };

        let udt = if generic_args.is_some() {
            let udt = UserDefinedType {
                name: *left,
                generic_args: generic_args.clone(),
            };
            let ty = Ty {
                kind: TyKind::UserDefined(udt.clone()).into(),
                mutability: Mutability::Not,
            };
            // Generate methods for generic types.
            self.cucx.conv_to_llvm_type(&ty, left.span())?;
            udt
        } else {
            UserDefinedType {
                name: *left,
                generic_args: None,
            }
        };

        // Try to create new enum variant with value
        // If it fails, try to call static methods
        let result = if let ExprKind::FnCall(right) = &node.rhs.kind {
            if right.args.0.len() != 1 {
                // Static methods
                self.build_static_method_call(&udt, right)
            } else {
                let value = right.args.0.front().unwrap();

                match self.create_enum_variant(
                    left,
                    &right.callee,
                    Some(value),
                    generic_args,
                    right.span,
                ) {
                    Ok(val) => Ok(val),
                    Err(err) => {
                        err.downcast().and_then(|e| match e {
                            CodegenError::Undeclared { name, .. }
                            | CodegenError::NoVariant {
                                parent_name: name, ..
                            } if name == left.symbol() => {
                                // Couldn't create enum variant, so try to call static methods
                                self.build_static_method_call(&udt, right)
                            }
                            _ => Err(e.into()),
                        })
                    }
                }
            }
        } else if let ExprKind::Ident(right) = &node.rhs.kind {
            // Create enum variant without value.
            self.create_enum_variant(left, right, None, generic_args, right.span())
        } else {
            todo!()
        };

        if let Some(bkup) = modules_bkup {
            self.cucx.modules_for_mangle.change_for_internal(bkup);
        }

        result
    }

    fn build_static_method_call(
        &mut self,
        udt: &UserDefinedType,
        call: &FnCall,
    ) -> anyhow::Result<Value<'ctx>> {
        let symbol_kind = self
            .cucx
            .tcx
            .lookup_symbol(mangle_name(self.cucx, udt.name.symbol()).into(), call.span)?;

        let mangled_udt_name = match &*symbol_kind.borrow() {
            SymbolTableValue::Enum(enum_info) => enum_info.mangled_name,
            SymbolTableValue::Struct(struct_info) => struct_info.mangled_name,
            SymbolTableValue::Generic(generic_info) => {
                let bkup = if let Some(externals) = &generic_info.is_external {
                    Some(
                        self.cucx
                            .modules_for_mangle
                            .change_for_external(externals.clone()),
                    )
                } else {
                    None
                };

                let mangled = match generic_info.kind {
                    GenericKind::Enum(_) | GenericKind::Struct(_) => {
                        mangle_udt_name(self.cucx, udt)
                    }
                    _ => unimplemented!(),
                };

                if let Some(bkup) = bkup {
                    self.cucx.modules_for_mangle.change_for_internal(bkup);
                }

                mangled
            }
            _ => unimplemented!(),
        };

        let mangled_method_name = format!("{}::{}", mangled_udt_name, call.callee.as_str());

        // Convert arguments(exprs) to values
        let args = {
            let mut args = VecDeque::new();

            for arg in call.args.0.iter() {
                args.push_back((self.build(arg)?, arg.span));
            }

            args
        };

        self.build_call_fn_without_mangle(mangled_method_name.into(), &args, call.span)
    }

    fn create_enum_variant(
        &mut self,
        enum_name: &Ident,
        variant_name: &Ident,
        value: Option<&Expr>,
        generic_args: Option<GenericArgs>,
        span: Span,
    ) -> anyhow::Result<Value<'ctx>> {
        let enum_udt = UserDefinedType {
            name: *enum_name,
            generic_args,
        };

        let mangled_name = mangle_udt_name(self.cucx, &enum_udt);

        let enum_ty = Ty {
            kind: TyKind::UserDefined(enum_udt).into(),
            mutability: Mutability::Not,
        };

        // If this is a generic type, the type is generated here.
        let enum_llvm_ty = self.cucx.conv_to_llvm_type(&enum_ty, span)?;

        let symbol_kind = self.cucx.tcx.lookup_symbol(mangled_name, span)?;
        let borrowed_symbol_kind = symbol_kind.borrow();
        let enum_info = match &*borrowed_symbol_kind {
            SymbolTableValue::Enum(enum_info) => enum_info,
            _ => {
                return Err(CodegenError::Undeclared {
                    name: enum_name.symbol(),
                    span: enum_name.span(),
                }
                .into())
            }
        };

        let variant_offset = self
            .get_enum_variant_info_from_name(
                enum_info,
                (variant_name.symbol(), variant_name.span()),
            )?
            .offset;

        let enum_ty = wrap_in_ref(enum_ty.into(), Mutability::Mut).into();

        let enum_ty = if let Some(externals) = &enum_info.is_external {
            Ty::wrap_in_externals(enum_ty, externals)
        } else {
            enum_ty
        };

        let offset_in_llvm = self
            .cucx
            .context()
            .i32_type()
            .const_int(variant_offset as u64, true)
            .into();

        let inits = match value {
            Some(v) => vec![offset_in_llvm, self.build(v)?.get_value()],
            None => vec![offset_in_llvm],
        };

        Ok(Value::new(
            create_gc_struct(self.cucx, enum_llvm_ty, &inits)?.into(),
            enum_ty,
        ))
    }

    fn logical_or(&self, b1: IntValue<'ctx>, b2: IntValue<'ctx>) -> anyhow::Result<Value<'ctx>> {
        let bool_type = self.cucx.context().bool_type();

        let result = self.cucx.builder.build_or(b1, b2, "")?;

        let zero = bool_type.const_zero();
        let cmp =
            self.cucx
                .builder
                .build_int_compare(inkwell::IntPredicate::EQ, result, zero, "")?;
        let result = self
            .cucx
            .builder
            .build_select(cmp, zero, bool_type.const_all_ones(), "")?;

        Ok(Value::new(
            result,
            Rc::new(make_fundamental_type(
                FundamentalTypeKind::Bool,
                Mutability::Not,
            )),
        ))
    }

    fn logical_and(&self, b1: IntValue<'ctx>, b2: IntValue<'ctx>) -> anyhow::Result<Value<'ctx>> {
        let bool_type = self.cucx.context().bool_type();

        let result = self.cucx.builder.build_and(b1, b2, "")?;

        let zero = bool_type.const_zero();
        let cmp =
            self.cucx
                .builder
                .build_int_compare(inkwell::IntPredicate::EQ, result, zero, "")?;
        let result = self
            .cucx
            .builder
            .build_select(cmp, zero, bool_type.const_all_ones(), "")?;

        Ok(Value::new(
            result,
            Rc::new(make_fundamental_type(
                FundamentalTypeKind::Bool,
                Mutability::Not,
            )),
        ))
    }

    fn build_int_equal(
        &self,
        left: IntValue<'ctx>,
        right: IntValue<'ctx>,
    ) -> anyhow::Result<Value<'ctx>> {
        Ok(Value::new(
            self.cucx
                .builder
                .build_int_compare(IntPredicate::EQ, left, right, "")?
                .into(),
            Rc::new(make_fundamental_type(
                FundamentalTypeKind::Bool,
                Mutability::Not,
            )),
        ))
    }

    fn binary_arithmetic_op(&mut self, node: &Binary) -> anyhow::Result<Value<'ctx>> {
        use BinaryKind::*;

        let left = self.build(node.lhs.as_ref())?;
        let right = self.build(node.rhs.as_ref())?;

        let left_value = left.get_value();
        let right_value = right.get_value();

        // If operands are not int
        if !(left_value.is_int_value() && right_value.is_int_value()) {
            todo!("Error");
        }

        let left_int = left_value.into_int_value();
        let right_int = right_value.into_int_value();

        Ok(match node.kind {
            LogicalOr => self.logical_or(left_int, right_int)?,

            LogicalAnd => self.logical_and(left_int, right_int)?,

            Add => Value::new(
                self.cucx
                    .builder
                    .build_int_add(left_int, right_int, "")?
                    .into(),
                left.get_type(),
            ),

            Sub => Value::new(
                self.cucx
                    .builder
                    .build_int_sub(left_int, right_int, "")?
                    .into(),
                left.get_type(),
            ),

            Mul => Value::new(
                self.cucx
                    .builder
                    .build_int_mul(left_int, right_int, "")?
                    .into(),
                left.get_type(),
            ),

            Rem => {
                if has_signed(&[&left, &right]) {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_signed_rem(left_int, right_int, "")?
                            .into(),
                        left.get_type(),
                    )
                } else {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_unsigned_rem(left_int, right_int, "")?
                            .into(),
                        left.get_type(),
                    )
                }
            }

            Div => {
                if has_signed(&[&left, &right]) {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_signed_div(left_int, right_int, "")?
                            .into(),
                        left.get_type(),
                    )
                } else {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_unsigned_div(left_int, right_int, "")?
                            .into(),
                        left.get_type(),
                    )
                }
            }

            Eq => self.build_int_equal(left_int, right_int)?,

            Ne => Value::new(
                self.cucx
                    .builder
                    .build_int_compare(IntPredicate::NE, left_int, right_int, "")?
                    .into(),
                Rc::new(make_fundamental_type(
                    FundamentalTypeKind::Bool,
                    Mutability::Not,
                )),
            ),

            Lt => {
                if has_signed(&[&left, &right]) {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::SLT, left_int, right_int, "")?
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                } else {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::ULT, left_int, right_int, "")?
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                }
            }

            Le => {
                if has_signed(&[&left, &right]) {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::SLE, left_int, right_int, "")?
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                } else {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::ULE, left_int, right_int, "")?
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                }
            }

            Gt => {
                if has_signed(&[&left, &right]) {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::SGT, left_int, right_int, "")?
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                } else {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::UGT, left_int, right_int, "")?
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                }
            }

            Ge => {
                if has_signed(&[&left, &right]) {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::SGE, left_int, right_int, "")?
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                } else {
                    Value::new(
                        self.cucx
                            .builder
                            .build_int_compare(IntPredicate::UGE, left_int, right_int, "")?
                            .into(),
                        Rc::new(make_fundamental_type(
                            FundamentalTypeKind::Bool,
                            Mutability::Not,
                        )),
                    )
                }
            }

            Access => unreachable!(),
            ScopeResolution => unreachable!(),
            Cast => unreachable!(),
        })
    }

    /// Struct access or module item access
    fn access(&mut self, node: &Binary) -> anyhow::Result<Value<'ctx>> {
        assert!(matches!(node.kind, BinaryKind::Access));

        let left = build_expression(self.cucx, &node.lhs)?;

        let left_ty = left.get_type();

        if left_ty.is_str() {
            return self.str_indexing_or_method_call(&left, &node.rhs, &left_ty);
        }

        // Prepare for external types
        let (bkup, left_ty) = if let TyKind::External(ety) = left_ty.kind.as_ref() {
            (
                Some(
                    self.cucx
                        .modules_for_mangle
                        .change_for_external(ety.get_module_names_recursively()),
                ),
                ety.get_base_type(),
            )
        } else {
            (None, left_ty.clone())
        };

        // Unwrap externals
        let left = Value::new(left.get_value(), left_ty.clone());

        let result = if let TyKind::Reference(rty) = left_ty.kind.as_ref() {
            // ---  Struct access or tuple indexing ---
            if matches!(
                rty.get_base_type().kind.as_ref(),
                TyKind::UserDefined(_) | TyKind::Tuple(_)
            ) {
                self.struct_access_or_tuple_indexing(&left, node.lhs.span, &node.rhs)
            } else {
                return Err(CodegenError::HasNoFields {
                    span: node.lhs.span,
                }
                .into());
            }
        } else if let TyKind::Generic(gty) = left_ty.kind.as_ref() {
            let generic_arg_type = self.cucx.tcx.lookup_generic_arg(gty.name.symbol()).unwrap();

            // GenericArg(type) -> type
            let left = Value::new(left.get_value(), generic_arg_type);
            self.struct_access_or_tuple_indexing(&left, node.lhs.span, &node.rhs)
        } else {
            // --- Fundamental type method calling ---
            let call_node = match &node.rhs.kind {
                ExprKind::FnCall(call_node) => call_node,
                _ => {
                    return Err(CodegenError::HasNoFields {
                        span: node.lhs.span,
                    }
                    .into())
                }
            };

            if let TyKind::Fundamental(fty) = left_ty.kind.as_ref() {
                self.call_fundamental_type_method(&left, fty.kind.to_string().into(), call_node)
            } else {
                Err(CodegenError::HasNoFields {
                    span: node.lhs.span,
                }
                .into())
            }
        };

        if let Some(bkup) = bkup {
            self.cucx.modules_for_mangle.change_for_internal(bkup);
        }

        result
    }

    fn call_fundamental_type_method(
        &mut self,
        value: &Value<'ctx>,
        fundamental_ty_name: Symbol,
        call_node: &FnCall,
    ) -> anyhow::Result<Value<'ctx>> {
        // For example: i32.add
        let method_name = format!("{}.{}", fundamental_ty_name, call_node.callee.as_str());

        // Convert arguments(exprs) to values
        let mut args = {
            let mut args = VecDeque::new();

            for arg in call_node.args.0.iter() {
                args.push_back((self.build(arg)?, arg.span));
            }

            args
        };

        // Push self to front
        args.push_front((value.clone(), call_node.args.1));

        self.build_call_fn_without_mangle(method_name.into(), &args, call_node.span)
    }

    fn struct_access_or_tuple_indexing(
        &mut self,
        left: &Value<'ctx>,
        left_span: Span,
        right: &Expr,
    ) -> anyhow::Result<Value<'ctx>> {
        assert!(matches!(
            left.get_type().kind.as_ref(),
            TyKind::Reference(_)
        ));

        let left_ty = &left.get_type();

        let base_type = match left_ty.kind.as_ref() {
            TyKind::Reference(rty) => rty.get_base_type(),
            _ => unreachable!(),
        };

        match base_type.kind.as_ref() {
            TyKind::UserDefined(_) => self.struct_access(left, right, &base_type, left_span),

            TyKind::Tuple(_) => self.tuple_indexing(left, right, &base_type),

            _ => Err(CodegenError::HasNoFields { span: left_span }.into()),
        }
    }

    /// Field access or method access
    fn struct_access(
        &mut self,
        left: &Value<'ctx>,
        right: &Expr,
        struct_ty: &Rc<Ty>,
        left_span: Span,
    ) -> anyhow::Result<Value<'ctx>> {
        let (udt, mangled_struct_name) = match struct_ty.kind.as_ref() {
            TyKind::UserDefined(udt) => (udt, mangle_udt_name(self.cucx, udt)),
            _ => unreachable!(),
        };

        match &right.kind {
            // Field
            ExprKind::Ident(field_name) => self.struct_field_access(
                left,
                udt.name.symbol(),
                mangled_struct_name,
                struct_ty,
                field_name,
                left_span,
            ),

            // Method
            ExprKind::FnCall(node) => {
                self.build_struct_method_call(left, mangled_struct_name, node)
            }

            kind => unreachable!("{:?}", kind),
        }
    }

    fn struct_field_access(
        &mut self,
        struct_value: &Value<'ctx>,
        struct_name: Symbol,
        mangled_struct_name: Symbol,
        struct_ty: &Rc<Ty>,
        field_name: &Ident,
        span: Span,
    ) -> anyhow::Result<Value<'ctx>> {
        let llvm_struct_ty = self.cucx.conv_to_llvm_type(struct_ty, span)?;

        let symbol_kind = self.cucx.tcx.lookup_symbol(mangled_struct_name, span)?;
        let borrowed_symbol_kind = symbol_kind.borrow();
        let struct_info = {
            match &*borrowed_symbol_kind {
                SymbolTableValue::Struct(t) => t,
                kind => unreachable!("{:?}", kind),
            }
        };

        let field_info = match struct_info.fields.get(&field_name.symbol()) {
            Some(field) => field,
            None => {
                return Err(CodegenError::NoMember {
                    member_name: field_name.symbol(),
                    parent_name: struct_name,
                    span: field_name.span(),
                }
                .into());
            }
        };

        let offset = field_info.offset;

        let gep = unsafe {
            self.cucx.builder.build_in_bounds_gep(
                llvm_struct_ty,
                struct_value.get_value().into_pointer_value(),
                &[
                    self.cucx.context().i32_type().const_zero(),
                    self.cucx.context().i32_type().const_int(offset, false),
                ],
                "",
            )?
        };

        let llvm_field_ty = self
            .cucx
            .conv_to_llvm_type(&field_info.ty, field_name.span())?;

        let field_ty = Rc::new(Ty {
            kind: field_info.ty.kind.clone(),
            mutability: struct_ty.mutability,
        });

        let field_ty = if let Some(externals) = &struct_info.is_external {
            if field_ty.is_udt() {
                Ty::wrap_in_externals(field_ty, externals)
            } else {
                field_ty
            }
        } else {
            field_ty
        };

        Ok(Value::new(
            self.cucx.builder.build_load(llvm_field_ty, gep, "")?,
            field_ty,
        ))
    }

    fn build_struct_method_call(
        &mut self,
        struct_value: &Value<'ctx>,
        mangled_struct_name: Symbol,
        call_node: &FnCall,
    ) -> anyhow::Result<Value<'ctx>> {
        // For example: test.Person.get_age
        let mangled_method_name = format!("{}.{}", mangled_struct_name, call_node.callee.as_str());

        // Convert arguments(exprs) to values
        let mut args = {
            let mut args = VecDeque::new();

            for arg in call_node.args.0.iter() {
                args.push_back((self.build(arg)?, arg.span));
            }

            args
        };

        // Push self to front
        args.push_front((struct_value.clone(), call_node.args.1));

        self.build_call_fn_without_mangle(mangled_method_name.into(), &args, call_node.span)
    }

    fn str_indexing_or_method_call(
        &mut self,
        left: &Value<'ctx>,
        right: &Expr,
        str_ty: &Rc<Ty>,
    ) -> anyhow::Result<Value<'ctx>> {
        match &right.kind {
            // Method call
            ExprKind::FnCall(node) => {
                let method_name = format!("str.{}", node.callee.symbol());

                let mut args = {
                    let mut args = VecDeque::new();

                    for arg in node.args.0.iter() {
                        args.push_back((self.build(arg)?, arg.span));
                    }

                    args
                };

                args.push_front((left.clone(), node.args.1));

                self.build_call_fn_without_mangle(method_name.into(), &args, node.span)
            }

            _ => self.str_indexing(left, right, str_ty),
        }
    }

    fn str_indexing(
        &mut self,
        left: &Value<'ctx>,
        right: &Expr,
        str_ty: &Rc<Ty>,
    ) -> anyhow::Result<Value<'ctx>> {
        self.tuple_indexing_internal(left, right, str_ty, true)
    }

    fn tuple_indexing(
        &mut self,
        left: &Value<'ctx>,
        right: &Expr,
        tuple_ty: &Rc<Ty>,
    ) -> anyhow::Result<Value<'ctx>> {
        self.tuple_indexing_internal(left, right, tuple_ty, false)
    }

    fn tuple_indexing_internal(
        &mut self,
        left: &Value<'ctx>,
        right: &Expr,
        tuple_ty: &Rc<Ty>,
        is_str: bool,
    ) -> anyhow::Result<Value<'ctx>> {
        let index = match &right.kind {
            ExprKind::Int(i) => i.as_u64(),
            _ => return Err(CodegenError::TupleRequireAccessByIndex { span: right.span }.into()),
        };

        let left_value = left.get_value().into_pointer_value();

        if is_str {
            build_str_indexing(self.cucx, left_value, index as u32, tuple_ty, right.span)
        } else {
            build_tuple_indexing(self.cucx, left_value, index as u32, tuple_ty, right.span)
        }
    }

    /// Returns the name of the function generated.
    fn define_generic_fn_instance(
        &mut self,
        name: Symbol,
        ast_vis: &(Fn, Visibility),
        args: &VecDeque<(Value<'ctx>, Span)>,
        span: Span,
    ) -> anyhow::Result<Symbol> {
        let ast = &ast_vis.0;

        assert!(ast.decl.generic_params.is_some());

        let generic_params = ast.decl.generic_params.as_ref().unwrap();

        if generic_params.names.len() != args.len() {
            todo!("Error");
        }

        let generic_args = GenericArgs {
            types: args.iter().map(|a| a.0.get_type()).collect(),
            span,
        };

        let mangled_name = mangle_generic_fn_name(self.cucx, name, &generic_args);

        // Skip if the function is already defined.
        let symbol_kind = self.cucx.tcx.lookup_symbol(mangled_name, span);
        if symbol_kind.is_ok()
            && matches!(
                *symbol_kind.unwrap().borrow(),
                SymbolTableValue::Function(_)
            )
        {
            return Ok(mangled_name);
        }

        let generic_fn_decl_ast = FnDecl {
            name: ast.decl.name,
            self_: ast.decl.self_,

            // Pass None to create a generic function instance.
            generic_params: None,

            params: ast.decl.params.clone(),
            return_ty: ast.decl.return_ty.clone(),
            span,
        };

        let generic_fn_ast = Fn {
            decl: generic_fn_decl_ast,
            body: ast.body.clone(),
            span,
        };

        let generic_fn_instance = GenericFnInstance {
            mangled_name,
            fn_: generic_fn_ast,
        };

        let top = TopLevel {
            kind: TopLevelKind::GenericFnInstance(generic_fn_instance),
            vis: ast_vis.1,
            span,
        };

        // Only declare the function, not define it.
        // If this function is defined now, for some reason we get an error when verifying modules.
        self.cucx.tcx.push_generic_arg_table(GenericArgTable::from((
            generic_params.clone(),
            generic_args.clone(),
        )));

        self.cucx.declare_fn(
            mangled_name,
            ast.decl
                .params
                .v
                .iter()
                .map(|e| e.ty.clone())
                .collect::<Vec<_>>(),
            ast.decl.return_ty.clone().into(),
            Some(Linkage::External),
            false,
            span,
        )?;

        self.cucx.tcx.pop_generic_arg_table();

        self.cucx
            .lazy_defines
            .push((top, generic_params.clone(), generic_args));

        Ok(mangled_name)
    }

    fn build_call_generic_fn(
        &mut self,
        name: Symbol,
        args: &VecDeque<(Value<'ctx>, Span)>,
        span: Span,
        generic_kind: &GenericKind,
    ) -> anyhow::Result<Value<'ctx>> {
        let fn_ast_vis = match generic_kind {
            GenericKind::Func(ast) => ast,
            _ => unreachable!(),
        };

        let mangled_name = self.define_generic_fn_instance(name, fn_ast_vis, args, span)?;

        self.build_call_fn_without_mangle(mangled_name, args, span)
    }

    fn fn_call(&mut self, node: &FnCall) -> anyhow::Result<Value<'ctx>> {
        // Prepare for external types
        let bkup = if !node.external_modules.is_empty() {
            Some(
                self.cucx
                    .modules_for_mangle
                    .change_for_external(node.external_modules.clone()),
            )
        } else {
            None
        };

        let args = {
            let mut args = VecDeque::new();

            for arg in node.args.0.iter() {
                args.push_back((self.build(arg)?, arg.span));
            }

            args
        };

        let mangled_name = mangle_name(self.cucx, node.callee.symbol()).into();

        let symbol_kind = self.cucx.tcx.lookup_symbol(mangled_name, node.span);

        let evaled = match symbol_kind {
            Ok(kind) => match &*kind.borrow() {
                SymbolTableValue::Generic(info) => {
                    // Generic function
                    self.build_call_generic_fn(node.callee.symbol(), &args, node.span, &info.kind)
                }
                SymbolTableValue::Function(_) => {
                    // Normal function
                    self.build_call_fn_without_mangle(mangled_name, &args, node.span)
                }
                _ => todo!("Error"),
            },

            Err(err) => {
                // FFI function
                self.build_call_fn_without_mangle(node.callee.symbol(), &args, node.span)
                    .map_err(|_| err)
            }
        };

        if let Some(bkup) = bkup {
            self.cucx.modules_for_mangle.change_for_internal(bkup);
        }

        evaled
    }

    fn build_call_fn_without_mangle(
        &mut self,
        mangled_name: Symbol,
        args: &VecDeque<(Value<'ctx>, Span)>,
        span: Span,
    ) -> anyhow::Result<Value<'ctx>> {
        let symbol_kind = self.cucx.tcx.lookup_symbol(mangled_name, span)?;
        let borrowed_symbol_kind = symbol_kind.borrow();
        let fn_info = match &*borrowed_symbol_kind {
            SymbolTableValue::Function(func) => func,
            _ => {
                return Err(CodegenError::Undeclared {
                    name: mangled_name,
                    span,
                }
                .into())
            }
        };

        if fn_info.value.get_type().is_var_arg() {
            self.verify_var_args(args, &fn_info.param_types)?;
        } else {
            self.verify_args(args, &fn_info.param_types)?;
        }

        let return_value = self
            .cucx
            .builder
            .build_call(
                fn_info.value,
                args.iter()
                    .map(|a| a.0.get_value().into())
                    .collect::<Vec<_>>()
                    .as_slice(),
                "",
            )?
            .try_as_basic_value()
            .left();

        Ok(match return_value {
            // With return value
            Some(val) => Value::new(
                val,
                match &fn_info.return_type {
                    ReturnType::Type(ty) => ty.clone(),
                    ReturnType::Void => unreachable!(),
                },
            ),

            // Without return value (void function)
            None => Value::new_void(),
        })
    }

    fn verify_var_args(
        &mut self,
        args: &VecDeque<(Value<'ctx>, Span)>,
        params: &[Rc<Ty>],
    ) -> anyhow::Result<()> {
        for (idx, param) in params.iter().enumerate() {
            self.verify_arg(&args[idx], param)?;
        }

        Ok(())
    }

    fn verify_args(
        &mut self,
        args: &VecDeque<(Value<'ctx>, Span)>,
        params: &[Rc<Ty>],
    ) -> anyhow::Result<()> {
        for (idx, arg) in args.iter().enumerate() {
            self.verify_arg(arg, &params[idx])?;
        }

        Ok(())
    }

    fn verify_arg(&mut self, arg: &(Value<'ctx>, Span), param: &Rc<Ty>) -> anyhow::Result<()> {
        let mismatched_err = Err(CodegenError::MismatchedTypes {
            types: (arg.0.get_type().kind.to_string(), param.kind.to_string()),
            span: arg.1,
        }
        .into());

        // If the parameter is generic, skip the type checking.
        if param.is_generic() {
            return Ok(());
        }

        if !is_same_type(&arg.0.get_type(), param) {
            return mismatched_err;
        }

        // Check mutability
        if arg.0.get_type().mutability.is_not() && param.mutability.is_mut() {
            return Err(CodegenError::CannotAssignImmutableToMutable { span: arg.1 }.into());
        }

        Ok(())
    }
}
