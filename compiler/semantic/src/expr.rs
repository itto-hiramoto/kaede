use std::{
    collections::{BTreeSet, HashSet},
    rc::Rc,
    vec,
};

use super::ClosureCapture;
use crate::{context::AnalyzeCommand, error::SemanticError, SemanticAnalyzer};

use kaede_ast as ast;
use kaede_ast_type::{self as ast_type};
use kaede_ir::{
    self as ir,
    qualified_symbol::QualifiedSymbol,
    ty::{change_mutability_dup, make_fundamental_type},
};
use kaede_ir::{module_path::ModulePath, ty as ir_type};
use kaede_span::Span;
use kaede_symbol::{Ident, Symbol};
use kaede_symbol_table::{
    GenericFuncInfo, GenericInfo, GenericKind, SymbolTable, SymbolTableValue, SymbolTableValueKind,
    VariableInfo,
};

struct DecomposedEnumVariantPattern<'p> {
    module_names: Vec<Ident>,
    enum_name: &'p Ident,
    variant_name: &'p Ident,
    param: Option<&'p ast::expr::Args>,
}

impl SemanticAnalyzer {
    pub fn analyze_expr(&mut self, expr: &ast::expr::Expr) -> anyhow::Result<ir::expr::Expr> {
        use ast::expr::ExprKind;

        let span = expr.span;

        match &expr.kind {
            ExprKind::ArrayLiteral(node) => self.analyze_array_literal(node, span),
            ExprKind::ArrayRepeat(node) => self.analyze_array_repeat(node, span),
            ExprKind::Int(node) => self.analyze_int(node),
            ExprKind::True => self.analyze_boolean_literal(true, span),
            ExprKind::False => self.analyze_boolean_literal(false, span),
            ExprKind::Block(node) => self.analyze_block_expr(node),
            ExprKind::StringLiteral(node) => self.analyze_string_literal(node),
            ExprKind::ByteStringLiteral(node) => self.analyze_byte_string_literal(node),
            ExprKind::ByteLiteral(node) => self.analyze_byte_literal(node),
            ExprKind::CharLiteral(node) => self.analyze_char_literal(node),
            ExprKind::Binary(node) => self.analyze_binary(node),
            ExprKind::Ident(node) => self.analyze_ident(node),
            ExprKind::LogicalNot(node) => self.analyze_logical_not(node),
            ExprKind::Return(node) => self.analyze_return(node),
            ExprKind::Indexing(node) => self.analyze_array_or_ptr_indexing(node),
            ExprKind::Slicing(node) => self.analyze_slicing(node),
            ExprKind::FnCall(node) => self.analyze_fn_call(node),
            ExprKind::If(node) => self.analyze_if(node),
            ExprKind::Break(node) => self.analyze_break(node),
            ExprKind::Loop(node) => self.analyze_loop(node),
            ExprKind::While(node) => self.analyze_while(node),
            ExprKind::Match(node) => self.analyze_match(node),
            ExprKind::StructLiteral(node) => self.analyze_struct_literal(node),
            ExprKind::TupleLiteral(node) => self.analyze_tuple_literal(node),
            ExprKind::Closure(node) => self.analyze_closure(node, None),
            ExprKind::Spawn(node) => self.analyze_spawn(node),

            ExprKind::Ty(_) => todo!(),
            ExprKind::GenericIdent(_) => todo!(),
        }
    }

    fn analyze_spawn(&mut self, node: &ast::expr::Spawn) -> anyhow::Result<ir::expr::Expr> {
        let call_node = match &node.callee.kind {
            ast::expr::ExprKind::FnCall(call) => call,
            _ => {
                return Err(SemanticError::SpawnTargetNotCall { span: node.span }.into());
            }
        };

        let call_expr = self.analyze_fn_call(call_node)?;

        let (callee, args) = match call_expr.kind {
            ir::expr::ExprKind::FnCall(call) => (call.callee, call.args.0),
            _ => {
                return Err(SemanticError::SpawnTargetNotCall { span: node.span }.into());
            }
        };

        let returns_unit = matches!(callee.return_ty.kind.as_ref(), ir_type::TyKind::Unit);

        if !returns_unit {
            return Err(SemanticError::SpawnReturnTypeNotUnit {
                ty: callee.return_ty.kind.to_string(),
                span: node.span,
            }
            .into());
        }

        let arg_types = args.iter().map(|arg| arg.ty.clone()).collect();
        let span = node.span;

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::Spawn(ir::expr::Spawn {
                callee,
                args,
                arg_types,
                span,
                is_main: false,
            }),
            ty: Rc::new(ir_type::Ty::new_unit()),
            span,
        })
    }

    fn analyze_tuple_literal(
        &mut self,
        node: &ast::expr::TupleLiteral,
    ) -> anyhow::Result<ir::expr::Expr> {
        let element_values = node
            .elements
            .iter()
            .map(|e| self.analyze_expr(e))
            .collect::<anyhow::Result<Vec<_>>>()?;

        let tuple_ty = ir::ty::Ty {
            kind: ir::ty::TyKind::Tuple(element_values.iter().map(|v| v.ty.clone()).collect())
                .into(),
            mutability: ir::ty::Mutability::Not,
        };

        let tuple_ir = ir::expr::TupleLiteral {
            elements: element_values,
            span: node.span,
        };

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::TupleLiteral(tuple_ir),
            ty: Rc::new(ir::ty::wrap_in_ref(
                Rc::new(tuple_ty),
                ir::ty::Mutability::Mut,
            )),
            span: node.span,
        })
    }

    fn analyze_struct_literal(
        &mut self,
        node: &ast::expr::StructLiteral,
    ) -> anyhow::Result<ir::expr::Expr> {
        let struct_ty =
            self.analyze_user_defined_type(&node.struct_ty, ir_type::Mutability::Not)?;

        let udt_ir = match struct_ty.kind.as_ref() {
            ir_type::TyKind::UserDefined(udt) => udt,
            _ => unreachable!("{:?}", struct_ty.kind),
        };

        let struct_ir = match &udt_ir.kind {
            ir_type::UserDefinedTypeKind::Struct(struct_ir) => struct_ir.clone(),
            ir_type::UserDefinedTypeKind::Placeholder(qsym) => {
                match &self
                    .lookup_qualified_symbol(qsym.clone())
                    .unwrap()
                    .borrow()
                    .kind
                {
                    SymbolTableValueKind::Struct(struct_ir) => struct_ir.clone(),
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        };

        let values = node
            .values
            .iter()
            .map(|(name, value)| {
                let value = self.analyze_expr(value)?;
                Ok((name.symbol(), value))
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        self.verify_struct_literal_values(&struct_ir, values.as_slice())?;

        let ir = ir::expr::StructLiteral {
            struct_info: struct_ir.clone(),
            values,
            span: node.span,
        };

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::StructLiteral(ir),
            ty: Rc::new(ir::ty::wrap_in_ref(struct_ty, ir_type::Mutability::Mut)),
            span: node.span,
        })
    }

    fn verify_struct_literal_values(
        &mut self,
        struct_ir: &ir::top::Struct,
        values: &[(Symbol, ir::expr::Expr)],
    ) -> anyhow::Result<()> {
        for (name, value) in values {
            let field_info = struct_ir.fields.iter().find(|f| f.name == *name);

            if field_info.is_none() {
                return Err(SemanticError::NoField {
                    field_name: *name,
                    parent_name: struct_ir.name.symbol(),
                    span: value.span,
                }
                .into());
            }

            let field_info = field_info.unwrap();

            if matches!(value.kind, ir::expr::ExprKind::Int(_))
                && !matches!(field_info.ty.kind.as_ref(), ir_type::TyKind::Fundamental(_))
            {
                return Err(SemanticError::MismatchedTypes {
                    types: (field_info.ty.kind.to_string(), value.ty.kind.to_string()),
                    span: value.span,
                }
                .into());
            }

            if !ir::ty::is_same_type(&field_info.ty, &value.ty) {
                return Err(SemanticError::MismatchedTypes {
                    types: (field_info.ty.kind.to_string(), value.ty.kind.to_string()),
                    span: value.span,
                }
                .into());
            }
        }

        Ok(())
    }

    /// Decompose enum variant patterns (like `A::B` or `A::B(a, b, c)`)
    fn decompose_enum_variant_pattern(
        pattern: &ast::expr::Expr,
    ) -> DecomposedEnumVariantPattern<'_> {
        let (module_names, enum_name, variant_name, param) = match &pattern.kind {
            ast::expr::ExprKind::Binary(b) => match b.kind {
                ast::expr::BinaryKind::ScopeResolution => match &b.lhs.kind {
                    ast::expr::ExprKind::Ident(i) => {
                        let (variant_name, param) = match &b.rhs.kind {
                            ast::expr::ExprKind::Ident(ident) => (ident, None),
                            ast::expr::ExprKind::FnCall(fncall) => match &fncall.callee.kind {
                                ast::expr::ExprKind::Ident(ident) => (ident, Some(&fncall.args)),
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        };

                        (vec![], i, variant_name, param)
                    }
                    _ => unreachable!(),
                },

                ast::expr::BinaryKind::Access => match &b.rhs.kind {
                    // m1.m2.m3.A::B(a, b, c)
                    ast::expr::ExprKind::Binary(ast::expr::Binary {
                        kind: ast::expr::BinaryKind::ScopeResolution,
                        ..
                    }) => {
                        let mut modules = Vec::new();
                        b.lhs.collect_access_chain(&mut modules);

                        let decomposed = Self::decompose_enum_variant_pattern(&b.rhs);

                        (
                            modules,
                            decomposed.enum_name,
                            decomposed.variant_name,
                            decomposed.param,
                        )
                    }

                    _ => unreachable!(),
                },

                _ => unreachable!(),
            },

            _ => unreachable!(),
        };

        DecomposedEnumVariantPattern {
            module_names,
            enum_name,
            variant_name,
            param,
        }
    }

    fn check_exhaustiveness_for_match_on_enum(
        &mut self,
        node: &ast::expr::Match,
        enum_ir: &ir::top::Enum,
    ) -> anyhow::Result<()> {
        // Check if there are only catch-all arms (which is not allowed)
        let non_catch_all_count = node.arms.iter().filter(|arm| !arm.is_catch_all).count();
        if non_catch_all_count == 0 {
            return Err(SemanticError::MatchMustHaveNonCatchAllArm { span: node.span }.into());
        }

        // If there is a catch-all arm, we don't need to check exhaustiveness for specific patterns
        if node.arms.iter().any(|arm| arm.is_catch_all) {
            return Ok(());
        }

        let variants = &enum_ir.variants;

        let arms = node.arms.iter().filter(|arm| !arm.is_catch_all);

        let mut pattern_variant_names = BTreeSet::new();

        for arm in arms {
            let decomposed = Self::decompose_enum_variant_pattern(&arm.pattern);

            // Check if the module names are the same as the enum's external modules
            let enum_external_modules = enum_ir.name.module_path().get_module_names_from_root();
            if decomposed
                .module_names
                .iter()
                .zip(enum_external_modules)
                .any(|(a, b)| a.as_str() != b.as_str())
            {
                return Err(SemanticError::MatchNotExhaustive {
                    variant_name: decomposed.variant_name.symbol(),
                    span: node.span,
                }
                .into());
            }

            // Check if the variant name is in the enum
            if !enum_ir
                .variants
                .iter()
                .any(|v| v.name == decomposed.variant_name.symbol())
            {
                return Err(SemanticError::NoVariant {
                    variant_name: decomposed.variant_name.symbol(),
                    parent_name: enum_ir.name.symbol(),
                    span: node.span,
                }
                .into());
            }

            // TODO: Check if the variant has the same number of parameters as the pattern

            if !pattern_variant_names.insert(decomposed.variant_name.as_str()) {
                // There were duplicate patterns
                return Err(SemanticError::DuplicatePattern {
                    variant_name: decomposed.variant_name.symbol(),
                    span: decomposed.variant_name.span(),
                }
                .into());
            }
        }

        let variant_names = variants
            .iter()
            .map(|v| v.name.as_str())
            .collect::<BTreeSet<_>>();

        // Check if there are any variants that are not covered by the patterns
        let dif = variant_names
            .difference(&pattern_variant_names)
            .collect::<Vec<_>>();

        if !dif.is_empty() {
            return Err(SemanticError::MatchNotExhaustive {
                variant_name: dif
                    .into_iter()
                    .map(|p| format!("`{p}`"))
                    .collect::<Vec<_>>()
                    .join(" and ")
                    .into(),
                span: node.span,
            }
            .into());
        }

        Ok(())
    }

    fn check_exhaustiveness_for_match_on_int(
        &mut self,
        fty: &ir_type::FundamentalType,
        arms: &[ast::expr::MatchArm],
        span: Span,
    ) -> anyhow::Result<()> {
        // Check if there are only catch-all arms (which is not allowed)
        let non_catch_all_count = arms.iter().filter(|arm| !arm.is_catch_all).count();
        if non_catch_all_count == 0 {
            return Err(SemanticError::MatchMustHaveNonCatchAllArm { span }.into());
        }

        // If there is a catch-all arm, we don't need to check exhaustiveness for specific patterns
        if arms.iter().any(|arm| arm.is_catch_all) {
            return Ok(());
        }

        assert!(fty.is_int_or_char_or_bool());

        if fty.kind == ir_type::FundamentalTypeKind::Bool {
            let mut has_true = false;
            let mut has_false = false;

            for arm in arms {
                match arm.pattern.kind {
                    ast::expr::ExprKind::True => has_true = true,
                    ast::expr::ExprKind::False => has_false = true,
                    _ => {
                        return Err(SemanticError::MismatchedTypes {
                            types: (
                                fty.kind.to_string(),
                                self.analyze_expr(&arm.pattern)?.ty.kind.to_string(),
                            ),
                            span: arm.pattern.span,
                        }
                        .into());
                    }
                }
            }

            if has_true && has_false {
                return Ok(());
            } else if has_true {
                return Err(SemanticError::MatchNotExhaustive {
                    variant_name: "`false`".to_owned().into(),
                    span,
                }
                .into());
            } else if has_false {
                return Err(SemanticError::MatchNotExhaustive {
                    variant_name: "`true`".to_owned().into(),
                    span,
                }
                .into());
            } else {
                return Err(SemanticError::MatchNotExhaustive {
                    variant_name: "`true` and `false`".to_owned().into(),
                    span,
                }
                .into());
            }
        }

        // Non-bool integer
        Err(SemanticError::MatchNotExhaustive {
            variant_name: "`_`".to_owned().into(),
            span,
        }
        .into())
    }

    fn conv_match_arms_on_enum_to_if(
        &mut self,
        enum_ir: Rc<ir::top::Enum>,
        target: Rc<ir::expr::Expr>,
        arms: &[&ast::expr::MatchArm],
    ) -> anyhow::Result<ir::expr::If> {
        // Skip catch-all arm for now, we'll handle it separately
        let non_catch_all_arms = arms
            .iter()
            .filter(|arm| !arm.is_catch_all)
            .cloned()
            .collect::<Vec<_>>();

        // Find catch-all arm if it exists
        let catch_all_arm = arms.iter().find(|arm| arm.is_catch_all);

        // Convert match arms on enum to nested if-else expressions
        let current_arm = &non_catch_all_arms[0];
        let remaining_arms = &non_catch_all_arms[1..];

        // Get the pattern variant information
        let decomposed = Self::decompose_enum_variant_pattern(&current_arm.pattern);
        let variant = enum_ir
            .variants
            .iter()
            .find(|v| v.name == decomposed.variant_name.symbol())
            .unwrap();

        let pattern_variant_offset = variant.offset;

        let enum_unpack = if let Some(params) = decomposed.param {
            if variant.ty.is_none() {
                return Err(SemanticError::UnitVariantCannotUnpack {
                    unit_variant_name: format!(
                        "{}::{}",
                        enum_ir.name.symbol(),
                        decomposed.variant_name.symbol()
                    )
                    .into(),
                    span: current_arm.pattern.span,
                }
                .into());
            }

            let variant_ty =
                change_mutability_dup(variant.ty.clone().unwrap(), target.ty.mutability);

            let udt = ir_type::UserDefinedType {
                kind: ir_type::UserDefinedTypeKind::Enum(enum_ir.clone()),
            };

            let name = match &params.args.front().unwrap().value.kind {
                ast::expr::ExprKind::Ident(ident) => *ident,
                param => unreachable!("{:?}", param),
            }
            .symbol();

            if name.as_str() == "_" {
                None
            } else {
                // Insert the unpacked variable into the symbol table
                self.insert_symbol_to_current_scope(
                    name,
                    SymbolTableValue::new(
                        SymbolTableValueKind::Variable(VariableInfo {
                            ty: variant_ty.clone(),
                        }),
                        self.current_module_path().clone(),
                    ),
                    current_arm.pattern.span,
                )?;

                Some(ir::expr::EnumUnpack {
                    name,
                    enum_ty: udt,
                    enum_value: target.clone(),
                    variant_ty,
                    span: current_arm.pattern.span,
                })
            }
        } else {
            None
        };

        // Analyze the arm's code
        let then_expr = self.analyze_expr(&current_arm.code)?;

        // Get the target variant information
        let target_variant_offset_expr = ir::expr::Expr {
            kind: ir::expr::ExprKind::TupleIndexing(ir::expr::TupleIndexing {
                tuple: target.clone(),
                element_ty: make_fundamental_type(
                    ir_type::FundamentalTypeKind::I32,
                    ir_type::Mutability::Not,
                )
                .into(),
                index: 0,
                span: current_arm.pattern.span,
            }),
            ty: Rc::new(make_fundamental_type(
                ir_type::FundamentalTypeKind::I32,
                ir_type::Mutability::Not,
            )),
            span: current_arm.pattern.span,
        };

        // Create condition
        let condition = ir::expr::Expr {
            kind: ir::expr::ExprKind::Binary(ir::expr::Binary {
                kind: ir::expr::BinaryKind::Eq,
                lhs: Rc::new(target_variant_offset_expr),
                rhs: Rc::new(ir::expr::Expr {
                    kind: ir::expr::ExprKind::Int(ir::expr::Int {
                        kind: ir::expr::IntKind::I32(pattern_variant_offset as i32),
                        span: current_arm.pattern.span,
                    }),
                    ty: Rc::new(make_fundamental_type(
                        ir_type::FundamentalTypeKind::I32,
                        ir_type::Mutability::Not,
                    )),
                    span: current_arm.pattern.span,
                }),
                span: current_arm.pattern.span,
            }),
            ty: Rc::new(ir_type::make_fundamental_type(
                ir_type::FundamentalTypeKind::Bool,
                ir_type::Mutability::Not,
            )),
            span: current_arm.pattern.span,
        };

        // Handle remaining arms recursively
        let else_ = if remaining_arms.is_empty() {
            // If no more non-catch-all arms, add catch-all arm if it exists
            if let Some(catch_all) = catch_all_arm {
                Some(ir::expr::Else::Block(Box::new(self.analyze_expr(&catch_all.code)?)).into())
            } else {
                None
            }
        } else {
            // Include catch-all arm in recursive call
            let mut remaining_with_catch_all = remaining_arms.to_vec();
            if let Some(catch_all) = catch_all_arm {
                remaining_with_catch_all.push(catch_all);
            }
            Some(
                ir::expr::Else::If(self.conv_match_arms_on_enum_to_if(
                    enum_ir.clone(),
                    target.clone(),
                    &remaining_with_catch_all,
                )?)
                .into(),
            )
        };

        Ok(ir::expr::If {
            cond: Box::new(condition),
            then: Box::new(then_expr),
            else_,
            enum_unpack: enum_unpack.map(Box::new),
            is_match: true,
            span: current_arm.pattern.span,
        })
    }

    fn analyze_match_on_reference(
        &mut self,
        node: &ast::expr::Match,
        value: &ir::expr::Expr,
        rty: &ir_type::ReferenceType,
    ) -> anyhow::Result<ir::expr::Expr> {
        let base_ty = rty.get_base_type();

        let err = || {
            Err(SemanticError::MatchCannotBeUsedWithValueOfType {
                ty: value.ty.kind.to_string(),
                span: node.span,
            }
            .into())
        };

        let enum_ir = if let ir_type::TyKind::UserDefined(udt) = base_ty.kind.as_ref() {
            match &udt.kind {
                ir_type::UserDefinedTypeKind::Enum(enum_ir) => enum_ir.clone(),
                ir_type::UserDefinedTypeKind::Placeholder(qsym) => {
                    match &self
                        .lookup_qualified_symbol(qsym.clone())
                        .unwrap()
                        .borrow()
                        .kind
                    {
                        SymbolTableValueKind::Enum(enum_ir) => enum_ir.clone(),
                        _ => unreachable!(),
                    }
                }
                _ => return err(),
            }
        } else {
            return err();
        };

        self.check_exhaustiveness_for_match_on_enum(node, &enum_ir)?;

        let target = Rc::new(self.analyze_expr(&node.value)?);

        let if_ = self.conv_match_arms_on_enum_to_if(
            enum_ir.clone(),
            target.clone(),
            node.arms.iter().collect::<Vec<_>>().as_slice(),
        )?;

        Ok(ir::expr::Expr {
            ty: if_.then.ty.clone(),
            kind: ir::expr::ExprKind::If(if_),
            span: node.span,
        })
    }

    fn conv_match_arms_on_int_to_if(
        &mut self,
        target: Rc<ir::expr::Expr>,
        arms: &[&ast::expr::MatchArm],
    ) -> anyhow::Result<ir::expr::If> {
        // Skip catch-all arm for now, we'll handle it separately
        let non_catch_all_arms = arms
            .iter()
            .filter(|arm| !arm.is_catch_all)
            .cloned()
            .collect::<Vec<_>>();

        // Find catch-all arm if it exists
        let catch_all_arm = arms.iter().find(|arm| arm.is_catch_all);

        let current_arm = &non_catch_all_arms[0];
        let remaining_arms = &non_catch_all_arms[1..];

        let condition = ir::expr::Expr {
            kind: ir::expr::ExprKind::Binary(ir::expr::Binary {
                kind: ir::expr::BinaryKind::Eq,
                lhs: target.clone(),
                rhs: Rc::new(self.analyze_expr(&current_arm.pattern)?),
                span: current_arm.pattern.span,
            }),
            ty: Rc::new(ir_type::make_fundamental_type(
                ir_type::FundamentalTypeKind::Bool,
                ir_type::Mutability::Not,
            )),
            span: current_arm.pattern.span,
        };

        let then_expr = self.analyze_expr(&current_arm.code)?;

        let else_ = if remaining_arms.is_empty() {
            if let Some(catch_all) = catch_all_arm {
                Some(ir::expr::Else::Block(Box::new(self.analyze_expr(&catch_all.code)?)).into())
            } else {
                None
            }
        } else {
            // Include catch-all arm in recursive call
            let mut remaining_with_catch_all = remaining_arms.to_vec();
            if let Some(catch_all) = catch_all_arm {
                remaining_with_catch_all.push(catch_all);
            }
            Some(
                ir::expr::Else::If(
                    self.conv_match_arms_on_int_to_if(target, &remaining_with_catch_all)?,
                )
                .into(),
            )
        };

        Ok(ir::expr::If {
            cond: Box::new(condition),
            then: Box::new(then_expr),
            else_,
            enum_unpack: None,
            is_match: true,
            span: current_arm.pattern.span,
        })
    }

    fn analyze_match_on_fundamental(
        &mut self,
        node: &ast::expr::Match,
        value: Rc<ir::expr::Expr>,
        fty: &ir_type::FundamentalType,
    ) -> anyhow::Result<ir::expr::Expr> {
        if !fty.is_int_or_char_or_bool() {
            return Err(SemanticError::MatchCannotBeUsedWithValueOfType {
                ty: value.ty.kind.to_string(),
                span: node.span,
            }
            .into());
        }

        self.check_exhaustiveness_for_match_on_int(fty, &node.arms, node.span)?;

        let if_ = self.conv_match_arms_on_int_to_if(
            value.clone(),
            node.arms.iter().collect::<Vec<_>>().as_slice(),
        )?;

        Ok(ir::expr::Expr {
            ty: if_.then.ty.clone(),
            kind: ir::expr::ExprKind::If(if_),
            span: node.span,
        })
    }

    fn analyze_match(&mut self, node: &ast::expr::Match) -> anyhow::Result<ir::expr::Expr> {
        let value = Rc::new(self.analyze_expr(&node.value)?);

        // Add a symbol table for the match
        self.push_scope(SymbolTable::new());

        let result = match value.ty.kind.as_ref() {
            ir_type::TyKind::Reference(rty) => {
                self.analyze_match_on_reference(node, &value, rty)?
            }

            ir_type::TyKind::Fundamental(fty) => {
                self.analyze_match_on_fundamental(node, value.clone(), fty)?
            }

            ir_type::TyKind::Var(_) => {
                let fty = ir_type::FundamentalType {
                    kind: ir_type::FundamentalTypeKind::I32,
                };
                self.analyze_match_on_fundamental(node, value.clone(), &fty)?
            }
            _ => {
                return Err(SemanticError::MatchCannotBeUsedWithValueOfType {
                    ty: value.ty.kind.to_string(),
                    span: node.span,
                }
                .into())
            }
        };

        self.pop_scope();

        Ok(result)
    }

    fn analyze_loop(&mut self, node: &ast::expr::Loop) -> anyhow::Result<ir::expr::Expr> {
        self.with_inside_loop(|analyzer| {
            let body = analyzer.analyze_block_expr(&node.body)?;

            if let ir::expr::ExprKind::Block(block) = body.kind {
                Ok(ir::expr::Expr {
                    kind: ir::expr::ExprKind::Loop(ir::expr::Loop {
                        body: block,
                        span: node.span,
                    }),
                    ty: Rc::new(ir_type::Ty::new_unit()),
                    span: node.span,
                })
            } else {
                unreachable!()
            }
        })
    }

    fn analyze_while(&mut self, node: &ast::expr::While) -> anyhow::Result<ir::expr::Expr> {
        let cond = self.analyze_expr(&node.cond)?;

        self.with_inside_loop(|analyzer| {
            let body = analyzer.analyze_block_expr(&node.body)?;

            if let ir::expr::ExprKind::Block(mut body_block) = body.kind {
                let negated_cond = ir::expr::Expr {
                    kind: ir::expr::ExprKind::LogicalNot(ir::expr::LogicalNot {
                        operand: Box::new(cond),
                        span: node.cond.span,
                    }),
                    ty: Rc::new(make_fundamental_type(
                        ir_type::FundamentalTypeKind::Bool,
                        ir_type::Mutability::Not,
                    )),
                    span: node.cond.span,
                };

                let break_expr = ir::expr::Expr {
                    kind: ir::expr::ExprKind::Break,
                    ty: Rc::new(ir_type::Ty::new_never()),
                    span: node.span,
                };

                let break_block = ir::stmt::Block {
                    body: vec![ir::stmt::Stmt::Expr(Rc::new(break_expr))],
                    last_expr: None,
                    span: node.span,
                };

                let if_break = ir::expr::Expr {
                    kind: ir::expr::ExprKind::If(ir::expr::If {
                        cond: Box::new(negated_cond),
                        then: Box::new(ir::expr::Expr {
                            kind: ir::expr::ExprKind::Block(break_block),
                            ty: Rc::new(ir_type::Ty::new_unit()),
                            span: node.span,
                        }),
                        else_: None,
                        enum_unpack: None,
                        is_match: false,
                        span: node.span,
                    }),
                    ty: Rc::new(ir_type::Ty::new_unit()),
                    span: node.span,
                };

                body_block
                    .body
                    .insert(0, ir::stmt::Stmt::Expr(Rc::new(if_break)));

                Ok(ir::expr::Expr {
                    kind: ir::expr::ExprKind::Loop(ir::expr::Loop {
                        body: body_block,
                        span: node.span,
                    }),
                    ty: Rc::new(ir_type::Ty::new_unit()),
                    span: node.span,
                })
            } else {
                unreachable!()
            }
        })
    }

    fn analyze_break(&self, node: &ast::expr::Break) -> anyhow::Result<ir::expr::Expr> {
        if !self.is_inside_loop() {
            return Err(SemanticError::BreakOutsideOfLoop { span: node.span }.into());
        }

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::Break,
            ty: Rc::new(ir_type::Ty::new_never()),
            span: node.span,
        })
    }

    fn analyze_if(&mut self, node: &ast::expr::If) -> anyhow::Result<ir::expr::Expr> {
        let cond = Box::new(self.analyze_expr(&node.cond)?);

        let then = Box::new(self.analyze_block_expr(&node.then)?);

        // TODO: Enahnce error handlings

        let else_ = match node.else_.as_ref() {
            Some(else_) => Some(Box::new(match else_.as_ref() {
                ast::expr::Else::If(if_) => {
                    let expr = self.analyze_if(if_)?;
                    if let ir::expr::ExprKind::If(if_) = expr.kind {
                        ir::expr::Else::If(if_)
                    } else {
                        unreachable!("analyze_if should return an If expression")
                    }
                }
                ast::expr::Else::Block(block) => {
                    ir::expr::Else::Block(Box::new(self.analyze_block_expr(block)?))
                }
            })),

            None => None,
        };

        // Determine the if expression's type
        // If there's an else branch, handle Never types appropriately
        // If there's no else branch, the type evaluates to unit
        let if_ty = if let Some(else_branch) = &else_ {
            let else_ty = match else_branch.as_ref() {
                ir::expr::Else::If(if_) => &if_.then.ty,
                ir::expr::Else::Block(block) => &block.ty,
            };

            // If then branch is Never, use else branch type
            if matches!(then.ty.kind.as_ref(), ir_type::TyKind::Never) {
                else_ty.clone()
            // If else branch is Never, use then branch type
            } else if matches!(else_ty.kind.as_ref(), ir_type::TyKind::Never) {
                then.ty.clone()
            } else {
                // Both branches have concrete types - use then type (will be unified by type inference)
                then.ty.clone()
            }
        } else {
            Rc::new(ir_type::Ty::new_unit())
        };

        Ok(ir::expr::Expr {
            ty: if_ty,
            kind: ir::expr::ExprKind::If(ir::expr::If {
                cond,
                then,
                else_,
                enum_unpack: None,
                is_match: false,
                span: node.span,
            }),
            span: node.span,
        })
    }

    fn generate_generic_fn(
        &mut self,
        info: &GenericFuncInfo,
        generic_args: &[Rc<ir_type::Ty>],
    ) -> anyhow::Result<Rc<ir::top::FnDecl>> {
        let generic_params = match info.ast.decl.generic_params.as_ref() {
            Some(generic_params) => generic_params,
            None => todo!("Error"),
        };

        let generated_generic_key =
            self.create_generated_generic_key(info.ast.decl.name.symbol(), generic_args);

        // If the generic function is already generated, return early
        if let Some(symbol_value) = self.lookup_symbol(generated_generic_key) {
            if let SymbolTableValueKind::Function(fn_) = &symbol_value.borrow().kind {
                return Ok(fn_.clone());
            } else {
                unreachable!()
            }
        }

        // Generic functions must always be generated regardless of the analyze command, so it is overwritten
        let fn_ = self.with_analyze_command(AnalyzeCommand::NoCommand, |analyzer| {
            // Generate the generic function
            analyzer.with_generic_arguments(generic_params, generic_args, |analyzer| {
                let mut fn_ = info.ast.clone();
                fn_.decl.name = Ident::new(generated_generic_key, Span::dummy());
                // Because generic functions maybe generated multiple times (across multiple files),
                // we need to set link_once to true to avoid errors
                fn_.decl.link_once = true;
                analyzer.analyze_fn_internal(fn_)
            })
        })?;

        self.generated_generics
            .push(ir::top::TopLevel::Fn(fn_.clone()));

        Ok(Rc::new(fn_.decl.clone()))
    }

    fn analyze_generic_fn_call(
        &mut self,
        info: &GenericInfo,
        node: &ast::expr::FnCall,
    ) -> anyhow::Result<ir::expr::Expr> {
        let func_info = match &info.kind {
            GenericKind::Func(info) => info,
            _ => unreachable!(),
        };

        let param_info = func_info
            .ast
            .decl
            .params
            .v
            .iter()
            .map(|param| (param.name.symbol(), param.default.is_some()))
            .collect::<Vec<_>>();

        let is_c_variadic = matches!(
            func_info.ast.decl.params.variadic,
            ast::top::VariadicKind::C
        );

        let (ordered_args, variadic_args) = self.resolve_call_arguments(
            &param_info,
            &node.args,
            func_info.ast.decl.name.symbol(),
            node.span,
            is_c_variadic,
        )?;

        let ordered_args = ordered_args
            .into_iter()
            .map(|arg| arg.map(|arg| self.analyze_expr(&arg.value)).transpose())
            .collect::<anyhow::Result<Vec<_>>>()?;

        let mut analyzed_variadic = Vec::with_capacity(variadic_args.len());
        for arg in &variadic_args {
            analyzed_variadic.push(self.analyze_expr(&arg.value)?);
        }

        let mut provided_args = Vec::new();
        for arg in ordered_args.iter().filter_map(|arg| arg.as_ref()) {
            provided_args.push(arg.clone());
        }
        provided_args.extend(analyzed_variadic.iter().cloned());

        // Generate the generic function
        let callee_decl = {
            let generic_args = if let Some(generic_args) = node.generic_args.as_ref() {
                generic_args
                    .types
                    .iter()
                    .map(|arg| self.analyze_type(arg))
                    .collect::<anyhow::Result<Vec<_>>>()?
            } else {
                // Infer generic arguments from the function call arguments
                provided_args
                    .iter()
                    .map(|arg| -> anyhow::Result<Rc<ir_type::Ty>> {
                        Ok(match arg.ty.kind.as_ref() {
                            ir_type::TyKind::Var(_) => match arg.kind {
                                ir::expr::ExprKind::Int(_) => {
                                    Rc::new(ir_type::make_fundamental_type(
                                        ir_type::FundamentalTypeKind::I32,
                                        ir_type::Mutability::Not,
                                    ))
                                }
                                _ => arg.ty.clone(),
                            },
                            _ => arg.ty.clone(),
                        })
                    })
                    .collect::<anyhow::Result<Vec<_>>>()?
            };

            self.generate_generic_fn(func_info, &generic_args)?
        };

        let mut args = Vec::with_capacity(callee_decl.params.len() + analyzed_variadic.len());

        for (arg, param) in ordered_args.into_iter().zip(callee_decl.params.iter()) {
            if let Some(arg) = arg {
                args.push(arg);
            } else if let Some(default) = &param.default {
                args.push((**default).clone());
            } else {
                unreachable!();
            }
        }

        args.extend(analyzed_variadic);

        self.verify_fn_call_arguments(&callee_decl, &args, node.span)?;

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::FnCall(ir::expr::FnCall {
                callee: callee_decl.clone(),
                args: ir::expr::Args(args, node.span),
                span: node.span,
            }),
            ty: callee_decl.return_ty.clone(),
            span: node.span,
        })
    }

    fn callee_ident(node: &ast::expr::FnCall) -> Option<Ident> {
        if let ast::expr::ExprKind::Ident(ident) = node.callee.kind {
            Some(ident)
        } else {
            None
        }
    }

    fn callee_symbol(&self, node: &ast::expr::FnCall) -> anyhow::Result<Symbol> {
        Self::callee_ident(node)
            .map(|ident| ident.symbol())
            .ok_or_else(|| {
                SemanticError::NotCallable {
                    ty: "expression".to_string(),
                    span: node.span,
                }
                .into()
            })
    }

    fn closure_ty_from_fn_decl(&self, decl: &ir::top::FnDecl) -> Rc<ir_type::Ty> {
        Rc::new(ir_type::Ty {
            kind: ir_type::TyKind::Closure(ir_type::ClosureType {
                param_tys: decl.params.iter().map(|p| p.ty.clone()).collect(),
                ret_ty: decl.return_ty.clone(),
                captures: Vec::new(),
            })
            .into(),
            mutability: ir_type::Mutability::Not,
        })
    }

    fn create_fn_decl_from_closure_ty(
        &self,
        name: Symbol,
        closure_ty: &ir_type::ClosureType,
    ) -> Rc<ir::top::FnDecl> {
        let params = closure_ty
            .param_tys
            .iter()
            .enumerate()
            .map(|(i, ty)| ir::top::Param {
                name: Symbol::from(format!("arg{i}")),
                ty: ty.clone(),
                default: None,
            })
            .collect::<Vec<_>>();

        ir::top::FnDecl {
            lang_linkage: kaede_common::LangLinkage::Default,
            link_once: false,
            name: QualifiedSymbol::new(self.current_module_path().clone(), name),
            params,
            is_c_variadic: false,
            return_ty: closure_ty.ret_ty.clone(),
        }
        .into()
    }

    fn create_fn_decl_from_callable_ty(
        &self,
        name: Symbol,
        ty: &ir_type::Ty,
    ) -> Option<Rc<ir::top::FnDecl>> {
        match ty.kind.as_ref() {
            ir_type::TyKind::Closure(closure_ty) => {
                Some(self.create_fn_decl_from_closure_ty(name, closure_ty))
            }

            ir_type::TyKind::Reference(rty) => {
                self.create_fn_decl_from_callable_ty(name, &rty.get_base_type())
            }

            ir_type::TyKind::Pointer(pty) => {
                self.create_fn_decl_from_callable_ty(name, &pty.get_base_type())
            }

            _ => None,
        }
    }

    /// Arrange positional/keyword call arguments to match parameter order.
    ///
    /// - Positional args fill parameters from the front until a keyword appears.
    /// - Keyword args match by name; duplicates or unknown names are rejected.
    /// - Positional arguments after a keyword are rejected.
    /// - Extra args are allowed only when `is_c_variadic` is true.
    ///
    /// Returns ordered arguments (one per param, `None` when filled by default) and any variadic tail.
    fn resolve_call_arguments<'a>(
        &self,
        params: &[(Symbol, bool)],
        ast_args: &'a ast::expr::Args,
        fn_name: Symbol,
        call_span: Span,
        is_c_variadic: bool,
    ) -> anyhow::Result<(Vec<Option<&'a ast::expr::Arg>>, Vec<&'a ast::expr::Arg>)> {
        let mut ordered_args: Vec<Option<&ast::expr::Arg>> = vec![None; params.len()];
        let mut variadic_args = Vec::new();
        let mut positional_index = 0usize;
        let mut seen_keyword = false;

        for arg in &ast_args.args {
            match &arg.name {
                Some(name) => {
                    seen_keyword = true;

                    let Some(param_index) = params
                        .iter()
                        .position(|(param_name, _)| *param_name == name.symbol())
                    else {
                        return Err(SemanticError::UnknownParameterName {
                            name: name.symbol(),
                            span: name.span(),
                        }
                        .into());
                    };

                    if ordered_args[param_index].is_some() {
                        return Err(SemanticError::DuplicateArgument {
                            name: name.symbol(),
                            span: arg.span,
                        }
                        .into());
                    }

                    ordered_args[param_index] = Some(arg);
                }

                None => {
                    if seen_keyword {
                        return Err(SemanticError::PositionalArgumentAfterKeyword {
                            span: arg.span,
                        }
                        .into());
                    }

                    if let Some(index) =
                        (positional_index..params.len()).find(|i| ordered_args[*i].is_none())
                    {
                        ordered_args[index] = Some(arg);
                        positional_index = index + 1;
                    } else if is_c_variadic {
                        variadic_args.push(arg);
                    } else {
                        return Err(SemanticError::TooManyArguments {
                            name: fn_name,
                            span: call_span,
                        }
                        .into());
                    }
                }
            }
        }

        for (arg, (_, has_default)) in ordered_args.iter().zip(params.iter()) {
            if arg.is_none() && !has_default {
                return Err(SemanticError::TooFewArguments {
                    name: fn_name,
                    span: call_span,
                }
                .into());
            }
        }

        Ok((ordered_args, variadic_args))
    }

    fn analyze_builtin_fn_call(
        &mut self,
        node: &ast::expr::FnCall,
        callee: Ident,
    ) -> Option<anyhow::Result<ir::expr::Expr>> {
        let keyword_arg = node.args.args.iter().find_map(|arg| arg.name.as_ref());
        let keyword_error = |name: &Ident| {
            Some(Err(SemanticError::UnknownParameterName {
                name: name.symbol(),
                span: name.span(),
            }
            .into()))
        };

        match callee.symbol().as_str() {
            "__unreachable" => {
                if let Some(name) = keyword_arg {
                    return keyword_error(name);
                }

                Some(Ok(ir::expr::Expr {
                    kind: ir::expr::ExprKind::BuiltinFnCall(ir::expr::BuiltinFnCall {
                        kind: ir::expr::BuiltinFnCallKind::Unreachable,
                        args: ir::expr::Args(vec![], node.span),
                        span: node.span,
                    }),
                    ty: Rc::new(ir_type::Ty::new_never()),
                    span: node.span,
                }))
            }

            "__str" => {
                if let Some(name) = keyword_arg {
                    return keyword_error(name);
                }

                Some(
                    node.args
                        .args
                        .iter()
                        .map(|arg| self.analyze_expr(&arg.value))
                        .collect::<anyhow::Result<Vec<_>>>()
                        .map(|args| ir::expr::Expr {
                            kind: ir::expr::ExprKind::BuiltinFnCall(ir::expr::BuiltinFnCall {
                                kind: ir::expr::BuiltinFnCallKind::Str,
                                args: ir::expr::Args(args, node.span),
                                span: node.span,
                            }),
                            ty: Rc::new(ir_type::wrap_in_ref(
                                Rc::new(ir_type::Ty::new_str(ir_type::Mutability::Not)),
                                ir_type::Mutability::Not,
                            )),
                            span: node.span,
                        }),
                )
            }

            "__slice_from_raw_parts" => {
                if let Some(name) = keyword_arg {
                    return keyword_error(name);
                }

                Some(
                    node.args
                        .args
                        .iter()
                        .map(|arg| self.analyze_expr(&arg.value))
                        .collect::<anyhow::Result<Vec<_>>>()
                        .and_then(|args| {
                            let elem_ty = match args[0].ty.kind.as_ref() {
                                ir_type::TyKind::Pointer(pty) => pty.pointee_ty.clone(),
                                ir_type::TyKind::Reference(rty) => match rty
                                    .get_base_type()
                                    .kind
                                    .as_ref()
                                {
                                    ir_type::TyKind::Pointer(pty) => pty.pointee_ty.clone(),
                                    _ => {
                                        return Err(SemanticError::MismatchedTypes {
                                            types: (args[0].ty.kind.to_string(), "*T".to_string()),
                                            span: node.span,
                                        }
                                        .into())
                                    }
                                },
                                _ => {
                                    return Err(SemanticError::MismatchedTypes {
                                        types: (args[0].ty.kind.to_string(), "*T".to_string()),
                                        span: node.span,
                                    }
                                    .into())
                                }
                            };

                            let slice_ty = Rc::new(ir_type::Ty {
                                kind: ir_type::TyKind::Slice(elem_ty).into(),
                                mutability: ir_type::Mutability::Not,
                            });

                            Ok(ir::expr::Expr {
                                kind: ir::expr::ExprKind::BuiltinFnCall(ir::expr::BuiltinFnCall {
                                    kind: ir::expr::BuiltinFnCallKind::SliceFromRawParts,
                                    args: ir::expr::Args(args, node.span),
                                    span: node.span,
                                }),
                                ty: Rc::new(ir_type::wrap_in_ref(
                                    slice_ty,
                                    ir_type::Mutability::Not,
                                )),
                                span: node.span,
                            })
                        }),
                )
            }

            "__ptr_add" => {
                if let Some(name) = keyword_arg {
                    return keyword_error(name);
                }

                Some(
                    node.args
                        .args
                        .iter()
                        .map(|arg| self.analyze_expr(&arg.value))
                        .collect::<anyhow::Result<Vec<_>>>()
                        .map(|args| ir::expr::Expr {
                            ty: args[0].ty.clone(),
                            kind: ir::expr::ExprKind::BuiltinFnCall(ir::expr::BuiltinFnCall {
                                kind: ir::expr::BuiltinFnCallKind::PointerAdd,
                                args: ir::expr::Args(args, node.span),
                                span: node.span,
                            }),
                            span: node.span,
                        }),
                )
            }

            "__sizeof" => {
                if let Some(name) = keyword_arg {
                    return keyword_error(name);
                }

                Some(
                    node.args
                        .args
                        .iter()
                        .map(|arg| self.analyze_expr(&arg.value))
                        .collect::<anyhow::Result<Vec<_>>>()
                        .map(|args| ir::expr::Expr {
                            ty: Rc::new(ir_type::make_fundamental_type(
                                ir_type::FundamentalTypeKind::U64,
                                ir_type::Mutability::Not,
                            )),
                            kind: ir::expr::ExprKind::BuiltinFnCall(ir::expr::BuiltinFnCall {
                                kind: ir::expr::BuiltinFnCallKind::SizeOf,
                                args: ir::expr::Args(args, node.span),
                                span: node.span,
                            }),
                            span: node.span,
                        }),
                )
            }

            "panic" => {
                if let Some(name) = keyword_arg {
                    return keyword_error(name);
                }

                Some(
                    node.args
                        .args
                        .iter()
                        .map(|arg| self.analyze_expr(&arg.value))
                        .collect::<anyhow::Result<Vec<_>>>()
                        .map(|args| ir::expr::Expr {
                            kind: ir::expr::ExprKind::BuiltinFnCall(ir::expr::BuiltinFnCall {
                                kind: ir::expr::BuiltinFnCallKind::Panic,
                                args: ir::expr::Args(args, node.span),
                                span: node.span,
                            }),
                            ty: Rc::new(ir_type::Ty::new_never()),
                            span: node.span,
                        }),
                )
            }

            _ => None,
        }
    }

    fn analyze_callable_value(
        &mut self,
        node: &ast::expr::FnCall,
        callee_name: Symbol,
        callee_ty: Rc<ir_type::Ty>,
        callee_value: Option<ir::expr::Expr>,
    ) -> anyhow::Result<ir::expr::Expr> {
        let callee_decl = self
            .create_fn_decl_from_callable_ty(callee_name, &callee_ty)
            .ok_or_else(|| SemanticError::NotCallable {
                ty: callee_ty.kind.to_string(),
                span: node.span,
            })?;

        let (ordered_args, variadic_args) = self.resolve_call_arguments(
            &callee_decl
                .params
                .iter()
                .map(|param| (param.name, param.default.is_some()))
                .collect::<Vec<_>>(),
            &node.args,
            callee_decl.name.symbol(),
            node.span,
            callee_decl.is_c_variadic,
        )?;

        let mut args = Vec::with_capacity(ordered_args.len() + variadic_args.len());

        for (arg, param) in ordered_args.into_iter().zip(callee_decl.params.iter()) {
            if let Some(arg) = arg {
                args.push(self.analyze_expr_with_expected_type(&arg.value, param.ty.clone())?);
            } else if let Some(default) = &param.default {
                args.push((**default).clone());
            } else {
                unreachable!();
            }
        }

        for arg in variadic_args {
            args.push(self.analyze_expr(&arg.value)?);
        }

        self.verify_fn_call_arguments(&callee_decl, &args, node.span)?;

        let call_expr = ir::expr::Expr {
            kind: ir::expr::ExprKind::FnCall(ir::expr::FnCall {
                callee: callee_decl.clone(),
                args: ir::expr::Args(args, node.span),
                span: node.span,
            }),
            ty: callee_decl.return_ty.clone(),
            span: node.span,
        };

        if let Some(callee_value) = callee_value {
            let call_ty = call_expr.ty.clone();
            let let_stmt = ir::stmt::Let {
                name: callee_decl.name.symbol(),
                init: Some(callee_value),
                ty: callee_ty,
                span: node.span,
            };

            return Ok(ir::expr::Expr {
                kind: ir::expr::ExprKind::Block(ir::stmt::Block {
                    body: vec![ir::stmt::Stmt::Let(let_stmt)],
                    last_expr: Some(Box::new(call_expr)),
                    span: node.span,
                }),
                ty: call_ty,
                span: node.span,
            });
        }

        Ok(call_expr)
    }

    fn analyze_fn_call_with_ident(
        &mut self,
        node: &ast::expr::FnCall,
        callee: Ident,
    ) -> anyhow::Result<ir::expr::Expr> {
        // Builtin functions
        if let Some(result) = self.analyze_builtin_fn_call(node, callee) {
            return result;
        }

        let symbol_value =
            self.lookup_symbol(callee.symbol())
                .ok_or_else(|| SemanticError::Undeclared {
                    name: callee.symbol(),
                    span: node.span,
                })?;

        let borrowed = symbol_value.borrow();
        match &borrowed.kind {
            SymbolTableValueKind::Function(fn_decl) => {
                let callee_decl = fn_decl.clone();
                let span = Span::new(node.span.start, node.span.finish, node.span.file);

                let (ordered_args, variadic_args) = self.resolve_call_arguments(
                    &callee_decl
                        .params
                        .iter()
                        .map(|param| (param.name, param.default.is_some()))
                        .collect::<Vec<_>>(),
                    &node.args,
                    callee_decl.name.symbol(),
                    node.span,
                    callee_decl.is_c_variadic,
                )?;

                let mut args = Vec::with_capacity(ordered_args.len() + variadic_args.len());

                for (arg, param) in ordered_args.into_iter().zip(callee_decl.params.iter()) {
                    if let Some(arg) = arg {
                        args.push(
                            self.analyze_expr_with_expected_type(&arg.value, param.ty.clone())?,
                        );
                    } else if let Some(default) = &param.default {
                        args.push((**default).clone());
                    } else {
                        unreachable!();
                    }
                }

                for arg in variadic_args {
                    args.push(self.analyze_expr(&arg.value)?);
                }

                self.verify_fn_call_arguments(&callee_decl, &args, node.span)?;

                Ok(ir::expr::Expr {
                    kind: ir::expr::ExprKind::FnCall(ir::expr::FnCall {
                        callee: callee_decl.clone(),
                        args: ir::expr::Args(args, node.span),
                        span,
                    }),
                    ty: callee_decl.return_ty.clone(),
                    span,
                })
            }

            SymbolTableValueKind::Generic(info) => self.analyze_generic_fn_call(info, node),

            SymbolTableValueKind::Variable(VariableInfo { ty }) => {
                self.analyze_callable_value(node, callee.symbol(), ty.clone(), None)
            }

            _ => unreachable!(),
        }
    }

    fn analyze_fn_call(&mut self, node: &ast::expr::FnCall) -> anyhow::Result<ir::expr::Expr> {
        if let Some(callee) = Self::callee_ident(node) {
            return self.analyze_fn_call_with_ident(node, callee);
        }

        let callee_expr = self.analyze_expr(&node.callee)?;
        let callee_name = self.fresh_temp_symbol("__callee");

        // Currently, generic arguments are only supported for named callees.
        debug_assert!(node.generic_args.is_none());

        self.analyze_callable_value(node, callee_name, callee_expr.ty.clone(), Some(callee_expr))
    }

    fn analyze_array_or_ptr_indexing(
        &mut self,
        node: &ast::expr::Indexing,
    ) -> anyhow::Result<ir::expr::Expr> {
        let operand = self.analyze_expr(&node.operand)?;

        let index = self.analyze_expr(&node.index)?;

        let span = Span::new(
            operand.span.start,
            node.index.span.finish,
            operand.span.file,
        );

        let not_indexable = |ty: &ir_type::TyKind| {
            Err(SemanticError::NotIndexable {
                ty: ty.to_string(),
                span,
            }
            .into())
        };

        let elem_ty = match operand.ty.kind.as_ref() {
            ir_type::TyKind::Pointer(pty) => pty.pointee_ty.clone(),

            ir_type::TyKind::Reference(rty) => {
                if let ir_type::TyKind::Array((elem_ty, _)) = rty.get_base_type().kind.as_ref() {
                    elem_ty.clone()
                } else if let ir_type::TyKind::Slice(elem_ty) = rty.get_base_type().kind.as_ref() {
                    elem_ty.clone()
                } else if let ir_type::TyKind::Fundamental(fty) = rty.get_base_type().kind.as_ref()
                {
                    if fty.kind == ir_type::FundamentalTypeKind::Str {
                        Rc::new(ir_type::make_fundamental_type(
                            ir_type::FundamentalTypeKind::Char,
                            ir_type::Mutability::Not,
                        ))
                    } else {
                        return not_indexable(&operand.ty.kind);
                    }
                } else {
                    return not_indexable(&operand.ty.kind);
                }
            }

            _ => {
                return not_indexable(&operand.ty.kind);
            }
        };

        Ok(ir::expr::Expr {
            ty: change_mutability_dup(elem_ty, operand.ty.mutability),
            kind: ir::expr::ExprKind::Indexing(ir::expr::Indexing {
                operand: Rc::new(operand),
                index: Box::new(index),
                span,
            }),
            span,
        })
    }

    fn analyze_slicing(&mut self, node: &ast::expr::Slicing) -> anyhow::Result<ir::expr::Expr> {
        let operand = Rc::new(self.analyze_expr(&node.operand)?);
        let span = node.span;

        let base_ty = match operand.ty.kind.as_ref() {
            ir_type::TyKind::Reference(rty) => rty.get_base_type(),
            _ => {
                return Err(SemanticError::NotIndexable {
                    ty: operand.ty.kind.to_string(),
                    span,
                }
                .into())
            }
        };

        let (elem_ty, default_end) = match base_ty.kind.as_ref() {
            ir_type::TyKind::Array((elem_ty, len)) => {
                (elem_ty.clone(), self.u64_literal(*len as u64, span))
            }
            ir_type::TyKind::Slice(elem_ty) => {
                let len_expr = self.slice_length_expr(operand.clone(), span)?;
                (elem_ty.clone(), len_expr)
            }
            _ => {
                return Err(SemanticError::NotIndexable {
                    ty: operand.ty.kind.to_string(),
                    span,
                }
                .into())
            }
        };

        self.generate_slice_impl(elem_ty.clone())?;

        let start_expr = match &node.start {
            Some(expr) => {
                let analyzed = self.analyze_expr(expr)?;
                self.cast_index_to_u64(analyzed, expr.span)?
            }
            None => self.u64_literal(0, span),
        };

        let end_expr = match &node.end {
            Some(expr) => {
                let analyzed = self.analyze_expr(expr)?;
                self.cast_index_to_u64(analyzed, expr.span)?
            }
            None => default_end,
        };

        let slice_ty = Rc::new(ir_type::Ty {
            kind: ir_type::TyKind::Slice(elem_ty.clone()).into(),
            mutability: ir_type::Mutability::Not,
        });

        let ty = Rc::new(ir_type::wrap_in_ref(slice_ty, operand.ty.mutability));

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::Slicing(ir::expr::Slicing {
                operand,
                start: Box::new(start_expr),
                end: Box::new(end_expr),
                elem_ty,
                span,
            }),
            ty,
            span,
        })
    }

    fn cast_index_to_u64(
        &self,
        expr: ir::expr::Expr,
        span: Span,
    ) -> anyhow::Result<ir::expr::Expr> {
        match expr.ty.kind.as_ref() {
            ir_type::TyKind::Fundamental(fty) if fty.is_int_or_char_or_bool() => {}
            ir_type::TyKind::Var(_) => {}
            _ => {
                return Err(SemanticError::MismatchedTypes {
                    types: (expr.ty.kind.to_string(), "integer".to_string()),
                    span,
                }
                .into());
            }
        }

        let target_ty = Rc::new(ir_type::make_fundamental_type(
            ir_type::FundamentalTypeKind::U64,
            ir_type::Mutability::Not,
        ));

        if matches!(
            expr.ty.kind.as_ref(),
            ir_type::TyKind::Fundamental(fty)
                if fty.kind == ir_type::FundamentalTypeKind::U64
        ) {
            return Ok(expr);
        }

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::Cast(ir::expr::Cast {
                operand: Box::new(expr),
                target_ty: target_ty.clone(),
                span,
            }),
            ty: target_ty,
            span,
        })
    }

    fn u64_literal(&self, value: u64, span: Span) -> ir::expr::Expr {
        let ty = Rc::new(ir_type::make_fundamental_type(
            ir_type::FundamentalTypeKind::U64,
            ir_type::Mutability::Not,
        ));

        ir::expr::Expr {
            kind: ir::expr::ExprKind::Int(ir::expr::Int {
                kind: ir::expr::IntKind::U64(value),
                span,
            }),
            ty,
            span,
        }
    }

    fn slice_length_expr(
        &self,
        operand: Rc<ir::expr::Expr>,
        span: Span,
    ) -> anyhow::Result<ir::expr::Expr> {
        let len_ty = Rc::new(ir_type::make_fundamental_type(
            ir_type::FundamentalTypeKind::U64,
            ir_type::Mutability::Not,
        ));

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::TupleIndexing(ir::expr::TupleIndexing {
                tuple: operand,
                element_ty: len_ty.clone(),
                index: 1,
                span,
            }),
            ty: len_ty,
            span,
        })
    }

    fn analyze_return(&mut self, node: &ast::expr::Return) -> anyhow::Result<ir::expr::Expr> {
        let expr = node
            .val
            .as_ref()
            .map(|val| self.analyze_expr(val))
            .transpose()?
            .map(Box::new);

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::Return(expr),
            ty: Rc::new(ir_type::Ty::new_never()),
            span: node.span,
        })
    }

    fn analyze_logical_not(
        &mut self,
        node: &ast::expr::LogicalNot,
    ) -> anyhow::Result<ir::expr::Expr> {
        let operand = self.analyze_expr(&node.operand)?;

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::LogicalNot(ir::expr::LogicalNot {
                operand: Box::new(operand),
                span: node.span,
            }),
            ty: Rc::new(ir_type::make_fundamental_type(
                ir_type::FundamentalTypeKind::Bool,
                ir_type::Mutability::Not,
            )),
            span: node.span,
        })
    }

    fn analyze_boolean_literal(&self, value: bool, span: Span) -> anyhow::Result<ir::expr::Expr> {
        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::BooleanLiteral(value),
            ty: Rc::new(ir_type::make_fundamental_type(
                ir_type::FundamentalTypeKind::Bool,
                ir_type::Mutability::Not,
            )),
            span,
        })
    }

    fn analyze_array_literal(
        &mut self,
        node: &ast::expr::ArrayLiteral,
        span: Span,
    ) -> anyhow::Result<ir::expr::Expr> {
        let mut elements = Vec::new();

        for element in node.elements.iter() {
            elements.push(self.analyze_expr(element)?);
        }

        // Create a fresh type variable for the element type
        // The type inference pass will unify all element types
        let elem_ty = self.infer_context.fresh();

        let ty = Rc::new(ir_type::wrap_in_ref(
            Rc::new(ir_type::Ty {
                kind: ir_type::TyKind::Array((elem_ty, elements.len() as u32)).into(),
                mutability: ir_type::Mutability::Not,
            }),
            ir_type::Mutability::Mut,
        ));

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::ArrayLiteral(ir::expr::ArrayLiteral { elements, span }),
            ty,
            span,
        })
    }

    fn analyze_array_repeat(
        &mut self,
        node: &ast::expr::ArrayRepeat,
        span: Span,
    ) -> anyhow::Result<ir::expr::Expr> {
        let value = self.analyze_expr(&node.value)?;
        let count = self.evaluate_array_repeat_count(&node.count)?;

        let elem_ty = self.infer_context.fresh();

        let ty = Rc::new(ir_type::wrap_in_ref(
            Rc::new(ir_type::Ty {
                kind: ir_type::TyKind::Array((elem_ty, count)).into(),
                mutability: ir_type::Mutability::Not,
            }),
            ir_type::Mutability::Mut,
        ));

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::ArrayRepeat(ir::expr::ArrayRepeat {
                value: Box::new(value),
                count,
                span,
            }),
            ty,
            span,
        })
    }

    fn evaluate_array_repeat_count(&self, count: &ast::expr::Expr) -> anyhow::Result<u32> {
        match &count.kind {
            ast::expr::ExprKind::Int(int) => {
                let value = int.as_u64();

                let value =
                    u32::try_from(value).map_err(|_| SemanticError::ArrayRepeatCountTooLarge {
                        span: count.span,
                        value,
                    })?;

                Ok(value)
            }

            _ => Err(SemanticError::ArrayRepeatCountNotConst { span: count.span }.into()),
        }
    }

    fn analyze_int(&mut self, node: &ast::expr::Int) -> anyhow::Result<ir::expr::Expr> {
        // Integer literals are always non-negative
        // Negative numbers are represented as unary minus operator
        let ast::expr::IntKind::Unsuffixed(n) = node.kind;

        // All integer literals use type inference
        // Type can be inferred to any integer type (i8, u8, i16, u16, i32, u32, i64, u64)
        Ok(ir::expr::Expr {
            ty: self.infer_context.fresh(),
            kind: ir::expr::ExprKind::Int(ir::expr::Int {
                kind: ir::expr::IntKind::Infer(n as i64),
                span: node.span,
            }),
            span: node.span,
        })
    }

    /// Analyze an expression with an optional expected type (e.g. from a function parameter).
    /// When the expression is a closure and the expected type is a closure type with matching
    /// param count, closure params are bound to the expected param types before analyzing the body.
    pub(super) fn analyze_expr_with_expected_type(
        &mut self,
        expr: &ast::expr::Expr,
        expected_ty: Rc<ir_type::Ty>,
    ) -> anyhow::Result<ir::expr::Expr> {
        if let ast::expr::ExprKind::Closure(node) = &expr.kind {
            return self.analyze_closure(node, Some(expected_ty));
        }
        self.analyze_expr(expr)
    }

    fn analyze_closure(
        &mut self,
        node: &ast::expr::Closure,
        expected_ty: Option<Rc<ir_type::Ty>>,
    ) -> anyhow::Result<ir::expr::Expr> {
        let base_depth = self.symbol_table_depth();
        self.push_closure_capture(base_depth);
        self.push_scope(SymbolTable::new());

        let expected_param_tys: Option<Vec<Rc<ir_type::Ty>>> =
            expected_ty.as_ref().and_then(|ty| {
                let unwrapped = match ty.kind.as_ref() {
                    ir_type::TyKind::Reference(rty) => rty.refee_ty.clone(),
                    _ => ty.clone(),
                };
                match unwrapped.kind.as_ref() {
                    ir_type::TyKind::Closure(closure_ty)
                        if closure_ty.param_tys.len() == node.params.len() =>
                    {
                        Some(closure_ty.param_tys.clone())
                    }
                    _ => None,
                }
            });

        let mut params = Vec::new();
        let mut param_tys = Vec::new();

        for (i, param) in node.params.iter().enumerate() {
            let ty = expected_param_tys
                .as_ref()
                .and_then(|tys| tys.get(i).cloned())
                .unwrap_or_else(|| self.infer_context.fresh());
            self.insert_symbol_to_current_scope(
                param.symbol(),
                SymbolTableValue::new(
                    SymbolTableValueKind::Variable(VariableInfo { ty: ty.clone() }),
                    self.current_module_path().clone(),
                ),
                param.span(),
            )?;
            params.push(param.symbol());
            param_tys.push(ty);
        }

        let body = self.analyze_expr(&node.body)?;

        self.pop_scope();
        let captures = self.pop_closure_capture().unwrap_or(ClosureCapture {
            base_depth,
            captured: HashSet::new(),
        });

        let param_symbols: HashSet<Symbol> = params.iter().copied().collect();

        let mut capture_exprs = Vec::new();
        let mut capture_tys = Vec::new();
        for symbol in captures.captured {
            // If the symbol is a parameter, it is not captured
            if param_symbols.contains(&symbol) {
                continue;
            }

            let ident = Ident::new(symbol, node.span);
            let captured_expr = self.analyze_ident(&ident)?;
            capture_tys.push(captured_expr.ty.clone());
            capture_exprs.push(captured_expr);
        }

        let closure_ty = Rc::new(ir_type::Ty {
            kind: ir_type::TyKind::Closure(ir_type::ClosureType {
                param_tys,
                ret_ty: body.ty.clone(),
                captures: capture_tys,
            })
            .into(),
            mutability: ir_type::Mutability::Not,
        });

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::Closure(ir::expr::Closure {
                params,
                body: Box::new(body),
                captures: capture_exprs,
                span: node.span,
            }),
            ty: closure_ty,
            span: node.span,
        })
    }

    fn analyze_block_expr(&mut self, node: &ast::stmt::Block) -> anyhow::Result<ir::expr::Expr> {
        if node.body.is_empty() {
            return Ok(ir::expr::Expr {
                kind: ir::expr::ExprKind::Block(kaede_ir::stmt::Block {
                    body: vec![],
                    last_expr: None,
                    span: node.span,
                }),
                ty: Rc::new(ir_type::Ty::new_unit()),
                span: node.span,
            });
        }

        self.push_scope(SymbolTable::new());

        let mut body = Vec::new();

        let mut idx: usize = 0;

        let last_stmt = loop {
            if idx + 1 == node.body.len() {
                // Last element
                break &node.body[idx];
            }

            body.push(self.analyze_stmt(&node.body[idx])?);

            idx += 1;
        };

        let retval = match &last_stmt.kind {
            ast::stmt::StmtKind::Expr(e) => {
                let last_expr = Box::new(self.analyze_expr(e)?);

                ir::expr::Expr {
                    ty: last_expr.ty.clone(),
                    kind: ir::expr::ExprKind::Block(kaede_ir::stmt::Block {
                        body,
                        last_expr: Some(last_expr),
                        span: node.span,
                    }),
                    span: node.span,
                }
            }

            // The end of the block is not an expression
            _ => {
                body.push(self.analyze_stmt(last_stmt)?);

                ir::expr::Expr {
                    kind: ir::expr::ExprKind::Block(kaede_ir::stmt::Block {
                        body,
                        last_expr: None,
                        span: node.span,
                    }),
                    ty: Rc::new(ir_type::Ty::new_unit()),
                    span: node.span,
                }
            }
        };

        self.pop_scope();

        Ok(retval)
    }

    fn analyze_string_literal(
        &self,
        node: &ast::expr::StringLiteral,
    ) -> anyhow::Result<ir::expr::Expr> {
        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::StringLiteral(ir::expr::StringLiteral {
                syb: node.syb,
                span: node.span,
            }),
            ty: Rc::new(ir_type::wrap_in_ref(
                Rc::new(ir_type::make_fundamental_type(
                    ir_type::FundamentalTypeKind::Str,
                    ir_type::Mutability::Not,
                )),
                ir_type::Mutability::Not,
            )),
            span: node.span,
        })
    }

    fn analyze_byte_string_literal(
        &self,
        node: &ast::expr::ByteStringLiteral,
    ) -> anyhow::Result<ir::expr::Expr> {
        let slice_ty = ir_type::Ty {
            kind: ir_type::TyKind::Slice(Rc::new(ir_type::make_fundamental_type(
                ir_type::FundamentalTypeKind::U8,
                ir_type::Mutability::Not,
            )))
            .into(),
            mutability: ir_type::Mutability::Not,
        };

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::ByteStringLiteral(ir::expr::ByteStringLiteral {
                bytes: node.bytes.clone(),
                span: node.span,
            }),
            ty: Rc::new(ir_type::wrap_in_ref(
                Rc::new(slice_ty),
                ir_type::Mutability::Not,
            )),
            span: node.span,
        })
    }

    fn analyze_char_literal(
        &self,
        node: &ast::expr::CharLiteral,
    ) -> anyhow::Result<ir::expr::Expr> {
        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::CharLiteral(ir::expr::CharLiteral {
                ch: node.ch,
                span: node.span,
            }),
            ty: Rc::new(ir_type::make_fundamental_type(
                ir_type::FundamentalTypeKind::Char,
                ir_type::Mutability::Not,
            )),
            span: node.span,
        })
    }

    fn analyze_byte_literal(
        &self,
        node: &ast::expr::ByteLiteral,
    ) -> anyhow::Result<ir::expr::Expr> {
        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::ByteLiteral(ir::expr::ByteLiteral {
                byte: node.byte,
                span: node.span,
            }),
            ty: Rc::new(ir_type::make_fundamental_type(
                ir_type::FundamentalTypeKind::U8,
                ir_type::Mutability::Not,
            )),
            span: node.span,
        })
    }

    fn analyze_binary(&mut self, node: &ast::expr::Binary) -> anyhow::Result<ir::expr::Expr> {
        use ast::expr::BinaryKind::*;

        match node.kind {
            Cast => self.analyze_cast(node),

            Access => self.analyze_access(node),
            ScopeResolution => self.analyze_scope_resolution(node),

            _ => self.analyze_arithmetic_binary(node),
        }
    }

    fn analyze_scope_resolution(
        &mut self,
        node: &ast::expr::Binary,
    ) -> anyhow::Result<ir::expr::Expr> {
        assert!(matches!(node.kind, ast::expr::BinaryKind::ScopeResolution));

        let (left, generic_args) = match &node.lhs.kind {
            // Foo::Bar
            ast::expr::ExprKind::Ident(id) => (id, None),

            // Foo<T>::Bar
            ast::expr::ExprKind::GenericIdent(gi) => (&gi.0, Some(gi.1.clone())),

            _ => todo!(),
        };

        // If generic arguments are provided, we need to analyze the user defined type to generate actual generic types.
        let udt = ast_type::UserDefinedType {
            name: *left,
            generic_args: generic_args.clone(),
        };
        let udt = self.analyze_user_defined_type(&udt, ir_type::Mutability::Not)?;

        // Expect the type to be a user defined type
        let udt = match udt.kind.as_ref() {
            ir_type::TyKind::UserDefined(udt) => udt,
            _ => unreachable!(),
        };

        // Try to create new enum variant with value
        // If it fails, try to call static methods
        let result = if let ast::expr::ExprKind::FnCall(right) = &node.rhs.kind {
            if right.args.args.len() != 1 {
                // Static methods
                self.analyze_static_method_call(udt, right)
            } else if let Some(callee) = Self::callee_ident(right) {
                let value = &right.args.args.front().unwrap().value;

                match self.analyze_enum_variant(udt, callee, Some(value), *left) {
                    Ok(val) => Ok(val),
                    Err(err) => {
                        err.downcast().and_then(|e| match e {
                            SemanticError::Undeclared { name, .. }
                            | SemanticError::NoVariant {
                                parent_name: name, ..
                            }
                            | SemanticError::NotAnEnum { name, .. }
                            | SemanticError::CannotAssignValueToVariant {
                                variant_name: name,
                                ..
                                // TODO: Handle generic type errors (Fix to not use starts_with)
                            } if name.as_str().starts_with(left.symbol().as_str()) => {
                                // Couldn't create enum variant, so try to call static methods
                                self.analyze_static_method_call(udt, right)
                            }
                            _ => Err(e.into()),
                        })
                    }
                }
            } else {
                self.analyze_static_method_call(udt, right)
            }
        } else if let ast::expr::ExprKind::Ident(right) = &node.rhs.kind {
            // Create enum variant without value.
            self.analyze_enum_variant(udt, *right, None, *left)
        } else {
            todo!()
        };

        result
    }

    fn analyze_enum_variant(
        &mut self,
        udt: &ir_type::UserDefinedType,
        variant_name: Ident,
        value: Option<&ast::expr::Expr>,
        left_ident: Ident,
    ) -> anyhow::Result<ir::expr::Expr> {
        let left_span = left_ident.span();

        let not_an_enum_error = || SemanticError::NotAnEnum {
            name: left_ident.symbol(),
            span: left_ident.span(),
        };

        let enum_ir = match &udt.kind {
            ir_type::UserDefinedTypeKind::Enum(em) => em.clone(),
            ir_type::UserDefinedTypeKind::Placeholder(qsym) => {
                match &self
                    .lookup_qualified_symbol(qsym.clone())
                    .unwrap()
                    .borrow()
                    .kind
                {
                    SymbolTableValueKind::Enum(enum_ir) => enum_ir.clone(),
                    _ => {
                        return Err(not_an_enum_error().into());
                    }
                }
            }
            _ => {
                return Err(not_an_enum_error().into());
            }
        };

        let variant_info = enum_ir
            .variants
            .iter()
            .find(|v| v.name == variant_name.symbol())
            .ok_or(SemanticError::NoVariant {
                variant_name: variant_name.symbol(),
                parent_name: udt.name(),
                span: left_span,
            })?;

        let value = match value {
            Some(value) => Some(self.analyze_expr(value)?),
            None => None,
        };

        if value.is_some() && variant_info.ty.is_none() {
            // No type is specified for the variant, but a value is provided.
            return Err(SemanticError::CannotAssignValueToVariant {
                variant_name: variant_name.symbol(),
                parent_name: udt.name(),
                span: left_span,
            }
            .into());
        }

        let span = if let Some(value) = &value {
            Span::new(left_span.start, value.span.finish, left_span.file)
        } else {
            Span::new(left_span.start, variant_name.span().finish, left_span.file)
        };

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::EnumVariant(ir::expr::EnumVariant {
                enum_info: enum_ir.clone(),
                variant_offset: variant_info.offset,
                value: value.map(Box::new),
                span,
            }),
            ty: Rc::new(ir_type::wrap_in_ref(
                Rc::new(ir_type::Ty {
                    kind: ir_type::TyKind::UserDefined(udt.clone()).into(),
                    mutability: ir_type::Mutability::Not,
                }),
                ir_type::Mutability::Mut,
            )),
            span,
        })
    }

    fn analyze_static_method_call(
        &mut self,
        udt: &ir_type::UserDefinedType,
        call_node: &ast::expr::FnCall,
    ) -> anyhow::Result<ir::expr::Expr> {
        let method_symbol = self.callee_symbol(call_node)?;
        let method_name = self.create_method_key(udt.name(), method_symbol, true);

        let qualified_method_name =
            QualifiedSymbol::new(udt.qualified_symbol().module_path().clone(), method_name);

        // Lookup method from symbol table
        let method_decl =
            self.lookup_qualified_symbol(qualified_method_name)
                .ok_or(SemanticError::NoMethod {
                    method_name: method_symbol,
                    parent_name: udt.name(),
                    span: call_node.span,
                })?;

        let method_decl = match &method_decl.borrow().kind {
            SymbolTableValueKind::Function(fn_) => fn_.clone(),
            _ => unreachable!(),
        };

        let (ordered_args, variadic_args) = self.resolve_call_arguments(
            &method_decl
                .params
                .iter()
                .map(|param| (param.name, param.default.is_some()))
                .collect::<Vec<_>>(),
            &call_node.args,
            method_decl.name.symbol(),
            call_node.span,
            method_decl.is_c_variadic,
        )?;

        let mut args = Vec::with_capacity(ordered_args.len() + variadic_args.len());

        for (arg, param) in ordered_args.into_iter().zip(method_decl.params.iter()) {
            if let Some(arg) = arg {
                args.push(self.analyze_expr_with_expected_type(&arg.value, param.ty.clone())?);
            } else if let Some(default) = &param.default {
                args.push((**default).clone());
            } else {
                unreachable!();
            }
        }

        for arg in variadic_args {
            args.push(self.analyze_expr(&arg.value)?);
        }

        self.verify_fn_call_arguments(&method_decl, &args, call_node.span)?;

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::FnCall(ir::expr::FnCall {
                callee: method_decl.clone(),
                args: ir::expr::Args(args, call_node.span),
                span: call_node.span,
            }),
            ty: method_decl.return_ty.clone(),
            span: call_node.span,
        })
    }

    fn analyze_access(&mut self, node: &ast::expr::Binary) -> anyhow::Result<ir::expr::Expr> {
        assert!(matches!(node.kind, ast::expr::BinaryKind::Access));

        let try_module_access = |analyzer: &mut Self| -> anyhow::Result<Option<ir::expr::Expr>> {
            let mut modules = Vec::new();
            node.lhs.collect_access_chain(&mut modules);

            if modules.is_empty() {
                return Ok(None);
            }

            if let Ok((_, module_path)) = analyzer.create_module_path_from_access_chain(
                modules
                    .iter()
                    .map(|i| i.symbol())
                    .collect::<Vec<_>>()
                    .as_slice(),
                node.lhs.span,
            ) {
                if analyzer.modules.contains_key(&module_path) {
                    let expr = analyzer
                        .with_module(module_path, |analyzer| analyzer.analyze_expr(&node.rhs))?;
                    return Ok(Some(expr));
                }
            }

            Ok(None)
        };

        let left = match self.analyze_expr(&node.lhs) {
            Ok(left) => left,
            Err(err) => match err.downcast::<SemanticError>() {
                Ok(semantic_error) => match semantic_error {
                    SemanticError::Undeclared { .. } => {
                        if let Some(expr) = try_module_access(self)? {
                            return Ok(expr);
                        }
                        return Err(semantic_error.into());
                    }
                    other => Err(other.into()),
                },
                Err(err) => Err(err),
            }?,
        };

        let left_ty = left.ty.clone();

        if left_ty.is_str() {
            // String indexing or method calling
            return self.analyze_str_indexing_or_method_call(left, &node.rhs);
        }

        if let ir_type::TyKind::Reference(rty) = left_ty.kind.as_ref() {
            // Struct access or tuple indexing
            if matches!(
                rty.get_base_type().kind.as_ref(),
                ir_type::TyKind::UserDefined(_)
                    | ir_type::TyKind::Tuple(_)
                    | ir_type::TyKind::Slice(_)
                    | ir_type::TyKind::Array(_)
            ) {
                self.analyze_struct_access_or_tuple_indexing(left, &node.rhs)
            } else {
                Err(SemanticError::HasNoFields {
                    span: node.lhs.span,
                }
                .into())
            }
        } else {
            // Fundamental type method calling
            let call_node = match &node.rhs.kind {
                kaede_ast::expr::ExprKind::FnCall(call_node) => call_node,
                _ => {
                    return Err(SemanticError::HasNoFields {
                        span: node.lhs.span,
                    }
                    .into())
                }
            };

            if let ir_type::TyKind::Fundamental(fty) = left_ty.kind.as_ref() {
                self.analyze_fundamental_type_method_call(left, fty, call_node)
            } else if matches!(left_ty.kind.as_ref(), ir_type::TyKind::Var(_)) {
                // Fallback for type variable receiver (e.g. let n = 3; n.abs() before inference).
                // Closure args are bound from expected type before body analysis, so they won't hit this.
                let fty = ir_type::FundamentalType {
                    kind: ir_type::FundamentalTypeKind::I32,
                };
                self.analyze_fundamental_type_method_call(left, &fty, call_node)
            } else {
                Err(SemanticError::HasNoFields {
                    span: node.lhs.span,
                }
                .into())
            }
        }
    }

    fn analyze_str_indexing_or_method_call(
        &mut self,
        left: ir::expr::Expr,
        ast_right: &ast::expr::Expr,
    ) -> anyhow::Result<ir::expr::Expr> {
        if let ast::expr::ExprKind::FnCall(call_node) = &ast_right.kind {
            // Method call
            let span = Span::new(left.span.start, ast_right.span.finish, left.span.file);
            let callee_symbol = self.callee_symbol(call_node)?;

            self.create_method_call_ir(
                callee_symbol,
                ModulePath::new(vec![]),
                "str".to_owned().into(),
                call_node,
                left,
                span,
            )
        } else {
            let right = self.analyze_expr(ast_right)?;
            self.analyze_str_indexing(left, right)
        }
    }

    fn analyze_str_indexing(
        &self,
        left: ir::expr::Expr,
        right: ir::expr::Expr,
    ) -> anyhow::Result<ir::expr::Expr> {
        let span = Span::new(left.span.start, right.span.finish, left.span.file);

        let index_ty = right.ty.clone();

        // Skip type checking if index is a type variable
        // Type inference pass will handle it
        let expected_index_ty = ir_type::make_fundamental_type(
            ir_type::FundamentalTypeKind::I32,
            ir_type::Mutability::Not,
        );

        if !matches!(index_ty.kind.as_ref(), ir_type::TyKind::Var(_))
            && !ir_type::is_same_type(&index_ty, &expected_index_ty)
        {
            return Err(SemanticError::MismatchedTypes {
                types: (
                    index_ty.kind.to_string(),
                    expected_index_ty.kind.to_string(),
                ),
                span,
            }
            .into());
        }

        let index = match &right.kind {
            ir::expr::ExprKind::Int(int) => int.as_u64(),
            _ => unreachable!(),
        };

        // (*i8, u64)
        let ty = Rc::new(match index {
            0 => ir_type::Ty {
                kind: ir_type::TyKind::Pointer(ir_type::PointerType {
                    pointee_ty: ir_type::make_fundamental_type(
                        ir_type::FundamentalTypeKind::I8,
                        ir_type::Mutability::Not,
                    )
                    .into(),
                })
                .into(),
                mutability: ir_type::Mutability::Not,
            },

            1 => ir_type::make_fundamental_type(
                ir_type::FundamentalTypeKind::U64,
                ir_type::Mutability::Not,
            ),

            _ => return Err(SemanticError::IndexOutOfRange { index, span }.into()),
        });

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::TupleIndexing(ir::expr::TupleIndexing {
                tuple: Rc::new(left),
                element_ty: ty.clone(),
                index: index as u32,
                span,
            }),
            ty,
            span,
        })
    }

    fn verify_fn_call_arguments(
        &self,
        fn_decl: &ir::top::FnDecl,
        args: &[ir::expr::Expr],
        call_node_span: Span,
    ) -> anyhow::Result<()> {
        if args.len() < fn_decl.params.len() {
            return Err(SemanticError::TooFewArguments {
                name: fn_decl.name.symbol(),
                span: call_node_span,
            }
            .into());
        }

        if !fn_decl.is_c_variadic && args.len() > fn_decl.params.len() {
            return Err(SemanticError::TooManyArguments {
                name: fn_decl.name.symbol(),
                span: call_node_span,
            }
            .into());
        }

        for (param, arg) in fn_decl.params.iter().zip(args.iter()) {
            if param.ty.mutability.is_mut() && arg.ty.mutability.is_not() {
                return Err(
                    SemanticError::CannotAssignImmutableToMutable { span: arg.span }.into(),
                );
            }
        }

        Ok(())
    }

    fn analyze_struct_access_or_tuple_indexing(
        &mut self,
        lhs: ir::expr::Expr,
        rhs: &ast::expr::Expr,
    ) -> anyhow::Result<ir::expr::Expr> {
        let span = Span::new(lhs.span.start, rhs.span.finish, lhs.span.file);

        assert!(matches!(
            lhs.ty.kind.as_ref(),
            ir_type::TyKind::Reference(_)
        ));

        let base_type = match lhs.ty.kind.as_ref() {
            ir_type::TyKind::Reference(rty) => rty.get_base_type(),
            _ => unreachable!(),
        };

        match base_type.kind.as_ref() {
            ir_type::TyKind::UserDefined(udt) => self.analyze_struct_access(lhs, rhs, udt),

            ir_type::TyKind::Tuple(elements_ty) => {
                self.analyze_tuple_indexing(lhs, rhs, elements_ty)
            }

            ir_type::TyKind::Slice(elem_ty) => match &rhs.kind {
                ast::expr::ExprKind::FnCall(call_node) => {
                    let span = Span::new(lhs.span.start, call_node.span.finish, lhs.span.file);
                    let callee_symbol = self.callee_symbol(call_node)?;

                    self.create_method_call_ir(
                        callee_symbol,
                        self.module_path().clone(),
                        self.slice_method_parent_name(elem_ty),
                        call_node,
                        lhs,
                        span,
                    )
                }

                _ => {
                    let elements_ty = vec![
                        Rc::new(ir_type::Ty {
                            kind: ir_type::TyKind::Pointer(ir_type::PointerType {
                                pointee_ty: elem_ty.clone(),
                            })
                            .into(),
                            mutability: ir_type::Mutability::Not,
                        }),
                        Rc::new(ir_type::make_fundamental_type(
                            ir_type::FundamentalTypeKind::U64,
                            ir_type::Mutability::Not,
                        )),
                    ];

                    self.analyze_tuple_indexing(lhs, rhs, &elements_ty)
                }
            },

            ir_type::TyKind::Array((elem_ty, _len)) => match &rhs.kind {
                ast::expr::ExprKind::FnCall(call_node) => {
                    let span = Span::new(lhs.span.start, call_node.span.finish, lhs.span.file);
                    let callee_symbol = self.callee_symbol(call_node)?;

                    // Generate slice impl for this element type
                    self.generate_slice_impl(elem_ty.clone())?;

                    // Arrays use slice methods (array is coerced to slice at codegen)
                    self.create_method_call_ir(
                        callee_symbol,
                        self.module_path().clone(),
                        self.slice_method_parent_name(elem_ty),
                        call_node,
                        lhs,
                        span,
                    )
                }

                _ => Err(SemanticError::HasNoFields { span }.into()),
            },

            // str is internally represented as (*i8, u64) tuple
            ir_type::TyKind::Fundamental(fty) if fty.kind == ir_type::FundamentalTypeKind::Str => {
                // str type: (*i8, u64)
                let elements_ty = vec![
                    Rc::new(ir_type::Ty {
                        kind: ir_type::TyKind::Pointer(ir_type::PointerType {
                            pointee_ty: Rc::new(ir_type::make_fundamental_type(
                                ir_type::FundamentalTypeKind::I8,
                                ir_type::Mutability::Not,
                            )),
                        })
                        .into(),
                        mutability: ir_type::Mutability::Not,
                    }),
                    Rc::new(ir_type::make_fundamental_type(
                        ir_type::FundamentalTypeKind::U64,
                        ir_type::Mutability::Not,
                    )),
                ];
                self.analyze_tuple_indexing(lhs, rhs, &elements_ty)
            }

            _ => Err(SemanticError::HasNoFields { span }.into()),
        }
    }

    fn analyze_tuple_indexing(
        &mut self,
        lhs: ir::expr::Expr,
        rhs: &ast::expr::Expr,
        elements_ty: &[Rc<ir_type::Ty>],
    ) -> anyhow::Result<ir::expr::Expr> {
        let span = Span::new(lhs.span.start, rhs.span.finish, lhs.span.file);

        let index = match &rhs.kind {
            ast::expr::ExprKind::Int(int) => int.as_u64() as u32,
            _ => return Err(SemanticError::TupleRequireAccessByIndex { span: rhs.span }.into()),
        };

        Ok(ir::expr::Expr {
            span,
            ty: change_mutability_dup(elements_ty[index as usize].clone(), lhs.ty.mutability),
            kind: ir::expr::ExprKind::TupleIndexing(ir::expr::TupleIndexing {
                tuple: Rc::new(lhs),
                index,
                element_ty: elements_ty[index as usize].clone(),
                span,
            }),
        })
    }

    fn analyze_struct_access(
        &mut self,
        lhs: ir::expr::Expr,
        rhs: &ast::expr::Expr,
        udt: &ir_type::UserDefinedType,
    ) -> anyhow::Result<ir::expr::Expr> {
        match &rhs.kind {
            // Field
            ast::expr::ExprKind::Ident(field_name) => {
                self.analyze_struct_field_access(lhs, rhs, udt, *field_name)
            }

            // Method
            ast::expr::ExprKind::FnCall(node) => self.analyze_struct_method_call(lhs, udt, node),

            kind => unreachable!("{:?}", kind),
        }
    }

    /// e.g. test.Person.get_age
    fn analyze_struct_method_call(
        &mut self,
        lhs: ir::expr::Expr,
        udt: &ir_type::UserDefinedType,
        call_node: &ast::expr::FnCall,
    ) -> anyhow::Result<ir::expr::Expr> {
        let span = Span::new(lhs.span.start, call_node.span.finish, lhs.span.file);
        let callee_symbol = self.callee_symbol(call_node)?;
        self.create_method_call_ir(
            callee_symbol,
            udt.module_path().clone(),
            udt.name(),
            call_node,
            lhs,
            span,
        )
    }

    fn analyze_struct_field_access(
        &self,
        lhs: ir::expr::Expr,
        rhs: &ast::expr::Expr,
        udt: &ir_type::UserDefinedType,
        field_name: Ident,
    ) -> anyhow::Result<ir::expr::Expr> {
        let span = Span::new(lhs.span.start, rhs.span.finish, lhs.span.file);

        let struct_info = self
            .lookup_qualified_symbol(udt.qualified_symbol())
            .map(|value| match &value.borrow().kind {
                SymbolTableValueKind::Struct(struct_info) => struct_info.clone(),

                _ => unreachable!(),
            })
            .unwrap();

        let (field_ty, field_offset) = struct_info
            .fields
            .iter()
            .find_map(|field| {
                if field.name == field_name.symbol() {
                    Some((field.ty.clone(), field.offset))
                } else {
                    None
                }
            })
            .ok_or_else(|| SemanticError::NoMember {
                member_name: field_name.symbol(),
                parent_name: struct_info.name.symbol(),
                span: rhs.span,
            })?;

        Ok(ir::expr::Expr {
            span,
            ty: change_mutability_dup(field_ty, lhs.ty.mutability),
            kind: ir::expr::ExprKind::FieldAccess(ir::expr::FieldAccess {
                struct_info,
                operand: Box::new(lhs),
                field_name: field_name.symbol(),
                field_offset,
                span,
            }),
        })
    }

    fn create_method_call_ir(
        &mut self,
        method_name: Symbol,
        module_path: ModulePath,
        parent_name: Symbol,
        call_node: &ast::expr::FnCall,
        this: ir::expr::Expr,
        span: Span,
    ) -> anyhow::Result<ir::expr::Expr> {
        let original_method_name = method_name;
        let no_method_err = || SemanticError::NoMethod {
            method_name: original_method_name,
            parent_name,
            span: call_node.span,
        };

        let method_name = self.create_method_key(parent_name, original_method_name, false);

        let callee =
            match self.lookup_qualified_symbol(QualifiedSymbol::new(module_path, method_name)) {
                Some(value) => {
                    let value = value.borrow();

                    if let SymbolTableValueKind::Function(fn_) = &value.kind {
                        fn_.clone()
                    } else {
                        return Err(no_method_err().into());
                    }
                }

                None => {
                    return Err(no_method_err().into());
                }
            };

        if let Some(self_param) = callee.params.first() {
            if self_param.name.as_str() == "self"
                && self_param.ty.mutability.is_mut()
                && this.ty.mutability.is_not()
            {
                return Err(SemanticError::CannotCallMutableMethodOnImmutableValue { span }.into());
            }
        }

        let params_without_self = if callee.params.is_empty() {
            &[][..]
        } else {
            &callee.params[1..]
        };

        let (ordered_args, variadic_args) = self.resolve_call_arguments(
            &params_without_self
                .iter()
                .map(|param| (param.name, param.default.is_some()))
                .collect::<Vec<_>>(),
            &call_node.args,
            callee.name.symbol(),
            call_node.span,
            callee.is_c_variadic,
        )?;

        let mut args = Vec::with_capacity(1 + ordered_args.len() + variadic_args.len());
        args.push(this);

        for (arg, param) in ordered_args.into_iter().zip(params_without_self.iter()) {
            if let Some(arg) = arg {
                args.push(self.analyze_expr_with_expected_type(&arg.value, param.ty.clone())?);
            } else if let Some(default) = &param.default {
                args.push((**default).clone());
            } else {
                unreachable!();
            }
        }

        for arg in variadic_args {
            args.push(self.analyze_expr(&arg.value)?);
        }

        self.verify_fn_call_arguments(&callee, &args, call_node.span)?;

        let call_node = ir::expr::FnCall {
            callee: callee.clone(),
            args: ir::expr::Args(args, span),
            span,
        };

        // For simplicity, we treat the method call as a function call.
        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::FnCall(call_node),
            ty: callee.return_ty.clone(),
            span,
        })
    }

    /// e.g. `i32.abs`
    fn analyze_fundamental_type_method_call(
        &mut self,
        left: ir::expr::Expr,
        fty: &ir_type::FundamentalType,
        call_node: &ast::expr::FnCall,
    ) -> anyhow::Result<ir::expr::Expr> {
        let span = Span::new(left.span.start, call_node.span.finish, left.span.file);
        let callee_symbol = self.callee_symbol(call_node)?;

        self.create_method_call_ir(
            callee_symbol,
            ModulePath::new(vec![]),
            fty.kind.to_string().into(),
            call_node,
            left,
            span,
        )
    }

    fn analyze_cast(&mut self, node: &ast::expr::Binary) -> anyhow::Result<ir::expr::Expr> {
        let operand = self.analyze_expr(&node.lhs)?;

        let target_ast_ty = match &node.rhs.kind {
            ast::expr::ExprKind::Ty(ty) => ty,
            _ => {
                return Err(SemanticError::ExpectedTypeInCast {
                    span: node.rhs.span,
                }
                .into())
            }
        };

        let target_ir_ty = self.analyze_type(target_ast_ty)?;

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::Cast(ir::expr::Cast {
                operand: Box::new(operand),
                target_ty: target_ir_ty.clone(),
                span: Span::new(
                    node.lhs.span.start,
                    node.rhs.span.finish,
                    node.lhs.span.file,
                ),
            }),
            ty: target_ir_ty,
            span: Span::new(
                node.lhs.span.start,
                node.rhs.span.finish,
                node.lhs.span.file,
            ),
        })
    }

    fn analyze_arithmetic_binary(
        &mut self,
        node: &ast::expr::Binary,
    ) -> anyhow::Result<ir::expr::Expr> {
        let lhs = Rc::new(self.analyze_expr(&node.lhs)?);
        let rhs = Rc::new(self.analyze_expr(&node.rhs)?);

        let span = Span::new(
            node.lhs.span.start,
            node.rhs.span.finish,
            node.lhs.span.file,
        );

        let to = |kind, ty| ir::expr::Expr {
            kind: ir::expr::ExprKind::Binary(ir::expr::Binary {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
                kind,
                span,
            }),
            ty,
            span,
        };

        let boolean_ty = || {
            Rc::new(ir_type::make_fundamental_type(
                ir_type::FundamentalTypeKind::Bool,
                ir_type::Mutability::Not,
            ))
        };

        use ast::expr::BinaryKind::*;

        Ok(match node.kind {
            Add => to(ir::expr::BinaryKind::Add, lhs.ty.clone()),
            Sub => to(ir::expr::BinaryKind::Sub, lhs.ty.clone()),
            Mul => to(ir::expr::BinaryKind::Mul, lhs.ty.clone()),
            Div => to(ir::expr::BinaryKind::Div, lhs.ty.clone()),
            Rem => to(ir::expr::BinaryKind::Rem, lhs.ty.clone()),
            Eq => to(ir::expr::BinaryKind::Eq, boolean_ty()),
            Lt => to(ir::expr::BinaryKind::Lt, boolean_ty()),
            Ne => to(ir::expr::BinaryKind::Ne, boolean_ty()),
            Le => to(ir::expr::BinaryKind::Le, boolean_ty()),
            Gt => to(ir::expr::BinaryKind::Gt, boolean_ty()),
            Ge => to(ir::expr::BinaryKind::Ge, boolean_ty()),
            LogicalOr => to(ir::expr::BinaryKind::LogicalOr, boolean_ty()),
            LogicalAnd => to(ir::expr::BinaryKind::LogicalAnd, boolean_ty()),

            _ => unreachable!(),
        })
    }

    fn analyze_ident(&mut self, node: &Ident) -> anyhow::Result<ir::expr::Expr> {
        let primary = self.lookup_symbol_with_depth(node.symbol());

        let fallback = if primary.is_none() && self.current_module_path() != self.module_path() {
            let mut new_ctx = self.context.clone();
            new_ctx.set_current_module_path(self.module_path().clone());
            self.with_context(new_ctx, |analyzer| {
                analyzer.lookup_symbol_with_depth(node.symbol())
            })
        } else {
            None
        };

        primary
            .or(fallback)
            .map(|(value, depth)| match &value.borrow().kind {
                SymbolTableValueKind::Variable(VariableInfo { ty }) => {
                    self.register_capture(node.symbol(), depth);
                    Ok(ir::expr::Expr {
                        kind: ir::expr::ExprKind::Variable(ir::expr::Variable {
                            name: node.symbol(),
                            ty: ty.clone(),
                            span: node.span(),
                        }),
                        ty: ty.clone(),
                        span: node.span(),
                    })
                }
                SymbolTableValueKind::Function(decl) => {
                    let ty = self.closure_ty_from_fn_decl(decl);
                    Ok(ir::expr::Expr {
                        kind: ir::expr::ExprKind::FnPointer(ir::expr::FnPointer {
                            decl: decl.clone(),
                            span: node.span(),
                        }),
                        ty,
                        span: node.span(),
                    })
                }
                _ => Err(SemanticError::ExpectedVariable {
                    name: node.symbol(),
                    span: node.span(),
                }),
            })
            .transpose()?
            .ok_or_else(|| {
                SemanticError::Undeclared {
                    name: node.symbol(),
                    span: node.span(),
                }
                .into()
            })
    }
}
