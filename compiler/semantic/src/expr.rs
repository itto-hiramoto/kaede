use std::{collections::BTreeSet, rc::Rc, vec};

use crate::{
    error::SemanticError,
    symbol_table::{
        GenericFuncInfo, GenericInfo, GenericKind, SymbolTable, SymbolTableValue,
        SymbolTableValueKind, VariableInfo,
    },
    SemanticAnalyzer,
};

use kaede_ast as ast;
use kaede_ast_type as ast_type;
use kaede_ir::{
    self as ir,
    qualified_symbol::QualifiedSymbol,
    ty::{change_mutability_dup, make_fundamental_type},
};
use kaede_ir::{module_path::ModulePath, ty as ir_type};
use kaede_span::Span;
use kaede_symbol::{Ident, Symbol};

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
            ExprKind::Int(node) => self.analyze_int(node),
            ExprKind::True => self.analyze_boolean_literal(true, span),
            ExprKind::False => self.analyze_boolean_literal(false, span),
            ExprKind::Block(node) => self.analyze_block_expr(node),
            ExprKind::StringLiteral(node) => self.analyze_string_literal(node),
            ExprKind::Binary(node) => self.analyze_binary(node),
            ExprKind::Ident(node) => self.analyze_ident(node),
            ExprKind::LogicalNot(node) => self.analyze_logical_not(node),
            ExprKind::Return(node) => self.analyze_return(node),
            ExprKind::Indexing(node) => self.analyze_arary_indexing(node),
            ExprKind::FnCall(node) => self.analyze_fn_call(node),
            ExprKind::If(node) => self.analyze_if(node),
            ExprKind::Break(node) => self.analyze_break(node),
            ExprKind::Loop(node) => self.analyze_loop(node),
            ExprKind::Match(node) => self.analyze_match(node),
            ExprKind::StructLiteral(node) => self.analyze_struct_literal(node),
            ExprKind::TupleLiteral(node) => self.analyze_tuple_literal(node),

            ExprKind::Ty(_) => todo!(),
            ExprKind::GenericIdent(_) => todo!(),
        }
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
            _ => unreachable!(),
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

        let ir = ir::expr::StructLiteral {
            struct_info: struct_ir.clone(),
            values: node
                .values
                .iter()
                .map(|(name, value)| {
                    let value = self.analyze_expr(value)?;
                    Ok((name.symbol(), value))
                })
                .collect::<anyhow::Result<Vec<_>>>()?,
        };

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::StructLiteral(ir),
            ty: Rc::new(ir::ty::wrap_in_ref(struct_ty, ir_type::Mutability::Mut)),
            span: node.span,
        })
    }

    /// Decompose enum variant patterns (like `A::B` or `A::B(a, b, c)`)
    fn decompose_enum_variant_pattern<'p>(
        &self,
        pattern: &'p ast::expr::Expr,
    ) -> DecomposedEnumVariantPattern<'p> {
        let (module_names, enum_name, variant_name, param) = match &pattern.kind {
            ast::expr::ExprKind::Binary(b) => match b.kind {
                ast::expr::BinaryKind::ScopeResolution => match &b.lhs.kind {
                    ast::expr::ExprKind::Ident(i) => {
                        let (variant_name, param) = match &b.rhs.kind {
                            ast::expr::ExprKind::Ident(ident) => (ident, None),
                            ast::expr::ExprKind::FnCall(fncall) => {
                                (&fncall.callee, Some(&fncall.args))
                            }
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
                        self.collect_access_chain(&b.lhs, &mut modules);

                        let decomposed = self.decompose_enum_variant_pattern(&b.rhs);

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
        // If there is a catch-all arm, we don't need to check exhaustiveness
        if node.arms.iter().any(|arm| arm.is_catch_all) {
            return Ok(());
        }

        let variants = &enum_ir.variants;

        let arms = node.arms.iter().filter(|arm| !arm.is_catch_all);

        let mut pattern_variant_names = BTreeSet::new();

        for arm in arms {
            let decomposed = self.decompose_enum_variant_pattern(&arm.pattern);

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
                    .map(|p| format!("`{}`", p))
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
        // If there is a catch-all arm, we don't need to check exhaustiveness
        if arms.iter().any(|arm| arm.is_catch_all) {
            return Ok(());
        }

        assert!(fty.is_int_or_bool());

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
        span: Span,
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
        let decomposed = self.decompose_enum_variant_pattern(&current_arm.pattern);
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

            let name = match params.0.front().unwrap().kind {
                ast::expr::ExprKind::Ident(ident) => ident,
                _ => unreachable!(),
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
                        self,
                    ),
                    current_arm.pattern.span,
                )?;

                Some(ir::expr::EnumUnpack {
                    name,
                    enum_ty: udt,
                    enum_value: target.clone(),
                    variant_ty,
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
                    }),
                    ty: Rc::new(make_fundamental_type(
                        ir_type::FundamentalTypeKind::I32,
                        ir_type::Mutability::Not,
                    )),
                    span: current_arm.pattern.span,
                }),
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
            Some(
                ir::expr::Else::If(self.conv_match_arms_on_enum_to_if(
                    enum_ir.clone(),
                    target.clone(),
                    remaining_arms,
                    span,
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
            &node.arms.iter().collect::<Vec<_>>().as_slice(),
            node.span,
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
        span: Span,
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
            Some(
                ir::expr::Else::If(self.conv_match_arms_on_int_to_if(
                    target,
                    remaining_arms,
                    span,
                )?)
                .into(),
            )
        };

        Ok(ir::expr::If {
            cond: Box::new(condition),
            then: Box::new(then_expr),
            else_,
            enum_unpack: None,
            is_match: true,
        })
    }

    fn analyze_match_on_fundamental(
        &mut self,
        node: &ast::expr::Match,
        value: Rc<ir::expr::Expr>,
        fty: &ir_type::FundamentalType,
    ) -> anyhow::Result<ir::expr::Expr> {
        if !fty.is_int_or_bool() {
            return Err(SemanticError::MatchCannotBeUsedWithValueOfType {
                ty: value.ty.kind.to_string(),
                span: node.span,
            }
            .into());
        }

        self.check_exhaustiveness_for_match_on_int(fty, &node.arms, node.span)?;

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::If(self.conv_match_arms_on_int_to_if(
                value.clone(),
                &node.arms.iter().collect::<Vec<_>>().as_slice(),
                node.span,
            )?),
            ty: value.ty.clone(),
            span: node.span,
        })
    }

    fn analyze_match(&mut self, node: &ast::expr::Match) -> anyhow::Result<ir::expr::Expr> {
        let value = Rc::new(self.analyze_expr(&node.value)?);

        Ok(match value.ty.kind.as_ref() {
            ir_type::TyKind::Reference(rty) => {
                self.analyze_match_on_reference(node, &value, rty)?
            }

            ir_type::TyKind::Fundamental(fty) => {
                self.analyze_match_on_fundamental(node, value.clone(), fty)?
            }

            _ => {
                return Err(SemanticError::MatchCannotBeUsedWithValueOfType {
                    ty: value.ty.kind.to_string(),
                    span: node.span,
                }
                .into())
            }
        })
    }

    fn analyze_loop(&mut self, node: &ast::expr::Loop) -> anyhow::Result<ir::expr::Expr> {
        self.with_inside_loop(|analyzer| {
            let body = analyzer.analyze_block_expr(&node.body)?;

            if let ir::expr::ExprKind::Block(block) = body.kind {
                Ok(ir::expr::Expr {
                    kind: ir::expr::ExprKind::Loop(ir::expr::Loop { body: block }),
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
            ty: Rc::new(ir_type::Ty::new_unit()),
            span: node.span,
        })
    }

    fn analyze_if(&mut self, node: &ast::expr::If) -> anyhow::Result<ir::expr::Expr> {
        let cond = Box::new(self.analyze_expr(&node.cond)?);

        // Type checking for condition.
        // The condition must be a boolean type.
        if !ir_type::is_same_type(
            &cond.ty,
            &ir_type::make_fundamental_type(
                ir_type::FundamentalTypeKind::Bool,
                ir_type::Mutability::Not,
            ),
        ) {
            return Err(SemanticError::MismatchedTypes {
                types: (cond.ty.kind.to_string(), "bool".to_string()),
                span: node.cond.span,
            }
            .into());
        }

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

        // Type checking for then and else branches.
        if let Some(else_) = &else_ {
            let else_ty = match else_.as_ref() {
                ir::expr::Else::If(if_) => if_.then.ty.clone(),
                ir::expr::Else::Block(block) => block.ty.clone(),
            };

            if !ir_type::is_same_type(&then.ty, &else_ty) {
                return Err(SemanticError::MismatchedTypes {
                    types: (then.ty.kind.to_string(), else_ty.kind.to_string()),
                    span: node.span,
                }
                .into());
            }
        }

        Ok(ir::expr::Expr {
            ty: then.ty.clone(),
            kind: ir::expr::ExprKind::If(ir::expr::If {
                cond,
                then,
                else_,
                enum_unpack: None,
                is_match: false,
            }),
            span: node.span,
        })
    }

    fn generate_generic_fn(
        &mut self,
        info: &GenericFuncInfo,
        generic_args: &ast_type::GenericArgs,
    ) -> anyhow::Result<Rc<ir::top::FnDecl>> {
        let generic_params = match info.ast.decl.generic_params.as_ref() {
            Some(generic_params) => generic_params,
            None => todo!("Error"),
        };

        let generic_args = generic_args
            .types
            .iter()
            .map(|arg| self.analyze_type(arg))
            .collect::<anyhow::Result<Vec<_>>>()?;

        let generated_generic_key =
            self.create_generated_generic_key(info.ast.decl.name.symbol(), &generic_args);

        // If the generic function is already generated, return early
        if let Some(symbol_value) = self.lookup_symbol(generated_generic_key) {
            if let SymbolTableValueKind::Function(fn_) = &symbol_value.borrow().kind {
                return Ok(fn_.clone());
            } else {
                unreachable!()
            }
        }

        // Generate the generic function
        let fn_ = self.with_generic_arguments(generic_params, &generic_args, |analyzer| {
            let mut fn_ = info.ast.clone();
            fn_.decl.name = Ident::new(generated_generic_key, Span::dummy());
            analyzer.analyze_fn_internal(fn_)
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
        // Generate the generic function
        let callee_decl = match &info.kind {
            GenericKind::Func(info) => {
                self.generate_generic_fn(info, node.generic_args.as_ref().unwrap())?
            }
            _ => unreachable!(),
        };

        let mut args = Vec::new();

        for arg in node.args.0.iter() {
            args.push(self.analyze_expr(arg)?);
        }

        self.verify_fn_call_arguments(&callee_decl, &args, node.span)?;

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::FnCall(ir::expr::FnCall {
                callee: callee_decl.clone(),
                args: ir::expr::Args(args),
            }),
            ty: match callee_decl.return_ty.as_ref() {
                Some(ty) => ty.clone(),
                None => Rc::new(ir_type::Ty::new_unit()),
            },
            span: node.span,
        })
    }

    fn analyze_fn_call(&mut self, node: &ast::expr::FnCall) -> anyhow::Result<ir::expr::Expr> {
        let symbol_value =
            self.lookup_symbol(node.callee.symbol())
                .ok_or_else(|| SemanticError::Undeclared {
                    name: node.callee.symbol(),
                    span: node.span,
                })?;

        // Create a separate binding for the borrowed value
        let borrowed = symbol_value.borrow();
        match &borrowed.kind {
            SymbolTableValueKind::Function(fn_decl) => {
                let callee_decl = fn_decl.clone();
                let span = Span::new(node.span.start, node.span.finish, node.span.file);

                let args = node
                    .args
                    .0
                    .iter()
                    .map(|arg| self.analyze_expr(arg))
                    .collect::<anyhow::Result<Vec<_>>>()?;

                if !callee_decl.is_var_args {
                    self.verify_fn_call_arguments(&callee_decl, &args, node.span)?;
                }

                Ok(ir::expr::Expr {
                    kind: ir::expr::ExprKind::FnCall(ir::expr::FnCall {
                        callee: callee_decl.clone(),
                        args: ir::expr::Args(args),
                    }),
                    ty: match callee_decl.return_ty.as_ref() {
                        Some(ty) => ty.clone(),
                        None => Rc::new(ir_type::Ty::new_unit()),
                    },
                    span,
                })
            }

            SymbolTableValueKind::Generic(info) => self.analyze_generic_fn_call(info, node),

            _ => unreachable!(),
        }
    }

    fn analyze_arary_indexing(
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

        let elem_ty = match operand.ty.kind.as_ref() {
            ir_type::TyKind::Reference(rty) => {
                if let ir_type::TyKind::Array((elem_ty, _)) = rty.get_base_type().kind.as_ref() {
                    elem_ty.clone()
                } else {
                    return Err(SemanticError::IndexingNonArray { span }.into());
                }
            }

            _ => {
                return Err(SemanticError::IndexingNonArray { span }.into());
            }
        };

        Ok(ir::expr::Expr {
            ty: change_mutability_dup(elem_ty, operand.ty.mutability),
            kind: ir::expr::ExprKind::Indexing(ir::expr::Indexing {
                operand: Rc::new(operand),
                index: Box::new(index),
            }),
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

        let span = Span::new(node.span.start, node.span.finish, node.span.file);

        let bool_ty = ir_type::make_fundamental_type(
            ir_type::FundamentalTypeKind::Bool,
            ir_type::Mutability::Not,
        );

        // Type checking
        if !ir_type::is_same_type(&operand.ty, &bool_ty) {
            return Err(SemanticError::MismatchedTypes {
                types: (operand.ty.kind.to_string(), bool_ty.kind.to_string()),
                span,
            }
            .into());
        }

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::LogicalNot(ir::expr::LogicalNot {
                operand: Box::new(operand),
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

        let ty = Rc::new(ir_type::wrap_in_ref(
            Rc::new(ir_type::Ty {
                kind: ir_type::TyKind::Array((elements[0].ty.clone(), elements.len() as u32))
                    .into(),
                mutability: ir_type::Mutability::Not,
            }),
            ir_type::Mutability::Mut,
        ));

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::ArrayLiteral(ir::expr::ArrayLiteral { elements }),
            ty,
            span,
        })
    }

    fn analyze_int(&self, node: &ast::expr::Int) -> anyhow::Result<ir::expr::Expr> {
        let kind = match node.kind {
            ast::expr::IntKind::I32(n) => ir::expr::IntKind::I32(n),
            ast::expr::IntKind::U64(n) => ir::expr::IntKind::U64(n),
        };

        Ok(ir::expr::Expr {
            ty: Rc::new(ir_type::make_fundamental_type(
                match kind {
                    ir::expr::IntKind::I32(_) => ir_type::FundamentalTypeKind::I32,
                    ir::expr::IntKind::U64(_) => ir_type::FundamentalTypeKind::U64,
                },
                ir_type::Mutability::Not,
            )),
            kind: ir::expr::ExprKind::Int(ir::expr::Int { kind }),
            span: node.span,
        })
    }

    fn analyze_block_expr(&mut self, node: &ast::stmt::Block) -> anyhow::Result<ir::expr::Expr> {
        if node.body.is_empty() {
            return Ok(ir::expr::Expr {
                kind: ir::expr::ExprKind::Block(kaede_ir::stmt::Block {
                    body: vec![],
                    last_expr: None,
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
            kind: ir::expr::ExprKind::StringLiteral(ir::expr::StringLiteral { syb: node.syb }),
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
            if right.args.0.len() != 1 {
                // Static methods
                self.analyze_static_method_call(&udt, right)
            } else {
                let value = right.args.0.front().unwrap();

                match self.analyze_enum_variant(&udt, right.callee, Some(value), *left) {
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
                            } if name == left.symbol() => {
                                // Couldn't create enum variant, so try to call static methods
                                self.analyze_static_method_call(&udt, right)
                            }
                            _ => Err(e.into()),
                        })
                    }
                }
            }
        } else if let ast::expr::ExprKind::Ident(right) = &node.rhs.kind {
            // Create enum variant without value.
            self.analyze_enum_variant(&udt, *right, None, *left)
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
                    _ => unreachable!(),
                }
            }
            _ => {
                return Err(SemanticError::NotAnEnum {
                    name: left_ident.symbol(),
                    span: left_ident.span(),
                }
                .into())
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

        // Type checking
        if let Some(value) = &value {
            if let Some(ty) = &variant_info.ty {
                if !ir_type::is_same_type(&value.ty, ty) {
                    return Err(SemanticError::MismatchedTypes {
                        types: (value.ty.kind.to_string(), ty.kind.to_string()),
                        span: left_span,
                    }
                    .into());
                }
            } else {
                // No type is specified for the variant, but a value is provided.
                return Err(SemanticError::CannotAssignValueToVariant {
                    variant_name: variant_name.symbol(),
                    parent_name: udt.name(),
                    span: left_span,
                }
                .into());
            }
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
                value: value.map(|v| Box::new(v)),
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
        let method_name = self.create_method_key(udt.name(), call_node.callee.symbol(), true);

        // Lookup method from symbol table
        let method_decl = self
            .lookup_symbol(method_name)
            .ok_or(SemanticError::NoMethod {
                method_name: call_node.callee.symbol(),
                parent_name: udt.name(),
                span: call_node.span,
            })?;

        let method_decl = match &method_decl.borrow().kind {
            SymbolTableValueKind::Function(fn_) => fn_.clone(),
            _ => unreachable!(),
        };

        let args = {
            let mut args = Vec::new();

            for arg in call_node.args.0.iter() {
                args.push(self.analyze_expr(arg)?);
            }

            args
        };

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::FnCall(ir::expr::FnCall {
                callee: method_decl.clone(),
                args: ir::expr::Args(args),
            }),
            ty: match method_decl.return_ty.as_ref() {
                Some(ty) => ty.clone(),
                None => Rc::new(ir_type::Ty::new_unit()),
            },
            span: call_node.span,
        })
    }

    fn collect_access_chain<'a>(&self, node: &'a ast::expr::Expr, out: &mut Vec<Ident>) {
        use ast::expr::Binary;
        use ast::expr::BinaryKind::*;
        use ast::expr::ExprKind::*;

        match &node.kind {
            Binary(Binary {
                kind: Access,
                lhs,
                rhs,
                ..
            }) => {
                self.collect_access_chain(lhs, out);

                match &rhs.kind {
                    Ident(ident) => out.push(ident.clone()),
                    _ => {}
                }
            }

            Ident(ident) => {
                out.push(ident.clone());
            }

            _ => {}
        }
    }

    fn analyze_access(&mut self, node: &ast::expr::Binary) -> anyhow::Result<ir::expr::Expr> {
        assert!(matches!(node.kind, ast::expr::BinaryKind::Access));

        // Try to collect module path from the left side
        {
            let mut modules = Vec::new();
            self.collect_access_chain(&node.lhs, &mut modules);
            let module_path = ModulePath::new(modules.iter().map(|i| i.symbol()).collect());

            // If the module path is valid, analyze the right side in the module
            if !modules.is_empty() && self.modules.contains_key(&module_path) {
                return self.with_module(module_path, |analyzer| analyzer.analyze_expr(&node.rhs));
            }
        }

        let left = self.analyze_expr(&node.lhs)?;

        let left_ty = left.ty.clone();

        if left_ty.is_str() {
            // String indexing or method calling
            return self.analyze_str_indexing_or_method_call(left, &node.rhs);
        }

        if let ir_type::TyKind::Reference(rty) = left_ty.kind.as_ref() {
            // Struct access or tuple indexing
            if matches!(
                rty.get_base_type().kind.as_ref(),
                ir_type::TyKind::UserDefined(_) | ir_type::TyKind::Tuple(_)
            ) {
                self.analyze_struct_access_or_tuple_indexing(left, &node.rhs)
            } else {
                return Err(SemanticError::HasNoFields {
                    span: node.lhs.span,
                }
                .into());
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

            self.create_method_call_ir(
                call_node.callee.symbol(),
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

        if !ir_type::is_same_type(
            &index_ty,
            &ir_type::make_fundamental_type(
                ir_type::FundamentalTypeKind::I32,
                ir_type::Mutability::Not,
            ),
        ) {
            return Err(SemanticError::MismatchedTypes {
                types: (
                    index_ty.kind.to_string(),
                    ir_type::make_fundamental_type(
                        ir_type::FundamentalTypeKind::I32,
                        ir_type::Mutability::Not,
                    )
                    .kind
                    .to_string(),
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
        match fn_decl.params.len().cmp(&args.len()) {
            std::cmp::Ordering::Less => {
                return Err(SemanticError::TooManyArguments {
                    name: fn_decl.name.symbol(),
                    span: call_node_span,
                }
                .into());
            }

            std::cmp::Ordering::Greater => {
                return Err(SemanticError::TooFewArguments {
                    name: fn_decl.name.symbol(),
                    span: call_node_span,
                }
                .into());
            }

            std::cmp::Ordering::Equal => {
                for (param, arg) in fn_decl.params.iter().zip(args.iter()) {
                    if !ir_type::is_same_type(&param.ty, &arg.ty) {
                        return Err(SemanticError::MismatchedTypes {
                            types: (arg.ty.kind.to_string(), param.ty.kind.to_string()),
                            span: arg.span,
                        }
                        .into());
                    }

                    // Check mutability
                    if arg.ty.mutability.is_not() && param.ty.mutability.is_mut() {
                        return Err(SemanticError::CannotAssignImmutableToMutable {
                            span: arg.span,
                        }
                        .into());
                    }
                }
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

            _ => Err(SemanticError::HasNoFields { span }.into()),
        }
    }

    fn analyze_tuple_indexing(
        &mut self,
        lhs: ir::expr::Expr,
        rhs: &ast::expr::Expr,
        elements_ty: &[Rc<ir_type::Ty>],
    ) -> anyhow::Result<ir::expr::Expr> {
        let index = match &rhs.kind {
            ast::expr::ExprKind::Int(int) => int.as_u64() as u32,
            _ => unreachable!(),
        };

        Ok(ir::expr::Expr {
            span: Span::new(lhs.span.start, rhs.span.finish, lhs.span.file),
            ty: change_mutability_dup(
                elements_ty[index as usize].clone(),
                lhs.ty.mutability.into(),
            ),
            kind: ir::expr::ExprKind::TupleIndexing(ir::expr::TupleIndexing {
                tuple: Rc::new(lhs),
                index,
                element_ty: elements_ty[index as usize].clone(),
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
                self.analyze_struct_field_access(lhs, rhs, udt, field_name.clone())
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
        self.create_method_call_ir(
            call_node.callee.symbol(),
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
        let struct_info = self
            .lookup_symbol(udt.name())
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
            span: Span::new(lhs.span.start, rhs.span.finish, lhs.span.file),
            ty: change_mutability_dup(field_ty, lhs.ty.mutability.into()),
            kind: ir::expr::ExprKind::FieldAccess(ir::expr::FieldAccess {
                struct_info,
                operand: Box::new(lhs),
                field_name: field_name.symbol(),
                field_offset,
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
        let no_method_err = || SemanticError::NoMethod {
            method_name: call_node.callee.symbol(),
            parent_name,
            span: call_node.span,
        };

        let method_name = self.create_method_key(parent_name, method_name, false);

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

        // Inject `this` to the front of the arguments
        let args = std::iter::once(this)
            .chain(
                call_node
                    .args
                    .0
                    .iter()
                    .map(|arg| self.analyze_expr(arg))
                    .collect::<anyhow::Result<Vec<_>>>()?,
            )
            .collect::<Vec<_>>();

        self.verify_fn_call_arguments(&callee, &args, call_node.span)?;

        let call_node = ir::expr::FnCall {
            callee: callee.clone(),
            args: ir::expr::Args(args),
        };

        // For simplicity, we treat the method call as a function call.
        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::FnCall(call_node),
            ty: callee
                .return_ty
                .clone()
                .unwrap_or_else(|| Rc::new(ir_type::Ty::new_unit())),
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

        self.create_method_call_ir(
            call_node.callee.symbol(),
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
            _ => todo!("Error"),
        };

        let target_ir_ty = self.analyze_type(target_ast_ty)?;

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::Cast(ir::expr::Cast {
                operand: Box::new(operand),
                target_ty: target_ir_ty.clone(),
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

        // Type checking
        if !ir_type::is_same_type(&lhs.ty, &rhs.ty) {
            return Err(SemanticError::MismatchedTypes {
                types: (lhs.ty.kind.to_string(), rhs.ty.kind.to_string()),
                span,
            }
            .into());
        }

        let to = |kind, ty| ir::expr::Expr {
            kind: ir::expr::ExprKind::Binary(ir::expr::Binary {
                lhs: lhs.clone(),
                rhs: rhs.clone(),
                kind,
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

    fn analyze_ident(&self, node: &Ident) -> anyhow::Result<ir::expr::Expr> {
        self.lookup_symbol(node.symbol())
            .map(|value| match &value.borrow().kind {
                SymbolTableValueKind::Variable(VariableInfo { ty }) => Ok(ir::expr::Expr {
                    kind: ir::expr::ExprKind::Variable(ir::expr::Variable {
                        name: node.symbol(),
                        ty: ty.clone(),
                    }),
                    ty: ty.clone(),
                    span: node.span(),
                }),
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
