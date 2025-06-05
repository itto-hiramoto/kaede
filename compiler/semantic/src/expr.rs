use std::{collections::BTreeSet, rc::Rc};

use crate::{
    error::SemanticError,
    symbol_table::{
        GenericFuncInfo, GenericInfo, SymbolTable, SymbolTableValue, SymbolTableValueKind,
        VariableInfo,
    },
    SemanticAnalyzer,
};

use kaede_ast as ast;
use kaede_ast_type as ast_type;
use kaede_ir as ir;
use kaede_ir_type::{self as ir_type, make_fundamental_type, ModulePath};
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
            // ExprKind::StructLiteral(StructLiteral),
            // ExprKind::TupleLiteral(TupleLiteral),
            ExprKind::Ty(_) => Ok(ir::expr::Expr {
                kind: ir::expr::ExprKind::DoNothing,
                ty: Rc::new(ir_type::Ty::new_unit()),
                span,
            }),

            ExprKind::ExternalIdent(_) => todo!(),
            ExprKind::GenericIdent(_) => todo!(),

            _ => unimplemented!(),
        }
    }

    /// Decompose enum variant patterns (like `A::B` or `A::B(a, b, c)`)
    fn decompose_enum_variant_pattern<'p>(
        &self,
        pattern: &'p ast::expr::Expr,
    ) -> DecomposedEnumVariantPattern<'p> {
        let (module_names, enum_name, variant_name_and_param) = match &pattern.kind {
            ast::expr::ExprKind::Binary(b) => match b.kind {
                ast::expr::BinaryKind::ScopeResolution => match &b.lhs.kind {
                    ast::expr::ExprKind::Ident(i) => (vec![], i, &b.rhs),
                    ast::expr::ExprKind::ExternalIdent(ei) => {
                        (ei.external_modules.clone(), &ei.ident, &b.rhs)
                    }
                    _ => unreachable!(),
                },

                _ => unreachable!(),
            },

            _ => unreachable!(),
        };

        let (variant_name, param) = match &variant_name_and_param.kind {
            ast::expr::ExprKind::Ident(ident) => (ident, None),
            ast::expr::ExprKind::FnCall(fncall) => (&fncall.callee, Some(&fncall.args)),
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
        enum_symbol: &SymbolTableValue,
    ) -> anyhow::Result<()> {
        // If there is a catch-all arm, we don't need to check exhaustiveness
        if node.arms.iter().any(|arm| arm.is_catch_all) {
            return Ok(());
        }

        let enum_ir = match &enum_symbol.kind {
            SymbolTableValueKind::Enum(ir) => ir.clone(),
            _ => unreachable!(),
        };

        let variants = &enum_ir.variants;

        let arms = node.arms.iter().filter(|arm| !arm.is_catch_all);

        let mut pattern_variant_names = BTreeSet::new();

        for arm in arms {
            let decomposed = self.decompose_enum_variant_pattern(&arm.pattern);

            // Check if the module names are the same as the enum's external modules
            let enum_external_modules = enum_symbol.module_path.get_module_names_from_root();
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
        enum_symbol_val: &SymbolTableValue,
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

        // Analyze the arm's code
        let then_expr = self.analyze_expr(&current_arm.code)?;

        // Get the pattern variant information
        let pattern_variant_offset = match &enum_symbol_val.kind {
            SymbolTableValueKind::Enum(enum_ir) => {
                let decomposed = self.decompose_enum_variant_pattern(&current_arm.pattern);
                enum_ir
                    .variants
                    .iter()
                    .find(|v| v.name == decomposed.variant_name.symbol())
                    .unwrap()
                    .offset
            }
            _ => unreachable!(),
        };

        // Get the target variant information
        let target_variant_offset_expr = ir::expr::Expr {
            kind: ir::expr::ExprKind::Indexing(ir::expr::Indexing {
                operand: target.clone(),
                index: Box::new(ir::expr::Expr {
                    kind: ir::expr::ExprKind::Int(ir::expr::Int {
                        kind: ir::expr::IntKind::I32(0),
                    }),
                    ty: Rc::new(make_fundamental_type(
                        ir_type::FundamentalTypeKind::I32,
                        ir_type::Mutability::Not,
                    )),
                    span: current_arm.pattern.span,
                }),
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
                    enum_symbol_val,
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

        let enum_symbol = if let ir_type::TyKind::UserDefined(udt) = base_ty.kind.as_ref() {
            let symbol = self
                .lookup_qualified_symbol(udt.name.clone())
                .ok_or_else(|| SemanticError::Undeclared {
                    name: udt.name.symbol(),
                    span: node.span,
                })?;

            symbol
        } else {
            return err();
        };

        // Check if the symbol is an enum
        if !matches!(enum_symbol.borrow().kind, SymbolTableValueKind::Enum(_)) {
            return err();
        }

        self.check_exhaustiveness_for_match_on_enum(node, &enum_symbol.borrow())?;

        let target = Rc::new(self.analyze_expr(&node.value)?);

        let enum_symbol = enum_symbol.borrow();
        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::If(self.conv_match_arms_on_enum_to_if(
                &enum_symbol,
                target.clone(),
                &node.arms.iter().collect::<Vec<_>>().as_slice(),
                node.span,
            )?),
            ty: target.ty.clone(),
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
            kind: ir::expr::ExprKind::If(ir::expr::If { cond, then, else_ }),
            span: node.span,
        })
    }

    fn analyze_fn_call(&mut self, node: &ast::expr::FnCall) -> anyhow::Result<ir::expr::Expr> {
        if node.external_modules.is_empty() {
            self.analyze_fn_call_internal(node)
        } else {
            self.with_external_module(
                ModulePath::new(node.external_modules.iter().map(|i| i.symbol()).collect()),
                |analyzer| analyzer.analyze_fn_call_internal(node),
            )
        }
    }

    // Returns the mangled name of the generated function
    fn generate_generic_fn(
        &mut self,
        info: &GenericFuncInfo,
        generic_args: &ast_type::GenericArgs,
    ) -> anyhow::Result<Rc<ir::top::Fn>> {
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
            self.create_generated_generic_key(info.ast.decl.name.symbol(), generic_args.clone());

        // Mangle the generic function name
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

        Ok(fn_)
    }

    fn analyze_generic_fn_call(
        &mut self,
        info: &GenericInfo,
        node: &ast::expr::FnCall,
    ) -> anyhow::Result<ir::expr::Expr> {
        // Generate the generic function
        let callee = match info {
            GenericInfo::Func(info) => {
                self.generate_generic_fn(info, node.generic_args.as_ref().unwrap())?
            }
            _ => unreachable!(),
        };

        let mut args = Vec::new();

        for arg in node.args.0.iter() {
            args.push(self.analyze_expr(arg)?);
        }

        self.verify_fn_call_arguments(&callee, &args, node.span)?;

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::FnCall(ir::expr::FnCall {
                callee: callee.clone(),
                args: ir::expr::Args(args),
            }),
            ty: match callee.decl.return_ty.as_ref() {
                Some(ty) => ty.clone(),
                None => Rc::new(ir_type::Ty::new_unit()),
            },
            span: node.span,
        })
    }

    fn analyze_fn_call_internal(
        &mut self,
        node: &ast::expr::FnCall,
    ) -> anyhow::Result<ir::expr::Expr> {
        let symbol_value =
            self.lookup_symbol(node.callee.symbol())
                .ok_or_else(|| SemanticError::Undeclared {
                    name: node.callee.symbol(),
                    span: node.span,
                })?;

        // Create a separate binding for the borrowed value
        let borrowed = symbol_value.borrow();
        match &borrowed.kind {
            SymbolTableValueKind::Function(fn_) => {
                let callee = fn_.clone();
                let span = Span::new(node.span.start, node.span.finish, node.span.file);

                let args = node
                    .args
                    .0
                    .iter()
                    .map(|arg| self.analyze_expr(arg))
                    .collect::<anyhow::Result<Vec<_>>>()?;

                self.verify_fn_call_arguments(&callee, &args, node.span)?;

                Ok(ir::expr::Expr {
                    kind: ir::expr::ExprKind::FnCall(ir::expr::FnCall {
                        callee: callee.clone(),
                        args: ir::expr::Args(args),
                    }),
                    ty: match callee.decl.return_ty.as_ref() {
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
            kind: ir::expr::ExprKind::Indexing(ir::expr::Indexing {
                operand: Rc::new(operand),
                index: Box::new(index),
            }),
            ty: elem_ty,
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
                let last_expr = Some(Box::new(self.analyze_expr(e)?));

                ir::expr::Expr {
                    kind: ir::expr::ExprKind::Block(kaede_ir::stmt::Block { body, last_expr }),
                    ty: Rc::new(ir_type::Ty::new_unit()),
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
            kind: ir::expr::ExprKind::StringLiteral(ir::expr::StringLiteral { lit: node.syb }),
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

            // The following requires consideration of name mangling.
            Access => self.analyze_access(node),
            ScopeResolution => {
                todo!();
                // self.scope_resolution(node),
            }

            _ => self.analyze_arithmetic_binary(node),
        }
    }

    fn analyze_access(&mut self, node: &ast::expr::Binary) -> anyhow::Result<ir::expr::Expr> {
        assert!(matches!(node.kind, ast::expr::BinaryKind::Access));

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
        let ir_right = self.analyze_expr(ast_right)?;

        match ir_right.kind {
            // Method call
            ir::expr::ExprKind::FnCall(call_node) => {
                let span = Span::new(left.span.start, ast_right.span.finish, left.span.file);

                let args = std::iter::once(left)
                    .chain(call_node.args.0.into_iter())
                    .collect::<Vec<_>>();

                self.verify_fn_call_arguments(&call_node.callee, &args, ast_right.span)?;

                let call_node = ir::expr::FnCall {
                    callee: call_node.callee.clone(),
                    args: ir::expr::Args(args),
                };

                // For simplicity, we treat the method call as a function call.
                Ok(ir::expr::Expr {
                    kind: ir::expr::ExprKind::FnCall(call_node),
                    ty: Rc::new(ir_type::Ty::new_never()),
                    span,
                })
            }

            _ => self.analyze_str_indexing(left, ir_right),
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
                ir_type::FundamentalTypeKind::U64,
                ir_type::Mutability::Not,
            ),
        ) {
            return Err(SemanticError::MismatchedTypes {
                types: (
                    index_ty.kind.to_string(),
                    ir_type::make_fundamental_type(
                        ir_type::FundamentalTypeKind::U64,
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
            kind: ir::expr::ExprKind::Indexing(ir::expr::Indexing {
                operand: Rc::new(left),
                index: Box::new(right),
            }),
            ty,
            span,
        })
    }

    fn verify_fn_call_arguments(
        &self,
        fn_: &ir::top::Fn,
        args: &[ir::expr::Expr],
        call_node_span: Span,
    ) -> anyhow::Result<()> {
        match fn_.decl.params.len().cmp(&args.len()) {
            std::cmp::Ordering::Less => {
                return Err(SemanticError::TooManyArguments {
                    name: fn_.decl.name.symbol(),
                    span: call_node_span,
                }
                .into());
            }

            std::cmp::Ordering::Greater => {
                return Err(SemanticError::TooFewArguments {
                    name: fn_.decl.name.symbol(),
                    span: call_node_span,
                }
                .into());
            }

            std::cmp::Ordering::Equal => {
                for (param, arg) in fn_.decl.params.iter().zip(args.iter()) {
                    if !ir_type::is_same_type(&param.ty, &arg.ty) {
                        return Err(SemanticError::MismatchedTypes {
                            types: (arg.ty.kind.to_string(), param.ty.kind.to_string()),
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
            ast::expr::ExprKind::Int(int) => int.as_u64(),
            _ => unreachable!(),
        };

        if elements_ty.len() <= index as usize {
            return Err(SemanticError::IndexOutOfRange {
                index: index as u64,
                span: rhs.span,
            }
            .into());
        }

        Ok(ir::expr::Expr {
            span: Span::new(lhs.span.start, rhs.span.finish, lhs.span.file),
            kind: ir::expr::ExprKind::Indexing(ir::expr::Indexing {
                operand: Rc::new(lhs),
                index: Box::new(self.analyze_expr(rhs)?),
            }),
            ty: elements_ty[index as usize].clone(),
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
        self.with_external_module(udt.name.module_path().clone(), |analyzer| {
            let span = Span::new(lhs.span.start, call_node.span.finish, lhs.span.file);
            analyzer.create_method_call_ir(
                call_node.callee.symbol(),
                udt.name.symbol(),
                call_node,
                lhs,
                span,
            )
        })
    }

    fn analyze_struct_field_access(
        &self,
        lhs: ir::expr::Expr,
        rhs: &ast::expr::Expr,
        udt: &ir_type::UserDefinedType,
        field_name: Ident,
    ) -> anyhow::Result<ir::expr::Expr> {
        let struct_info = self
            .lookup_symbol(udt.name.symbol())
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
            kind: ir::expr::ExprKind::FieldAccess(ir::expr::FieldAccess {
                operand: Box::new(lhs),
                field_name: field_name.symbol(),
                field_offset,
            }),
            ty: field_ty,
        })
    }

    fn create_method_call_ir(
        &mut self,
        method_name: Symbol,
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

        let method_name = self.create_method_key(parent_name, method_name);

        let callee = match self.lookup_symbol(method_name) {
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
            ty: Rc::new(ir_type::Ty::new_never()),
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
