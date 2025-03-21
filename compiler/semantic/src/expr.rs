use std::rc::Rc;

use crate::{
    error::SemanticError,
    symbol_table::{SymbolTable, SymbolTableValueKind, VariableInfo},
    SemanticAnalyzer,
};

use kaede_ast as ast;
use kaede_ir as ir;
use kaede_ir_type as ir_type;
use kaede_span::Span;
use kaede_symbol::Ident;

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
            // ExprKind::ExternalIdent(ExternalIdent),
            // ExprKind::GenericIdent((Ident, GenericArgs)),
            // ExprKind::FnCall(node) => self.analyze_fn_call(node),
            // ExprKind::StructLiteral(StructLiteral),
            // ExprKind::Indexing(Indexing),
            // ExprKind::TupleLiteral(TupleLiteral),
            // ExprKind::If(If),
            // ExprKind::Loop(Loop),
            // ExprKind::Break(Break),
            // ExprKind::Match(Match),
            ExprKind::Ty(_) => Ok(ir::expr::Expr {
                kind: ir::expr::ExprKind::DoNothing,
                ty: Rc::new(ir_type::Ty::new_unit()),
                span,
            }),
            _ => unimplemented!(),
        }
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
        if ir_type::is_same_type(&operand.ty, &bool_ty) {
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

    fn analyze_boolean_literal(
        &self,
        value: bool,
        span: Span,
    ) -> anyhow::Result<ir::expr::Expr> {
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

    fn analyze_block_expr(
        &mut self,
        node: &ast::stmt::Block,
    ) -> anyhow::Result<ir::expr::Expr> {
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

        self.symbol_tables.push(SymbolTable::new());

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

        self.symbol_tables.pop();

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
                self.analyze_struct_access_or_tuple_indexing(&left, &node.lhs, &node.rhs)
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
                let method_name = format!("str.{}", call_node.callee).into();

                let no_method_err = || SemanticError::NoMethod {
                    method_name: call_node.callee,
                    parent_name: "str".to_string().into(),
                    span: ast_right.span,
                };

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

                let span = Span::new(left.span.start, ast_right.span.finish, left.span.file);

                let args = std::iter::once(left)
                    .chain(call_node.args.0.into_iter())
                    .collect::<Vec<_>>();

                self.verify_fn_call_arguments(&callee, &args, ast_right.span)?;

                let call_node = ir::expr::FnCall {
                    callee: callee.decl.name,
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

        let offset = match right.kind {
            ir::expr::ExprKind::Int(int) => int.as_u64() as usize,
            _ => unreachable!(),
        };

        // (*i8, u64)
        let ty = Rc::new(match offset {
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

            _ => {
                return Err(SemanticError::IndexOutOfRange {
                    index: offset as u64,
                    span,
                }
                .into())
            }
        });

        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::Indexing(ir::expr::Indexing {
                operand: Box::new(left),
                offset,
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
                    name: fn_.decl.name,
                    span: call_node_span,
                }
                .into());
            }

            std::cmp::Ordering::Greater => {
                return Err(SemanticError::TooFewArguments {
                    name: fn_.decl.name,
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
        &self,
        left: &ir::expr::Expr,
        lhs: &ast::expr::Expr,
        rhs: &ast::expr::Expr,
    ) -> anyhow::Result<ir::expr::Expr> {
        // let span = Span::new(lhs.span.start, rhs.span.finish, lhs.span.file);

        // let (field_name, field_ty) = match &left.ty.kind {
        //     TyKind::Reference(rty) => {
        //         let base_ty = rty.get_base_type();

        //         match base_ty.kind.as_ref() {
        //             TyKind::UserDefined(udty) => {
        //                 let field = udty
        //                     .fields
        //                     .iter()
        //                     .find(|field| field.name == rhs.symbol())
        //                     .ok_or_else(|| SemanticError::FieldNotFound {
        //                         name: rhs.symbol(),
        //                         span,
        //                     })?;

        //                 (field.name, field.ty.clone())
        //             }
        //             TyKind::Tuple(tuple_ty) => {
        //                 let index = rhs.symbol().parse::<u32>().map_err(|_| {
        //                     SemanticError::TupleIndexIsNotNumber {
        //                         index: rhs.symbol(),
        //                         span,
        //                     }
        //                 })?;

        //                 let field_ty = tuple_ty.get(index as usize).ok_or_else(|| {
        //                     SemanticError::TupleIndexOutOfRange {
        //                         index,
        //                         len: tuple_ty.len(),
        //                         span,
        //                     }
        //                 })?;

        //                 (rhs.symbol(), field_ty.clone())
        //             }
        //             _ => unreachable!(),
        //         }
        //     }
        //     _ => unreachable!(),
        // };

        // Ok(ir::expr::Expr {
        //     kind: ir::expr::ExprKind::StructAccess(ir::expr::StructAccess {
        //         base: Box::new(left.clone()),
        //         field_name,
        //         field_ty,
        //     }),
        //     ty: field_ty,
        // })
        todo!()
    }

    fn analyze_fundamental_type_method_call(
        &mut self,
        left: ir::expr::Expr,
        fty: &ir_type::FundamentalType,
        call_node: &ast::expr::FnCall,
    ) -> anyhow::Result<ir::expr::Expr> {
        // e.g. i32.abs
        let method_name = format!("{}.{}", fty.kind, call_node.callee.as_str()).into();

        let no_method_err = || SemanticError::NoMethod {
            method_name: call_node.callee.symbol(),
            parent_name: fty.kind.to_string().into(),
            span: call_node.span,
        };

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

        let span = Span::new(left.span.start, call_node.span.finish, left.span.file);

        let args = std::iter::once(left)
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
            callee: callee.decl.name,
            args: ir::expr::Args(args),
        };

        // For simplicity, we treat the method call as a function call.
        Ok(ir::expr::Expr {
            kind: ir::expr::ExprKind::FnCall(call_node),
            ty: Rc::new(ir_type::Ty::new_never()),
            span,
        })
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
        if ir_type::is_same_type(&lhs.ty, &rhs.ty) {
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
                SymbolTableValueKind::Variable(VariableInfo { ty }) => ir::expr::Expr {
                    kind: ir::expr::ExprKind::Variable(ir::expr::Variable {
                        name: node.symbol(),
                        ty: ty.clone(),
                    }),
                    ty: ty.clone(),
                    span: node.span(),
                },
                _ => todo!("Error"),
            })
            .ok_or_else(|| anyhow::anyhow!("Symbol not found: {:?}", node))
    }
}
