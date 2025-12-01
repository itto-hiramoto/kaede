use crate::{SemanticAnalyzer, SemanticError};
use kaede_symbol_table::{SymbolTableValue, SymbolTableValueKind, VariableInfo};

use kaede_ast as ast;
use kaede_ir as ir;

enum LetResult {
    NormalLet(ir::stmt::Let),
    TupleUnpack(ir::stmt::TupleUnpack),
}

impl SemanticAnalyzer {
    pub fn analyze_stmt(&mut self, stmt: &ast::stmt::Stmt) -> anyhow::Result<ir::stmt::Stmt> {
        use kaede_ast::stmt::StmtKind;

        Ok(match &stmt.kind {
            StmtKind::Expr(expr) => ir::stmt::Stmt::Expr(self.analyze_expr(expr)?.into()),

            StmtKind::Let(node) => match self.analyze_let(node)? {
                LetResult::NormalLet(let_stmt) => ir::stmt::Stmt::Let(let_stmt),
                LetResult::TupleUnpack(tuple_unpack_stmt) => {
                    ir::stmt::Stmt::TupleUnpack(tuple_unpack_stmt)
                }
            },

            StmtKind::Assign(node) => ir::stmt::Stmt::Assign(self.analyze_assign(node)?),
        })
    }

    fn analyze_assign(&mut self, node: &ast::stmt::Assign) -> anyhow::Result<ir::stmt::Assign> {
        let left = self.analyze_expr(&node.lhs)?;

        if left.ty.mutability.is_not() {
            return Err(SemanticError::CannotAssignTwiceToImutable { span: node.span }.into());
        }

        let right = self.analyze_expr(&node.rhs)?;

        if !ir::ty::is_same_type(&left.ty, &right.ty) {
            return Err(SemanticError::MismatchedTypes {
                types: (left.ty.kind.to_string(), right.ty.kind.to_string()),
                span: node.span,
            }
            .into());
        }

        Ok(ir::stmt::Assign {
            assignee: left,
            value: right,
            span: node.span,
        })
    }

    fn analyze_let(&mut self, node: &ast::stmt::Let) -> anyhow::Result<LetResult> {
        use kaede_ast::stmt::LetKind;

        Ok(match &node.kind {
            LetKind::NormalLet(node) => LetResult::NormalLet(self.analyze_normal_let(node)?),
            LetKind::TupleUnpack(node) => {
                LetResult::TupleUnpack(self.analyze_tuple_unpacking(node)?)
            }
        })
    }

    fn analyze_normal_let(&mut self, node: &ast::stmt::NormalLet) -> anyhow::Result<ir::stmt::Let> {
        if let Some(init) = &node.init {
            let init = self.analyze_expr(init)?;
            let mutability = node.mutability;
            let span = node.span;

            if matches!(init.ty.kind.as_ref(), ir::ty::TyKind::Reference(_))
                && mutability.is_mut()
                && init.ty.mutability.is_not()
            {
                return Err(SemanticError::CannotAssignImmutableToMutable { span }.into());
            }

            // Determine the variable type
            let var_ty = if node.ty.kind.is_inferred() {
                // No type annotation
                // If init expression has a concrete type (not a type variable), use it
                // Otherwise, create fresh type variable for type inference pass to resolve
                if matches!(init.ty.kind.as_ref(), ir::ty::TyKind::Var(_)) {
                    ir::ty::change_mutability_dup(self.infer_context.fresh(), mutability.into())
                } else {
                    ir::ty::change_mutability_dup(init.ty.clone(), mutability.into())
                }
            } else {
                // Type annotation present - use the annotated type
                ir::ty::change_mutability_dup(self.analyze_type(&node.ty)?, mutability.into())
            };

            // Insert the variable into the symbol table
            self.insert_symbol_to_current_scope(
                node.name.symbol(),
                SymbolTableValue::new(
                    SymbolTableValueKind::Variable(VariableInfo { ty: var_ty.clone() }),
                    self.current_module_path().clone(),
                ),
                span,
            )?;

            Ok(ir::stmt::Let {
                name: node.name.symbol(),
                ty: var_ty,
                init: Some(init),
                span: node.span,
            })
        } else {
            todo!()
        }
    }

    fn analyze_tuple_unpacking(
        &mut self,
        node: &ast::stmt::TupleUnpack,
    ) -> anyhow::Result<ir::stmt::TupleUnpack> {
        let tuple = self.analyze_expr(&node.init)?;

        let tuple_ref_ty = tuple.ty.as_ref();

        let (element_types, tuple_mutability) = if let ir::ty::TyKind::Reference(rty) =
            tuple_ref_ty.kind.as_ref()
        {
            match rty.refee_ty.kind.as_ref() {
                ir::ty::TyKind::Tuple(element_types) => (element_types, tuple_ref_ty.mutability),

                _ => todo!("Error"),
            }
        } else {
            todo!("Error")
        };

        let element_len = element_types.len();

        if node.names.len() != element_len {
            return Err(SemanticError::NumberOfTupleFieldsDoesNotMatch {
                lens: (node.names.len(), element_len),
                span: node.span,
            }
            .into());
        }

        let mut names = Vec::with_capacity(node.names.len());

        for (index, name_and_mutability) in node.names.iter().enumerate() {
            let mutability = match name_and_mutability {
                Some(x) => {
                    let name = x.0.symbol();

                    self.insert_symbol_to_current_scope(
                        name,
                        SymbolTableValue::new(
                            SymbolTableValueKind::Variable(VariableInfo {
                                ty: element_types[index].clone(),
                            }),
                            self.current_module_path().clone(),
                        ),
                        node.span,
                    )?;

                    names.push(Some(name));
                    x.1
                }

                // Ignore field
                None => {
                    names.push(None);
                    continue;
                }
            };

            if mutability.is_mut() && tuple_mutability.is_not() {
                todo!("Error")
            }
        }

        Ok(ir::stmt::TupleUnpack {
            names,
            init: tuple,
            span: node.span,
        })
    }

    pub fn analyze_block(&mut self, block: &ast::stmt::Block) -> anyhow::Result<ir::stmt::Block> {
        let mut stmts = Vec::new();

        for stmt in block.body.iter() {
            stmts.push(self.analyze_stmt(stmt)?);
        }

        Ok(ir::stmt::Block {
            body: stmts,
            last_expr: None,
            span: block.span,
        })
    }
}
