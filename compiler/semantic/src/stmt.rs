use crate::{
    symbol_table::{SymbolTableValue, SymbolTableValueKind, VariableInfo},
    SemanticAnalyzer, SemanticError,
};

use kaede_ast as ast;
use kaede_ir as ir;

impl SemanticAnalyzer {
    pub fn analyze_stmt(&mut self, stmt: &ast::stmt::Stmt) -> anyhow::Result<ir::stmt::Stmt> {
        use kaede_ast::stmt::StmtKind;

        Ok(match &stmt.kind {
            StmtKind::Expr(expr) => ir::stmt::Stmt::Expr(self.analyze_expr(expr)?.into()),
            StmtKind::Let(node) => ir::stmt::Stmt::Let(self.analyze_let(node)?.into()),
            _ => unimplemented!(),
        })
    }

    fn analyze_let(&mut self, node: &ast::stmt::Let) -> anyhow::Result<ir::stmt::Let> {
        use kaede_ast::stmt::LetKind;

        match &node.kind {
            LetKind::NormalLet(node) => self.analyze_normal_let(node),
            LetKind::TupleUnpack(node) => self.analyze_tuple_unpacking(node),
        }
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

            let var_ty = ir::ty::change_mutability_dup(init.ty.clone(), mutability.into());

            if !node.ty.kind.is_inferred() {
                // Check if the type of the initializer is the same as the type of the variable
                if !ir::ty::is_same_type(self.analyze_type(&node.ty)?.as_ref(), &init.ty) {
                    return Err(SemanticError::MismatchedTypes {
                        types: (node.ty.kind.to_string(), init.ty.kind.to_string()),
                        span: node.span,
                    }
                    .into());
                }
            }

            // Insert the variable into the symbol table
            self.insert_symbol_to_current_scope(
                node.name.symbol(),
                SymbolTableValue::new(
                    SymbolTableValueKind::Variable(VariableInfo { ty: var_ty.clone() }),
                    self,
                ),
                span,
            )?;

            Ok(ir::stmt::Let {
                name: node.name.symbol(),
                ty: var_ty,
                init: Some(init),
            })
        } else {
            todo!()
        }
    }

    fn analyze_tuple_unpacking(
        &mut self,
        _node: &ast::stmt::TupleUnpack,
    ) -> anyhow::Result<ir::stmt::Let> {
        todo!()
    }

    pub fn analyze_block(&mut self, block: &ast::stmt::Block) -> anyhow::Result<ir::stmt::Block> {
        let mut stmts = Vec::new();

        for stmt in block.body.iter() {
            stmts.push(self.analyze_stmt(stmt)?);
        }

        Ok(ir::stmt::Block {
            body: stmts,
            last_expr: None,
        })
    }
}
