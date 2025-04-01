use crate::SemanticAnalyzer;

use kaede_ast as ast;
use kaede_ir as ir;

impl SemanticAnalyzer {
    pub fn analyze_stmt(&mut self, stmt: &ast::stmt::Stmt) -> anyhow::Result<ir::stmt::Stmt> {
        use kaede_ast::stmt::StmtKind;

        match &stmt.kind {
            StmtKind::Expr(expr) => Ok(ir::stmt::Stmt::Expr(self.analyze_expr(expr)?.into())),
            _ => unimplemented!(),
        }
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
