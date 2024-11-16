use crate::SemanticAnalyzer;

use kaede_ast as ast;
use kaede_ir as ir;

impl SemanticAnalyzer {
    pub fn analyze_stmt(&self, stmt: &ast::stmt::Stmt) -> anyhow::Result<ir::stmt::Stmt> {
        match stmt.kind {
            _ => unimplemented!(),
        }
    }

    pub fn analyze_block(&self, block: &ast::stmt::Block) -> anyhow::Result<ir::stmt::Block> {
        let mut stmts = Vec::new();

        for stmt in block.body.iter() {
            stmts.push(self.analyze_stmt(stmt)?);
        }

        Ok(ir::stmt::Block { body: stmts })
    }
}
