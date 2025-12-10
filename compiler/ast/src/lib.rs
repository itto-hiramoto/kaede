use std::collections::VecDeque;

use stmt::Stmt;
use top::TopLevel;

pub mod expr;
pub mod stmt;
pub mod top;

#[derive(Debug)]
pub struct CompileUnit {
    pub top_levels: VecDeque<TopLevel>,
    pub top_level_stmts: VecDeque<Stmt>,
}
