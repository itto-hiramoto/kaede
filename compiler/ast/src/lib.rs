use std::collections::VecDeque;

use stmt::Stmt;
use top::TopLevel;

pub mod expr;
pub mod stmt;
pub mod top;

#[derive(Debug)]
pub enum ModuleItem {
    Decl(TopLevel),
    Stmt(Stmt),
}

#[derive(Debug)]
pub struct CompileUnit {
    pub items: VecDeque<ModuleItem>,
}
