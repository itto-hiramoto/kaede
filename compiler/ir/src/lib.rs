use top::TopLevel;

pub mod expr;
pub mod stmt;
pub mod top;
pub mod ty;
pub mod module_path;
pub mod qualified_symbol;

#[derive(Debug)]
pub struct CompileUnit {
    pub top_levels: Vec<TopLevel>,
}
