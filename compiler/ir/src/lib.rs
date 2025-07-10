use top::TopLevel;

pub mod expr;
pub mod module_path;
pub mod qualified_symbol;
pub mod stmt;
pub mod top;
pub mod ty;

#[derive(Debug)]
pub struct CompileUnit {
    pub top_levels: Vec<TopLevel>,
}
