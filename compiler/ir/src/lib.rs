use top::TopLevel;

pub mod expr;
pub mod module_path;
pub mod qualified_symbol;
pub mod stmt;
pub mod top;
pub mod ty;
pub mod validate;

pub use validate::{validate_compile_unit, ResolvedCompileUnit, ValidationError, ValidationIssue};

#[derive(Debug)]
pub struct CompileUnit {
    pub top_levels: Vec<TopLevel>,
}
