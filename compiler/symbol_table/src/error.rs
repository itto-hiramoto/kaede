use thiserror::Error;

use kaede_span::Span;
use kaede_symbol::Symbol;

#[derive(Error, Debug)]
pub enum SymbolTableError {
    #[error("{}:{}:{} `{}` is already declared", span.file, span.start.line, span.start.column, .name)]
    AlreadyDeclared { name: Symbol, span: Span },
}
