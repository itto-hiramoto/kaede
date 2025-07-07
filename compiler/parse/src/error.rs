use kaede_span::Span;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("{}:{}:{} Expected {} but {} found", span.file, span.start.line, span.start.column, .expected, .but)]
    ExpectedError {
        expected: String,
        but: String,
        span: Span,
    },

    #[error("{}:{}:{} Out of range for 'i32'", .0.file, .0.start.line, .0.start.column)]
    OutOfRangeForI32(Span),

    #[error("{}:{}:{} Out of range for 'u32'", .0.file, .0.start.line, .0.start.column)]
    OutOfRangeForU32(Span),
}

pub type ParseResult<T> = anyhow::Result<T>;
