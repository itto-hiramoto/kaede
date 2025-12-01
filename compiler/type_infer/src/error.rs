use thiserror::Error;

use kaede_ir::qualified_symbol::QualifiedSymbol;
use kaede_span::Span;
use kaede_symbol::Symbol;

#[derive(Error, Debug)]
pub enum TypeInferError {
    #[error("{}:{}:{} array literal size mismatch: expected {}, got {}", span.file, span.start.line, span.start.column, expected, actual)]
    ArraySizeMismatch {
        expected: usize,
        actual: usize,
        span: Span,
    },

    #[error("{}:{}:{} expected array type, got {:?}", span.file, span.start.line, span.start.column, actual)]
    ExpectedArrayType { actual: String, span: Span },

    #[error("{}:{}:{} tuple literal arity mismatch: expected {}, got {}", span.file, span.start.line, span.start.column, expected, actual)]
    TupleArityMismatch {
        expected: usize,
        actual: usize,
        span: Span,
    },

    #[error("{}:{}:{} expected tuple type, got {:?}", span.file, span.start.line, span.start.column, actual)]
    ExpectedTupleType { actual: String, span: Span },

    #[error("{}:{}:{} undefined variable: {}", span.file, span.start.line, span.start.column, name)]
    UndefinedVariable { name: Symbol, span: Span },

    #[error("{}:{}:{} field {} not found in struct", span.file, span.start.line, span.start.column, field_name)]
    FieldNotFound { field_name: Symbol, span: Span },

    #[error("{}:{}:{} expected tuple type for tuple indexing at index {}, got: {:?} (unwrapped: {:?})", span.file, span.start.line, span.start.column, index, ty, unwrapped_ty)]
    NotATuple {
        index: u32,
        ty: String,
        unwrapped_ty: String,
        span: Span,
    },

    #[error("{}:{}:{} tuple index {} out of bounds", span.file, span.start.line, span.start.column, index)]
    TupleIndexOutOfBounds { index: u64, span: Span },

    #[error("{}:{}:{} str only has indices 0 and 1", span.file, span.start.line, span.start.column)]
    StrIndexOutOfBounds { span: Span },

    #[error("{}:{}:{} cannot index into non-array/pointer type", span.file, span.start.line, span.start.column)]
    NotIndexable { span: Span },

    #[error("{}:{}:{} function {:?} expects {} arguments, got {}", span.file, span.start.line, span.start.column, fn_name, expected, actual)]
    ArgumentCountMismatch {
        fn_name: QualifiedSymbol,
        expected: usize,
        actual: usize,
        span: Span,
    },

    #[error("{}:{}:{} tuple unpacking: expected {} elements, got {}", span.file, span.start.line, span.start.column, expected, actual)]
    TupleUnpackCountMismatch {
        expected: usize,
        actual: usize,
        span: Span,
    },

    #[error("{}:{}:{} expected tuple type for tuple unpacking", span.file, span.start.line, span.start.column)]
    ExpectedTupleForUnpack { span: Span },

    #[error("{}:{}:{} cannot infer type for expression. Please add a type annotation.", span.file, span.start.line, span.start.column)]
    CannotInferType { span: Span },

    #[error("{}:{}:{} cannot infer type for variable. Please add a type annotation.", span.file, span.start.line, span.start.column)]
    CannotInferVariableType { span: Span },

    #[error("{}:{}:{} integer literal cannot have type {:?}", span.file, span.start.line, span.start.column, ty)]
    InvalidIntegerLiteralType { ty: String, span: Span },

    #[error("occurs check failed: α{} occurs in {:?}", var_id, ty)]
    OccursCheckFailed { var_id: usize, ty: String },

    #[error("cannot unify {:?} with {:?}", a, b)]
    CannotUnify { a: String, b: String },

    #[error("arity mismatch in tuple types: {:?} and {:?}", a, b)]
    TupleArityMismatchInUnify { a: String, b: String },
}
