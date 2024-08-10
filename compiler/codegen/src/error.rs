use kaede_span::Span;
use kaede_symbol::Symbol;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("{}:{}:{} `{}` was not declared in this scope", span.file, span.start.line, span.start.column, .name)]
    Undeclared { name: Symbol, span: Span },

    #[error("{}:{}:{} variable `{}` declared void", span.file, span.start.line, span.start.column, .name)]
    VoidVariable { name: Symbol, span: Span },

    #[error("{}:{}:{} `break` outside of a loop", span.file, span.start.line, span.start.column)]
    BreakOutsideOfLoop { span: Span },

    #[error("{}:{}:{} mismatched types: `{}` vs `{}`", span.file, span.start.line, span.start.column, types.0, types.1)]
    MismatchedTypes { types: (String, String), span: Span },

    #[error("{}:{}:{} invalid left-hand side of assignment", span.file, span.start.line, span.start.column)]
    InvalidLeftOfAssignment { span: Span },

    #[error("{}:{}:{} cannot assign twice to immutable variable", span.file, span.start.line, span.start.column)]
    CannotAssignTwiceToImutable { span: Span },

    #[error("{}:{}:{} has no fields", span.file, span.start.line, span.start.column)]
    HasNoFields { span: Span },

    #[error("{}:{}:{} file not found for module `{}`", span.file, span.start.line, span.start.column, mod_name)]
    FileNotFoundForModule { span: Span, mod_name: Symbol },

    #[error("{}:{}:{} tuples require access by index", span.file, span.start.line, span.start.column)]
    TupleRequireAccessByIndex { span: Span },

    #[error("{}:{}:{} index `{}` is out of range", span.file, span.start.line, span.start.column, index)]
    IndexOutOfRange { index: u64, span: Span },

    #[error("{}:{}:{} number of tuple fields does not match: `{}` vs `{}`", span.file, span.start.line, span.start.column, lens.0, lens.1)]
    NumberOfTupleFieldsDoesNotMatch { lens: (usize, usize), span: Span },

    #[error("{}:{}:{} `if` must have both main and `else` branches if used as an expression", span.file, span.start.line, span.start.column)]
    IfMustHaveElseUsedAsExpr { span: Span },

    #[error("{}:{}:{} `if` and `else` have incompatible types: `{}` vs `{}`", span.file, span.start.line, span.start.column, types.0, types.1)]
    IfAndElseHaveIncompatibleTypes { types: (String, String), span: Span },

    #[error("{}:{}:{} cannot assign an immutable to a mutable", span.file, span.start.line, span.start.column)]
    CannotAssignImmutableToMutable { span: Span },

    #[error("{}:{}:{} no member named `{}` in `{}`", span.file, span.start.line, span.start.column, member_name, parent_name)]
    NoMember {
        member_name: Symbol,
        parent_name: Symbol,
        span: Span,
    },

    #[error("{}:{}:{} no variant named `{}` in `{}`", span.file, span.start.line, span.start.column, variant_name, parent_name)]
    NoVariant {
        variant_name: Symbol,
        parent_name: Symbol,
        span: Span,
    },

    #[error("{}:{}:{} unreachable pattern", span.file, span.start.line, span.start.column)]
    UnreachablePattern { span: Span },

    #[error("{}:{}:{} all control paths will be `never` (make it a statement, not an if expression)", span.file, span.start.line, span.start.column)]
    NeverIfExpr { span: Span },

    #[error("{}:{}:{} non-exhaustive patterns: {} not covered", span.file, span.start.line, span.start.column, non_exhaustive_patterns)]
    NonExhaustivePatterns {
        non_exhaustive_patterns: String,
        span: Span,
    },

    #[error("{}:{}:{} cannot unpack values from unit variant `{}`", span.file, span.start.line, span.start.column, unit_variant_name)]
    UnitVariantCannotUnpack {
        unit_variant_name: Symbol,
        span: Span,
    },

    #[error("{}:{}:{} unsupported language linkage `{}`", span.file, span.start.line, span.start.column, lang_linkage)]
    UnsupportedLanguageLinkage { lang_linkage: Symbol, span: Span },

    #[error("`main` function not found")]
    MainNotFound,

    /// Error issued by LLVM
    #[error("{}", .what)]
    LLVMError { what: String },

    #[error("failed to lookup target `{}`: {}", triple, what)]
    FailedToLookupTarget { triple: String, what: String },

    #[error("failed to create target machine")]
    FailedToCreateTargetMachine,
}
