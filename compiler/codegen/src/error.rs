use kaede_symbol::Symbol;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("`main` function not found")]
    MainNotFound,

    /// Error issued by LLVM
    #[error("{}", .what)]
    LLVMError { what: String },

    #[error("failed to lookup target `{}`: {}", triple, what)]
    FailedToLookupTarget { triple: String, what: String },

    #[error("failed to create target machine")]
    FailedToCreateTargetMachine,

    #[error("expected closure type")]
    ExpectedClosureType,

    #[error("unsupported capture expression")]
    UnsupportedCaptureExpression,

    #[error("integer inference should have been resolved before codegen")]
    UnresolvedInferInt,

    #[error("unknown callee: {}", name)]
    UnknownCallee { name: Symbol },

    #[error("callee is not a closure value: {}", name)]
    CalleeNotClosureValue { name: Symbol },
}
