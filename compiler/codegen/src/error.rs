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
}
