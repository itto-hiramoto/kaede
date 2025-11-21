//! Bidirectional Type Inference

pub use crate::context::InferContext;

mod context;

pub struct TypeInferrer {
    context: InferContext,
}
