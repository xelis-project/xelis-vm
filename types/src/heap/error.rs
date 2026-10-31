use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub enum HeapError {
    #[error("runtime opaque values cannot escape their execution")]
    NonPortableValue,
    #[error("heap handle is invalid or no longer live")]
    InvalidPointer,
    #[error("heap slot storage capacity exceeded")]
    CapacityExceeded,
    #[error("opaque child traversal changed during detachment")]
    InvalidChildren,
}
