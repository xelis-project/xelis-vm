use thiserror::Error;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub enum HeapError {
    #[error("heap handle index is out of bounds")]
    InvalidPointer,
    #[error("heap capacity exceeds the maximum slot index")]
    CapacityExceeded,
}
