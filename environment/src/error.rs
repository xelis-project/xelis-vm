use thiserror::Error;
use xelis_types::{ValueError, Value};

#[derive(Debug, Error)]
pub enum EnvironmentError {
    #[error("Invalid function call")]
    InvalidFnCall,
    #[error("Invalid function call: expected instance")]
    FnExpectedInstance,
    #[error("Panic: {0}")]
    Panic(Value),
    #[error("Out of bounds: {0} > {1}")]
    OutOfBounds(usize, usize),
    #[error("Invalid range: {0} > {1}")]
    InvalidRange(u32, u32),
    #[error("No value found at index: {0}")]
    NoValueFoundAtIndex(u32),
    #[error("Invalid type for value: {0}")]
    InvalidType(Value),
    #[error(transparent)]
    ValueError(#[from] ValueError)
}