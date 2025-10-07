use thiserror::Error;
use xelis_types::ValueError;

#[derive(Debug, Error)]
pub enum EnvironmentError {
    #[error("Invalid parameter")]
    InvalidParameter,
    #[error("Assertion failed")]
    AssertionFailed,
    #[error("missing instance for function call")]
    MissingInstanceFnCall,
    #[error("Invalid function call, expected {} parameters got {}", _0, _1)]
    InvalidFnCall(usize, usize),
    #[error("Invalid function call: expected instance")]
    FnExpectedInstance,
    #[error("Panic: {0}")]
    Panic(String),
    #[error("Out of bounds: {0} > {1}")]
    OutOfBounds(usize, usize),
    #[error("Invalid range: {0} > {1}")]
    InvalidRange(u32, u32),
    #[error("No value found at index: {0}")]
    NoValueFoundAtIndex(u32),
    #[error("Invalid type")]
    InvalidType,
    #[error(transparent)]
    ValueError(#[from] ValueError),
    #[error("Invalid range: too large")]
    RangeTooLarge,
    #[error("Not enough gas: limit {limit}, actual {actual}")]
    NotEnoughGas {
        limit: u64,
        actual: u64,
    },
    #[error("Gas overflow")]
    GasOverflow,
    #[error("Invalid key type")]
    InvalidKeyType,
    #[error(transparent)]
    Any(#[from] anyhow::Error),
    #[error("Out of memory")]
    OutOfMemory,
    #[error("Unexpected behavior: {0}")]
    Expect(String),
    #[error("Invalid expect message, require alphanumeric chars only")]
    InvalidExpect,
    #[error("{0}")]
    Static(&'static str)
}


impl From<&'static str> for EnvironmentError {
    fn from(value: &'static str) -> Self {
        EnvironmentError::Static(value)
    }
}