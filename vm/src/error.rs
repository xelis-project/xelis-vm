use thiserror::Error;
use xelis_environment::EnvironmentError;
use xelis_types::{Value, ValueError};

#[derive(Debug, Error)]
pub enum VMError {
    #[error("enum not found")]
    EnumNotFound,
    #[error("enum variant not found")]
    InvalidEnumVariant,
    #[error("stack not cleaned")]
    StackNotCleaned,
    #[error("invalid range type")]
    InvalidRangeType,
    #[error("empty stack")]
    EmptyStack,
    #[error("incompatible values: {0:?} and {1:?}")]
    IncompatibleValues(Value, Value),
    #[error("chunk was not found")]
    ChunkNotFound,
    #[error("chunk is not an entry")]
    ChunkNotEntry,
    #[error("struct was not found")]
    StructNotFound,
    #[error("missing instruction in module")]
    MissingInstruction,
    #[error("invalid opcode")]
    InvalidOpCode,
    #[error("invalid primitive type")]
    InvalidPrimitiveType,
    #[error("register was not found")]
    RegisterNotFound,
    #[error("empty register")]
    EmptyRegister,
    #[error("constant not found")]
    ConstantNotFound,
    #[error("unsupported cast type")]
    UnsupportedCastType,
    #[error("unsupported syscall operation")]
    UnknownSysCall,
    #[error("error from environment: {0}")]
    EnvironmentError(EnvironmentError),
    #[error("error from value: {0}")]
    ValueError(ValueError),
    #[error("empty iterator")]
    EmptyIterator,
    #[error("stack index out of bounds")]
    StackIndexOutOfBounds,
    #[error("not enough arguments")]
    NotEnoughArguments,
    #[error("stack overflow")]
    StackOverflow,
    #[error("call stack overflow")]
    CallStackOverflow
}

impl From<EnvironmentError> for VMError {
    fn from(error: EnvironmentError) -> Self {
        VMError::EnvironmentError(error)
    }
}

impl From<ValueError> for VMError {
    fn from(error: ValueError) -> Self {
        VMError::ValueError(error)
    }
}