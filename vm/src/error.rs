use std::borrow::Cow;

use thiserror::Error;
use xelis_bytecode::ChunkReaderError;
use xelis_environment::EnvironmentError;
use xelis_types::ValueError;

#[derive(Debug, Error)]
pub enum VMError {
    #[error("call stack not empty, size is {0}")]
    CallStackNotEmpty(usize),
    #[error(transparent)]
    ChunkError(#[from] ChunkReaderError),
    #[error("unknown op code called")]
    UnknownOpCode,
    #[error("iterator overflow")]
    IteratorOverflow,
    #[error("chunk manager not found with id {0}")]
    ChunkManagerNotFound(usize),
    #[error("invalid dynamic call")]
    InvalidDynamicCall,
    #[error("no module found")]
    NoModule,
    #[error("register max size reached")]
    RegisterMaxSize,
    #[error("register overflow")]
    RegisterOverflow,
    #[error("division by zero")]
    DivisionByZero,
    #[error("illegal call: expected a public chunk")]
    ExpectedPublicChunk,
    #[error("illegal call: expected a normal chunk")]
    ExpectedNormalChunk,
    #[error("stack not cleaned, we have {0} elements left")]
    StackNotCleaned(usize),
    #[error("invalid range type")]
    InvalidRangeType,
    #[error("empty stack")]
    EmptyStack,
    #[error("chunk was not found")]
    ChunkNotFound,
    #[error("chunk is not an entry")]
    ChunkNotEntry,
    #[error("register {0} was not found")]
    RegisterNotFound(usize),
    #[error("empty register")]
    EmptyRegister,
    #[error("constant not found at {0}")]
    ConstantNotFound(usize),
    #[error("unsupported cast type")]
    UnsupportedCastType,
    #[error("unsupported syscall operation '{0}'")]
    UnknownSysCall(u16),
    #[error("error from environment: {0}")]
    EnvironmentError(EnvironmentError),
    #[error("error from value: {0}")]
    ValueError(#[from] ValueError),
    #[error("empty iterator")]
    EmptyIterator,
    #[error("stack index out of bounds")]
    StackIndexOutOfBounds,
    #[error("not enough arguments")]
    NotEnoughArguments,
    #[error("stack overflow")]
    StackOverflow,
    #[error("call stack overflow")]
    CallStackOverflow,
    #[error("call stack underflow")]
    CallStackUnderflow,
    #[error("modules stack overflow")]
    ModulesStackOverflow,
    #[error("illegal state")]
    IllegalState,
    #[error("{0}")]
    Static(Cow<'static, str>),
    #[error("out of memory")]
    OutOfMemory,
    #[error(transparent)]
    Any(#[from] anyhow::Error)
}

impl From<EnvironmentError> for VMError {
    fn from(error: EnvironmentError) -> Self {
        match error {
            EnvironmentError::DivisionByZero => VMError::DivisionByZero,
            EnvironmentError::OutOfMemory => VMError::OutOfMemory,
            other => VMError::EnvironmentError(other),
        }
    }
}

impl From<&'static str> for VMError {
    fn from(value: &'static str) -> Self {
        VMError::Static(Cow::Borrowed(value))
    }
}

impl From<String> for VMError {
    fn from(value: String) -> Self {
        VMError::Static(Cow::Owned(value))
    }
}