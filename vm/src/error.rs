use thiserror::Error;
use xelis_environment::EnvironmentError;
use xelis_types::{Primitive, ValueError};

#[derive(Debug, Error)]
pub enum VMError {
    #[error("iterator overflow")]
    IteratorOverflow,
    #[error("chunk manager not found with id {0}")]
    ChunkManagerNotFound(usize),
    #[error("instance callback")]
    InstanceCallback,
    #[error("invalid dynamic call")]
    InvalidDynamicCall,
    #[error("no stack checkpoint")]
    NoCheckPoint,
    #[error("no module found")]
    NoModule,
    #[error("Expected checkpoint")]
    ExpectedCheckPoint,
    #[error("register max size reached")]
    RegisterMaxSize,
    #[error("register overflow")]
    RegisterOverflow,
    #[error("invalid return value")]
    InvalidReturnValue,
    #[error("division by zero")]
    DivisionByZero,
    #[error("illegal call: entry chunk")]
    EntryChunkCalled,
    #[error("string too large")]
    StringTooLarge,
    #[error("out of bounds")]
    OutOfBounds,
    #[error("enum not found")]
    EnumNotFound,
    #[error("enum variant not found")]
    InvalidEnumVariant,
    #[error("stack not cleaned, we have {0} elements left")]
    StackNotCleaned(usize),
    #[error("invalid range type")]
    InvalidRangeType,
    #[error("empty stack")]
    EmptyStack,
    #[error("incompatible values: {0:?} and {1:?}")]
    IncompatibleValues(Primitive, Primitive),
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
    #[error("unsupported syscall operation '{0}'")]
    UnknownSysCall(u16),
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
    CallStackOverflow,
    #[error("call stack underflow")]
    CallStackUnderflow,
    #[error("modules stack overflow")]
    ModulesStackOverflow,
    #[error("unexpected type")]
    UnexpectedType,
    #[error("{0}")]
    Static(&'static str),
    #[error("Error, memory not cleaned, we have {0} bytes left")]
    MemoryNotCleaned(usize),
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

impl From<&'static str> for VMError {
    fn from(value: &'static str) -> Self {
        VMError::Static(value)
    }
}