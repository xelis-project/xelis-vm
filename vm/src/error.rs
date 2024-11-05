// use crate::{environment::EnvironmentError};
use types::{Value, ValueError};

#[derive(Debug)]
pub enum VMError {
    EmptyStack,
    IncompatibleValues(Value, Value),
    ChunkNotFound,
    ChunkNotEntry,
    StructNotFound,
    MissingInstruction,
    InvalidOpCode,
    TryInto,
    InvalidPrimitiveType,
    RegisterNotFound,
    EmptyRegister,
    MissingConstant,
    NoValue,
    ConstantNotFound,
    UnsupportedCastType,
    UnknownSysCall,
    // EnvironmentError(EnvironmentError),
    ValueError(ValueError),
    IncrementError,
    EmptyIterator,
    StackIndexOutOfBounds,
    NotEnoughArguments,
    StackOverflow,
    CallStackOverflow
}

// impl From<EnvironmentError> for VMError {
//     fn from(error: EnvironmentError) -> Self {
//         VMError::EnvironmentError(error)
//     }
// }

impl From<ValueError> for VMError {
    fn from(error: ValueError) -> Self {
        VMError::ValueError(error)
    }
}