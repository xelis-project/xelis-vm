use crate::{environment::EnvironmentError, values::ValueError};

#[derive(Debug)]
pub enum VMError {
    EmptyStack,
    IncompatibleValues,
    ChunkNotFound,
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
    EnvironmentError(EnvironmentError),
    ValueError(ValueError),
    IncrementError,
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