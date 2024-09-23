use crate::environment::EnvironmentError;

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
}

impl From<EnvironmentError> for VMError {
    fn from(error: EnvironmentError) -> Self {
        VMError::EnvironmentError(error)
    }
}