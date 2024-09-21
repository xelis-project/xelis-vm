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
}