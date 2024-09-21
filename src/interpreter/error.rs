use crate::{
    ast::Expression,
    environment::EnvironmentError,
    IdentifierType,
    Type,
    Value
};

#[derive(Debug)]
pub enum InterpreterError {
    EnvironmentError(EnvironmentError),
    SubValue,
    Panic(Value),
    MissingValueOnStack,
    StackError,
    Unknown,
    NoReturnValue,
    OptionalIsNull,
    NoMatchingFunction,
    TypeNotFound(Value),
    FunctionEntry(bool, bool), // expected, got
    LimitReached,
    NotImplemented,
    NoExitCode,
    ExpectedValue,
    InvalidNativeFunctionCall,
    ExpectedPath(Expression),
    UnexpectedInstanceType,
    ExpectedInstanceType,
    UnexpectedOperator,
    ExpectedStructType,
    NativeFunctionExpectedInstance,
    OverflowOccured,
    DivByZero,
    StructureNotFound(IdentifierType),
    StructureFieldNotFound(IdentifierType, IdentifierType),
    ExpectedValueType(Type),
    InvalidType(Type),
    OutOfBounds(usize, usize),
    InvalidRange(u64, u64),
    NoValueFoundAtIndex(u64),
    MissingValueForFunctionCall,
    InvalidStructValue(Value),
    InvalidValue(Value, Type), // got value, but expected type
    VariableNotFound(IdentifierType),
    VariableAlreadyExists(IdentifierType),
    NoScopeFound,
    ExpectedAssignOperator,
    OperationNotNumberType,
    OperationNotBooleanType,
    CastNumberError,
    RecursiveLimitReached,
    GasLimitReached,
    InvalidCastType(Type),
}

impl From<EnvironmentError> for InterpreterError {
    fn from(error: EnvironmentError) -> Self {
        InterpreterError::EnvironmentError(error)
    }
}