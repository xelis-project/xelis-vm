use xelis_ast::Expression;
use xelis_environment::EnvironmentError;
use thiserror::Error;
use xelis_types::{IdentifierType, Type, ValueError};


#[derive(Debug, Error)]
pub enum InterpreterError {
    #[error(transparent)]
    EnvironmentError(#[from] EnvironmentError),
    #[error(transparent)]
    ValueError(#[from] ValueError),
    #[error("Missing value on stack")]
    MissingValueOnStack,
    #[error("Stack error")]
    StackError,
    #[error("no matching function found")]
    NoMatchingFunction,
    #[error("Function entry error: expected {0}, got {1}")]
    FunctionEntry(bool, bool), // expected, got
    #[error("Limit reached")]
    LimitReached,
    #[error("Not implemented")]
    NotImplemented,
    #[error("no exit code found")]
    NoExitCode,
    #[error("Expected a value")]
    ExpectedValue,
    #[error("Expected path, got {0:?}")]
    ExpectedPath(Expression),
    #[error("Expected instance for native function call")]
    NativeFunctionExpectedInstance,
    #[error("Invalid type for value: {0}")]
    InvalidType(Type),
    #[error("Variable not found: {0}")]
    VariableNotFound(IdentifierType),
    #[error("Recursive limit reached")]
    RecursiveLimitReached,
    #[error("Gas limit reached")]
    GasLimitReached,
    #[error("Invalid cast type: {0}")]
    InvalidCastType(Type),
    #[error("Division by zero")]
    DivByZero,
    #[error("Operation not number type")]
    OperationNotNumberType,
    #[error("Operation not boolean type")]
    OperationNotBooleanType,
    #[error("Unexpected operator")]
    UnexpectedOperator,
}