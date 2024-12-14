use thiserror::Error;

use crate::Type;
use super::{constant::Constant, Value};


#[derive(Debug, Error)]
pub enum ValueError {
    #[error("Type mismatch")]
    InvalidOpaqueTypeMismatch,
    #[error("max depth reached")]
    MaxDepthReached,
    #[error("Invalid value: {0:?} is not of type {1:?}")]
    InvalidValue(Value, Type),
    #[error("Invalid value type: {0:?} is not of type {1:?}")]
    InvalidValueType(Constant, Type),
    #[error("Expected a value of type {0:?}")]
    ExpectedValueOfType(Type),
    #[error("Expected opaque value")]
    ExpectedOpaque,
    #[error("expected a struct")]
    ExpectedStruct,
    #[error("Invalid cast type: {0:?}")]
    InvalidCastType(Type),
    #[error("Operation not supported on non-number type")]
    OperationNotNumberType,
    #[error("Sub value")]
    SubValue,
    #[error("Optional value is null")]
    OptionalIsNull,
    #[error("Value out of bounds: {0} on {1}")]
    OutOfBounds(usize, usize),
    #[error("Cast error")]
    CastError,
    #[error("Invalid primitive type")]
    InvalidPrimitiveType,
    #[error("Invalid unknown type")]
    UnknownType,
}
