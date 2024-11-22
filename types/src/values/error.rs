use thiserror::Error;

use crate::Type;
use super::{value_type::ValueType, Value, ValueCell};


#[derive(Debug, Error)]
pub enum ValueError {
    #[error("Invalid value: {0:?} is not of type {1:?}")]
    InvalidValue(Value, Type),
    #[error("Invalid value type: {0:?} is not of type {1:?}")]
    InvalidValueType(ValueType, Type),
    #[error("Invalid value cell: {0:?} is not of type {1:?}")]
    InvalidValueCell(ValueCell, Type),
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
