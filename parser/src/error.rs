use std::borrow::Cow;

use thiserror::Error;
use xelis_ast::{Expression, Token};
use xelis_builder::BuilderError;
use xelis_types::{Type, ValueError, IdentifierType};

#[derive(Debug, Error)]
#[error("error at line {line}, column {column_start} to {column_end}: {kind}")]
pub struct ParserError<'a> {
    pub line: usize,
    pub column_start: usize,
    pub column_end: usize,
    pub kind: ParserErrorKind<'a>
}

#[derive(Debug, Error)]
pub enum ParserErrorKind<'a> {
    #[error("cannot create a method on type '{0}'")]
    InvalidFunctionType(Type),
    #[error("number is too big for type '{0}'")]
    NumberTooBigForType(Type),
    #[error("invalid constant name '{0}'")]
    InvalidConstName(&'a str),
    #[error("invalid constant value")]
    InvalidConstantValue,
    #[error("too many parameters")]
    TooManyParameters,
    #[error("too many variants")]
    TooManyVariants,
    #[error("expected a value for variable '{0}' or an optional type")]
    NoValueForVariable(&'a str),
    #[error("cannot call this function, its an entry function")]
    FunctionIsEntry,
    #[error("invalid field name, got '{0}' but expected '{1}'")]
    InvalidFieldName(&'a str, &'a str),
    #[error("enum variant name '{0}' is already used")]
    EnumVariantAlreadyUsed(&'a str),
    #[error("invalid enum field name '{0}'")]
    InvalidEnumFieldName(&'a str),
    #[error("enum type name '{0}' is already used")]
    TypeNameAlreadyUsed(&'a str),
    #[error("enum name cannot be empty")]
    EmptyEnumName,
    #[error("invalid enum name '{0}'")]
    InvalidEnumName(&'a str),
    #[error("enum variant '{0}' not found")]
    EnumVariantNotFound(&'a str),
    #[error("constant not found for type '{0}' and name '{1}'")]
    ConstantNotFound(Type, &'a str),
    #[error("type '{0}' is not iterable")]
    NotIterable(Type),
    #[error("invalid range type '{0}' and '{1}'")]
    InvalidRangeType(Type, Type),
    #[error("invalid range type '{0}'")]
    InvalidRangeTypePrimitive(Type),
    #[error(transparent)]
    ValueError(#[from] ValueError),
    #[error(transparent)]
    BuilderError(#[from] BuilderError),
    #[error("invalid struct field order")]
    InvalidStructFieldOrder,
    #[error("invalid field count")]
    InvalidFieldCount,
    #[error("unexpected path in function call")]
    UnexpectedPathInFunctionCall,
    #[error("invalid import")]
    InvalidImport,
    #[error("invalid import path '{0}'")]
    InvalidImportPath(Cow<'a, str>),
    #[error("constant name is not in uppercase: '{0}'")]
    ConstantNameNotUppercase(&'a str),
    #[error("type name not found '{0}'")]
    TypeNameNotFound(&'a str),
    #[error("assign operation return nothing")]
    AssignReturnNothing,
    #[error("entry function cannot have a type")]
    EntryFunctionCannotHaveForType,
    #[error("expected token")]
    ExpectedToken,
    #[error("variable name must start with an alphabetic character: '{0}'")]
    VariableMustStartWithAlphabetic(&'a str),
    #[error("expected identifier token got '{0:?}'")]
    ExpectedIdentifierToken(Token<'a>),
    #[error("unexpected token '{0:?}'")]
    UnexpectedToken(Token<'a>),
    #[error("unexpected token in postfix expresion '{0:?}'")]
    UnexpectedTokenInPostfix(Token<'a>),
    #[error("invalid token, got '{0:?}' expected '{1:?}'")]
    InvalidToken(Token<'a>, Token<'a>),
    #[error("variable name is already used: {0}")]
    VariableNameAlreadyUsed(&'a str),
    #[error("variable id is already used: {0}")]
    VariableIdAlreadyUsed(IdentifierType),
    #[error("duplicated function signature")]
    FunctionSignatureAlreadyExist,
    #[error("unexpected variable name '{0}'")]
    UnexpectedVariable(&'a str),
    #[error("unexpected mapped variable id '{0}'")]
    UnexpectedMappedVariableId(IdentifierType),
    #[error("unexpected type '{0}'")]
    UnexpectedType(Type),
    #[error("invalid struct name '{0}'")]
    InvalidStructureName(&'a str),
    #[error("function was not found")]
    FunctionNotFound,
    #[error("function has no return type")]
    FunctionNoReturnType,
    #[error("invalid type T")]
    InvalidTypeT,
    #[error("no return found in function")]
    NoReturnFound,
    #[error("empty value")]
    EmptyValue,
    #[error("invalid 'null' value with type {0}")]
    IncompatibleNullWith(Type),
    #[error("empty struct name")]
    EmptyStructName,
    #[error("invalid array call")]
    InvalidArrayCall,
    #[error("not implemented")]
    NotImplemented,
    #[error("invalid operation")]
    InvalidOperation,
    #[error("invalid ternary: no previous expression")]
    InvalidTernaryNoPreviousExpression,
    #[error("dead code not allowed")]
    DeadCodeNotAllowed,
    #[error("invalid for expression '{0:?}'")]
    InvalidForExpression(Expression),
    #[error("operator not found for token '{0:?}'")]
    OperatorNotFound(Token<'a>),
    #[error("invalid condition for type '{0}': {1:?}")]
    InvalidCondition(Type, Expression),
    #[error("invalid operation: not same type: '{0}' and '{1}'")]
    InvalidOperationNotSameType(Type, Type),
    #[error("cast error: '{0}' and '{1:?}'")]
    CastError(Type, Type),
    #[error("invalid primitive cast: '{0}' and '{1}'")]
    CastPrimitiveError(Type, Type),
    #[error("invalid array call index type, got: '{0}'")]
    InvalidArrayCallIndexType(Type),
    #[error("invalid type in array: got '{0}' expected '{1}'")]
    InvalidTypeInArray(Type, Type),
    #[error("invalid value type: got '{0}' expected '{1}'")]
    InvalidValueType(Type, Type),
    #[error("no value type found")]
    NoValueType,
    #[error("empty array constructor")]
    EmptyArrayConstructor,
    #[error("invalid map key type")]
    InvalidMapKeyType,
    #[error("invalid expression")]
    InvalidExpression,
    #[error("unknown error")]
    UnknownError,
}