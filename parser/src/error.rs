use xelis_ast::{Expression, Token};
use xelis_builder::BuilderError;
use xelis_types::{Type, ValueError, IdentifierType};

#[derive(Debug)]
pub enum ParserError<'a> {
    InvalidFieldName(&'a str, &'a str),
    EnumVariantAlreadyUsed(&'a str),
    InvalidEnumFieldName(&'a str),
    TypeNameAlreadyUsed(&'a str),
    EmptyEnumName,
    InvalidEnumName(&'a str),
    EnumVariantNotFound(&'a str),
    ConstantNotFound(Type, &'a str),
    NotIterable(Type),
    InvalidRangeType(Type, Type),
    InvalidRangeTypePrimitive(Type),
    ValueError(ValueError),
    BuilderError(BuilderError),
    InvalidStructFieldOrder,
    InvalidFieldCount,
    UnexpectedPathInFunctionCall,
    InvalidImport,
    InvalidImportPath(String),
    ImportNotFound(String),
    MappingExists(IdentifierType),
    ConstantNameNotUppercase(String),
    TypeNameNotFound(&'a str),
    StructIdNotFound(IdentifierType),
    AssignReturnNothing,
    EntryFunctionCannotHaveForType,
    ExpectedToken,
    VariableTooLong(String),
    VariableMustStartWithAlphabetic(String),
    ExpectedIdentifierToken(Token<'a>),
    UnexpectedToken(Token<'a>),
    InvalidToken(Token<'a>, Token<'a>),
    TypeNotFound(Token<'a>),
    NoIfBeforeElse,
    StructNameAlreadyUsed(String),
    VariableNameAlreadyUsed(&'a str),
    VariableIdAlreadyUsed(IdentifierType),
    FunctionSignatureAlreadyExist,
    UnexpectedVariable(String),
    UnexpectedMappedVariableId(IdentifierType),
    MappingNotFound(String),
    UnexpectedType(Type),
    InvalidStructField(String),
    InvalidStructureName(String),
    FunctionNotFound,
    LastFunction,
    FunctionNoReturnType,
    InvalidTypeT,
    NoScopeFound,
    NoReturnFound,
    ReturnAlreadyInElse,
    EmptyValue,
    IncompatibleNullWith(Type),
    EmptyStructName,
    InvalidArrayCall,
    NotImplemented,
    InvalidOperation,
    InvalidTernaryNoPreviousExpression,
    DeadCodeNotAllowed,
    InvalidForExpression(Expression),
    OperatorNotFound(Token<'a>),
    InvalidCondition(Type, Expression),
    InvalidOperationNotSameType(Type, Type),
    CastError(Type, Type),
    CastPrimitiveError(Type, Type),
    InvalidArrayCallIndexType(Type),
    InvalidTypeInArray(Type, Type),
    InvalidValueType(Type, Type),
    NoValueType,
    ExpectedArrayType,
    InvalidFunctionType(Type),
    EmptyArrayConstructor,
    ExpectedNumberType(Type),
    InvalidNumberValueForType
}

impl<'a> From<ValueError> for ParserError<'a> {
    fn from(e: ValueError) -> Self {
        ParserError::ValueError(e)
    }
}

impl<'a> From<BuilderError> for ParserError<'a> {
    fn from(e: BuilderError) -> Self {
        ParserError::BuilderError(e)
    }
}