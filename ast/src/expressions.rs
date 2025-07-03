use xelis_types::{
    EnumValueType,
    IdentifierType,
    StructType,
    Type,
    Constant
};

use super::Operator;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Expression {
    FunctionCall(Option<Box<Expression>>, IdentifierType, Vec<Expression>), // path, function name, parameters
    DynamicCall(IdentifierType, Vec<Expression>, bool), // var_id, parameters, return value
    ArrayCall(Box<Expression>, Box<Expression>), // expr, index
    ArrayConstructor(Vec<Expression>),
    TuplesConstructor(Vec<Expression>),
    StructConstructor(Vec<Expression>, StructType),
    RangeConstructor(Box<Expression>, Box<Expression>), // start, end
    MapConstructor(Vec<(Expression, Expression)>, Type, Type),
    EnumConstructor(Vec<Expression>, EnumValueType),
    Variable(IdentifierType), // variable name
    Constant(Constant), // hardcoded value
    Operator(Operator, Box<Expression>, Box<Expression>),
    SubExpression(Box<Expression>), // ( ... )
    Path(Box<Expression>, Box<Expression>), // struct.value
    IsNot(Box<Expression>), // !expr (where expr is a bool)
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>), // bool expr, if true expr, else expr
    Cast(Box<Expression>, Type), // expr, type
    ForceType(Box<Expression>, Type),
    // Each sub variable/attribute of the enum, its type
    EnumPattern(Vec<Expression>, EnumValueType),
    // Function id registered, is closure
    FunctionPointer(IdentifierType, bool),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Statement {
    If(Expression, Vec<Statement>, Option<Vec<Statement>>),
    While(Expression, Vec<Statement>),
    ForEach(IdentifierType, Expression, Vec<Statement>), // for a in array
    For(DeclarationStatement, Expression, Expression, Vec<Statement>), // for i: u64 = 0; i < 10; i++ (; will not be saved)
    Expression(Expression),
    Return(Option<Expression>),
    Scope(Vec<Statement>),
    Break,
    Continue,
    Variable(DeclarationStatement),
    TuplesDeconstruction(Expression, Vec<TupleStatement>), // let (a, b): (u64, u64) = (1, 2)
    // match expr { condition => statement }
    Match(Box<Expression>, Vec<(MatchStatement, Statement)>, Option<Box<Statement>>, Option<Type>),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum MatchStatement {
    // match the variant and flatten it
    Variant(usize, EnumValueType),
    Cond(Expression),

}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum TupleStatement {
    Deconstruct(TupleDeconstruction),
    Depth
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct TupleDeconstruction {
    // If none, pop from stack
    pub id: Option<IdentifierType>,
    pub value_type: Type
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct DeclarationStatement {
    pub id: Option<IdentifierType>,
    pub value_type: Type,
    pub value: Expression,
}

#[derive(Debug, Hash, Eq, PartialEq, Clone)]
pub struct ConstantDeclaration {
    pub value: Constant,
    pub value_type: Type,
}