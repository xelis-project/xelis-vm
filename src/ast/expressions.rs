use crate::{
    types::Type,
    values::Value,
    IdentifierType,
    NoHashMap
};

use super::Operator;

#[derive(Debug)]
pub enum Expression {
    FunctionCall(IdentifierType, Vec<Expression>), // function name, parameters
    ArrayCall(Box<Expression>, Box<Expression>), // expr, index
    ArrayConstructor(Vec<Expression>),
    StructConstructor(IdentifierType, NoHashMap<Expression>),
    Variable(IdentifierType), // variable name
    Value(Value), // hardcoded value
    Operator(Operator, Box<Expression>, Box<Expression>),
    SubExpression(Box<Expression>), // ( ... )
    Path(Box<Expression>, Box<Expression>), // struct.value
    IsNot(Box<Expression>), // !expr (where expr is a bool)
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>), // bool expr, if true expr, else expr
    Cast(Box<Expression>, Type), // expr, type
}

#[derive(Debug)]
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
}

#[derive(Debug)]
pub struct DeclarationStatement {
    pub id: IdentifierType,
    pub value_type: Type,
    pub value: Expression,
}

impl std::hash::Hash for DeclarationStatement {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl std::cmp::PartialEq for DeclarationStatement {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl std::cmp::Eq for DeclarationStatement {}