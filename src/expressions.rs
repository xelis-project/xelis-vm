use crate::types::{Value, Type};
use crate::token::Token;
use std::collections::HashMap;

#[derive(Debug)]
pub enum Expression {
    FunctionCall(String, Vec<Expression>), // function name, parameters
    ArrayCall(Box<Expression>, Box<Expression>), // expr, index
    ArrayConstructor(Vec<Expression>),
    StructConstructor(String, HashMap<String, Expression>),
    Variable(String), // variable name
    Value(Value), // hardcoded value
    Operator(Token, Box<Expression>, Box<Expression>),
    SubExpression(Box<Expression>), // ( ... )
    Path(Box<Expression>, Box<Expression>)
}

#[derive(Debug)]
pub enum Statement {
    If(Expression, Vec<Statement>),
    Else(Vec<Statement>),
    ElseIf(Expression, Vec<Statement>),
    While(Expression, Vec<Statement>),
    ForEach(String, Expression, Vec<Statement>), // for a in array
    For(DeclarationStatement, Expression, Expression, Vec<Statement>), // for i: int = 0; i < 10; i++ (; will not be saved)
    Expression(Expression),
    Return(Option<Expression>),
    Scope(Vec<Statement>),
    Break,
    Continue,
    Variable(DeclarationStatement),
}

#[derive(Debug)]
pub struct DeclarationStatement {
    pub name: String,
    pub value_type: Type,
    pub value: Expression,
}

#[derive(Debug)]
pub struct Parameter {
    name: String,
    value_type: Type
}

impl Parameter {
    pub fn new(name: String, value_type: Type) -> Self {
        Parameter {
            name,
            value_type
        }
    }

    pub fn get_name(&self) -> &String {
        &self.name
    }

    pub fn get_type(&self) -> &Type {
        &self.value_type
    }

    pub fn consume(self) -> (String, Type) {
        (self.name, self.value_type)
    }
}
