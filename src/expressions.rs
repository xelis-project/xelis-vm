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
    Operator(Operator, Box<Expression>, Box<Expression>),
    SubExpression(Box<Expression>), // ( ... )
    Path(Box<Expression>, Box<Expression>), // struct.value
    IsNot(Box<Expression>), // !expr (where expr is a bool)
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>), // bool expr, if true expr, else expr
    Cast(Box<Expression>, Type), // expr, type
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

#[derive(Debug, PartialEq)]
pub enum Operator {
    Equals, // ==
    NotEquals, // !=
    And, // &&
    Or, // ||
    GreaterThan, // >
    LessThan, // <
    GreaterOrEqual, // >=
    LessOrEqual, // <=
    Plus, // +
    Minus, // -
    Multiply, // *
    Divide, // /
    Modulo, // %

    BitwiseXor, // ^
    BitwiseAnd, // &
    BitwiseOr, // |
    BitwiseLeft, // <<
    BitwiseRight, // >>

    Assign, // =
    AssignPlus, // +=
    AssignMinus, // -=
    AssignDivide, // /=
    AssignMultiply, // *=
    AssignModulo, // %=
    AssignBitwiseXor, // ^=
    AssignBitwiseAnd, // &=
    AssignBitwiseOr, // |=
    AssignBitwiseLeft, // <<=
    AssignBitwiseRight, // >>=
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

impl Operator {
    pub fn value_of(token: &Token) -> Option<Operator> {
        use Operator::*;
        let value = match token {
            Token::OperatorEquals => Equals,
            Token::OperatorNotEquals => NotEquals,
            Token::OperatorAnd => And,
            Token::OperatorOr => Or,
            Token::OperatorGreaterThan => GreaterThan,
            Token::OperatorLessThan => LessThan,
            Token::OperatorGreaterOrEqual => GreaterOrEqual,
            Token::OperatorLessOrEqual => LessOrEqual,
            Token::OperatorPlus => Plus,
            Token::OperatorMinus => Minus,
            Token::OperatorMultiply => Multiply,
            Token::OperatorDivide => Divide,
            Token::OperatorModulo => Modulo,

            Token::OperatorBitwiseXor => BitwiseXor,
            Token::OperatorBitwiseAnd => BitwiseAnd,
            Token::OperatorBitwiseOr => BitwiseOr,
            Token::OperatorBitwiseLeft => BitwiseLeft,
            Token::OperatorBitwiseRight => BitwiseRight,

            Token::OperatorAssign => Assign,
            Token::OperatorPlusAssign => AssignPlus,
            Token::OperatorMinusAssign => AssignMinus,
            Token::OperatorDivideAssign => AssignDivide,
            Token::OperatorMultiplyAssign => AssignMultiply,
            Token::OperatorModuloAssign => AssignModulo,
            Token::OperatorBitwiseXorAssign => AssignBitwiseXor,
            Token::OperatorBitwiseAndAssign => AssignBitwiseAnd,
            Token::OperatorBitwiseOrAssign => AssignBitwiseOr,
            Token::OperatorBitwiseLeftAssign => AssignBitwiseLeft,
            Token::OperatorBitwiseRightAssign => AssignBitwiseRight,

            _ => return None,
        };
        Some(value)
    }

    pub fn is_assignation(&self) -> bool {
        use Operator::*;
        match &self {
            Assign
            | AssignPlus
            | AssignMinus
            | AssignDivide
            | AssignMultiply => true,
            _ => false
        }
    }

    pub fn is_number_operator(&self) -> bool {
        match &self {
            Operator::AssignPlus
            | Operator::AssignMinus
            | Operator::AssignDivide
            | Operator::AssignMultiply
            | Operator::Minus
            | Operator::Divide
            | Operator::Multiply
            | Operator::Modulo
            | Operator::BitwiseXor
            | Operator::BitwiseAnd
            | Operator::BitwiseOr
            | Operator::BitwiseLeft
            | Operator::BitwiseRight
            | Operator::GreaterOrEqual
            | Operator::GreaterThan
            | Operator::LessOrEqual
            | Operator::LessThan => true,
            _ => false
        }
    }

    pub fn is_and_or_or(&self) -> bool {
        match &self {
            Operator::And | Operator::Or => true,
            _ => false
        }
    }
}