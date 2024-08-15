use crate::{
    token::Token,
    types::Type,
    values::Value,
    IdentifierType,
    NoHashMap
};

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
    If(Expression, Vec<Statement>),
    Else(Vec<Statement>),
    ElseIf(Expression, Vec<Statement>),
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

#[derive(Debug, PartialEq, Clone)]
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
    Rem, // %

    BitwiseXor, // ^
    BitwiseAnd, // &
    BitwiseOr, // |
    BitwiseLeft, // <<
    BitwiseRight, // >>

    Assign(Option<Box<Operator>>),
    // Assign, // =
    // AssignPlus, // +=
    // AssignMinus, // -=
    // AssignDivide, // /=
    // AssignMultiply, // *=
    // AssignRem, // %=
    // AssignBitwiseXor, // ^=
    // AssignBitwiseAnd, // &=
    // AssignBitwiseOr, // |=
    // AssignBitwiseLeft, // <<=
    // AssignBitwiseRight, // >>=
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

#[derive(Debug)]
pub struct Parameter {
    name: IdentifierType,
    value_type: Type
}

impl Parameter {
    pub fn new(name: IdentifierType, value_type: Type) -> Self {
        Parameter {
            name,
            value_type
        }
    }

    pub fn get_name(&self) -> &IdentifierType {
        &self.name
    }

    pub fn get_type(&self) -> &Type {
        &self.value_type
    }

    pub fn consume(self) -> (IdentifierType, Type) {
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
            Token::OperatorModulo => Rem,

            Token::OperatorBitwiseXor => BitwiseXor,
            Token::OperatorBitwiseAnd => BitwiseAnd,
            Token::OperatorBitwiseOr => BitwiseOr,
            Token::OperatorBitwiseLeft => BitwiseLeft,
            Token::OperatorBitwiseRight => BitwiseRight,

            Token::OperatorAssign => Assign(None),
            Token::OperatorPlusAssign => Assign(Some(Box::new(Plus))),
            Token::OperatorMinusAssign => Assign(Some(Box::new(Minus))),
            Token::OperatorDivideAssign => Assign(Some(Box::new(Divide))),
            Token::OperatorMultiplyAssign => Assign(Some(Box::new(Multiply))),
            Token::OperatorModuloAssign => Assign(Some(Box::new(Rem))),
            Token::OperatorBitwiseXorAssign => Assign(Some(Box::new(BitwiseXor))),
            Token::OperatorBitwiseAndAssign => Assign(Some(Box::new(BitwiseAnd))),
            Token::OperatorBitwiseOrAssign => Assign(Some(Box::new(BitwiseOr))),
            Token::OperatorBitwiseLeftAssign => Assign(Some(Box::new(BitwiseLeft))),
            Token::OperatorBitwiseRightAssign => Assign(Some(Box::new(BitwiseRight))),

            _ => return None,
        };
        Some(value)
    }

    pub fn is_assignation(&self) -> bool {
        match &self {
            Operator::Assign(_) => true,
            _ => false
        }
    }

    pub fn is_number_operator(&self) -> bool {
        match &self {
            | Operator::Minus
            | Operator::Divide
            | Operator::Multiply
            | Operator::Rem
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

    pub fn is_bool_operator(&self) -> bool {
        match &self {
            Operator::Equals
            | Operator::NotEquals
            | Operator::And
            | Operator::Or
            | Operator::GreaterThan
            | Operator::LessThan
            | Operator::GreaterOrEqual
            | Operator::LessOrEqual => true,
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