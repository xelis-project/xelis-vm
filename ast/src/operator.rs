use super::Token;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
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
    Pow, // **

    BitwiseXor, // ^
    BitwiseAnd, // &
    BitwiseOr, // |
    BitwiseLeft, // <<
    BitwiseRight, // >>

    Assign(Option<Box<Operator>>),
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