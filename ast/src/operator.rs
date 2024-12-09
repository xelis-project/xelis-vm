use super::Token;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Operator {
    Eq, // ==
    Neq, // !=
    And, // &&
    Or, // ||
    Gt, // >
    Lt, // <
    Gte, // >=
    Lte, // <=

    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %
    Pow, // **

    BitwiseXor, // ^
    BitwiseAnd, // &
    BitwiseOr, // |
    BitwiseShl, // <<
    BitwiseShr, // >>

    Assign(Option<Box<Operator>>),
}

impl Operator {
    pub fn value_of(token: &Token) -> Option<Operator> {
        use Operator::*;
        let value = match token {
            Token::OperatorEquals => Eq,
            Token::OperatorNotEquals => Neq,
            Token::OperatorAnd => And,
            Token::OperatorOr => Or,
            Token::OperatorGreaterThan => Gt,
            Token::OperatorLessThan => Lt,
            Token::OperatorGreaterOrEqual => Gte,
            Token::OperatorLessOrEqual => Lte,
            Token::OperatorPlus => Add,
            Token::OperatorMinus => Sub,
            Token::OperatorMultiply => Mul,
            Token::OperatorDivide => Div,
            Token::OperatorModulo => Mod,
            Token::OperatorPow => Pow,

            Token::OperatorBitwiseXor => BitwiseXor,
            Token::OperatorBitwiseAnd => BitwiseAnd,
            Token::OperatorBitwiseOr => BitwiseOr,
            Token::OperatorBitwiseShl => BitwiseShl,
            Token::OperatorBitwiseShr => BitwiseShr,

            Token::OperatorAssign => Assign(None),
            Token::OperatorPlusAssign => Assign(Some(Box::new(Add))),
            Token::OperatorMinusAssign => Assign(Some(Box::new(Sub))),
            Token::OperatorDivideAssign => Assign(Some(Box::new(Div))),
            Token::OperatorMultiplyAssign => Assign(Some(Box::new(Mul))),
            Token::OperatorModuloAssign => Assign(Some(Box::new(Mod))),
            Token::OperatorPowAssign => Assign(Some(Box::new(Pow))),

            Token::OperatorBitwiseXorAssign => Assign(Some(Box::new(BitwiseXor))),
            Token::OperatorBitwiseAndAssign => Assign(Some(Box::new(BitwiseAnd))),
            Token::OperatorBitwiseOrAssign => Assign(Some(Box::new(BitwiseOr))),
            Token::OperatorBitwiseShlAssign => Assign(Some(Box::new(BitwiseShl))),
            Token::OperatorBitwiseShrAssign => Assign(Some(Box::new(BitwiseShr))),

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
            | Operator::Sub
            | Operator::Div
            | Operator::Mul
            | Operator::Mod
            | Operator::Pow

            | Operator::BitwiseXor
            | Operator::BitwiseAnd
            | Operator::BitwiseOr
            | Operator::BitwiseShl
            | Operator::BitwiseShr

            | Operator::Gte
            | Operator::Gt
            | Operator::Lte
            | Operator::Lt => true,
            _ => false
        }
    }

    pub fn is_bool_operator(&self) -> bool {
        match &self {
            Operator::Eq
            | Operator::Neq
            | Operator::And
            | Operator::Or
            | Operator::Gt
            | Operator::Lt
            | Operator::Gte
            | Operator::Lte => true,
            _ => false
        }
    }

    pub fn is_and_or_or(&self) -> bool {
        match &self {
            Operator::And | Operator::Or => true,
            _ => false
        }
    }

    pub fn is_left_associative(&self) -> bool {
        match &self {
            Operator::Eq
            | Operator::Neq
            | Operator::And
            | Operator::Or
            | Operator::Gt
            | Operator::Lt
            | Operator::Gte
            | Operator::Lte
            | Operator::Add
            | Operator::Sub
            | Operator::Mul
            | Operator::Div
            | Operator::Mod
            | Operator::BitwiseXor
            | Operator::BitwiseAnd
            | Operator::BitwiseOr
            | Operator::BitwiseShl
            | Operator::BitwiseShr => true,

            Operator::Assign(_)
            | Operator::Pow => false
        }
    }
    
    pub fn precedence(&self) -> u8 {
        match &self {
            Operator::Assign(_) => 1,
            
            Operator::Or => 2,
            
            Operator::And => 3,
            
            Operator::BitwiseOr => 4,
            
            Operator::BitwiseXor => 5,
            
            Operator::BitwiseAnd => 6,
            
            Operator::Eq
            | Operator::Neq => 7,
            
            Operator::Gt
            | Operator::Lt
            | Operator::Gte
            | Operator::Lte => 8,
            
            Operator::BitwiseShl
            | Operator::BitwiseShr => 9,
            
            Operator::Add
            | Operator::Sub => 10,
            
            Operator::Mul
            | Operator::Div
            | Operator::Mod => 11,

            Operator::Pow => 12,
        }
    }
}