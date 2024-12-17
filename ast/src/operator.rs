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

#[derive(Debug, PartialEq)]
pub enum Associativity {
    LeftToRight,
    RightToLeft,
    RequireParentheses,
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

    pub fn to_token(&self) -> Token<'static> {
        use Operator::*;
        match self {
            Eq => Token::OperatorEquals,
            Neq => Token::OperatorNotEquals,
            And => Token::OperatorAnd,
            Or => Token::OperatorOr,
            Gt => Token::OperatorGreaterThan,
            Lt => Token::OperatorLessThan,
            Gte => Token::OperatorGreaterOrEqual,
            Lte => Token::OperatorLessOrEqual,
            Add => Token::OperatorPlus,
            Sub => Token::OperatorMinus,
            Mul => Token::OperatorMultiply,
            Div => Token::OperatorDivide,
            Mod => Token::OperatorModulo,
            Pow => Token::OperatorPow,
            BitwiseXor => Token::OperatorBitwiseXor,
            BitwiseAnd => Token::OperatorBitwiseAnd,
            BitwiseOr => Token::OperatorBitwiseOr,
            BitwiseShl => Token::OperatorBitwiseShl,
            BitwiseShr => Token::OperatorBitwiseShr,

            Assign(None) => Token::OperatorAssign,
            Assign(Some(op)) => match **op {
                Add => Token::OperatorPlusAssign,
                Sub => Token::OperatorMinusAssign,
                Div => Token::OperatorDivideAssign,
                Mul => Token::OperatorMultiplyAssign,
                Mod => Token::OperatorModuloAssign,
                Pow => Token::OperatorPowAssign,
                BitwiseXor => Token::OperatorBitwiseXorAssign,
                BitwiseAnd => Token::OperatorBitwiseAndAssign,
                BitwiseOr => Token::OperatorBitwiseOrAssign,
                BitwiseShl => Token::OperatorBitwiseShlAssign,
                BitwiseShr => Token::OperatorBitwiseShrAssign,
                _ => unreachable!(), // Should not occur
            },
        }
    }

    pub fn precedence(&self) -> (u8, Associativity) {
        use Associativity::*;
        match self {
            // Unary operators
            Operator::Pow => (11, RightToLeft), // ** in Rust is right-associative
            Operator::Mul | Operator::Div | Operator::Mod => (10, LeftToRight),
            Operator::Add | Operator::Sub => (9, LeftToRight),
            Operator::BitwiseShl | Operator::BitwiseShr => (8, LeftToRight),
            Operator::BitwiseAnd => (7, LeftToRight),
            Operator::BitwiseXor => (6, LeftToRight),
            Operator::BitwiseOr => (5, LeftToRight),

            // Comparison operators (Require Parentheses in Rust)
            Operator::Eq | Operator::Neq | Operator::Gt | Operator::Lt | Operator::Gte | Operator::Lte => (4, RequireParentheses),

            // Logical operators
            Operator::And => (3, LeftToRight), // &&
            Operator::Or => (2, LeftToRight),  // ||

            // Assignment operators
            Operator::Assign(_) => (1, RightToLeft),
        }
    }

    /// Helper functions to check associativity
    pub fn is_left_to_right(&self) -> bool {
        matches!(self.precedence().1, Associativity::LeftToRight)
    }

    pub fn is_right_to_left(&self) -> bool {
        matches!(self.precedence().1, Associativity::RightToLeft)
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
}