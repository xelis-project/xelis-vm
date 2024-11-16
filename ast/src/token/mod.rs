use std::borrow::Cow;
use xelis_types::U256;


#[derive(Debug, Clone)]
pub struct TokenResult<'a> {
    // the token value
    pub token: Token<'a>,
    // the line number where the token is located
    pub line: usize,
    // the column number where the token starts
    pub column_start: usize,
    // the column number where the token ends
    pub column_end: usize
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum NumberType {
    U8,
    U16,
    U32,
    U64,
    U128,
    U256
}

impl NumberType {
    pub fn value_of(s: &str) -> Option<NumberType> {
        Some(match s {
            "u8" => Self::U8,
            "u16" => Self::U16,
            "u32" => Self::U32,
            "u64" => Self::U64,
            "u128" => Self::U128,
            "u256" => Self::U256,
            _ => return None,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Literal<'a> {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    U256(U256),
    // Default number type when no type is specified
    Number(u64),
    String(Cow<'a, str>),
    Bool(bool),
    Null,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token<'a> {
    // Variable / function names
    Identifier(&'a str),
    Value(Literal<'a>),

    // Types supported
    Number(NumberType),
    Bool,
    String,
    Optional,
    Range,
    Map,
    Enum,

    BraceOpen,
    BraceClose,
    BracketOpen,
    BracketClose,

    Const,
    Let,
    Entry,
    Function,
    Dot,
    Comma,
    Colon,
    Return,
    If,
    Else,
    For,
    ForEach,
    While,
    Break,
    Continue,
    In,
    IsNot, // !

    ParenthesisOpen,
    ParenthesisClose,
    Struct,

    OperatorAssign,
    OperatorEquals,
    OperatorNotEquals,
    OperatorAnd,
    OperatorOr,
    OperatorGreaterThan,
    OperatorLessThan,
    OperatorGreaterOrEqual,
    OperatorLessOrEqual,

    OperatorPlus,
    OperatorMinus,
    OperatorMultiply,
    OperatorDivide,
    OperatorModulo,
    OperatorBitwiseXor,
    OperatorBitwiseOr,
    OperatorBitwiseAnd,
    OperatorBitwiseLeft,
    OperatorBitwiseRight,

    OperatorPlusAssign,
    OperatorMinusAssign,
    OperatorMultiplyAssign,
    OperatorDivideAssign,
    OperatorModuloAssign,

    OperatorBitwiseXorAssign,
    OperatorBitwiseOrAssign,
    OperatorBitwiseAndAssign,
    OperatorBitwiseLeftAssign,
    OperatorBitwiseRightAssign,

    OperatorTernary,

    Import,
    From,
    As,
    ReturnType,
    Match,
    FatArrow,
}

impl Token<'_> {
    pub fn value_of(s: &str) -> Option<Token> {
        use Token::*;

        Some(match s {
            "{" => BraceOpen,
            "}" => BraceClose,
            "[" => BracketOpen,
            "]" => BracketClose,

            "(" => ParenthesisOpen,
            ")" => ParenthesisClose,

            "=" => OperatorAssign,
            "==" => OperatorEquals,
            "!=" => OperatorNotEquals,
            "&&" => OperatorAnd,
            "||" => OperatorOr,
            ">" => OperatorGreaterThan,
            "<" => OperatorLessThan,
            ">=" => OperatorGreaterOrEqual,
            "<=" => OperatorLessOrEqual,

            "+" => OperatorPlus,
            "-" => OperatorMinus,
            "*" => OperatorMultiply,
            "/" => OperatorDivide,
            "%" => OperatorModulo,
            "^" => OperatorBitwiseXor,
            "&" => OperatorBitwiseAnd,
            "|" => OperatorBitwiseOr,
            "<<" => OperatorBitwiseLeft,
            ">>" => OperatorBitwiseRight,

            "+=" => OperatorPlusAssign,
            "-=" => OperatorMinusAssign,
            "*=" => OperatorMultiplyAssign,
            "/=" => OperatorDivideAssign,
            "%=" => OperatorModuloAssign,

            "^=" => OperatorBitwiseXorAssign,
            "<<=" => OperatorBitwiseLeftAssign,
            ">>=" => OperatorBitwiseRightAssign,
            "&=" => OperatorBitwiseAndAssign,
            "|=" => OperatorBitwiseOrAssign,

            "?" => OperatorTernary,

            "." => Dot,
            "," => Comma,
            ":" => Colon,

            "bool" => Bool,
            "string" => String,
            "struct" => Struct,
            "optional" => Optional,
            "range" => Range,
            "map" => Map,
            "enum" => Enum,

            "let" => Let,

            "const" => Const,
            "entry" => Entry,
            "fn" => Function,

            "return" => Return,
            "if" => If,
            "else" => Else,
            "for" => For,
            "foreach" => ForEach,
            "while" => While,
            "break" => Break,
            "continue" => Continue,
            "in" => In,
            "!" => IsNot,

            "null" => Value(Literal::Null),
            "true" => Value(Literal::Bool(true)),
            "false" => Value(Literal::Bool(false)),

            "import" => Import,
            "from" => From,
            "as" => As,
            "->" => ReturnType,
            "match" => Match,
            "=>" => FatArrow,

            e => Number(NumberType::value_of(e)?),
        })
    }

    pub fn accept_generic(&self) -> bool {
        use Token::*;
        matches!(self, Identifier(_) | Optional | Range | Map)
    }

    pub fn should_stop(&self) -> bool {
        use Token::*;
        match self {
            ParenthesisClose | BraceOpen | BraceClose | BracketClose => true,
            _ => false
        }
    }
    pub fn is_operator(&self) -> bool {
        use Token::*;
        match self {
            | OperatorEquals
            | OperatorNotEquals
            | OperatorAnd
            | OperatorOr
            | OperatorGreaterThan
            | OperatorLessThan
            | OperatorGreaterOrEqual
            | OperatorLessOrEqual
            | OperatorPlus
            | OperatorMinus
            | OperatorMultiply
            | OperatorDivide
            | OperatorModulo
            | OperatorBitwiseXor
            | OperatorBitwiseOr
            | OperatorBitwiseLeft
            | OperatorBitwiseRight
            | OperatorBitwiseAnd
            | OperatorPlusAssign
            | OperatorMinusAssign
            | OperatorMultiplyAssign
            | OperatorDivideAssign
            | OperatorModuloAssign
            | OperatorBitwiseXorAssign
            | OperatorBitwiseOrAssign
            | OperatorBitwiseAndAssign
            | OperatorBitwiseLeftAssign
            | OperatorBitwiseRightAssign
            | OperatorAssign
            | BracketOpen
            | Dot
            | OperatorTernary
            | As => true,
            _ => false,
        }
    }

    pub fn is_type(&self) -> bool {
        use Token::*;
        match self {
            | Number(_)
            | Bool
            | String
            | Identifier(_)
            | Optional
            | Range
            | Map => true,
            _ => false,
        }
    }

    pub fn is_number_type(&self) -> bool {
        use Token::*;
        match self {
            | Number(_) => true,
            _ => false,
        }
    }
}