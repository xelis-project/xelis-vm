#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    // Variable / function names
    Identifier(String),
    // Values
    IntValue(u64),
    LongValue(u128),
    StringValue(String),
    Null,
    True,
    False,

    // Types supported
    Byte,
    Short,
    Int,
    Long,
    Boolean,
    String,

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

    OperatorPlusAssign,
    OperatorMinusAssign,
    OperatorMultiplyAssign,
    OperatorDivideAssign,
    OperatorBitwiseLeft,
    OperatorBitwiseRight,

    OperatorTernary,

    Import,
    As
}

impl Token {
    pub fn value_of(s: &str) -> Option<Token> {
        use Token::*;
        Some(match s {
            "{" => BraceOpen,
            "}" => BraceClose,
            "[" => BracketOpen,
            "]" => BracketClose,

            "(" => ParenthesisOpen,
            ")" => ParenthesisClose,
            "struct" => Struct,

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

            "+=" => OperatorPlusAssign,
            "-=" => OperatorMinusAssign,
            "*=" => OperatorMultiplyAssign,
            "/=" => OperatorDivideAssign,

            "<<" => OperatorBitwiseLeft,
            ">>" => OperatorBitwiseRight,

            "?" => OperatorTernary,

            "." => Dot,
            "," => Comma,
            ":" => Colon,

            "byte" => Byte,
            "short" => Short,
            "int" => Int,
            "long" => Long,
            "bool" => Boolean,
            "string" => String,
            
            "let" => Let,

            "const" => Const,
            "entry" => Entry,
            "func" => Function,

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

            "null" => Null,
            "true" => True,
            "false" => False,

            "import" => Import,
            "as" => As,

            _ => return None,
        })
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
            | OperatorPlusAssign
            | OperatorMinusAssign
            | OperatorMultiplyAssign
            | OperatorDivideAssign
            | OperatorBitwiseLeft
            | OperatorBitwiseRight
            | OperatorAssign
            | BracketOpen
            | Dot
            | OperatorTernary
            | As => true,
            _ => false,
        }
    }
}