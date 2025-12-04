use std::borrow::Cow;
use xelis_types::U256;
use std::fmt;

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
    Bytes(Cow<'a, [u8]>),
    Bool(bool),
    Null,
}

impl<'a> fmt::Display for Token<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
      use Token::*;

      let output = match self {
        // Values and types
        Value(Literal::Null) => "null",
        Value(Literal::Bool(true)) => "true",
        Value(Literal::Bool(false)) => "false",
        Value(Literal::String(s)) => return write!(f, "\"{}\"", s),
        Value(Literal::Bytes(b)) => {
            write!(f, "b\"")?;
            for byte in b.iter() {
                if byte.is_ascii_graphic() && *byte != b'"' && *byte != b'\\' {
                    write!(f, "{}", *byte as char)?;
                } else {
                    write!(f, "\\x{:02x}", byte)?;
                }
            }
            return write!(f, "\"");
        },
        Value(Literal::Number(n)) => return write!(f, "{}", n),
        Value(Literal::U8(n)) => return write!(f, "{}_u8", n),
        Value(Literal::U16(n)) => return write!(f, "{}_u16", n),
        Value(Literal::U32(n)) => return write!(f, "{}_u32", n),
        Value(Literal::U64(n)) => return write!(f, "{}_u64", n),
        Value(Literal::U128(n)) => return write!(f, "{}_u128", n),
        Value(Literal::U256(n)) => return write!(f, "{}_u256", n),
        
        Identifier(id) => return write!(f, "{}", id),
        Number(t) => return write!(f, "{:?}", t),

        // Types
        token => return write!(f, "{}", token.as_str().expect("Token has no string representation")),
      };

      write!(f, "{}", output)
  }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token<'a> {
    // Variable / function names
    Identifier(&'a str),
    Value(Literal<'a>),

    // Types supported
    Number(NumberType),
    Bool,
    Bytes,
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
    Closure,
    Hook,
    Dot,
    Comma,
    Colon,
    SemiColon,
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
    OperatorPow,
    OperatorBitwiseXor,
    OperatorBitwiseOr,
    OperatorBitwiseAnd,
    OperatorBitwiseShl,
    OperatorBitwiseShr,

    OperatorPlusAssign,
    OperatorMinusAssign,
    OperatorMultiplyAssign,
    OperatorDivideAssign,
    OperatorModuloAssign,
    OperatorPowAssign,

    OperatorBitwiseXorAssign,
    OperatorBitwiseOrAssign,
    OperatorBitwiseAndAssign,
    OperatorBitwiseShlAssign,
    OperatorBitwiseShrAssign,

    OperatorTernary,

    Import,
    From,
    As,
    ReturnType,
    Match,
    FatArrow,
    Pub,
    Any,
}

impl<'a> Token<'a> {
    pub fn value_of(s: &'a str) -> Option<Token<'a>> {
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
            "**" => OperatorPow,
            "^" => OperatorBitwiseXor,
            "&" => OperatorBitwiseAnd,
            "|" => OperatorBitwiseOr,
            "<<" => OperatorBitwiseShl,
            ">>" => OperatorBitwiseShr,

            "+=" => OperatorPlusAssign,
            "-=" => OperatorMinusAssign,
            "*=" => OperatorMultiplyAssign,
            "/=" => OperatorDivideAssign,
            "%=" => OperatorModuloAssign,
            "**=" => OperatorPowAssign,

            "^=" => OperatorBitwiseXorAssign,
            "<<=" => OperatorBitwiseShlAssign,
            ">>=" => OperatorBitwiseShrAssign,
            "&=" => OperatorBitwiseAndAssign,
            "|=" => OperatorBitwiseOrAssign,

            "?" => OperatorTernary,

            "." => Dot,
            "," => Comma,
            ":" => Colon,
            ";" => SemiColon,

            "bool" => Bool,
            "bytes" => Bytes,
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
            "closure" => Closure,
            "hook" => Hook,

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
            "pub" => Pub,
            "any" => Any,

            e => Number(NumberType::value_of(e)?),
        })
    }

    pub fn as_str(&self) -> Option<&'a str> {
        use Token::*;

        Some(match self {
            BraceOpen => "{",
            BraceClose => "}",
            BracketOpen => "[",
            BracketClose => "]",
            ParenthesisOpen => "(",
            ParenthesisClose => ")",

            OperatorAssign => "=",
            OperatorEquals => "==",
            OperatorNotEquals => "!=",
            OperatorAnd => "&&",
            OperatorOr => "||",
            OperatorGreaterThan => ">",
            OperatorLessThan => "<",
            OperatorGreaterOrEqual => ">=",
            OperatorLessOrEqual => "<=",

            OperatorPlus => "+",
            OperatorMinus => "-",
            OperatorMultiply => "*",
            OperatorDivide => "/",
            OperatorModulo => "%",
            OperatorPow => "**",
            OperatorBitwiseXor => "^",
            OperatorBitwiseAnd => "&",
            OperatorBitwiseOr => "|",
            OperatorBitwiseShl => "<<",
            OperatorBitwiseShr => ">>",

            OperatorPlusAssign => "+=",
            OperatorMinusAssign => "-=",
            OperatorMultiplyAssign => "*=",
            OperatorDivideAssign => "/=",
            OperatorModuloAssign => "%=",
            OperatorPowAssign => "**=",
            OperatorBitwiseXorAssign => "^=",
            OperatorBitwiseAndAssign => "&=",
            OperatorBitwiseOrAssign => "|=",
            OperatorBitwiseShlAssign => "<<=",
            OperatorBitwiseShrAssign => ">>=",

            OperatorTernary => "?",
            Dot => ".",
            Comma => ",",
            Colon => ":",
            SemiColon => ";",

            // Keywords
            Let => "let",
            Const => "const",
            Entry => "entry",
            Function => "fn",
            Closure => "closure",
            Hook => "hook",
            Return => "return",
            If => "if",
            Else => "else",
            For => "for",
            ForEach => "foreach",
            While => "while",
            Break => "break",
            Continue => "continue",
            In => "in",
            IsNot => "!",

            Import => "import",
            From => "from",
            As => "as",
            ReturnType => "->",
            Match => "match",
            FatArrow => "=>",
            Pub => "pub",
            Any => "any",

            Bool => "bool",
            Bytes => "bytes",
            String => "string",
            Optional => "optional",
            Range => "range",
            Map => "map",
            Enum => "enum",
            Struct => "struct",
            Identifier(id) => id,

            _ => return None,
        })
    }

    pub fn as_identifier(&self) -> Option<&'a str> {
        match self {
            Token::Identifier(_)
            | Token::Map
            | Token::Optional
            | Token::Range
            | Token::Bytes
            | Token::Bool
            | Token::String
            | Token::From
            | Token::Entry => self.as_str(),

            _ => None,
        }
    }

    pub fn accept_generic(&self) -> bool {
        use Token::*;
        matches!(self, Identifier(_) | Optional | Range | Map | Struct)
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
            | OperatorPow

            | OperatorBitwiseXor
            | OperatorBitwiseOr
            | OperatorBitwiseAnd
            | OperatorBitwiseShl
            | OperatorBitwiseShr

            | OperatorPlusAssign
            | OperatorMinusAssign
            | OperatorMultiplyAssign
            | OperatorDivideAssign
            | OperatorModuloAssign
            | OperatorPowAssign

            | OperatorBitwiseXorAssign
            | OperatorBitwiseOrAssign
            | OperatorBitwiseAndAssign
            | OperatorBitwiseShlAssign
            | OperatorBitwiseShrAssign
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
            | Map
            | Enum
            | Bytes
            | Struct => true,
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