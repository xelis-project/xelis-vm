use xelis_ast::{Literal, NumberType, Token};
use xelis_types::U256;
use std::{borrow::Cow, collections::VecDeque};

macro_rules! parse_number {
    ($self: expr, $t: ident, $l: ident, $s: expr, $radix: expr) => {
        match $t::from_str_radix($s, $radix) {
            Ok(value) => Token::Value(Literal::$l(value)),
            Err(_) => return Err(LexerError::ParseToNumber($self.line, $self.column))
        }
    };
}

#[derive(Debug)]
pub enum LexerError { // left is line, right is column
    EndOfFile,
    ParseToNumber(usize, usize),
    NoTokenFound(usize, usize),
    ExpectedChar(usize, usize),
    ExpectedType
}

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

pub struct Lexer<'a> {
    // input code
    input: &'a str,
    // characters in the input
    // used to build tokens
    chars: VecDeque<char>,
    // current position index in the input
    // this is used to get slices from it
    pos: usize,
    // current line number we are reading
    line: usize,
    // current column number we are reading
    column: usize,
    // Used to keep track of the depth of the generics <...>
    generic_depth: usize,
    // Track if the last parsed token was an identifier
    accept_generic: bool
}

impl<'a> Lexer<'a> {
    // create a new lexer
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            chars: input.chars().collect::<Vec<_>>().into(),
            pos: 0,
            line: 1,
            column: 0,
            generic_depth: 0,
            accept_generic: false
        }
    }

    // peek the next character
    fn peek(&self) -> Result<char, LexerError> {
        self.chars.front()
            .copied()
            .ok_or_else(|| LexerError::ExpectedChar(self.line, self.column))
    }

    // advance by n characters
    fn advance_by(&mut self, n: usize) -> Result<(), LexerError> {
        let drain = self.chars.drain(0..n);

        if drain.len() != n {
            return Err(LexerError::EndOfFile);
        }

        self.pos += n;
        self.column += n;

        Ok(())
    }

    // consume the next character
    fn advance(&mut self) -> Result<char, LexerError> {
        self.next_char()
            .ok_or_else(|| LexerError::ExpectedChar(self.line, self.column))
    }

    // get the next character
    fn next_char(&mut self) -> Option<char> {
        self.chars.pop_front().map(|c| {
            self.pos += 1;
            self.column += 1;
            c
        })
    }

    // get a str slice of the input
    // this is done to prevent copying the string
    fn get_slice(&self, start: usize, end: usize) -> Result<&'a str, LexerError> {
        self.input.get(start..end).ok_or_else(|| LexerError::EndOfFile)
    }

    // push a character back to the list
    fn push_back(&mut self, c: char) {
        if c == '\n' {
            self.line -= 1;
            self.column = 1;
        } else {
            self.column -= 1;
        }
        self.pos -= 1;
        self.chars.push_front(c);
    }

    // try to parse a slice of string as a token using n+1 characters
    fn find_potential_token(&mut self) -> Option<(TokenResult<'a>, usize)> {
        let slice = self.input.get(self.pos - 1..)?;
        let alone_special_chars = &[':', '(', ')', '[', ']', '{', '}', ',', ';', '.', '?'];

        let mut is_alphanumeric = true;
        let mut is_special = true;
        let mut end_index = 0;
        for (i, s) in slice.chars().enumerate() {
            if alone_special_chars.contains(&s) {
                break;
            }

            if is_alphanumeric {
                is_alphanumeric = s.is_alphanumeric();
            }

            if is_special {
                if is_alphanumeric {
                    is_special = false;
                } else {
                    is_special = !s.is_alphanumeric();

                    // Check based on depth
                    if is_special {
                        if self.accept_generic && s == '<' {
                            self.generic_depth += 1;
                        } else if self.generic_depth != 0 && s == '>' {
                            self.generic_depth -= 1;
                            break;
                        }
                    }
                }
            }

            if s.is_whitespace() {
                break;
            }

            if !is_alphanumeric && !is_special {    
                break;
            }

            end_index = i;
            self.accept_generic = false;
        }

        let slice = slice.get(..=end_index)?;

        Token::value_of(slice).map(|t| (TokenResult {
            token: t,
            line: self.line,
            column_start: self.column,
            column_end: self.column + end_index
        }, end_index))
    }

    // read characters while the delimiter is true
    fn read_while<F>(&mut self, delimiter: F, diff: usize) -> Result<&'a str, LexerError>
    where 
        F: Fn(&char) -> bool,
    {
        let init_pos = self.pos - diff;
        while let Some(value) = self.next_char() {
            if !delimiter(&value) {
                // push the last character back
                self.push_back(value);
                break;
            }
        }

        self.get_slice(init_pos, self.pos)
    }

    // this will consume characters until the delimiter returns true
    fn skip_until<F>(&mut self, delimiter: F) -> Result<(), LexerError>
    where 
        F: Fn(&char) -> bool,
    {
        loop {
            let c = self.advance()?;
            if delimiter(&c) {
                break;
            }
        }

        Ok(())
    }

    // this will read the whole string until the end character
    // it supports escaped characters
    fn read_string(&mut self, end: char) -> Result<Cow<'a, str>, LexerError> {
        let mut escape = false;
        let mut init_pos = self.pos;
        let mut transformed_string: Option<String> = None;

        loop {
            let c = self.advance()?;
            if c == end && !escape {
                if let Some(value) = transformed_string.as_mut() {
                    value.push_str(self.get_slice(init_pos, self.pos - 1)?);
                }

                break;
            } else if c == '\\' && !escape {
                escape = true;

                let slice = self.get_slice(init_pos, self.pos - 1)?;
                if let Some(value) = transformed_string.as_mut() {
                    value.push_str(slice);
                } else {
                    transformed_string = Some(slice.to_owned());
                }
                // skip the escaped character
            } else {
                if escape {
                    init_pos = self.pos - 1;
                    escape = false;
                }
            }
        }

        Ok(match transformed_string {
            Some(value) => Cow::Owned(value),
            None => Cow::Borrowed(self.get_slice(init_pos, self.pos - 1)?)
        })
    }

    // Read a number
    // Support base 10 and base 16, also support u128 numbers
    fn read_number(&mut self, c: char) -> Result<TokenResult<'a>, LexerError> {

        // Default number type to use
        let mut number_type = None;
        let is_hex = c == '0' && self.peek()? == 'x';
        let column_start = self.column;

        let mut init_pos = if is_hex {
            // Skip the x
            self.advance()?;
            self.pos
        } else {
            self.pos - 1
        };

        let mut offset = 0;
        let mut transformed_string: Option<String> = None;
        while let Some(v) = self.next_char() {
            // Skip the underscore
            if v == '_' {
                let slice = self.get_slice(init_pos, self.pos - 1)?;
                if let Some(value) = transformed_string.as_mut() {
                    value.push_str(slice);
                } else {
                    transformed_string = Some(slice.to_owned());
                }
                init_pos = self.pos;
            } else if !(v.is_digit(10) || v.is_digit(16)) {
                if v.is_alphabetic() {
                    let s = self.read_while(|c| c.is_ascii_alphanumeric(), 1)?;
                    let Some(t) = NumberType::value_of(&s) else {
                        return Err(LexerError::ExpectedType);
                    };

                    number_type = Some(t);
                    offset = s.len();
                } else {
                    // push the character back
                    self.push_back(v);
                }

                break;
            }
        }

        let v = match transformed_string.as_mut() {
            Some(value) => {
                value.push_str(self.get_slice(init_pos, self.pos - offset)?);
                value
            },
            None => self.get_slice(init_pos, self.pos - offset)?
        };

        let radix = if is_hex { 16 } else { 10 };
        let token = match number_type {
            Some(t) => match t {
                NumberType::U8 => parse_number!(self, u8, U8, v, radix),
                NumberType::U16 => parse_number!(self, u16, U16, v, radix),
                NumberType::U32 => parse_number!(self, u32, U32, v, radix),
                NumberType::U64 => parse_number!(self, u64, U64, v, radix),
                NumberType::U128 => parse_number!(self, u128, U128, v, radix),
                NumberType::U256 => parse_number!(self, U256, U256, v, radix),
            }
            None => parse_number!(self, u64, Number, v, radix),
        };
        Ok(TokenResult {
            token,
            line: self.line,
            column_start,
            column_end: self.column
        })
    }

    // read a multi-line comment
    // expected format is /* ... */
    fn skip_multi_line_comment(&mut self) -> Result<(), LexerError> {
        loop {
            let c = self.advance()?;
            if c == '*' && self.peek()? == '/' {
                self.advance()?;
                break;
            }
        }

        Ok(())
    }

    // read a token
    // it also supports optional types
    fn read_token(&mut self, diff: usize) -> Result<TokenResult<'a>, LexerError> {
        let column_start = self.column;
        let value = self.read_while(|v| -> bool {
            *v == '_' || v.is_ascii_alphanumeric()
        }, diff)?;

        let token = Token::value_of(value)
            .unwrap_or_else(|| Token::Identifier(value));

        Ok(TokenResult {
            token,
            line: self.line,
            column_start,
            column_end: self.column
        })
    }

    // retrieve the next token available
    fn next_token(&mut self) -> Result<Option<TokenResult<'a>>, LexerError> {
        while let Some(c) = self.next_char() {
            let token: TokenResult<'a> = match c {
                '\n' | '\r' | '\t' => {
                    self.line += 1;
                    self.column = 0;
                    self.accept_generic = false;
                    continue;
                },
                // skipped characters
                ' ' | ';' => {
                    // we just skip these characters
                    self.accept_generic = false;
                    continue;
                },
                // read a string value
                // It supports escaped characters
                '"' | '\'' => {
                    let column_start = self.column;
                    let value = self.read_string(c)?;
                    TokenResult {
                        token: Token::Value(Literal::String(value)),
                        line: self.line,
                        column_start,
                        column_end: self.column
                    }
                },
                // it's only a comment, skip until its end
                '/' if {
                    let v = self.peek()?;
                    v == '/' || v == '*'
                } => {
                    let v = self.advance()?;
                    if v == '/' {
                        self.skip_until(|c| *c == '\n')?;
                    } else {
                        self.skip_multi_line_comment()?;
                    }
                    continue;
                },
                // read a number value
                c if c.is_digit(10) => self.read_number(c)?,
                c if c == '_' || c.is_alphabetic() => self.read_token(1)?,
                _ => {
                    if let Some((token, diff)) = self.find_potential_token() {
                        self.advance_by(diff)?;
                        token
                    } else {
                        return Err(LexerError::NoTokenFound(self.line, self.column));
                    }
                }
            };

            self.accept_generic = token.token.accept_generic();
            return Ok(Some(token));
        }

        Ok(None)
    }

    // Parse the code into a list of tokens
    // This returns only the list of tokens without any other information
    pub fn get(mut self) -> Result<VecDeque<Token<'a>>, LexerError> {
        let mut tokens = VecDeque::new();
        while let Some(token) = self.next_token()? {
            // push the token to the list
            tokens.push_back(token.token);
        }

        Ok(tokens)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<TokenResult<'a>, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().transpose()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_operators() {
        let code = "+ - * / % ^ | & << >> == != > < >= <=";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::OperatorPlus,
            Token::OperatorMinus,
            Token::OperatorMultiply,
            Token::OperatorDivide,
            Token::OperatorModulo,
            Token::OperatorBitwiseXor,
            Token::OperatorBitwiseOr,
            Token::OperatorBitwiseAnd,
            Token::OperatorBitwiseLeft,
            Token::OperatorBitwiseRight,
            Token::OperatorEquals,
            Token::OperatorNotEquals,
            Token::OperatorGreaterThan,
            Token::OperatorLessThan,
            Token::OperatorGreaterOrEqual,
            Token::OperatorLessOrEqual
        ]);
    }

    #[test]
    fn test_assign_operator_without_whitespace() {
        let code = "a>>=10+7*3";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Identifier("a"),
            Token::OperatorBitwiseRightAssign,
            Token::Value(Literal::Number(10)),
            Token::OperatorPlus,
            Token::Value(Literal::Number(7)),
            Token::OperatorMultiply,
            Token::Value(Literal::Number(3))
        ]);
    }

    #[test]
    fn test_assign_operators() {
        let code = "= += -= *= /= %= ^= |= &= <<= >>=";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::OperatorAssign,
            Token::OperatorPlusAssign,
            Token::OperatorMinusAssign,
            Token::OperatorMultiplyAssign,
            Token::OperatorDivideAssign,
            Token::OperatorModuloAssign,
            Token::OperatorBitwiseXorAssign,
            Token::OperatorBitwiseOrAssign,
            Token::OperatorBitwiseAndAssign,
            Token::OperatorBitwiseLeftAssign,
            Token::OperatorBitwiseRightAssign
        ]);
    }

    #[test]
    fn test_assign() {
        let code = "let a = 10;";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("a"),
            Token::OperatorAssign,
            Token::Value(Literal::Number(10))
        ]);
    }

    #[test]
    fn test_function() {
        let code = "fn main() { return 10; }";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Function,
            Token::Identifier("main"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::Return,
            Token::Value(Literal::Number(10)),
            Token::BraceClose
        ]);
    }

    #[test]
    fn test_function_type() {
        let code = "fn main() -> u64 { return 10; }";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Function,
            Token::Identifier("main"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::ReturnType,
            Token::Number(NumberType::U64),
            Token::BraceOpen,
            Token::Return,
            Token::Value(Literal::Number(10)),
            Token::BraceClose
        ]);
    }

    #[test]
    fn test_u128_with_underscore() {
        let code = "10_000u128";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Value(Literal::U128(10_000))
        ]);
    }

    #[test]
    fn test_string() {
        let code = "\"Hello, World!\"";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Value(Literal::String(Cow::Borrowed("Hello, World!")))
        ]);
    }

    #[test]
    fn test_inner_string() {
        let code = "\"'Hello, World!'\"";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Value(Literal::String(Cow::Borrowed("'Hello, World!'")))
        ]);
    }

    #[test]
    fn test_inner_escaped_string() {
        let code = "'Hello, \\'World!'";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Value(Literal::String(Cow::Borrowed("Hello, 'World!")))
        ]);
    }

    #[test]
    fn test_comment() {
        let code = "// This is a comment\nlet a = 10;";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("a"),
            Token::OperatorAssign,
            Token::Value(Literal::Number(10))
        ]);
    }

    #[test]
    fn test_multi_line_comment() {
        let code = "/* This is a comment\n * with multiple lines */\nlet a = 10;";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("a"),
            Token::OperatorAssign,
            Token::Value(Literal::Number(10))
        ]);
    }

    #[test]
    fn test_assignation() {
        let code = "let a = 10; a += 20;";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("a"),
            Token::OperatorAssign,
            Token::Value(Literal::Number(10)),
            Token::Identifier("a"),
            Token::OperatorPlusAssign,
            Token::Value(Literal::Number(20))
        ]);
    }

    #[test]
    fn test_number() {
        let code = "10";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Value(Literal::Number(10))
        ]);
    }

    #[test]
    fn test_number_with_underscore() {
        let code = "10_000";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Value(Literal::Number(10_000))
        ]);
    }

    #[test]
    fn test_number_with_hex() {
        let code = "0x10";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Value(Literal::Number(16))
        ]);
    }

    #[test]
    fn test_number_u128() {
        let code = "10u128";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Value(Literal::U128(10))
        ]);
    }

    #[test]
    fn test_number_u128_with_hex() {
        let code = "0x10u128";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Value(Literal::U128(16))
        ]);
    }

    #[test]
    fn test_function_with_args() {
        let code = "fn sum(a: u64, b: u64) -> u64 { return a + b; }";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Function,
            Token::Identifier("sum"),
            Token::ParenthesisOpen,
            Token::Identifier("a"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::Comma,
            Token::Identifier("b"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::ParenthesisClose,
            Token::ReturnType,
            Token::Number(NumberType::U64),
            Token::BraceOpen,
            Token::Return,
            Token::Identifier("a"),
            Token::OperatorPlus,
            Token::Identifier("b"),
            Token::BraceClose
        ]);
    }

    #[test]
    fn test_function_call() {
        let code = "sum(10, 20)";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Identifier("sum"),
            Token::ParenthesisOpen,
            Token::Value(Literal::Number(10)),
            Token::Comma,
            Token::Value(Literal::Number(20)),
            Token::ParenthesisClose
        ]);
    }

    #[test]
    fn test_optional() {
        let code = "optional<u64>";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Optional,
            Token::OperatorLessThan,
            Token::Number(NumberType::U64),
            Token::OperatorGreaterThan,
        ]);
    }

    #[test]
    fn test_optional_2() {
        let code = "optional<optional<u64>>";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Optional,
            Token::OperatorLessThan,
            Token::Optional,
            Token::OperatorLessThan,
            Token::Number(NumberType::U64),
            Token::OperatorGreaterThan,
            Token::OperatorGreaterThan,
        ]);
    }

    #[test]
    fn test_optional_3() {
        let code = "optional<optional<optional<u64>>>";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Optional,
            Token::OperatorLessThan,
            Token::Optional,
            Token::OperatorLessThan,
            Token::Optional,
            Token::OperatorLessThan,
            Token::Number(NumberType::U64),
            Token::OperatorGreaterThan,
            Token::OperatorGreaterThan,
            Token::OperatorGreaterThan,
        ]);
    }

    #[test]
    fn test_bitwise_right() {
        let code = "a >> b";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Identifier("a"),
            Token::OperatorBitwiseRight,
            Token::Identifier("b")
        ]);
    }

    #[test]
    fn test_bitwise_left() {
        let code = "a << b";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Identifier("a"),
            Token::OperatorBitwiseLeft,
            Token::Identifier("b")
        ]);
    }

    #[test]
    fn test_bitwise_both() {
        let code = "a << b >> c";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Identifier("a"),
            Token::OperatorBitwiseLeft,
            Token::Identifier("b"),
            Token::OperatorBitwiseRight,
            Token::Identifier("c")
        ]);
    }

    #[test]
    fn test_range() {
        let code = "range<u64>";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Range,
            Token::OperatorLessThan,
            Token::Number(NumberType::U64),
            Token::OperatorGreaterThan,
        ]);
    }

    #[test]
    fn test_identifier_cmp() {
        let code = "a<b";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Identifier("a"),
            Token::OperatorLessThan,
            Token::Identifier("b")
        ]);
    }

    #[test]
    fn test_condition() {
        let code = "if a == 10 { return 10; } else { return 20; }";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::If,
            Token::Identifier("a"),
            Token::OperatorEquals,
            Token::Value(Literal::Number(10)),
            Token::BraceOpen,
            Token::Return,
            Token::Value(Literal::Number(10)),
            Token::BraceClose,
            Token::Else,
            Token::BraceOpen,
            Token::Return,
            Token::Value(Literal::Number(20)),
            Token::BraceClose
        ]);
    }

    #[test]
    fn test_struct() {
        let code = "struct User { name: string, age: u64 }";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Struct,
            Token::Identifier("User"),
            Token::BraceOpen,
            Token::Identifier("name"),
            Token::Colon,
            Token::String,
            Token::Comma,
            Token::Identifier("age"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::BraceClose
        ]);
    }

    #[test]
    fn test_cast() {
        let code = "let a = 10 as u8;";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("a"),
            Token::OperatorAssign,
            Token::Value(Literal::Number(10)),
            Token::As,
            Token::Number(NumberType::U8)
        ]);
    }

    #[test]
    fn test_import() {
        let code = "from \"file\" import TestStruct;";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::From,
            Token::Value(Literal::String(Cow::Borrowed("file"))),
            Token::Import,
            Token::Identifier("TestStruct"),
        ]);
    }

    #[test]
    fn test_import_as() {
        let code = "from \"file\" import TestStruct as Test;";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::From,
            Token::Value(Literal::String(Cow::Borrowed("file"))),
            Token::Import,
            Token::Identifier("TestStruct"),
            Token::As,
            Token::Identifier("Test")
        ]);
    }

    #[test]
    fn test_token_result() {
        let code = "let a = 10;";
        let mut lexer = Lexer::new(code);
        let token = lexer.next().unwrap().unwrap();
        assert_eq!(token.token, Token::Let);
        assert_eq!(token.line, 1);
        assert_eq!(token.column_start, 1);
        assert_eq!(token.column_end, 3);

        let token = lexer.next().unwrap().unwrap();
        assert_eq!(token.token, Token::Identifier("a"));
        assert_eq!(token.line, 1);
        assert_eq!(token.column_start, 5);
        assert_eq!(token.column_end, 5);

        let token = lexer.next().unwrap().unwrap();
        assert_eq!(token.token, Token::OperatorAssign);
        assert_eq!(token.line, 1);
        assert_eq!(token.column_start, 7);
        assert_eq!(token.column_end, 7);

        let token = lexer.next().unwrap().unwrap();
        assert_eq!(token.token, Token::Value(Literal::Number(10)));
        assert_eq!(token.line, 1);
        assert_eq!(token.column_start, 9);
        assert_eq!(token.column_end, 10);
    }

    // test access to u64::MAX constant
    #[test]
    fn test_type_const() {
        let code = "u64::MAX";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Number(NumberType::U64),
            Token::Colon,
            Token::Colon,
            Token::Identifier("MAX")
        ]);
    }
}