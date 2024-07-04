use crate::token::Token;
use std::{borrow::Cow, collections::VecDeque};

#[derive(Debug)]
pub enum LexerError { // left is line, right is column
    EndOfFile,
    ParseToNumber(usize, usize),
    NoTokenFound(usize, usize),
    ExpectedChar(usize, usize)
}

pub struct Lexer<'a> {
    input: &'a str,
    chars: VecDeque<char>,
    // Position
    pos: usize,
    line: usize,
    column: usize
}

impl<'a> Lexer<'a> {
    // create a new lexer
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            chars: input.chars().collect::<Vec<_>>().into(),
            pos: 0,
            line: 1,
            column: 1
        }
    }

    // peek the next character
    fn peek(&self) -> Result<char, LexerError> {
        self.chars.front().copied().ok_or_else(|| LexerError::ExpectedChar(self.line, self.column))
    }

    // check if there is a next character
    fn has_next(&self) -> bool {
        !self.chars.is_empty()
    }

    // consume the next character
    fn next_char(&mut self) -> Result<char, LexerError> {
        let c = self.chars.pop_front().ok_or_else(|| LexerError::ExpectedChar(self.line, self.column))?;
        self.pos += 1;
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Ok(c)
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

    // get the next token
    fn next_token(&self, diff: usize) -> Option<Token> {
        let slice = self.input.get(self.pos - 1..self.pos + diff)?;
        Token::value_of(slice)
    }

    // read characters while the delimiter is true
    fn read_while<F>(&mut self, delimiter: F) -> Result<&'a str, LexerError>
    where 
        F: Fn(&char) -> bool,
    {
        let init_pos = self.pos - 1;
        loop {
            let value = self.next_char()?;
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
            let c = self.next_char()?;
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
            let c = self.next_char()?;
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
    // Support base 10 and base 16, also support long numbers
    fn read_number(&mut self, c: char) -> Result<Token, LexerError> {
        let mut is_long = false;
        let is_hex = c == '0' && self.peek()? == 'x';

        let mut init_pos = if is_hex {
            // Skip the x
            self.next_char()?;
            self.pos
        } else {
            self.pos - 1
        };

        let mut offset = 0;
        let mut transformed_string: Option<String> = None;
        while self.has_next() {
            let v = self.next_char()?;
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
                if v == 'L' {
                    is_long = true;
                    offset = 1;
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
        Ok(if is_long {
            Token::Long(u128::from_str_radix(&v, radix).map_err(|_| LexerError::ParseToNumber(self.line, self.column))?)
        } else {
            Token::Number(u64::from_str_radix(&v, radix).map_err(|_| LexerError::ParseToNumber(self.line, self.column))?)
        })
    }

    // read a multi-line comment
    // expected format is /* ... */
    fn skip_multi_line_comment(&mut self) -> Result<(), LexerError> {
        loop {
            let c = self.next_char()?;
            if c == '*' && self.peek()? == '/' {
                self.next_char()?;
                break;
            }
        }

        Ok(())
    }

    // Parse the code into a list of tokens
    pub fn get(mut self) -> Result<VecDeque<Token>, LexerError> {
        let mut tokens: Vec<Token> = Vec::new();
        while self.has_next() {
            let c = self.next_char()?;
            let token = match c {
                // skipped characters
                '\n' | ' ' | ';' => {
                    // we just skip these characters
                    continue;
                },
                // read a string value
                // It supports escaped characters
                '"' | '\'' => {
                    let value = self.read_string(c)?;
                    Token::ValString(value.into_owned())
                },
                // it's only a comment, skip until its end
                '/' if {
                    let v = self.peek()?;
                    v == '/' || v == '*'
                } => {
                    let v = self.next_char()?;
                    if v == '/' {
                        self.skip_until(|c| *c == '\n')?;
                    } else {
                        self.skip_multi_line_comment()?;
                    }
                    continue;
                },
                // read a number value
                c if c.is_digit(10) => self.read_number(c)?,
                c if c.is_alphabetic() => {
                    let value = self.read_while(|v| -> bool {
                        v.is_alphanumeric() || *v == '_'
                    })?;

                    if let Some(token) = Token::value_of(&value) {
                        token
                    } else {
                        Token::Identifier(value.into())
                    }
                },
                _ => {
                    // We must check token with the next character because of operations with assignations
                    if let Some(token) = self.next_token(1) {
                        self.next_char()?;
                        token
                    } else if let Some(token) = self.next_token(0) {
                        token
                    } else {
                        return Err(LexerError::NoTokenFound(self.line, self.column));
                    }
                }
            };

            // push the token to the list
            tokens.push(token);
        }

        Ok(tokens.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_assign() {
        let code = "let a = 10;";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("a".to_owned()),
            Token::OperatorAssign,
            Token::Number(10)
        ]);
    }

    #[test]
    fn test_function() {
        let code = "func main() { return 10; }";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Function,
            Token::Identifier("main".to_owned()),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::Return,
            Token::Number(10),
            Token::BraceClose
        ]);
    }

    #[test]
    fn test_long_with_underscore() {
        let code = "10_000L";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Long(10_000)
        ]);
    }

    #[test]
    fn test_string() {
        let code = "\"Hello, World!\"";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::ValString("Hello, World!".to_owned())
        ]);
    }

    #[test]
    fn test_inner_string() {
        let code = "\"'Hello, World!'\"";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::ValString("'Hello, World!'".to_owned())
        ]);
    }

    #[test]
    fn test_inner_escaped_string() {
        let code = "'Hello, \\'World!'";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::ValString("Hello, 'World!".to_owned())
        ]);
    }

    #[test]
    fn test_comment() {
        let code = "// This is a comment\nlet a = 10;";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("a".to_owned()),
            Token::OperatorAssign,
            Token::Number(10)
        ]);
    }

    #[test]
    fn test_multi_line_comment() {
        let code = "/* This is a comment\n * with multiple lines */\nlet a = 10;";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("a".to_owned()),
            Token::OperatorAssign,
            Token::Number(10)
        ]);
    }

    #[test]
    fn test_assignation() {
        let code = "let a = 10; a += 20;";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("a".to_owned()),
            Token::OperatorAssign,
            Token::Number(10),
            Token::Identifier("a".to_owned()),
            Token::OperatorPlusAssign,
            Token::Number(20)
        ]);
    }

    #[test]
    fn test_number() {
        let code = "10";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Number(10)
        ]);
    }

    #[test]
    fn test_number_with_underscore() {
        let code = "10_000";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Number(10_000)
        ]);
    }

    #[test]
    fn test_number_with_hex() {
        let code = "0x10";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Number(16)
        ]);
    }

    #[test]
    fn test_number_long() {
        let code = "10L";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Long(10)
        ]);
    }

    #[test]
    fn test_number_long_with_underscore() {
        let code = "10_000L";
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens, vec![
            Token::Long(10_000)
        ]);
    }
}