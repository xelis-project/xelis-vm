use crate::token::Token;
use std::{collections::VecDeque, str::FromStr};

#[derive(Debug)]
pub enum LexerError { // left is line, right is column
    EndOfFile,
    ParseToNumber(usize, usize),
    NoTokenFound(usize, usize),
    ExpectedChar(usize, usize)
}

pub struct Lexer {
    chars: VecDeque<char>,
    line: usize,
    column: usize
}

impl Lexer {
    // create a new lexer
    pub fn new<S: Into<VecDeque<char>>>(chars: S) -> Self {
        Lexer {
            chars: chars.into(),
            line: 1,
            column: 1
        }
    }

    // peek the next character
    fn peek(&self) -> Result<&char, LexerError> {
        self.chars.front().ok_or_else(|| LexerError::ExpectedChar(self.line, self.column))
    }

    // consume the next character
    fn next_char(&mut self) -> Result<char, LexerError> {
        let c = self.chars.pop_front().ok_or_else(|| LexerError::ExpectedChar(self.line, self.column))?;
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Ok(c)
    }

    // get the next token
    fn next_token(&self, c: char) -> Option<Token> {
        self.peek().ok().map(|v| Token::value_of(&vec![c, *v].into_iter().collect())).flatten()
    }

    // read characters while the delimiter is true
    fn read_while<F>(&mut self, c: char, delimiter: F) -> Result<String, LexerError>
    where 
        F: Fn(&char) -> bool,
    {
        let mut values = vec![c];
        loop {
            let value = self.next_char()?;
            if !delimiter(&value) {
                // push the last character back
                self.chars.push_front(value);
                break;
            } else {
                values.push(value);
            }
        }

        Ok(String::from_iter(values))
    }

    // this will consume characters until the delimiter returns true
    fn read_until<F>(&mut self, delimiter: F) -> Result<String, LexerError>
    where 
        F: Fn(&char) -> bool,
    {
        let mut values = Vec::new();
        loop {
            let c = self.next_char()?;
            if delimiter(&c) {
                break;
            }
            values.push(c);
        }

        Ok(String::from_iter(values))
    }

    // this will read the whole string until the end character
    // it supports escaped characters
    fn read_string(&mut self, end: char) -> Result<String, LexerError> {
        let mut values = Vec::new();
        let mut escape = false;
        loop {
            let c = self.next_char()?;
            if c == end && !escape {
                break;
            }
            else if c == '\\' && !escape {
                escape = true;
            } else {
                escape = false;
                values.push(c);
            }
        }

        Ok(String::from_iter(values))
    }

    // read a multi-line comment
    // expected format is /* ... */
    fn read_multi_line_comment(&mut self) -> Result<String, LexerError> {
        let mut values = Vec::new();
        loop {
            let c = self.next_char()?;
            if c == '*' && *self.peek()? == '/' {
                self.next_char()?;
                break;
            }
            values.push(c);
        }

        Ok(String::from_iter(values))
    }

    // parse a number from a string
    fn parse_number<F: FromStr>(&self, value: String) -> Result<F, LexerError> {
        match value.parse::<F>() {
            Ok(v) => Ok(v),
            Err(_) => Err(LexerError::ParseToNumber(self.line, self.column))
        }
    }

    // Parse the code into a list of tokens
    pub fn get(mut self) -> Result<VecDeque<Token>, LexerError> {
        let mut tokens: Vec<Token> = Vec::new();
        while !self.chars.is_empty() {
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
                    Token::ValString(value)
                },
                // it's only a comment, skip until its end
                '/' if {
                    let v = self.peek()?;
                    *v == '/' || *v == '*'
                } => {
                    let v = self.next_char()?;
                    if v == '/' {
                        self.read_until(|c| *c == '\n')?;
                    } else {
                        self.read_multi_line_comment()?;
                    }
                    continue;
                },
                // read a number value
                c if c.is_digit(10) => {
                    let mut chars = vec![c];
                    let mut is_long = false;
                    loop {
                        let v = self.next_char()?;
                        // Support base 10 and base 16
                        if v.is_digit(10) || v.is_digit(16) {
                            chars.push(v);
                        } else if v == '_' {
                            // Skip the underscore
                        } else {
                            if v == 'L' {
                                is_long = true;
                            } else {
                                self.chars.push_front(v);
                            }

                            break;
                        }
                    }

                    let v = String::from_iter(chars);
                    if is_long {
                        Token::Long(self.parse_number(v)?)
                    } else {
                        Token::Number(self.parse_number(v)?)
                    }
                },
                c if c.is_alphabetic() => {
                    let value = self.read_while(c, |v| -> bool {
                        v.is_alphanumeric() || *v == '_'
                    })?;

                    if let Some(token) = Token::value_of(&value) {
                        token
                    } else {
                        Token::Identifier(value)
                    }
                },
                c => {
                    if let Some(v) = self.next_token(c) {
                        self.next_char()?;
                        v
                    } else if let Some(v) = Token::value_of(&c.to_string()) {
                        v
                    } else {
                        return Err(LexerError::NoTokenFound(self.line, self.column))
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
        let code: Vec<char> = "let a = 10;".chars().collect();
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("a".to_owned()),
            Token::OperatorAssign,
            Token::Number(10)
        ]);
    }

    #[test]
    fn test_function() {
        let code: Vec<char> = "func main() { return 10; }".chars().collect();
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens.len(), 8);
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
        let code: Vec<char> = "10_000L".chars().collect();
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens, vec![
            Token::Long(10_000)
        ]);
    }

    #[test]
    fn test_inner_string() {
        let code: Vec<char> = "\"'Hello, World!'\"".chars().collect();
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens, vec![
            Token::ValString("'Hello, World!'".to_owned())
        ]);
    }

    #[test]
    fn test_inner_escaped_string() {
        let code: Vec<char> = "'Hello, \\'World\\'!'".chars().collect();
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        println!("{:?}", tokens);
        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens, vec![
            Token::ValString("Hello, 'World'!".to_owned())
        ]);
    }

    #[test]
    fn test_comment() {
        let code: Vec<char> = "// This is a comment\nlet a = 10;".chars().collect();
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("a".to_owned()),
            Token::OperatorAssign,
            Token::Number(10)
        ]);
    }

    #[test]
    fn test_multi_line_comment() {
        let code: Vec<char> = "/* This is a comment\n * with multiple lines */\nlet a = 10;".chars().collect();
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens, vec![
            Token::Let,
            Token::Identifier("a".to_owned()),
            Token::OperatorAssign,
            Token::Number(10)
        ]);
    }
}