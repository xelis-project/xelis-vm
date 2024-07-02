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
    pub fn new(chars: Vec<char>) -> Self {
        Lexer {
            chars: chars.into(),
            line: 1,
            column: 1
        }
    }

    // peek the next character
    fn peek(&self) -> Result<&char, LexerError> {
        self.chars.front().ok_or(LexerError::ExpectedChar(self.line, self.column))
    }

    // consume the next character
    fn next_char(&mut self) -> Result<char, LexerError> {
        let c = self.chars.pop_front().ok_or(LexerError::ExpectedChar(self.line, self.column))?;
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
        if let Some(peek) = self.peek().ok() {
            Token::value_of(&vec![c, *peek].into_iter().collect())
        } else {
            None
        }
    }

    // read characters while the delimiter is true
    fn read_while<F>(&mut self, c: char, delimiter: F) -> Result<String, LexerError>
    where 
        F: Fn(&char) -> bool,
    {
        let mut values = vec![c];
        while delimiter(self.peek()?) {
            values.push(self.next_char()?);
        }

        Ok(values.into_iter().collect())
    }

    // this will consume characters until the delimiter returns true
    fn read_until<F>(&mut self, delimiter: F) -> Result<String, LexerError>
    where 
        F: Fn(&char) -> bool,
    {
        let mut values = vec![];
        loop {
            let c = self.next_char()?;
            if delimiter(&c) {
                break;
            }
            values.push(c);
        }

        Ok(values.into_iter().collect())
    }

    // parse a number from a string
    fn parse_number<F: FromStr>(&self, value: String) -> Result<F, LexerError> {
        match value.parse::<F>() {
            Ok(v) => Ok(v),
            Err(_) => Err(LexerError::ParseToNumber(self.line, self.column))
        }
    }

    // Parse the code into a list of tokens
    pub fn get(mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens: Vec<Token> = Vec::new();
        while self.chars.len() > 0 {
            let c = self.next_char()?;

            match c {
                // it's only a comment, no need to parse it
                '/' if *self.peek()? == '/' => {
                    // TODO support multi line comments
                    self.read_until(|c| *c == '\n')?;
                },
                // skipped characters
                '\n' | ' ' | ';' => {
                    // we just skip these characters
                },
                // read a string value
                '"' | '\'' => {
                    let value = self.read_until(|v| -> bool {
                        *v == c
                    })?;
                    tokens.push(Token::ValString(value));
                },
                // read a number value
                c if c.is_digit(10) => {
                    let mut chars = vec![c];
                    while {
                        let v = self.peek()?;
                        v.is_digit(10) || *v == '_' || *v == 'L'
                    } {
                        let c = self.next_char()?;
                        chars.push(c);

                        if c == 'L' {
                            break;
                        }
                    }

                    let token: Token = match chars.last() {
                        Some(last) => match last {
                            'L' => {
                                chars.pop();
                                Token::Long(self.parse_number(chars.into_iter().collect())?)
                            },
                            _ => Token::Number(self.parse_number(chars.into_iter().collect())?)
                        },
                        None => {
                            return Err(LexerError::ExpectedChar(self.line, self.column))
                        }
                    };
                    tokens.push(token);
                },
                c if c.is_alphabetic() => {
                    let value = self.read_while(c, |v| -> bool {
                        v.is_alphanumeric() || *v == '_'
                    })?;
                    if let Some(token) = Token::value_of(&value) {
                        tokens.push(token);
                    } else {
                        tokens.push(Token::Identifier(value));
                    }
                },
                c => {
                    if let Some(v) = self.next_token(c) {
                        self.next_char()?;
                        tokens.push(v);
                    } else if let Some(v) = Token::value_of(&c.to_string()) {
                        tokens.push(v);
                    } else {
                        return Err(LexerError::NoTokenFound(self.line, self.column))
                    }
                }
            };
        }
        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_lexer_assign() {
        use super::*;
        let code = "let a = 10;".chars().collect();
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
    fn test_lexer_function() {
        use super::*;
        let code = "func main() { return 10; }".chars().collect();
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
}