use crate::token::Token;
use std::str::FromStr;

#[derive(Debug)]
pub enum LexerError { // left is line, right is column
    EndOfFile,
    ParseToNumber(usize, usize),
    NoTokenFound(usize, usize),
    ExpectedChar
}

pub struct Lexer {
    chars: Vec<char>,
    line: usize,
    column: usize
}

impl Lexer {
    pub fn new(chars: Vec<char>) -> Self {
        Lexer {
            chars,
            line: 1,
            column: 1
        }
    }

    fn see(&self) -> &char {
        &self.chars[0]
    }

    fn next_char(&mut self) -> char {
        let c = self.chars.remove(0);
        if c == '\n' {
            self.line += 1;
            self.column = 0;
        } else {
            self.column += 1;
        }
        c
    }

    fn next_token(&self, c: char) -> Option<Token> {
        if self.chars.len() > 0 {
            Token::value_of(&vec![c, self.chars[0]].into_iter().collect())
        } else {
            None
        }
    }

    fn read_chars_while<F>(&mut self, c: char, delimiter: F) -> Result<Vec<char>, LexerError>
    where 
        F: Fn(&char) -> bool,
    {
        let mut values = vec![c];
        while delimiter(match self.chars.get(0) {
            Some(v) => v,
            None => return Err(LexerError::EndOfFile)
        }) {
            values.push(self.next_char());
        }

        Ok(values)
    }

    fn read_while<F>(&mut self, c: char, delimiter: F) -> Result<String, LexerError>
    where 
        F: Fn(&char) -> bool,
    {
        Ok(self.read_chars_while(c, delimiter)?.into_iter().collect())
    }

    fn read_until<F>(&mut self, delimiter: F) -> Result<String, LexerError>
    where 
        F: Fn(&char) -> bool,
    {
        let mut values = vec![];
        loop {
            if self.chars.len() == 0 {
                return Err(LexerError::EndOfFile)
            }

            let c = self.next_char();
            if delimiter(&c) {
                break;
            }
            values.push(c);
        }

        Ok(values.into_iter().collect())
    }

    fn parse_number<F: FromStr>(&self, mut chars: Vec<char>, remove_last: bool) -> Result<F, LexerError> {
        if remove_last {
            chars.pop();
        }
        let s: String = chars.into_iter().collect();
        match s.parse::<F>() {
            Ok(v) => Ok(v),
            Err(_) => Err(LexerError::ParseToNumber(self.line, self.column))
        }
    }

    pub fn get(mut self) -> Result<Vec<Token>, LexerError> {
        let mut tokens: Vec<Token> = vec![];
        while self.chars.len() > 0 {
            let c = self.next_char();
            if c == '/' && *self.see() == '/' { // it's only a comment, no need to parse it
                self.read_until(|c| *c == '\n')?; // TODO support multi line comments
            }
            else if c == '\n' || c == ' ' || c == ';' {
                // we just skip these characters
            } else if c.is_digit(10) { // read a number value
                let mut chars = vec![c];
                while match self.chars.get(0) {
                    Some(v) => v.is_digit(10) || *v == '_' || *v == 'L',
                    None => return Err(LexerError::EndOfFile)
                } {
                    let c = self.next_char();
                    chars.push(c);
                    if c == 'L' {
                        break;
                    }
                }

                let token: Token = match chars.last() {
                    Some(last) => match last {
                        'L' => Token::Long(self.parse_number(chars, true)?),
                        _ => Token::Number(self.parse_number(chars, false)?)
                    },
                    None => {
                        return Err(LexerError::ExpectedChar)
                    }
                };
                tokens.push(token);
            } else if c == '"' || c == '\'' { // read a string value
                let value = self.read_until(|v| -> bool {
                    *v == c
                })?;
                tokens.push(Token::ValString(value));
            } else if c.is_alphabetic() { // read a word
                let value = self.read_while(c, |v| -> bool {
                    v.is_alphanumeric() || *v == '_'
                })?;
                if let Some(token) = Token::value_of(&value) {
                    tokens.push(token);
                } else {
                    tokens.push(Token::Identifier(value));
                }
            } else if let Some(v) = self.next_token(c) { // read a token from two characters
                self.next_char();
                tokens.push(v);
            } else if let Some(v) = Token::value_of(&c.to_string()) { // last chance, read a token from current character
                tokens.push(v);
            } else {
                return Err(LexerError::NoTokenFound(self.line, self.column))
            }
        }
        Ok(tokens)
    }
}