mod parser;
mod lexer;
mod token;
mod operator;
mod types;
mod environment;
mod functions;
mod expressions;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::environment::Environment;

use std::fs;

fn main() {
    let code: String =
    fs::read_to_string("examples/struct.xel").expect("Something went wrong reading the file");

    match Lexer::new(code.chars().collect()).get() {
        Ok(result) => {
            println!("{:?}", result);
            match Parser::new(result, Environment::default()).parse() {
                Ok(result) => println!("Result: {:?}", result),
                Err(e) => println!("Parser error: {:?}", e)
            };
        }
        Err(e) => println!("Lexer error: {:?}", e)
    };
}