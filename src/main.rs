mod parser;
mod lexer;
mod token;
mod types;
mod environment;
mod functions;
mod expressions;
mod interpreter;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::environment::Environment;
use crate::interpreter::Interpreter;

use std::fs;

fn main() {
    let code: String =
    fs::read_to_string("examples/condition.xel").expect("Something went wrong reading the file");

    match Lexer::new(code.chars().collect()).get() {
        Ok(result) => {
            //println!("{:?}", result);
            match Parser::new(result, Environment::default()).parse() {
                Ok(result) => {
                    println!("Parser: {:?}", result);
                    match Interpreter::new(result) {
                        Ok(interpreter) => match interpreter.call_entry_function(&"main".to_owned(), vec![]) {
                            Ok(value) => println!("Exit code: {}", value),
                            Err(e) => println!("Error: {:?}", e)
                        },
                        Err(e) => println!("Interpreter error: {:?}", e)
                    };
                } 
                Err(e) => println!("Parser error: {:?}", e)
            };
        }
        Err(e) => println!("Lexer error: {:?}", e)
    };
}