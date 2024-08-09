mod environment;
mod interpreter;
mod parser;
mod expressions;
mod lexer;
mod token;
mod types;
mod functions;
mod mapper;
mod values;

pub use crate::{
    environment::{Environment, EnvironmentBuilder},
    token::Token,
    types::Type,
    functions::{FnInstance, DeclaredFunction, EntryFunction, FnReturnType, Signature},
    interpreter::{Interpreter, InterpreterError, State},
    parser::{Parser, ParserError, Program},
    lexer::{Lexer, LexerError}
};

// Variable identifier used in the parser and interpreter
// This is used to optimize the memory usage by using a smaller type
// to represent an identifier
// A mapper is done to map a string name into an identifier
pub type IdentifierType = u16;