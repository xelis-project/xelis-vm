mod lexer;
mod types;
mod values;
mod parser;
mod environment;
mod interpreter;
mod path;

pub mod bytecode;
pub mod ast;

use std::{collections::HashMap, hash::{BuildHasherDefault, Hasher}};

pub(crate) use path::Path;

pub use crate::{
    environment::{Environment, EnvironmentBuilder},
    types::Type,
    interpreter::{Interpreter, InterpreterError, State},
    parser::{Parser, ParserError},
    lexer::{Lexer, LexerError},
    values::Value,
};

