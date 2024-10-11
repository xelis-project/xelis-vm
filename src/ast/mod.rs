mod expressions;
mod operator;
mod token;
mod functions;

pub use expressions::{Expression, Statement, DeclarationStatement};
pub use operator::Operator;
pub use token::Token;
pub use functions::*;

use crate::{
    environment::NativeFunction,
    types::Type,
};

// Functions types supported
// Native functions are functions that are implemented in Rust and registered in the environment
// Declared functions are functions that are declared in the program
#[derive(Debug)]
pub enum Function<'a> {
    Native(&'a NativeFunction),
    Program(&'a FunctionType),
}

impl<'a> Function<'a> {
    // Get the returned type of the function
    pub fn return_type(self) -> &'a Option<Type> {
        match self {
            Function::Native(ref f) => f.return_type(),
            Function::Program(ref f) => f.return_type()
        }
    }

    // Is this function an entry function
    pub fn is_entry(&self) -> bool {
        match &self {
            Function::Program(f) => f.is_entry(),
            _ => false
        }
    }
}