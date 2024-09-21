use crate::{InterpreterError, Value};

#[derive(Debug)]
pub enum EnvironmentError {
    InvalidFnCall,
    FnExpectedInstance,
    Panic(Value),
    OutOfBounds(usize, usize),
    InvalidRange(u64, u64),
    NoValueFoundAtIndex(u64),
    InvalidType(Value),
}

impl From<InterpreterError> for EnvironmentError {
    fn from(error: InterpreterError) -> Self {
        match error {
            _ => EnvironmentError::InvalidFnCall
        }
    }
}