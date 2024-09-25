use crate::{values::ValueError, Value};

#[derive(Debug)]
pub enum EnvironmentError {
    InvalidFnCall,
    FnExpectedInstance,
    Panic(Value),
    OutOfBounds(usize, usize),
    InvalidRange(u32, u32),
    NoValueFoundAtIndex(u32),
    InvalidType(Value),
    ValueError(ValueError)
}

impl From<ValueError> for EnvironmentError {
    fn from(error: ValueError) -> Self {
        EnvironmentError::ValueError(error)
    }
}