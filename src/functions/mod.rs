mod native;
mod declared;

pub use declared::{DeclaredFunction, EntryFunction};
pub use native::NativeFunction;

use crate::{types::Type, values::Value, InterpreterError};

// first parameter is the current value / instance
// second is the list of all parameters for this function call
pub type FnReturnType = Result<Option<Value>, InterpreterError>;
pub type FnInstance<'a> = Result<&'a mut Value, InterpreterError>;
pub type OnCallFn = fn(FnInstance, Vec<Value>) -> FnReturnType;

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Signature {
    name: String,
    on_type: Option<Type>,
    parameters: Vec<Type>
}

impl Signature {
    pub fn new(name: String, on_type: Option<Type>, parameters: Vec<Type>) -> Self {
        Signature {
            name,
            on_type,
            parameters
        }
    }

    pub fn get_name(&self) -> &String {
        &self.name
    }

    pub fn get_on_type(&self) -> &Option<Type> {
        &self.on_type
    }

    pub fn get_parameters(&self) -> &Vec<Type> {
        &self.parameters
    }
}

#[derive(Debug)]
pub enum FunctionType {
    Native(NativeFunction),
    Declared(DeclaredFunction),
    Entry(EntryFunction)
}

impl FunctionType {
    // Get the returned type of the function
    pub fn return_type(&self) -> &Option<Type> {
        match &self {
            FunctionType::Native(ref f) => &f.get_return_type(),
            FunctionType::Declared(ref f) => &f.get_return_type() ,
            FunctionType::Entry(_) => &None
        }
    }

    // Is this function an entry function
    pub fn is_entry(&self) -> bool {
        match &self {
            FunctionType::Entry(_) => true,
            _ => false
        }
    }
}