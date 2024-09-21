mod native;
mod declared;

pub use declared::{DeclaredFunction, EntryFunction};
pub use native::NativeFunction;

use crate::{types::Type, values::Value, interpreter::Path, IdentifierType, InterpreterError};

// The return type of the entry function
// This is hardcoded to u64 so people can't return anything else
// except an exit code
pub const ENTRY_FN_RETURN_TYPE: Type = Type::U64;

// first parameter is the current value / instance
// second is the list of all parameters for this function call
pub type FnReturnType = Result<Option<Value>, InterpreterError>;
pub type FnInstance<'a> = Result<&'a mut Value, InterpreterError>;
pub type FnParams<'a> = Vec<Path<'a>>;
pub type OnCallFn = fn(FnInstance, FnParams) -> FnReturnType;

// Function parameter
#[derive(Debug)]
pub struct Parameter {
    name: IdentifierType,
    value_type: Type
}

impl Parameter {
    #[inline(always)]
    pub fn new(name: IdentifierType, value_type: Type) -> Self {
        Parameter {
            name,
            value_type
        }
    }

    #[inline(always)]
    pub fn get_name(&self) -> &IdentifierType {
        &self.name
    }

    #[inline(always)]
    pub fn get_type(&self) -> &Type {
        &self.value_type
    }

    #[inline(always)]
    pub fn consume(self) -> (IdentifierType, Type) {
        (self.name, self.value_type)
    }
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct Signature {
    name: String,
    on_type: Option<Type>,
    parameters: Vec<Type>
}

impl Signature {
    #[inline(always)]
    pub fn new(name: String, on_type: Option<Type>, parameters: Vec<Type>) -> Self {
        Signature {
            name,
            on_type,
            parameters
        }
    }

    #[inline(always)]
    pub fn get_name(&self) -> &String {
        &self.name
    }

    #[inline(always)]
    pub fn get_on_type(&self) -> &Option<Type> {
        &self.on_type
    }

    #[inline(always)]
    pub fn get_parameters(&self) -> &Vec<Type> {
        &self.parameters
    }
}

// Functions types supported
// Native functions are functions that are implemented in Rust and registered in the environment
// Declared functions are functions that are declared in the program
#[derive(Debug)]
pub enum Function<'a> {
    Native(&'a NativeFunction),
    Declared(&'a DeclaredFunction),
    Entry(&'a EntryFunction)
}

// Declared function type by a Program
// They are separated in two types for better handling
#[derive(Debug)]
pub enum DeclaredFunctionType {
    Declared(DeclaredFunction),
    Entry(EntryFunction)
}

impl DeclaredFunctionType {
    // Is this function an entry function
    pub fn is_entry(&self) -> bool {
        match &self {
            DeclaredFunctionType::Entry(_) => true,
            _ => false
        }
    }

    // Get the returned type of the function
    pub fn return_type(&self) -> &Option<Type> {
        match &self {
            DeclaredFunctionType::Declared(f) => f.get_return_type(),
            DeclaredFunctionType::Entry(_) => &Some(ENTRY_FN_RETURN_TYPE)
        }
    }

    // Get the function as a function variant
    pub fn as_function(&self) -> Function {
        match &self {
            DeclaredFunctionType::Declared(f) => Function::Declared(f),
            DeclaredFunctionType::Entry(f) => Function::Entry(f)
        }
    }
}

impl<'a> Function<'a> {
    // Get the returned type of the function
    pub fn return_type(self) -> &'a Option<Type> {
        match self {
            Function::Native(ref f) => f.get_return_type(),
            Function::Declared(ref f) => f.get_return_type() ,
            Function::Entry(_) => &Some(ENTRY_FN_RETURN_TYPE)
        }
    }

    // Is this function an entry function
    pub fn is_entry(&self) -> bool {
        match &self {
            Function::Entry(_) => true,
            _ => false
        }
    }
}