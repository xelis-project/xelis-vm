mod declared;

use xelis_types::{Type, IdentifierType};
use super::Statement;

pub use declared::{DeclaredFunction, EntryFunction};

// The return type of the entry function
// This is hardcoded to u64 so people can't return anything else
// except an exit code
pub const ENTRY_FN_RETURN_TYPE: Type = Type::U64;

// Function parameter
#[derive(Debug, PartialEq, Eq)]
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

// Declared function type by a Program
// They are separated in two types for better handling
#[derive(Debug, PartialEq, Eq)]
pub enum FunctionType {
    Declared(DeclaredFunction),
    Entry(EntryFunction)
}

impl FunctionType {
    // Is this function an entry function
    #[inline(always)]
    pub fn is_entry(&self) -> bool {
        match &self {
            FunctionType::Entry(_) => true,
            _ => false
        }
    }

    // Get the returned type of the function
    #[inline(always)]
    pub fn return_type(&self) -> &Option<Type> {
        match &self {
            FunctionType::Declared(f) => f.get_return_type(),
            FunctionType::Entry(_) => &Some(ENTRY_FN_RETURN_TYPE)
        }
    }

    // Get the function as a declared function
    #[inline(always)]
    pub fn get_instance_name(&self) -> Option<&IdentifierType> {
        match self {
            FunctionType::Declared(f) => f.get_instance_name(),
            _ => None
        }
    }

    // Get the parameters of the function
    #[inline(always)]
    pub fn get_parameters(&self) -> &Vec<Parameter> {
        match self {
            FunctionType::Declared(f) => f.get_parameters(),
            FunctionType::Entry(f) => f.get_parameters()
        }
    }

    // Get the statements of the function
    #[inline(always)]
    pub fn get_statements(&self) -> &Vec<Statement> {
        match self {
            FunctionType::Declared(f) => f.get_statements(),
            FunctionType::Entry(f) => f.get_statements()
        }
    }

    // Get the count of variables declared in the function
    pub fn get_variables_count(&self) -> u16 {
        match self {
            FunctionType::Declared(f) => f.get_variables_count(),
            FunctionType::Entry(f) => f.get_variables_count()
        }
    }

    // Set the statements of the function
    pub fn set_statements(&mut self, statements: Vec<Statement>) {
        match self {
            FunctionType::Declared(f) => f.set_statements(statements),
            FunctionType::Entry(f) => f.set_statements(statements)
        }
    }

    // Set the count of variables declared in the function
    pub fn set_max_variables_count(&mut self, count: u16) {
        match self {
            FunctionType::Declared(f) => f.set_max_variables_count(count),
            FunctionType::Entry(f) => f.set_max_variables_count(count)
        }
    }
}
