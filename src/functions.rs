use crate::types::{Type, Value};
use crate::expressions::{Statement, Parameter};

// first parameter is the current value that represent (if the function has no 'for_type' value is Null)
// second is the list of all parameters for this function call
pub type OnCallFn = fn(&mut Value, Vec<Value>) -> Option<Value>;

pub struct NativeFunction {
    name: String,
    for_type: Option<Type>,
    parameters: Vec<Type>,
    on_call: OnCallFn,
    return_type: Option<Type>
}

impl NativeFunction {
    pub fn new(name: String, for_type: Option<Type>, parameters: Vec<Type>, on_call: OnCallFn, return_type: Option<Type>) -> Self {
        Self {
            name,
            for_type,
            parameters,
            on_call,
            return_type
        }
    }

    pub fn call_function(&self, current_value: &mut Value, parameters: Vec<Value>) -> Option<Value> {
        (self.on_call)(current_value, parameters)
    }
}

// we implement it ourself to prevent the printing of on_call param
impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Point")
         .field("name", &self.name)
         .field("for_type", &self.for_type)
         .field("parameters", &self.parameters)
         .field("return_type", &self.return_type)
         .finish()
    }
}

#[derive(Debug)]
pub struct CustomFunction {
    name: String,
    for_type: Option<Type>,
    instance_name: Option<String>,
    parameters: Vec<Parameter>,
    statements: Vec<Statement>,
    entry: bool,
    return_type: Option<Type>
}

impl CustomFunction {
    pub fn new(name: String, for_type: Option<Type>, instance_name: Option<String>, parameters: Vec<Parameter>, statements: Vec<Statement>, entry: bool, return_type: Option<Type>) -> Self {
        CustomFunction {
            name,
            for_type,
            instance_name,
            parameters,
            statements,
            entry,
            return_type
        }
    }

    pub fn set_statements(&mut self, statements: Vec<Statement>) {
        self.statements = statements;
    }
}

#[derive(Debug)]
pub enum FunctionType {
    Native(NativeFunction),
    Custom(CustomFunction)
}

impl FunctionType {
    pub fn get_name(&self) -> &String {
        match &self {
            FunctionType::Native(ref f) => &f.name,
            FunctionType::Custom(ref f) => &f.name
        }
    }

    pub fn get_parameters_types(&self) -> Vec<&Type> {
        match &self {
            FunctionType::Native(ref f) => f.parameters.iter().map(|p| p).collect(),
            FunctionType::Custom(ref f) => f.parameters.iter().map(|p| p.get_type()).collect()
        }
    }

    pub fn for_type(&self) -> &Option<Type> {
        match &self {
            FunctionType::Native(ref f) => &f.for_type,
            FunctionType::Custom(ref f) => &f.for_type
        }
    }

    pub fn return_type(&self) -> &Option<Type> {
        match &self {
            FunctionType::Native(ref f) => &f.return_type,
            FunctionType::Custom(ref f) => &f.return_type 
        }
    }

    pub fn is_entry(&self) -> bool {
        match &self {
            FunctionType::Custom(ref f) => f.entry,
            _ => false
        }
    }
}