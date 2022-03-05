use crate::expressions::{Statement, Parameter};
use crate::interpreter::{Interpreter, InterpreterError};
use crate::types::{Type, Value};

// first parameter is the current value / instance
// second is the list of all parameters for this function call
pub type FnReturnType = Result<Option<Value>, InterpreterError>;
pub type FnInstance<'a> = Result<&'a mut Value, InterpreterError>;
pub type OnCallFn = fn(FnInstance, Vec<Value>) -> FnReturnType;

pub struct NativeFunction {
    name: String,
    for_type: Option<Type>, // function on type
    parameters: Vec<Type>,
    on_call: OnCallFn,
    cost: u64, // cost for each call
    return_type: Option<Type> // expected type of the returned value
}

impl NativeFunction {
    pub fn new(name: String, for_type: Option<Type>, parameters: Vec<Type>, on_call: OnCallFn, cost: u64, return_type: Option<Type>) -> Self {
        Self {
            name,
            for_type,
            parameters,
            on_call,
            cost,
            return_type
        }
    }

    pub fn call_function(&self, interpreter: &Interpreter, instance_value: Option<&mut Value>, parameters: Vec<Value>) -> Result<Option<Value>, InterpreterError> {
        if parameters.len() != self.parameters.len() || (instance_value.is_some() != self.for_type.is_some()) {
            return Err(InterpreterError::InvalidNativeFunctionCall)
        }

        interpreter.add_count_expr(self.cost);
        let instance = match instance_value {
            Some(v) => Ok(v),
            None => Err(InterpreterError::NativeFunctionExpectedInstance)
        };
        let res: Option<Value> = (self.on_call)(instance, parameters)?;

        if let (Some(v), Some(ret_type)) = (&res, &self.return_type) {
            let value_type = interpreter.get_type_from_value(v)?;
            if !value_type.is_compatible_with(ret_type) {
                return Err(InterpreterError::InvalidType(value_type))
            }
        }

        Ok(res)
    }
}

// we implement it ourself to prevent the printing of on_call param
impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFunction")
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

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn get_parameters(&self) -> &Vec<Parameter> {
        &self.parameters
    }

    pub fn get_instance_name(&self) -> &Option<String> {
        &self.instance_name
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

    pub fn get_parameters_count(&self) -> usize {
        match &self {
            FunctionType::Native(ref f) => f.parameters.len(),
            FunctionType::Custom(ref f) => f.parameters.len()
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
/*
pub struct NativeFunctionCallManager {
    instance_type: Option<Value>,
    values: Vec<Value>,
}

impl NativeFunctionCallManager {
    pub fn new(instance_type: Option<Value>, values: Vec<Value>) -> Self {
        Self {
            instance_type,
            values
        }
    }

    pub fn get_parameters_count(&self) -> usize {
        self.values.len()
    }

    pub fn has_instance_type(&self) -> bool {
        self.instance_type.is_some()
    }

    pub fn get_instance_type(&mut self) -> Result<&mut Value, InterpreterError> {
        match self.instance_type {
            Some(ref mut v) => Ok(v),
            None => Err(InterpreterError::NoInstanceType) 
        }
    }

    pub fn get_parameter_at(&mut self, index: usize) -> Result<Value, InterpreterError> {
        if self.get_parameters_count() <= index {
            return Err(InterpreterError::OutOfBounds(self.get_parameters_count(), index))
        }

        Ok(self.values.remove(index))
    }

    pub fn get_parameter(&mut self) -> Result<Value, InterpreterError> {
        self.get_parameter_at(0)
    }
}*/