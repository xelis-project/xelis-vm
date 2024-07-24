use std::collections::VecDeque;

use crate::{
    expressions::{Parameter, Statement},
    interpreter::{InterpreterError, State},
    types::{Type, Value},
    IdentifierType
};

// first parameter is the current value / instance
// second is the list of all parameters for this function call
pub type FnReturnType = Result<Option<Value>, InterpreterError>;
pub type FnInstance<'a> = Result<&'a mut Value, InterpreterError>;
pub type OnCallFn = fn(FnInstance, Vec<Value>) -> FnReturnType;

pub struct NativeFunction {
    for_type: Option<Type>, // function on type
    parameters: Vec<Type>,
    on_call: OnCallFn,
    cost: u64, // cost for each call
    return_type: Option<Type> // expected type of the returned value
}

impl NativeFunction {
    pub fn new(for_type: Option<Type>, parameters: Vec<Type>, on_call: OnCallFn, cost: u64, return_type: Option<Type>) -> Self {
        Self {
            for_type,
            parameters,
            on_call,
            cost,
            return_type
        }
    }

    pub fn call_function(&self, instance_value: Option<&mut Value>, parameters: VecDeque<Value>, state: &mut State) -> Result<Option<Value>, InterpreterError> {
        if parameters.len() != self.parameters.len() || (instance_value.is_some() != self.for_type.is_some()) {
            return Err(InterpreterError::InvalidNativeFunctionCall)
        }

        state.increase_expressions_executed_by(self.cost)?;

        let instance = match instance_value {
            Some(v) => Ok(v),
            None => Err(InterpreterError::NativeFunctionExpectedInstance)
        };
        let res: Option<Value> = (self.on_call)(instance, parameters.into())?;

        // if let (Some(v), Some(ret_type)) = (&res, &self.return_type) {
        //     let value_type = interpreter.get_type_from_value(v)?;
        //     if !value_type.is_compatible_with(ret_type) {
        //         return Err(InterpreterError::InvalidType(value_type))
        //     }
        // }

        Ok(res)
    }
}

// we implement it ourself to prevent the printing of on_call param
impl std::fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("NativeFunction")
         .field("for_type", &self.for_type)
         .field("parameters", &self.parameters)
         .field("return_type", &self.return_type)
         .finish()
    }
}

#[derive(Debug)]
pub struct DeclaredFunction {
    for_type: Option<Type>,
    instance_name: Option<IdentifierType>,
    parameters: Vec<Parameter>,
    statements: Vec<Statement>,
    return_type: Option<Type>
}

impl DeclaredFunction {
    pub fn new(for_type: Option<Type>, instance_name: Option<IdentifierType>, parameters: Vec<Parameter>, statements: Vec<Statement>, return_type: Option<Type>) -> Self {
        DeclaredFunction {
            for_type,
            instance_name,
            parameters,
            statements,
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

    pub fn get_instance_name(&self) -> &Option<IdentifierType> {
        &self.instance_name
    }
}

#[derive(Debug)]
pub struct EntryFunction {
    parameters: Vec<Parameter>,
    statements: Vec<Statement>,
}

impl EntryFunction {
    pub fn new(parameters: Vec<Parameter>, statements: Vec<Statement>) -> Self {
        EntryFunction {
            parameters,
            statements
        }
    }

    pub fn get_parameters(&self) -> &Vec<Parameter> {
        &self.parameters
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }
}

#[derive(Debug)]
pub enum FunctionType {
    Native(NativeFunction),
    Declared(DeclaredFunction),
    Entry(EntryFunction)
}

impl FunctionType {
    pub fn get_parameters_types(&self) -> Vec<&Type> {
        match &self {
            FunctionType::Native(ref f) => f.parameters.iter().map(|p| p).collect(),
            FunctionType::Declared(ref f) => f.parameters.iter().map(|p| p.get_type()).collect(),
            FunctionType::Entry(ref f) => f.parameters.iter().map(|p| p.get_type()).collect()
        }
    }

    pub fn get_parameters_count(&self) -> usize {
        match &self {
            FunctionType::Native(ref f) => f.parameters.len(),
            FunctionType::Declared(ref f) => f.parameters.len(),
            FunctionType::Entry(ref f) => f.parameters.len()
        } 
    }

    pub fn for_type(&self) -> &Option<Type> {
        match &self {
            FunctionType::Native(ref f) => &f.for_type,
            FunctionType::Declared(ref f) => &f.for_type,
            FunctionType::Entry(_) => &None
        }
    }

    pub fn return_type(&self) -> &Option<Type> {
        match &self {
            FunctionType::Native(ref f) => &f.return_type,
            FunctionType::Declared(ref f) => &f.return_type ,
            FunctionType::Entry(_) => &None
        }
    }

    pub fn is_entry(&self) -> bool {
        match &self {
            FunctionType::Entry(_) => true,
            _ => false
        }
    }
}