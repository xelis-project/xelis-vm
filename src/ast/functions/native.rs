use crate::{
    interpreter::{InterpreterError, State},
    types::Type,
    values::Value,
};

use super::{FnParams, OnCallFn};

pub struct NativeFunction {
    // function on type
    for_type: Option<Type>,
    parameters: Vec<Type>,
    on_call: OnCallFn,
    // cost for each call
    cost: u64,
    // expected type of the returned value
    return_type: Option<Type>
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

    // Execute the function
    pub fn call_function(&self, instance_value: Option<&mut Value>, parameters: FnParams, state: &mut State) -> Result<Option<Value>, InterpreterError> {
        if parameters.len() != self.parameters.len() || (instance_value.is_some() != self.for_type.is_some()) {
            return Err(InterpreterError::InvalidNativeFunctionCall)
        }

        state.increase_gas_usage(self.cost)?;

        let instance = match instance_value {
            Some(v) => Ok(v),
            None => Err(InterpreterError::NativeFunctionExpectedInstance)
        };
        (self.on_call)(instance, parameters)
    }

    // Get parameters of the function
    pub fn get_parameters(&self) -> &Vec<Type> {
        &self.parameters
    }

    // Get the expected type of the returned value
    pub fn get_return_type(&self) -> &Option<Type> {
        &self.return_type
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