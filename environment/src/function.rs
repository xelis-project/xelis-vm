use std::{fmt, sync::Arc};

use xelis_types::{StackValue, Type, ValueCell};
use crate::Context;

use super::EnvironmentError;

// first parameter is the current value / instance
// second is the list of all parameters for this function call
pub type FnReturnType = Result<Option<ValueCell>, EnvironmentError>;
pub type FnInstance<'a> = Result<&'a mut ValueCell, EnvironmentError>;
pub type FnParams = Vec<StackValue>;
// pub type OnCallFn = for<'a, 'ty, 'r> fn(FnInstance<'a>, FnParams, &'a mut Context<'ty, 'r>) -> FnReturnType;
pub type OnCallFn = Box<
    dyn for<'a, 'ty, 'r> Fn(
        FnInstance<'a>,
        FnParams,
        &'a mut Context<'ty, 'r>,
    ) -> FnReturnType + Send + Sync
>;

// Native function that is implemented in Rust
// This is used to register functions in the environment
// #[derive(Clone)]
pub struct NativeFunction {
    // function on type
    on_type: Option<Type>,
    require_instance: bool,
    parameters: Vec<Type>,
    on_call: OnCallFn,
    // cost for each call
    cost: u64,
    // expected type of the returned value
    return_type: Option<Type>
}

impl NativeFunction {
    // Create a new instance of the NativeFunction
    #[inline]
    pub fn new(on_type: Option<Type>, require_instance: bool, parameters: Vec<Type>, on_call: OnCallFn, cost: u64, return_type: Option<Type>) -> Self {
        Self {
            on_type,
            require_instance,
            parameters,
            on_call,
            cost,
            return_type
        }
    }

    // Get function on type
    // example: Foo::bar
    // Foo is on_type
    #[inline]
    pub fn on_type(&self) -> Option<&Type> {
        self.on_type.as_ref()
    }

    // Check if the function requires an instance
    #[inline]
    pub fn is_on_instance(&self) -> bool {
        self.require_instance
    }

    // Execute the function
    pub fn call_function<'ty, 'r>(&self, instance_value: Option<&mut ValueCell>, parameters: FnParams, context: &mut Context<'ty, 'r>) -> Result<Option<ValueCell>, EnvironmentError> {
        if parameters.len() != self.parameters.len() || (instance_value.is_some() != self.require_instance) {
            return Err(EnvironmentError::InvalidFnCall(parameters.len(), self.parameters.len(), instance_value.is_some(), self.require_instance));
        }

        let instance = match instance_value {
            Some(v) => Ok(v),
            None => Err(EnvironmentError::FnExpectedInstance)
        };
        (self.on_call)(instance, parameters, context)
    }

    // Set the function on call
    #[inline]
    pub fn set_on_call(&mut self, on_call: OnCallFn) {
        self.on_call = on_call;
    }

    // Get parameters of the function
    #[inline]
    pub fn get_parameters(&self) -> &Vec<Type> {
        &self.parameters
    }

    // Get the expected type of the returned value
    #[inline]
    pub fn return_type(&self) -> &Option<Type> {
        &self.return_type
    }

    // Get the cost of the function
    #[inline]
    pub fn get_cost(&self) -> u64 {
        self.cost
    }

    // Set the cost of the function
    #[inline]
    pub fn set_cost(&mut self, cost: u64) {
        self.cost = cost;
    }
}

impl Clone for NativeFunction {
    fn clone(&self) -> Self {
        todo!()
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativeFunction")
            .field("on_type", &self.on_type)
            .field("require_instance", &self.require_instance)
            .field("parameters", &self.parameters)
            .field("return_type", &self.return_type)
            .field("cost", &self.cost)
            .finish()
    }
}