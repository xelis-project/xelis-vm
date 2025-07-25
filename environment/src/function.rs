use std::{fmt, sync::Arc};

use futures::future::LocalBoxFuture;
use xelis_bytecode::Module;
use xelis_types::{Primitive, StackValue, Type, ValueCell};
use crate::Context;

use super::EnvironmentError;

pub enum SysCallResult<M> {
    None,
    Return(ValueCell),
    DynamicCall {
        // Should contains Vec<u16, bool>
        ptr: ValueCell,
        params: FnParams
    },
    // NOTE: due to the invariant lifetime issue
    // we don't provide any reference
    ModuleCall {
        module: Arc<Module>,
        metadata: Arc<M>,
        chunk: u16,
    }
}

impl<M> SysCallResult<M> {
    pub const fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }
}

impl<M> From<Option<Primitive>> for SysCallResult<M> {
    fn from(value: Option<Primitive>) -> Self {
        match value {
            Some(primitive) => SysCallResult::Return(primitive.into()),
            None => SysCallResult::None,
        }
    }
}

impl<M> From<Primitive> for SysCallResult<M> {
    fn from(value: Primitive) -> Self {
        SysCallResult::Return(value.into())
    }
}

impl<M> From<ValueCell> for SysCallResult<M> {
    fn from(value: ValueCell) -> Self {
        SysCallResult::Return(value)
    }
}

// first parameter is the current value / instance
// second is the list of all parameters for this function call
pub type FnReturnType<M> = Result<SysCallResult<M>, EnvironmentError>;
pub type FnInstance<'a> = Result<&'a mut ValueCell, EnvironmentError>;
pub type FnParams = Vec<StackValue>;
pub type OnCallSyncFn<M> = for<'a, 'ty, 'r> fn(
        FnInstance<'a>,
        FnParams,
        &'a mut Context<'ty, 'r>,
    ) -> FnReturnType<M>;

pub type OnCallAsyncFn<M> = for<'a, 'ty, 'r> fn(
        FnInstance<'a>,
        FnParams,
        &'a mut Context<'ty, 'r>,
    ) -> LocalBoxFuture<'a, FnReturnType<M>>;

#[derive(Clone, Copy, Debug)]
pub enum FunctionHandler<M> {
    Sync(OnCallSyncFn<M>),
    Async(OnCallAsyncFn<M>),
}

// Native function that is implemented in Rust
// This is used to register functions in the environment
#[derive(Clone)]
pub struct NativeFunction<M> {
    // function on type
    on_type: Option<Type>,
    require_instance: bool,
    parameters: Vec<Type>,
    on_call: FunctionHandler<M>,
    // cost for each call
    cost: u64,
    // expected type of the returned value
    return_type: Option<Type>
}

impl<M> NativeFunction<M> {
    // Create a new instance of the NativeFunction
    #[inline]
    pub fn new(on_type: Option<Type>, require_instance: bool, parameters: Vec<Type>, on_call: FunctionHandler<M>, cost: u64, return_type: Option<Type>) -> Self {
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
    pub async fn call_function<'ty, 'r>(&self, instance_value: Option<&mut ValueCell>, parameters: FnParams, context: &mut Context<'ty, 'r>) -> Result<SysCallResult<M>, EnvironmentError> {
        if parameters.len() != self.parameters.len() || (instance_value.is_some() != self.require_instance) {
            return Err(EnvironmentError::InvalidFnCall(parameters.len(), self.parameters.len(), instance_value.is_some(), self.require_instance));
        }

        let instance = match instance_value {
            Some(v) => Ok(v),
            None => Err(EnvironmentError::FnExpectedInstance)
        };

        match self.on_call {
            FunctionHandler::Sync(on_call) => (on_call)(instance, parameters, context),
            FunctionHandler::Async(on_call) => on_call(instance, parameters, context).await
        }
    }

    // Set the function on call
    #[inline]
    pub fn set_on_call(&mut self, on_call: FunctionHandler<M>) {
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

impl<'ty, M> fmt::Debug for NativeFunction<M> {
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