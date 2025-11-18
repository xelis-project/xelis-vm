use std::{collections::VecDeque, fmt, sync::Arc};

use futures::future::BoxFuture;
use indexmap::IndexMap;
use xelis_bytecode::Module;
use xelis_types::{Primitive, StackValue, Type, ValueCell};
use crate::{Context, Environment, IdentityBuildHasher, ModuleMetadata};

use super::EnvironmentError;

// SysCall Result represent all possible actions
// from a SysCall
pub enum SysCallResult<M> {
    // Do nothing
    None,
    // Add to the VM stack the returned value
    Return(StackValue),
    // Async Call instruction
    AsyncCall {
        ptr: OnCallAsyncFn<M>,
        instance: bool,
        params: VecDeque<StackValue>,
    },
    // Call dynamically a chunk or a syscall fn
    DynamicCall {
        // Should contains [id: u16, is_syscall bool, from: u16]
        ptr: StackValue,
        params: VecDeque<StackValue>,
    },
    // NOTE: due to the invariant lifetime issue
    // we don't provide any reference
    ModuleCall {
        module: Arc<Module>,
        metadata: Arc<M>,
        // if set to None, it means reuse the current environment
        environment: Option<Arc<Environment<M>>>,
        chunk: u16,
        // It must be a list of parameters
        // that will be passed to the chunk
        params: VecDeque<StackValue>,
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
        SysCallResult::Return(value.into())
    }
}

impl<M> From<StackValue> for SysCallResult<M> {
    fn from(value: StackValue) -> Self {
        SysCallResult::Return(value.into())
    }
}

// first parameter is the current value / instance
// second is the list of all parameters for this function call
pub type FnReturnType<M> = Result<SysCallResult<M>, EnvironmentError>;
pub type FnInstance<'a> = Result<StackValue, EnvironmentError>;
pub type FnParams = Vec<StackValue>;
pub type OnCallSyncFn<M> = for<'a, 'ty, 'r> fn(
        FnInstance<'a>,
        FnParams,
        &'a ModuleMetadata<'_, M>,
        &'a mut Context<'ty, 'r>,
    ) -> FnReturnType<M>;

pub type OnCallAsyncFn<M> = for<'a, 'ty, 'r> fn(
        FnInstance<'a>,
        FnParams,
        &'a ModuleMetadata<'_, M>,
        &'a mut Context<'ty, 'r>,
    ) -> BoxFuture<'a, FnReturnType<M>>;

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
    #[inline(always)]
    pub fn is_on_instance(&self) -> bool {
        self.require_instance
    }

    // Verify that all parameters are safe to use
    // If a parameter is a pointer that is shared with another parameter
    pub fn verify_parameters(parameters: &mut [StackValue]) -> Result<(), EnvironmentError> {
        let mut pointers: IndexMap<*const ValueCell, usize, IdentityBuildHasher> = IndexMap::default();

        for param in parameters.iter_mut() {
            if !param.is_owned() {
                *pointers.entry(param.ptr())
                    .or_default() += 1;
            }
        }

        // Now, for each parameter that is a pointer
        // check if its count is more than 1
        for param in parameters.iter_mut().rev() {
            if let Some(count) = pointers.get_mut(&param.ptr()) {
                if *count > 1 {
                    // Clone the parameter to make it owned
                    *param = param.to_owned();
                    *count -= 1;
                }
            }
        }

        Ok(())
    }

    // Execute the function
    pub fn call_function<'ty, 'r>(&self, mut parameters: VecDeque<StackValue>, metadata: &ModuleMetadata<'_, M>, context: &mut Context<'ty, 'r>) -> Result<SysCallResult<M>, EnvironmentError> {
        if parameters.len() != self.parameters.len() + self.require_instance as usize {
            return Err(EnvironmentError::InvalidFnCall(parameters.len(), self.parameters.len()));
        }

        // Raw call to the function if its a sync function
        // otherwise return the async call
        match self.on_call {
            FunctionHandler::Sync(on_call) => {
                // Verify only the parameters for direct execution
                // For the async case, it will be done in the VM loop
                Self::verify_parameters(parameters.make_contiguous())?;

                let on_value = if self.is_on_instance() {
                    let instance = parameters.pop_front()
                        .ok_or(EnvironmentError::MissingInstanceFnCall)?;

                    Ok(instance)
                } else {
                    Err(EnvironmentError::FnExpectedInstance)
                };

                (on_call)(on_value, parameters.into(), metadata, context)
            },
            FunctionHandler::Async(ptr) => Ok(SysCallResult::AsyncCall {
                ptr,
                instance: self.is_on_instance(),
                params: parameters
            })
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