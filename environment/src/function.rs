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

#[cfg(test)]
mod tests {
    use super::*;
    use xelis_types::{ValuePointer, Primitive};

    #[test]
    fn test_no_duplicated_params_all_owned() {
        // All parameters are owned - no duplicates possible
        let mut params = vec![
            StackValue::from(Primitive::U64(100)),
            StackValue::from(Primitive::U64(200)),
            StackValue::from(Primitive::U64(300)),
        ];

        let result = NativeFunction::<()>::verify_parameters(&mut params);
        assert!(result.is_ok());
        
        // All should remain owned
        assert!(params[0].is_owned());
        assert!(params[1].is_owned());
        assert!(params[2].is_owned());
    }

    #[test]
    fn test_no_duplicated_params_different_pointers() {
        // Different pointers - no duplicates
        let ptr1 = ValuePointer::new(ValueCell::from(Primitive::U64(100)));
        let ptr2 = ValuePointer::new(ValueCell::from(Primitive::U64(200)));
        let ptr3 = ValuePointer::new(ValueCell::from(Primitive::U64(300)));

        let mut params = vec![
            StackValue::from(ptr1),
            StackValue::from(ptr2),
            StackValue::from(ptr3),
        ];

        let init = params.clone();
        let result = NativeFunction::<()>::verify_parameters(&mut params);
        assert!(result.is_ok());
        
        // All should remain as pointers (not cloned)
        assert!(!params[0].is_owned());
        assert!(!params[1].is_owned());
        assert!(!params[2].is_owned());

        // Verify values are still correct
        for (a, b) in params.iter().zip(init.iter()) {
            assert_eq!(a.ptr(), b.ptr());
        }
    }

    #[test]
    fn test_duplicated_params_same_pointer_twice() {
        // Same pointer used twice - should clone one
        let ptr = ValuePointer::new(ValueCell::from(Primitive::U64(100)));
        
        let mut params = vec![
            StackValue::from(ptr.clone()),
            StackValue::from(ptr.clone()),
        ];

        let result = NativeFunction::<()>::verify_parameters(&mut params);
        assert!(result.is_ok());
        
        // First paraemter stays as a pointer, the second is cloned (owned)
        assert!(!params[0].is_owned());
        assert!(params[1].is_owned());
    }

    #[test]
    fn test_duplicated_params_same_pointer_three_times() {
        // Same pointer used three times - should clone two of them
        let ptr = ValuePointer::new(ValueCell::from(Primitive::U64(100)));
        
        let mut params = vec![
            StackValue::from(ptr.clone()),
            StackValue::from(ptr.clone()),
            StackValue::from(ptr.clone()),
        ];

        let init = params.clone();
        let result = NativeFunction::<()>::verify_parameters(&mut params);
        assert!(result.is_ok());
        
        // First parameter stays as a pointer, the next two are cloned (owned)
        assert!(!params[0].is_owned());
        assert!(params[1].is_owned());
        assert!(params[2].is_owned());

        assert_eq!(params[0].ptr(), init[0].ptr());

        // Verify values are still correct
        for (a, b) in params.iter().zip(init.iter()).skip(1) {
            assert_ne!(a.ptr(), b.ptr());
        }
    }

    #[test]
    fn test_mixed_owned_and_duplicated_pointers() {
        // Mix of owned values and duplicated pointers
        let ptr = ValuePointer::new(ValueCell::from(Primitive::U64(100)));
        
        let mut params = vec![
            StackValue::from(Primitive::U64(999)), // Owned
            StackValue::from(ptr.clone()),          // Pointer
            StackValue::from(Primitive::U64(888)), // Owned
            StackValue::from(ptr.clone()),          // Same pointer again
        ];

        let result = NativeFunction::<()>::verify_parameters(&mut params);
        assert!(result.is_ok());
        
        // Owned values remain owned
        assert!(params[0].is_owned());
        assert!(params[2].is_owned());
        
        // First duplicated pointer is cloned, second remains
        assert!(!params[1].is_owned());  // Original pointer
        assert!(params[3].is_owned()); // Cloned
    }

    #[test]
    fn test_multiple_different_duplicated_pointers() {
        // Multiple sets of duplicated pointers
        let ptr1 = ValuePointer::new(ValueCell::from(Primitive::U64(100)));
        let ptr2 = ValuePointer::new(ValueCell::from(Primitive::U64(200)));
        
        let mut params = vec![
            StackValue::from(ptr1.clone()),
            StackValue::from(ptr2.clone()),
            StackValue::from(ptr1.clone()), // ptr1 duplicate
            StackValue::from(ptr2.clone()), // ptr2 duplicate
        ];

        let result = NativeFunction::<()>::verify_parameters(&mut params);
        assert!(result.is_ok());
        
        // First occurrence of each pointer stay as pointer, duplicates are cloned
        assert!(!params[0].is_owned());  // ptr1 first - remains pointer
        assert!(!params[1].is_owned());  // ptr2 first - remains pointer
        assert!(params[2].is_owned()); // ptr1 second - cloned
        assert!(params[3].is_owned()); // ptr2 second - cloned
    }

    #[test]
    fn test_empty_params() {
        // Empty parameter list should work fine
        let mut params: Vec<StackValue> = vec![];
        
        let result = NativeFunction::<()>::verify_parameters(&mut params);
        assert!(result.is_ok());
        assert!(params.is_empty());
    }

    #[test]
    fn test_single_param_pointer() {
        // Single pointer parameter - no duplicates possible
        let ptr = ValuePointer::new(ValueCell::from(Primitive::U64(100)));
        
        let mut params = vec![StackValue::from(ptr)];

        let result = NativeFunction::<()>::verify_parameters(&mut params);
        assert!(result.is_ok());
        
        // Should remain as pointer (not cloned)
        assert!(!params[0].is_owned());
    }

    #[test]
    fn test_complex_value_duplication() {
        // Test with complex values (arrays/objects)
        let complex_value = ValueCell::Object(vec![
            ValuePointer::new(ValueCell::from(Primitive::U64(1))),
            ValuePointer::new(ValueCell::from(Primitive::U64(2))),
        ]);
        let ptr = ValuePointer::new(complex_value);
        
        let mut params = vec![
            StackValue::from(ptr.clone()),
            StackValue::from(ptr.clone()),
        ];

        let result = NativeFunction::<()>::verify_parameters(&mut params);
        assert!(result.is_ok());
        
        // Last should be cloned, first remains pointer
        assert!(!params[0].is_owned());
        assert!(params[1].is_owned());
    }

    #[test]
    fn test_reverse_processing_order() {
        // Verify that processing happens in reverse order
        // (last occurrences keep the pointer, earlier ones are cloned)
        let ptr = ValuePointer::new(ValueCell::from(Primitive::U64(100)));
        
        let mut params = vec![
            StackValue::from(ptr.clone()), // Should be cloned (index 0)
            StackValue::from(ptr.clone()), // Should be cloned (index 1)
            StackValue::from(ptr.clone()), // Should remain pointer (index 2 - last)
        ];

        let result = NativeFunction::<()>::verify_parameters(&mut params);
        assert!(result.is_ok());

        // First two are pointers, last is cloned (owned)
        assert!(!params[0].is_owned());
        assert!(params[1].is_owned());
        assert!(params[2].is_owned());
        
        // Verify the values are still correct after cloning
        assert_eq!(params[0].as_u64().unwrap(), 100);
        assert_eq!(params[1].as_u64().unwrap(), 100);
        assert_eq!(params[2].as_u64().unwrap(), 100);
    }
}