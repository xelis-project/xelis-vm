use std::{any::Any, fmt, sync::Arc};

use futures::future::{BoxFuture, LocalBoxFuture};
use std::collections::HashMap;
use silex_bytecode::Module;
use silex_types::{Primitive, StackValue, Type, RuntimeValue, Heap, ValuePointer, OwnedValue};
use crate::scope::resume_owned;
use crate::{NativeState, NativeCallback, NativeScope, VMContext, Environment, ModuleMetadata};

use super::EnvironmentError;

pub type CallbackState = Box<dyn Any>;

pub type CallbackFunction<M> = fn(
    state: CallbackState,
    params: FnParams,
    heap: &mut Heap,
) -> Result<SysCallResult<M>, EnvironmentError>;

/// Callback type can be sync or async
pub enum CallbackType<M> {
    Sync(CallbackFunction<M>),
    Async(for<'a> fn(
        state: CallbackState,
        params: FnParams<'a>,
        heap: &'a mut Heap,
    ) -> LocalBoxFuture<'a, Result<SysCallResult<M>, EnvironmentError>>),
}

// SysCall Result represent all possible actions
// from a SysCall
pub enum SysCallResult<M> {
    // Do nothing
    None,
    // Add to the VM stack the returned value
    Return(StackValue),
    // Async Call instruction
    AsyncCall {
        ptr: AsyncHandler<M>,
        instance: bool,
        params: Vec<StackValue>,
    },
    // Call dynamically a chunk or a syscall fn
    DynamicCall {
        // Should contains [id: u16, is_syscall bool, from: u16]
        ptr: StackValue,
        params: Vec<StackValue>,
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
        params: Vec<StackValue>,
    },
    // Execute the said function ptr and after that call the callback function
    // Same as a dynamic call but with a callback after execution
    ExecuteAndCallback {
        // Should contains [id: u16, is_syscall bool, from: u16]
        ptr: StackValue,
        // Stack params for the above ptr call
        params: Vec<StackValue>,
        // The state can be anything
        state: CallbackState,
        // Count of required parameters for the callback
        callback_params_len: usize,
        // The callback function to call after execution
        callback: CallbackType<M>,
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

impl<M> From<RuntimeValue> for SysCallResult<M> {
    fn from(value: RuntimeValue) -> Self {
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
pub type FnInstance<'a> = Result<&'a mut StackValue, EnvironmentError>;
pub type FnParams<'a> = &'a mut Vec<StackValue>;
pub type OnCallSyncFn<M> = for<'a, 'ty, 'r> fn(
        FnInstance<'a>,
        FnParams<'a>,
        &'a ModuleMetadata<'_, M>,
        &'a mut VMContext<'ty, 'r>,
        &'a mut Heap,
    ) -> FnReturnType<M>;

pub type RawAsyncFn<M> = for<'a, 'ty, 'r> fn(
        FnInstance<'a>,
        FnParams<'a>,
        &'a ModuleMetadata<'_, M>,
        &'a mut VMContext<'ty, 'r>,
        &'a mut Heap,
    ) -> LocalBoxFuture<'a, FnReturnType<M>>;

pub type ScopedNativeFn<M> = for<'call, 'ty, 'r> fn(
    &mut NativeScope<'call, M>,
    &ModuleMetadata<'_, M>,
    &mut VMContext<'ty, 'r>,
) -> Result<(), EnvironmentError>;

pub type OnCallAsyncFn<M> = for<'a, 'ty, 'r> fn(
    Option<OwnedValue>,
    Vec<OwnedValue>,
    &'a ModuleMetadata<'_, M>,
    &'a mut VMContext<'ty, 'r>,
) -> BoxFuture<'a, Result<OwnedValue, EnvironmentError>>;

#[derive(Debug)]
enum RawHandler<M> {
    Sync(OnCallSyncFn<M>),
    Async(RawAsyncFn<M>),
}

impl<M> Copy for RawHandler<M> {}
impl<M> Clone for RawHandler<M> {
    fn clone(&self) -> Self {
        *self
    }
}

/// An explicitly trusted extension of the execution engine.
/// Safe native registration uses `FunctionHandler::Sync` or `Async` instead.
///
/// ```compile_fail
/// use silex_environment::{FunctionHandler, FnInstance, FnParams, FnReturnType, ModuleMetadata, VMContext, SysCallResult};
/// use silex_types::Heap;
/// fn raw(_: FnInstance, _: FnParams, _: &ModuleMetadata<'_, ()>, _: &mut VMContext, _: &mut Heap) -> FnReturnType<()> {
///     Ok(SysCallResult::None)
/// }
/// let handler = FunctionHandler::sync_unchecked(raw);
/// ```
#[derive(Debug)]
pub struct UncheckedFunction<M>(RawHandler<M>);

impl<M> Copy for UncheckedFunction<M> {}
impl<M> Clone for UncheckedFunction<M> {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Clone, Copy, Debug)]
pub enum FunctionHandler<M> {
    Sync(ScopedNativeFn<M>),
    Async(OnCallAsyncFn<M>),
    Unchecked(UncheckedFunction<M>),
}

impl<M> FunctionHandler<M> {
    /// # Safety
    /// The handler and every callback/opaque it creates must keep all runtime
    /// owners inside this execution, including across suspension and destruction.
    /// Heap mutations must preserve acyclicity by detaching inserted graphs.
    /// Any retained foreign state must be Send: moving the Rc family does not
    /// make thread-affine host resources safe to move.
    pub unsafe fn sync_unchecked(handler: OnCallSyncFn<M>) -> Self {
        Self::Unchecked(UncheckedFunction(RawHandler::Sync(handler)))
    }

    /// # Safety
    /// The complete returned future and all retained runtime owners must remain
    /// confined to this execution. Foreign captured data must be Send.
    /// Heap mutations must preserve acyclicity by detaching inserted graphs.
    pub unsafe fn async_unchecked(handler: RawAsyncFn<M>) -> Self {
        Self::Unchecked(UncheckedFunction(RawHandler::Async(handler)))
    }
}

pub enum AsyncHandler<M> {
    Owned(OnCallAsyncFn<M>),
    Prepared {
        future: BoxFuture<'static, Result<OwnedValue, EnvironmentError>>,
        state: NativeState,
        callback: NativeCallback<M>,
        retained: Vec<StackValue>,
    },
    Unchecked(UncheckedFunction<M>),
}

impl<M: 'static> AsyncHandler<M> {
    pub async fn call<'ty, 'r>(self, instance: FnInstance<'_>, params: FnParams<'_>, metadata: &ModuleMetadata<'_, M>, context: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> FnReturnType<M> {
        match self {
            Self::Prepared { future, state, callback, retained } => {
                let response = future.await?;
                let response = heap.import_owned(response);
                resume_owned(state, callback, retained, vec![response.into()], heap)
            }
            Self::Owned(handler) => {
                let instance = instance.ok().map(|value| heap.export(value.take())).transpose()?;
                let params = params.iter_mut().map(|value| heap.export(value.take()))
                    .collect::<Result<Vec<_>, _>>()?;
                let value = handler(instance, params, metadata, context).await?;
                Ok(SysCallResult::Return(heap.import_owned(value).into()))
            }
            Self::Unchecked(UncheckedFunction(RawHandler::Async(handler))) => {
                handler(instance, params, metadata, context, heap).await
            }
            Self::Unchecked(_) => unreachable!("synchronous handler cannot suspend"),
        }
    }
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

    // Execute the function
    pub fn call_function<'ty, 'r>(&self, mut parameters: Vec<StackValue>, metadata: &ModuleMetadata<'_, M>, context: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<SysCallResult<M>, EnvironmentError>
    where
        M: 'static,
    {
        if parameters.len() != self.parameters.len() + self.require_instance as usize {
            return Err(EnvironmentError::InvalidFnCall(parameters.len(), self.parameters.len()));
        }

        // Raw call to the function if its a sync function
        // otherwise return the async call
        match self.on_call {
            FunctionHandler::Unchecked(UncheckedFunction(RawHandler::Sync(on_call))) => {
                // Verify only the parameters for direct execution
                // For the async case, it will be done in the VM loop

                let mut instance = if self.is_on_instance() {
                    let instance = parameters.remove(0);
                    verify_instance_against_parameters(&instance, parameters.as_mut_slice(), heap)?;
                    Some(instance)
                } else { None };
                // Arguments must be released before the instance, including on errors.
                let mut arguments = parameters;
                (on_call)(instance.as_mut().ok_or_else(|| EnvironmentError::FnExpectedInstance), &mut arguments, metadata, context, heap)
            },
            FunctionHandler::Sync(handler) => {
                let instance = if self.is_on_instance() {
                    let instance = parameters.remove(0);
                    verify_instance_against_parameters(&instance, parameters.as_mut_slice(), heap)?;
                    Some(instance)
                } else {
                    None
                };
                let mut scope = NativeScope::new(heap, parameters, instance);
                handler(&mut scope, metadata, context)?;
                Ok(scope.finish())
            }
            FunctionHandler::Async(handler) => Ok(SysCallResult::AsyncCall {
                ptr: AsyncHandler::Owned(handler),
                instance: self.is_on_instance(),
                params: parameters
            }),
            FunctionHandler::Unchecked(handler) => Ok(SysCallResult::AsyncCall {
                ptr: AsyncHandler::Unchecked(handler),
                instance: self.is_on_instance(),
                params: parameters,
            }),
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

// Verify that the instance is not duplicated in the parameters
// Preserve the existing argument detachment semantics. Heap borrowing itself
// enforces Rust reference exclusivity.
pub fn verify_instance_against_parameters(instance: &StackValue, parameters: &mut [StackValue], heap: &mut Heap) -> Result<(), EnvironmentError> {
    if !instance.is_owned() {
        // Check if the instance is duplicated in the parameters
        let instance_ptr = instance.id();
        for param in parameters.iter_mut() {
            if param.id() == instance_ptr {
                let owned = param.to_owned(heap)?;
                *param = owned;
            }
        }
    }

    Ok(())
}

// Explicitly detach duplicate parameters when independent argument values are
// wanted. This is no longer needed to enforce Rust aliasing rules.
pub fn verify_parameters(parameters: &mut [StackValue], heap: &mut Heap) -> Result<(), EnvironmentError> {
    let mut pointers: HashMap<ValuePointer, usize> = HashMap::new();

    for param in parameters.iter() {
        if let Some(ptr) = param.id() {
            if let Some(count) = pointers.get_mut(ptr) {
                *count += 1;
            } else {
                pointers.insert(ptr.clone(), 1);
            }
        }
    }

    // Now, for each parameter that is a pointer
    // check if its count is more than 1
    for param in parameters.iter_mut().rev() {
        if let Some(count) = param.id().and_then(|ptr| pointers.get_mut(ptr)) {
            if *count > 1 {
                // Clone the parameter to make it owned
                let owned = param.to_owned(heap)?;
                *param = owned;
                *count -= 1;
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use silex_types::{Primitive, PrimitiveInteger};

    #[test]
    fn test_no_duplicated_params_all_owned() {
        let mut heap = unsafe { Heap::new() };
        // All parameters are owned - no duplicates possible
        let mut params = vec![
            StackValue::from(Primitive::Integer(PrimitiveInteger::U64(100))),
            StackValue::from(Primitive::Integer(PrimitiveInteger::U64(200))),
            StackValue::from(Primitive::Integer(PrimitiveInteger::U64(300))),
        ];

        let result = verify_parameters(&mut params, &mut heap);
        assert!(result.is_ok());
        
        // All should remain owned
        assert!(params[0].is_owned());
        assert!(params[1].is_owned());
        assert!(params[2].is_owned());
    }

    #[test]
    fn test_no_duplicated_params_different_pointers() {
        let mut heap = unsafe { Heap::new() };
        // Different pointers - no duplicates
        let ptr1 = heap.insert(RuntimeValue::from(Primitive::Integer(PrimitiveInteger::U64(100))));
        let ptr2 = heap.insert(RuntimeValue::from(Primitive::Integer(PrimitiveInteger::U64(200))));
        let ptr3 = heap.insert(RuntimeValue::from(Primitive::Integer(PrimitiveInteger::U64(300))));

        let mut params = vec![
            StackValue::from(ptr1),
            StackValue::from(ptr2),
            StackValue::from(ptr3),
        ];

        let init = params.clone();
        let result = verify_parameters(&mut params, &mut heap);
        assert!(result.is_ok());
        
        // All should remain as pointers (not cloned)
        assert!(!params[0].is_owned());
        assert!(!params[1].is_owned());
        assert!(!params[2].is_owned());

        // Verify values are still correct
        for (a, b) in params.iter().zip(init.iter()) {
            assert_eq!(a.id(), b.id());
        }
    }

    #[test]
    fn test_duplicated_params_same_pointer_twice() {
        let mut heap = unsafe { Heap::new() };
        // Same pointer used twice - should clone one
        let ptr = heap.insert(RuntimeValue::from(Primitive::Integer(PrimitiveInteger::U64(100))));
        
        let mut params = vec![
            StackValue::from(ptr.clone()),
            StackValue::from(ptr.clone()),
        ];

        let result = verify_parameters(&mut params, &mut heap);
        assert!(result.is_ok());
        
        // First paraemter stays as a pointer, the second is cloned (owned)
        assert!(!params[0].is_owned());
        assert!(params[1].is_owned());
    }

    #[test]
    fn test_duplicated_params_same_pointer_three_times() {
        let mut heap = unsafe { Heap::new() };
        // Same pointer used three times - should clone two of them
        let ptr = heap.insert(RuntimeValue::from(Primitive::Integer(PrimitiveInteger::U64(100))));
        
        let mut params = vec![
            StackValue::from(ptr.clone()),
            StackValue::from(ptr.clone()),
            StackValue::from(ptr.clone()),
        ];

        let init = params.clone();
        let result = verify_parameters(&mut params, &mut heap);
        assert!(result.is_ok());
        
        // First parameter stays as a pointer, the next two are cloned (owned)
        assert!(!params[0].is_owned());
        assert!(params[1].is_owned());
        assert!(params[2].is_owned());

        assert_eq!(params[0].id(), init[0].id());

        // Verify values are still correct
        for (a, b) in params.iter().zip(init.iter()).skip(1) {
            assert_ne!(a.id(), b.id());
        }
    }

    #[test]
    fn test_mixed_owned_and_duplicated_pointers() {
        let mut heap = unsafe { Heap::new() };
        // Mix of owned values and duplicated pointers
        let ptr = heap.insert(RuntimeValue::from(Primitive::Integer(PrimitiveInteger::U64(100))));
        
        let mut params = vec![
            StackValue::from(Primitive::Integer(PrimitiveInteger::U64(999))), // Owned
            StackValue::from(ptr.clone()),          // Pointer
            StackValue::from(Primitive::Integer(PrimitiveInteger::U64(888))), // Owned
            StackValue::from(ptr.clone()),          // Same pointer again
        ];

        let result = verify_parameters(&mut params, &mut heap);
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
        let mut heap = unsafe { Heap::new() };
        // Multiple sets of duplicated pointers
        let ptr1 = heap.insert(RuntimeValue::from(Primitive::Integer(PrimitiveInteger::U64(100))));
        let ptr2 = heap.insert(RuntimeValue::from(Primitive::Integer(PrimitiveInteger::U64(200))));
        
        let mut params = vec![
            StackValue::from(ptr1.clone()),
            StackValue::from(ptr2.clone()),
            StackValue::from(ptr1.clone()), // ptr1 duplicate
            StackValue::from(ptr2.clone()), // ptr2 duplicate
        ];

        let result = verify_parameters(&mut params, &mut heap);
        assert!(result.is_ok());
        
        // First occurrence of each pointer stay as pointer, duplicates are cloned
        assert!(!params[0].is_owned());  // ptr1 first - remains pointer
        assert!(!params[1].is_owned());  // ptr2 first - remains pointer
        assert!(params[2].is_owned()); // ptr1 second - cloned
        assert!(params[3].is_owned()); // ptr2 second - cloned
    }

    #[test]
    fn test_empty_params() {
        let mut heap = unsafe { Heap::new() };
        // Empty parameter list should work fine
        let mut params: Vec<StackValue> = vec![];
        
        let result = verify_parameters(&mut params, &mut heap);
        assert!(result.is_ok());
        assert!(params.is_empty());
    }

    #[test]
    fn test_single_param_pointer() {
        let mut heap = unsafe { Heap::new() };
        // Single pointer parameter - no duplicates possible
        let ptr = heap.insert(RuntimeValue::from(Primitive::Integer(PrimitiveInteger::U64(100))));
        
        let mut params = vec![StackValue::from(ptr)];

        let result = verify_parameters(&mut params, &mut heap);
        assert!(result.is_ok());
        
        // Should remain as pointer (not cloned)
        assert!(!params[0].is_owned());
    }

    #[test]
    fn test_complex_value_duplication() {
        let mut heap = unsafe { Heap::new() };
        // Test with complex values (arrays/objects)
        let complex_value = RuntimeValue::Object(vec![
            heap.insert(RuntimeValue::from(Primitive::Integer(PrimitiveInteger::U64(1)))),
            heap.insert(RuntimeValue::from(Primitive::Integer(PrimitiveInteger::U64(2)))),
        ]);
        let ptr = heap.insert(complex_value);
        
        let mut params = vec![
            StackValue::from(ptr.clone()),
            StackValue::from(ptr.clone()),
        ];

        let result = verify_parameters(&mut params, &mut heap);
        assert!(result.is_ok());
        
        // Last should be cloned, first remains pointer
        assert!(!params[0].is_owned());
        assert!(params[1].is_owned());
    }

    #[test]
    fn test_reverse_processing_order() {
        let mut heap = unsafe { Heap::new() };
        // Verify that processing happens in reverse order
        // (last occurrences keep the pointer, earlier ones are cloned)
        let ptr = heap.insert(RuntimeValue::from(Primitive::Integer(PrimitiveInteger::U64(100))));
        
        let mut params = vec![
            StackValue::from(ptr.clone()), // Should be cloned (index 0)
            StackValue::from(ptr.clone()), // Should be cloned (index 1)
            StackValue::from(ptr.clone()), // Should remain pointer (index 2 - last)
        ];

        let result = verify_parameters(&mut params, &mut heap);
        assert!(result.is_ok());

        // First two are pointers, last is cloned (owned)
        assert!(!params[0].is_owned());
        assert!(params[1].is_owned());
        assert!(params[2].is_owned());
        
        // Verify the values are still correct after cloning
        assert_eq!(params[0].as_u64(&heap).unwrap(), 100);
        assert_eq!(params[1].as_u64(&heap).unwrap(), 100);
        assert_eq!(params[2].as_u64(&heap).unwrap(), 100);
    }
}