use std::{cell::Cell, marker::PhantomData, rc::Rc, sync::Arc, any::Any, mem};
use futures::future::BoxFuture;
use silex_bytecode::Module;
use silex_types::{Heap, OwnedValue, Primitive, RuntimeValue, StackValue, Opaque};
use crate::{Environment, EnvironmentError, SysCallResult, AsyncHandler, CallbackType, CallbackState, FnParams};

/// A non-owning value valid only during one native call.
///
/// ```compile_fail
/// use silex_environment::{NativeScope, ValueView};
/// fn escape<'call>(scope: &NativeScope<'call, ()>) -> ValueView<'static> {
///     scope.argument(0).unwrap()
/// }
/// ```
///
/// ```compile_fail
/// use silex_environment::NativeScope;
/// fn parallel(scope: &NativeScope<'_, ()>) {
///     let view = scope.argument(0).unwrap();
///     std::thread::scope(|threads| {
///         threads.spawn(move || { drop(view); });
///     });
/// }
/// ```
///
/// ```compile_fail
/// use silex_environment::NativeScope;
/// fn retain(scope: &NativeScope<'_, ()>) {
///     let view = scope.argument(0).unwrap();
///     let state: Box<dyn std::any::Any + Send + Sync> = Box::new(view);
/// }
/// ```
/// It cannot be sent to a thread, retained in host state, or resolved without
/// the scope that created it.
#[derive(Clone, Copy)]
pub struct ValueView<'call> {
    index: usize,
    brand: PhantomData<Cell<&'call ()>>,
    local: PhantomData<Rc<()>>,
}

pub type NativeState = Box<dyn Any + Send + Sync>;
pub type NativeCallback<M> = for<'call> fn(
    NativeState,
    &mut NativeScope<'call, M>,
) -> Result<(), EnvironmentError>;

struct CallbackOwner<M> {
    state: NativeState,
    retained: Vec<StackValue>,
    callback: NativeCallback<M>,
}

enum Action<M> {
    Return(usize),
    Call {
        values: Vec<usize>,
        callback: Option<(NativeState, NativeCallback<M>, usize, usize)>,
    },
    Module {
        module: Arc<Module>,
        metadata: Arc<M>,
        environment: Option<Arc<Environment<M>>>,
        chunk: u16,
        params: Vec<usize>,
    },
    Await {
        future: BoxFuture<'static, Result<OwnedValue, EnvironmentError>>,
        state: NativeState,
        callback: NativeCallback<M>,
        retained: Vec<usize>,
    },
}

/// The safe native boundary. Runtime owners and the heap remain private.
///
/// ```compile_fail
/// use silex_environment::NativeScope;
/// fn steal(scope: &mut NativeScope<'_, ()>) {
///     let heap = &mut scope.heap;
/// }
/// ```
///
/// Borrows of payload data exclude mutation, even through a copied view.
///
/// ```compile_fail
/// use silex_environment::NativeScope;
/// use silex_types::OwnedValue;
/// fn alias(scope: &mut NativeScope<'_, ()>) {
///     let view = scope.argument(0).unwrap();
///     let text = scope.as_string(view).unwrap();
///     scope.replace(view, OwnedValue::from(42u64)).unwrap();
///     println!("{text}");
/// }
/// ```
pub struct NativeScope<'call, M> {
    heap: &'call mut Heap,
    roots: Vec<StackValue>,
    arguments: usize,
    instance: Option<usize>,
    retained: Vec<usize>,
    result: Option<Action<M>>,
    brand: PhantomData<Cell<&'call M>>,
}

impl<'call, M: 'static> NativeScope<'call, M> {
    pub(crate) fn new(heap: &'call mut Heap, mut roots: Vec<StackValue>, instance: Option<StackValue>) -> Self {
        let arguments = roots.len();
        let instance = instance.map(|value| {
            roots.push(value);
            arguments
        });
        Self {
            heap,
            roots,
            arguments,
            instance,
            retained: Vec::new(),
            result: None,
            brand: PhantomData,
        }
    }

    fn view(index: usize) -> ValueView<'call> {
        ValueView {
            index,
            brand: PhantomData,
            local: PhantomData,
        }
    }

    pub fn argument(&self, index: usize) -> Result<ValueView<'call>, EnvironmentError> {
        if index >= self.arguments {
            return Err(EnvironmentError::InvalidParameter);
        }
        Ok(Self::view(index))
    }

    pub fn arguments_len(&self) -> usize {
        self.arguments
    }

    pub fn instance(&self) -> Result<ValueView<'call>, EnvironmentError> {
        self.instance.map(Self::view).ok_or_else(|| EnvironmentError::MissingInstanceFnCall)
    }

    fn value(&self, view: ValueView<'call>) -> &RuntimeValue {
        self.roots[view.index].as_ref(self.heap)
    }

    pub fn primitive(&self, view: ValueView<'call>) -> Result<&Primitive, EnvironmentError> {
        Ok(self.value(view).as_value()?)
    }

    pub fn as_u64(&self, view: ValueView<'call>) -> Result<u64, EnvironmentError> {
        Ok(self.value(view).as_u64()?)
    }

    pub fn as_u32(&self, view: ValueView<'call>) -> Result<u32, EnvironmentError> {
        Ok(self.value(view).as_u32()?)
    }

    pub fn as_bool(&self, view: ValueView<'call>) -> Result<bool, EnvironmentError> {
        Ok(self.value(view).as_bool()?)
    }

    pub fn as_string(&self, view: ValueView<'call>) -> Result<&str, EnvironmentError> {
        Ok(self.value(view).as_string()?)
    }

    pub fn as_bytes(&self, view: ValueView<'call>) -> Result<&[u8], EnvironmentError> {
        Ok(self.value(view).as_bytes()?)
    }

    pub fn opaque<T: Opaque>(&self, view: ValueView<'call>) -> Result<&T, EnvironmentError> {
        Ok(self.value(view).as_opaque_type::<T>()?)
    }

    pub fn opaque_mut<T: Opaque>(&mut self, view: ValueView<'call>) -> Result<&mut T, EnvironmentError> {
        Ok(self.roots[view.index].as_opaque_type_mut::<T>(self.heap)?)
    }

    /// Check binary serialization capabilities without copying runtime values.
    pub fn is_serializable(&self, view: ValueView<'call>) -> bool {
        self.value(view).is_serializable(self.heap)
    }

    /// Check JSON serialization capabilities without copying runtime values.
    pub fn is_json_serializable(&self, view: ValueView<'call>) -> bool {
        self.value(view).is_json_serializable(self.heap)
    }

    pub fn snapshot(&self, view: ValueView<'call>) -> Result<OwnedValue, EnvironmentError> {
        Ok(self.heap.snapshot(self.value(view))?)
    }

    /// Consume this scope root as portable data, moving unique buffers. Copied
    /// views of the consumed root subsequently observe null.
    pub fn take_owned(&mut self, view: ValueView<'call>) -> Result<OwnedValue, EnvironmentError> {
        let value = self.roots[view.index].take();
        Ok(self.heap.export(value)?)
    }

    pub fn array_len(&self, view: ValueView<'call>) -> Result<usize, EnvironmentError> {
        Ok(self.value(view).as_vec()?.len())
    }

    pub fn array_get(&mut self, view: ValueView<'call>, index: usize) -> Result<ValueView<'call>, EnvironmentError> {
        let values = self.value(view).as_vec()?;
        let value = values.get(index).ok_or_else(|| EnvironmentError::OutOfBounds(index, values.len()))?.clone();
        self.roots.push(value.into());
        Ok(Self::view(self.roots.len() - 1))
    }

    pub fn array_pop(&mut self, view: ValueView<'call>) -> Result<ValueView<'call>, EnvironmentError> {
        let value = self.roots[view.index].as_mut_vec(self.heap)?.pop()
            .map(StackValue::from).unwrap_or_default();
        self.roots.push(value);
        Ok(Self::view(self.roots.len() - 1))
    }

    pub fn owned(&mut self, value: OwnedValue) -> Result<ValueView<'call>, EnvironmentError> {
        let value = self.heap.import_owned(value);
        self.roots.push(value.into());
        Ok(Self::view(self.roots.len() - 1))
    }

    pub fn replace(&mut self, view: ValueView<'call>, value: OwnedValue) -> Result<(), EnvironmentError> {
        let value = self.heap.import_owned(value);
        self.heap.replace_stack(&mut self.roots[view.index], value);
        Ok(())
    }

    /// The result moves into the engine after the call, preserving its owned
    /// versus borrowed representation without cloning the result payload.
    pub fn return_value(&mut self, view: ValueView<'call>) {
        self.result = Some(Action::Return(view.index));
    }

    pub fn return_owned(&mut self, value: OwnedValue) -> Result<(), EnvironmentError> {
        let view = self.owned(value)?;
        self.return_value(view);
        Ok(())
    }

    pub fn retained(&self, index: usize) -> Result<ValueView<'call>, EnvironmentError> {
        self.retained.get(index).copied().map(Self::view).ok_or_else(|| EnvironmentError::InvalidParameter)
    }

    pub fn is_null(&self, view: ValueView<'call>) -> bool {
        self.value(view).is_null()
    }

    pub fn ptr_eq(&self, left: ValueView<'call>, right: ValueView<'call>) -> bool {
        self.roots[left.index].ptr_eq(&self.roots[right.index])
    }

    pub fn array_push(&mut self, array: ValueView<'call>, value: ValueView<'call>) -> Result<(), EnvironmentError> {
        if self.array_len(array)? >= u32::MAX as usize {
            return Err(EnvironmentError::OutOfMemory);
        }
        // Insert a detached graph so an ancestor cannot be reinserted below itself.
        let value = self.roots[value.index].as_ref(self.heap).clone();
        let value = self.heap.detach(value)?;
        let id = self.heap.insert(value);
        self.roots[array.index].as_mut_vec(self.heap)?.push(id);
        Ok(())
    }

    pub fn call(&mut self, function: ValueView<'call>, params: &[ValueView<'call>]) {
        let mut values = vec![function.index];
        values.extend(params.iter().map(|view| view.index));
        self.result = Some(Action::Call { values, callback: None });
    }

    pub fn call_and_resume(&mut self, function: ValueView<'call>, params: &[ValueView<'call>], retained: &[ValueView<'call>], state: NativeState, results: usize, callback: NativeCallback<M>) {
        let mut values = vec![function.index];
        values.extend(params.iter().map(|view| view.index));
        values.extend(retained.iter().map(|view| view.index));
        self.result = Some(Action::Call { values, callback: Some((state, callback, results, retained.len())) });
    }

    pub fn call_module(&mut self, module: Arc<Module>, metadata: Arc<M>, environment: Option<Arc<Environment<M>>>, chunk: u16, params: &[ValueView<'call>]) {
        self.result = Some(Action::Module {
            module,
            metadata,
            environment,
            chunk,
            params: params.iter().map(|view| view.index).collect(),
        });
    }

    /// Suspend on owned, Send I/O; retained VM values stay inside the engine.
    /// The resume callback receives a fresh scope with the response as argument 0.
    pub fn await_and_resume(&mut self, future: BoxFuture<'static, Result<OwnedValue, EnvironmentError>>, retained: &[ValueView<'call>], state: NativeState, callback: NativeCallback<M>) {
        self.result = Some(Action::Await {
            future,
            retained: retained.iter().map(|view| view.index).collect(),
            state,
            callback,
        });
    }

    fn take_values(&mut self, indices: &[usize]) -> Vec<StackValue> {
        let mut uses = vec![0; self.roots.len()];
        for &index in indices {
            uses[index] += 1;
        }
        indices.iter().map(|&index| {
            uses[index] -= 1;
            if uses[index] == 0 {
                self.roots[index].take()
            } else {
                self.roots[index].clone()
            }
        }).collect()
    }

    pub(crate) fn finish(mut self) -> SysCallResult<M> {
        match self.result.take() {
            Some(Action::Return(index)) => SysCallResult::Return(self.roots[index].take()),
            Some(Action::Call { values, callback }) => {
                let mut values = self.take_values(&values);
                let ptr = values.remove(0);
                match callback {
                    Some((state, callback, callback_params_len, retained)) => {
                        let retained = values.split_off(values.len() - retained);
                        SysCallResult::ExecuteAndCallback {
                            ptr,
                            params: values,
                            state: Box::new(CallbackOwner { state, retained, callback }),
                            callback_params_len,
                            callback: CallbackType::Sync(resume),
                        }
                    }
                    None => SysCallResult::DynamicCall { ptr, params: values },
                }
            }
            Some(Action::Module { module, metadata, environment, chunk, params }) => {
                let params = self.take_values(&params);
                SysCallResult::ModuleCall { module, metadata, environment, chunk, params }
            }
            Some(Action::Await { future, state, callback, retained }) => {
                let retained = self.take_values(&retained);
                SysCallResult::AsyncCall {
                    ptr: AsyncHandler::Prepared { future, state, callback, retained },
                    instance: false,
                    params: Vec::new(),
                }
            }
            None => SysCallResult::None,
        }
    }
}

fn resume<M: 'static>(state: CallbackState, params: FnParams<'_>, heap: &mut Heap) -> Result<SysCallResult<M>, EnvironmentError> {
    let owner = state.downcast::<CallbackOwner<M>>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
    resume_owned(owner.state, owner.callback, owner.retained, mem::take(params), heap)
}

pub(crate) fn resume_owned<M: 'static>(state: NativeState, callback: NativeCallback<M>, retained: Vec<StackValue>, params: Vec<StackValue>, heap: &mut Heap) -> Result<SysCallResult<M>, EnvironmentError> {
    let mut scope = NativeScope::new(heap, params, None);
    for value in retained {
        scope.retained.push(scope.roots.len());
        scope.roots.push(value);
    }
    callback(state, &mut scope)?;
    Ok(scope.finish())
}
