use silex_types::{Heap, StackValue};
use silex_environment::{CallbackType, CallbackState, VMContext, ModuleMetadata, SysCallResult};

use crate::{stack::Stack, Backend, InstructionResult, VMError};

pub struct Callback<M> {
    pub state: CallbackState,
    pub params_len: usize,
    pub callback: CallbackType<M>,
}

/// Invoke one native function, preserving its per-call gas charge.
pub fn call_syscall<'a, 'ty, 'r, M: 'static>(
    backend: &Backend<'a, 'ty, 'r, M>,
    id: u16,
    params: Vec<StackValue>,
    context: &mut VMContext<'ty, 'r>,
    heap: &mut Heap,
) -> Result<SysCallResult<M>, VMError> {
    let metadata = backend.current()?;
    let function = metadata.environment.get_functions().get(id as usize)
        .ok_or_else(|| VMError::UnknownSysCall(id))?;
    context.increase_gas_usage(function.get_cost())?;
    Ok(function.call_function(params, metadata, context, heap)?)
}

/// Complete synchronous calls, async returns, and callback results through the
/// same loop. Further synchronous native calls do not grow the Rust stack.
pub fn complete_syscall<'a, 'ty, 'r, M: 'static>(
    backend: &Backend<'a, 'ty, 'r, M>,
    stack: &mut Stack<M>,
    context: &mut VMContext<'ty, 'r>,
    mut result: SysCallResult<M>,
    metadata: &ModuleMetadata<'a, M>,
    heap: &mut Heap,
) -> Result<InstructionResult<'a, M>, VMError> {
    loop {
        let (id, params) = match result {
            SysCallResult::None => return Ok(InstructionResult::Nothing),
            SysCallResult::AsyncCall { ptr, instance, params } => {
                return Ok(InstructionResult::AsyncCall { ptr, instance, params });
            }
            SysCallResult::Return(value) => {
                let memory = value.estimate_memory_usage(heap, context.memory_left())?;
                context.increase_memory_usage_unchecked(memory)?;
                stack.push_stack(value)?;
                return Ok(InstructionResult::Nothing);
            }
            SysCallResult::DynamicCall { ptr, params } => {
                let values = ptr.as_ref(heap).as_vec()?;
                if values.len() != 3 {
                    return Err(VMError::InvalidDynamicCall);
                }
                let id = values[0].as_ref(heap).as_u16()?;
                let syscall = values[1].as_ref(heap).as_bool()?;
                let from = values[2].as_ref(heap).as_u16()?;
                if syscall {
                    (id, params)
                } else {
                    for param in params {
                        let memory = param.estimate_memory_usage(heap, context.memory_left())?;
                        context.increase_memory_usage_unchecked(memory)?;
                        stack.push_stack(param)?;
                    }
                    return Ok(InstructionResult::InvokeDynamicChunk {
                        chunk_id: id as usize,
                        from: from as usize,
                    });
                }
            }
            SysCallResult::ModuleCall { module, metadata: new_metadata, environment, chunk, params } => {
                stack.extend_stack(params.into_iter())?;
                return Ok(InstructionResult::AppendModule {
                    module: ModuleMetadata {
                        module: module.into(),
                        metadata: new_metadata.into(),
                        environment: environment.map(Into::into).unwrap_or_else(|| metadata.environment.clone()),
                    },
                    chunk_id: chunk,
                });
            }
            SysCallResult::ExecuteAndCallback { ptr, params, state, callback_params_len, callback } => {
                let values = ptr.as_ref(heap).as_vec()?;
                if values.len() != 3 {
                    return Err(VMError::InvalidDynamicCall);
                }
                let id = values[0].as_ref(heap).as_u16()?;
                let syscall = values[1].as_ref(heap).as_bool()?;
                let from = values[2].as_ref(heap).as_u16()?;
                stack.push_callback(Callback {
                    state,
                    params_len: callback_params_len,
                    callback,
                })?;
                if syscall {
                    (id, params)
                } else {
                    for param in params.into_iter().rev() {
                        let memory = param.estimate_memory_usage(heap, context.memory_left())?;
                        context.increase_memory_usage_unchecked(memory)?;
                        stack.push_stack(param)?;
                    }
                    return Ok(InstructionResult::InvokeDynamicChunk {
                        chunk_id: id as usize,
                        from: from as usize,
                    });
                }
            }
        };
        // The match releases the consumed function pointer before invoking the
        // next native, preserving the previous ownership and drop ordering.
        result = call_syscall(backend, id, params, context, heap)?;
    }
}
