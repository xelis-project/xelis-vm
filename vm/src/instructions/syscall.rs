use std::collections::VecDeque;

use xelis_environment::{Context, NativeFunction, SysCallResult};
use xelis_types::StackValue;

use crate::{stack::Stack, Backend, InstructionResult, VMError};

pub enum PerformSysCallHelper<'a, M> {
    Next {
        f: &'a NativeFunction<M>,
        params: VecDeque<StackValue>,
    },
    End(InstructionResult<'a, M>),
}

pub fn handle_perform_syscall<'a, 'ty, 'r, M>(
    backend: &Backend<'a, 'ty, 'r, M>,
    stack: &mut Stack,
    context: &mut Context<'ty, 'r>,
    result: SysCallResult<M>,
) -> Result<PerformSysCallHelper<'a, M>, VMError> {
    match result {
        SysCallResult::None => Ok(PerformSysCallHelper::End(InstructionResult::Nothing)),
        SysCallResult::AsyncCall { ptr, instance, params } => Ok(PerformSysCallHelper::End(InstructionResult::AsyncCall {
            ptr,
            instance,
            params,
        })),
        SysCallResult::Return(v) => {
            let memory_usage = v.as_ref().calculate_memory_usage(context.memory_left())?;
            context.increase_memory_usage_unchecked(memory_usage)?;
            stack.push_stack(v)?;
            Ok(PerformSysCallHelper::End(InstructionResult::Nothing))
        },
        SysCallResult::DynamicCall { ptr, params } => {
            let values = ptr.as_ref().as_vec()?;
            if values.len() != 3 {
                return Err(VMError::InvalidDynamicCall);
            }

            let id = values[0].as_u16()?;
            let syscall = values[1].as_bool()?;
            let from = values[2].as_u16()?;

            if syscall {
                let f = backend.environment.get_functions()
                    .get(id as usize)
                    .ok_or(VMError::UnknownSysCall(id))?;

                context.increase_gas_usage(f.get_cost())?;

                Ok(PerformSysCallHelper::Next {
                    f,
                    params,
                })
            } else {
                stack.extend_stack(params.into_iter().map(Into::into))?;
                Ok(PerformSysCallHelper::End(InstructionResult::InvokeDynamicChunk {
                    chunk_id: id as _,
                    from: from as _,
                }))
            }
        },
        SysCallResult::ModuleCall { module, metadata, chunk, params } => {
            stack.extend_stack(params.into_iter())?;
            Ok(PerformSysCallHelper::End(InstructionResult::AppendModule {
                module: module.into(),
                metadata: metadata.into(),
                chunk_id: chunk,
            }))
        },
    }
}

pub fn perform_syscall<'a, 'ty, 'r, M>(
    backend: &Backend<'a, 'ty, 'r, M>,
    f: &NativeFunction<M>,
    mut fn_params: VecDeque<StackValue>,
    stack: &mut Stack,
    context: &mut Context<'ty, 'r>,
) -> Result<InstructionResult<'a, M>, VMError> {
    let mut function = f;
    let m = backend.current()?;
    loop {
        let res = function.call_function(fn_params, m, context)?;
        match handle_perform_syscall(backend, stack, context, res)? {
            PerformSysCallHelper::Next { f, params } => {
                fn_params = params;
                function = f;
            },
            PerformSysCallHelper::End(result) => return Ok(result),
        }
    }
}
