use std::collections::VecDeque;

use xelis_environment::{Context, ModuleMetadata, SysCallResult};
use xelis_types::StackValue;

use crate::{stack::Stack, Backend, InstructionResult, VMError};

pub enum PerformSysCallHelper<'a, M> {
    Next {
        // syscall id
        f: u16,
        params: VecDeque<StackValue>,
    },
    End(InstructionResult<'a, M>),
}

pub fn handle_perform_syscall<'a, 'ty, 'r, M>(
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
                Ok(PerformSysCallHelper::Next {
                    f: id,
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
        SysCallResult::ModuleCall { module, metadata, environment, chunk, params } => {
            stack.extend_stack(params.into_iter())?;
            Ok(PerformSysCallHelper::End(InstructionResult::AppendModule {
                module: ModuleMetadata {
                    module: module.into(),
                    metadata: metadata.into(),
                    environment: environment.into(),
                },
                chunk_id: chunk,
            }))
        },
    }
}

pub fn perform_syscall<'a, 'ty, 'r, M>(
    backend: &Backend<'a, 'ty, 'r, M>,
    mut syscall: PerformSysCallHelper<'a, M>,
    stack: &mut Stack,
    context: &mut Context<'ty, 'r>,
) -> Result<InstructionResult<'a, M>, VMError> {
    let m = backend.current()?;
    loop {
        match syscall {
            PerformSysCallHelper::Next { f, params } => {
                let f = m
                    .environment
                    .get_functions()
                    .get(f as usize)
                    .ok_or(VMError::UnknownSysCall(f))?;

                context.increase_gas_usage(f.get_cost())?;
                let res = f.call_function(params, m, context)?;
                syscall = handle_perform_syscall(
                    stack,
                    context,
                    res,
                )?;
            },
            PerformSysCallHelper::End(result) => return Ok(result),
        }
    }
}
