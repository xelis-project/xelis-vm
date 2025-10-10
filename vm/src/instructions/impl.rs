use std::collections::VecDeque;

use xelis_types::Either;

use crate::{
    debug,
    perform_syscall,
    stack::Stack,
    Backend,
    ChunkContext,
    ChunkManager,
    ChunkReader,
    Context,
    VMError
};
use super::InstructionResult;

pub fn constant<'a: 'r, 'ty: 'a, 'r, M>(backend: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let index = reader.read_u16()? as usize;
    debug!("constant load at {}", index);

    let constant = backend.get_constant_with_id(index)?;

    let memory_usage = constant.calculate_memory_usage(context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack(constant.clone().into())?;
    Ok(InstructionResult::Nothing)
}

pub fn subload<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let index = reader.read_u8()?;
    debug!("subload at {}", index);

    let path = stack.pop_stack()?;
    let sub = path.get_at_index(index as usize)?;

    let memory_usage = sub.as_ref()
        .calculate_memory_usage(context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack_unchecked(sub);

    Ok(InstructionResult::Nothing)
}

pub fn copy<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, _: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("copy");
    let value = stack.last_stack()?;

    let memory_usage = value.as_ref()
        .calculate_memory_usage(context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack(value.to_owned())?;

    Ok(InstructionResult::Nothing)
}

pub fn copy_n<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let index = reader.read_u8()?;
    debug!("copy at {}", index);

    let value = stack.get_stack_at(index as usize)?;

    let memory_usage = value.as_ref()
        .calculate_memory_usage(context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack(value.to_owned())?;

    Ok(InstructionResult::Nothing)
}

pub fn to_owned<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, _: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("to_owned");

    let value = stack.last_mut_stack()?;
    if value.make_owned() {
        let memory = value.as_ref().calculate_memory_usage(context.memory_left())?;
        context.increase_memory_usage_unchecked(memory)?;
    }

    Ok(InstructionResult::Nothing)
}

pub fn pop<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("pop");

    stack.pop_stack()?;
    Ok(InstructionResult::Nothing)
}

pub fn pop_n<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let n = reader.read_u8()?;
    debug!("pop n {}", n);

    stack.pop_stack_n(n)?;
    Ok(InstructionResult::Nothing)
}

pub fn swap<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let index = reader.read_u8()?;
    debug!("swap at {}", index);

    stack.swap_stack(index as usize)?;
    Ok(InstructionResult::Nothing)
}

pub fn swap2<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let index_a = reader.read_u8()?;
    let index_b = reader.read_u8()?;

    debug!("swap at {} {}", index_a, index_b);

    stack.swap_stack_both(index_a as usize, index_b as usize)?;
    Ok(InstructionResult::Nothing)
}

pub fn array_call<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, _: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let value = stack.pop_stack()?;
    let index = value.as_u32()?;
    debug!("array call at {}", index);

    let value = stack.pop_stack()?;
    let sub = value.get_at_index(index as usize)?;

    let memory_usage = sub.as_ref()
        .calculate_memory_usage(context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack_unchecked(sub);
    Ok(InstructionResult::Nothing)
}

pub fn invoke_chunk<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let id = reader.read_u16()?;
    let args = reader.read_u8()? as usize;
    internal_invoke_chunk(stack, id, args, None)
}

#[inline]
pub fn internal_invoke_chunk<'a, M>(stack: &mut Stack, id: u16, args: usize, from: Option<u16>) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("invoke chunk: {}, args: {}", id, args);

    // We need to reverse the order of the arguments
    let inner = stack.get_inner_mut();
    let len = inner.len();
    if len < args {
        return Err(VMError::NotEnoughArguments);
    }

    let slice = &mut stack.get_inner_mut()[len - args..len];
    slice.reverse();

    Ok(if let Some(from) = from {
        InstructionResult::InvokeDynamicChunk {
            chunk_id: id as _,
            from: from as _,
        }
    } else {
        InstructionResult::InvokeChunk(id)
    })
}

pub fn syscall<'a: 'r, 'ty: 'a, 'r, M>(backend: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let id = reader.read_u16()?;
    internal_syscall(backend, id, stack, context)
}

fn internal_syscall<'a: 'r, 'ty: 'a, 'r, M>(backend: &Backend<'a, 'ty, 'r, M>, id: u16, stack: &mut Stack, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("syscall: {}", id);

    let f = backend.environment.get_functions()
        .get(id as usize)
        .ok_or(VMError::UnknownSysCall(id))?;

    context.increase_gas_usage(f.get_cost())?;

    let args = f.get_parameters().len();
    let mut arguments = VecDeque::with_capacity(args);
    for _ in 0..args + f.is_on_instance() as usize {
        arguments.push_front(stack.pop_stack()?);
    }

    perform_syscall(backend, f, arguments, stack, context)
}

pub fn dynamic_call<'a: 'r, 'ty: 'a, 'r, M>(backend: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let value = stack.pop_stack()?;
    let ptr = value.as_ref()
        .as_fn_ptr()?;

    let args = reader.read_u8()? as usize;

    match ptr {
        Either::Left(chunk_id) => internal_invoke_chunk(stack, chunk_id, args, None),
        Either::Right(values) => {
            if values.len() != 3 {
                return Err(VMError::InvalidDynamicCall)
            }

            let id = values[0].as_u16()?;
            let syscall = values[1].as_bool()?;
            let from = values[2].as_u16()?;

            debug!("dynamic call: {}, syscall: {}, args: {}", id, syscall, args);

            if syscall {
                internal_syscall(backend, id, stack, context)
            } else {
                internal_invoke_chunk(stack, id, args, Some(from))
            }
        }
    }
}

pub fn capture_context<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, _: &mut Stack, manager: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    manager.set_context(ChunkContext::ShouldKeep);
    Ok(InstructionResult::Nothing)
}