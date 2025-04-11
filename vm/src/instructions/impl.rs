use std::collections::VecDeque;

use crate::{stack::Stack, Backend, ChunkManager, Context, VMError};
use super::InstructionResult;

pub fn constant<'a>(backend: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, context: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let index = manager.read_u16()? as usize;
    let constant = backend.get_constant_with_id(index)?;

    let memory_usage = constant.calculate_memory_usage(context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack(constant.clone().into())?;
    Ok(InstructionResult::Nothing)
}

pub fn subload<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let index = manager.read_u8()?;
    let path = stack.pop_stack()?;
    let sub = path.get_at_index(index as usize)?;
    stack.push_stack_unchecked(sub);

    Ok(InstructionResult::Nothing)
}

pub fn copy<'a>(_: &Backend<'a>, stack: &mut Stack, _: &mut ChunkManager<'a>, context: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let value = stack.last_stack()?;

    let memory_usage = value.as_ref()?
        .calculate_memory_usage(context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack(value.to_owned()?)?;

    Ok(InstructionResult::Nothing)
}

pub fn copy_n<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, context: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let index = manager.read_u8()?;
    let value = stack.get_stack_at(index as usize)?;

    let memory_usage = value.as_ref()?
        .calculate_memory_usage(context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack(value.to_owned()?)?;

    Ok(InstructionResult::Nothing)
}

pub fn to_owned<'a>(_: &Backend<'a>, stack: &mut Stack, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let value = stack.last_mut_stack()?;
    value.make_owned()?;

    Ok(InstructionResult::Nothing)
}

pub fn pop<'a>(_: &Backend<'a>, stack: &mut Stack, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    stack.pop_stack()?;
    Ok(InstructionResult::Nothing)
}

pub fn pop_n<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let n = manager.read_u8()?;
    stack.pop_stack_n(n)?;
    Ok(InstructionResult::Nothing)
}

pub fn swap<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let index = manager.read_u8()?;
    stack.swap_stack(index as usize)?;
    Ok(InstructionResult::Nothing)
}

pub fn swap2<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let index_a = manager.read_u8()?;
    let index_b = manager.read_u8()?;

    stack.swap_stack_both(index_a as usize, index_b as usize)?;
    Ok(InstructionResult::Nothing)
}

pub fn array_call<'a>(_: &Backend<'a>, stack: &mut Stack, _: &mut ChunkManager<'a>, context: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let value = stack.pop_stack()?;
    let index = value.as_u32()?;
    let value = stack.pop_stack()?;
    let sub = value.get_at_index(index as usize)?;

    let memory_usage = sub.as_ref()?
        .calculate_memory_usage(context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack_unchecked(sub);
    Ok(InstructionResult::Nothing)
}

pub fn invoke_chunk<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let id = manager.read_u16()?;
    let on_value = manager.read_bool()?;
    let mut args = manager.read_u8()? as usize;
    if on_value {
        args += 1;
    }

    // We need to reverse the order of the arguments
    let inner = stack.get_inner_mut();
    let len = inner.len();
    if len < args {
        return Err(VMError::NotEnoughArguments);
    }

    let slice = &mut stack.get_inner_mut()[len - args..len];
    slice.reverse();

    Ok(InstructionResult::InvokeChunk(id))
}

pub fn syscall<'a>(backend: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, context: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let id = manager.read_u16()?;
    let on_value = manager.read_bool()?;
    let args = manager.read_u8()?;

    let mut arguments = VecDeque::with_capacity(args as usize);
    for _ in 0..args {
        arguments.push_front(stack.pop_stack()?);
    }

    let mut on_value = if on_value {
        Some(stack.pop_stack()?)
    } else {
        None
    };

    let f = backend.environment.get_functions()
        .get(id as usize)
        .ok_or(VMError::UnknownSysCall(id))?;

    context.increase_gas_usage(f.get_cost())?;

    // We need to find if we are using two times the same instance

    let instance = match on_value.as_mut() {
        Some(v) => {
            let ptr = v.ptr();
            for argument in arguments.iter_mut() {
                argument.make_owned_if_same_ptr(ptr)?;
            }

            Some(v.as_mut()?)
        },
        None => None,
    };

    if let Some(v) = f.call_function(instance, arguments.into(), context)? {
        let memory_usage = v.calculate_memory_usage(context.memory_left())?;
        context.increase_memory_usage_unchecked(memory_usage)?;

        stack.push_stack(v.into())?;
    }

    Ok(InstructionResult::Nothing)
}