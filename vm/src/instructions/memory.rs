use xelis_environment::Context;
use xelis_types::Primitive;
use crate::{stack::Stack, Backend, ChunkManager, VMError};
use super::InstructionResult;


pub fn memory_load<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let index = manager.read_u16()?;
    let value = manager.from_register(index as usize)?;
    stack.push_stack(value.reference())?;

    Ok(InstructionResult::Nothing)
}

pub fn memory_set<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let index = manager.read_u16()?;
    let value = stack.pop_stack()?;
    if let Some((ptr, old)) = manager.set_register(index as usize, value)? {
        stack.verify_pointers(ptr)?;
        stack.add_to_garbage_collector(old);
    }

    Ok(InstructionResult::Nothing)
}

pub fn memory_pop<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, context: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let v = manager.pop_register()?;
    let memory_usage = v.as_ref()?
        .calculate_memory_usage(context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack(v)?;
    Ok(InstructionResult::Nothing)
}

pub fn memory_len<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, context: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let len = Primitive::U32(manager.registers_len() as u32);
    let memory_usage = len.get_memory_usage();
    context.increase_memory_usage(memory_usage)?;

    stack.push_stack(len.into())?;

    Ok(InstructionResult::Nothing)
}

pub fn memory_to_owned<'a>(_: &Backend<'a>, _: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let index = manager.read_u16()?;
    manager.to_owned_register(index as usize)?;

    Ok(InstructionResult::Nothing)
}