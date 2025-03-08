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

pub fn memory_pop<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    stack.push_stack(manager.pop_register()?)?;
    Ok(InstructionResult::Nothing)
}

pub fn memory_len<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let len = manager.registers_len();
    stack.push_stack(Primitive::U32(len as u32).into())?;

    Ok(InstructionResult::Nothing)
}

pub fn memory_to_owned<'a>(_: &Backend<'a>, _: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let index = manager.read_u16()?;
    manager.to_owned_register(index as usize)?;

    Ok(InstructionResult::Nothing)
}