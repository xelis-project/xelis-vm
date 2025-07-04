use xelis_environment::Context;
use xelis_types::Primitive;
use crate::{debug, stack::Stack, Backend, ChunkManager, ChunkReader, VMError};
use super::InstructionResult;

pub const REFERENCE_SIZE: usize = 8;

pub fn memory_load<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, manager: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let index = reader.read_u16()?;
    debug!("memory load at {}", index);

    let value = manager.from_register(index as usize)?;

    // We don't increase the memory usage because its only a pointer
    context.increase_memory_usage(REFERENCE_SIZE)?;

    stack.push_stack(value.reference())?;

    Ok(InstructionResult::Nothing)
}

pub fn memory_set<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, manager: &mut ChunkManager, reader: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let index = reader.read_u16()?;
    debug!("memory set at {}", index);

    let value = stack.pop_stack()?;
    manager.set_register(index as usize, value, stack)?;

    Ok(InstructionResult::Nothing)
}

pub fn memory_pop<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, manager: &mut ChunkManager, _: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("memory pop");

    let v = manager.pop_register()?;
    let memory_usage = v.as_ref()?
        .calculate_memory_usage(context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack(v)?;
    Ok(InstructionResult::Nothing)
}

pub fn memory_len<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, manager: &mut ChunkManager, _: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("memory len");

    let len = Primitive::U32(manager.registers_len() as u32);
    let memory_usage = len.get_memory_usage();
    context.increase_memory_usage(memory_usage)?;

    stack.push_stack(len.into())?;

    Ok(InstructionResult::Nothing)
}

pub fn memory_to_owned<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, _: &mut Stack, manager: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("memory to owned");

    let index = reader.read_u16()?;
    let value = manager.from_register(index as _)?;
    if value.make_owned()? {
        let memory = value.as_ref()?.calculate_memory_usage(context.memory_left())?;
        context.increase_memory_usage_unchecked(memory)?;
    }

    Ok(InstructionResult::Nothing)
}