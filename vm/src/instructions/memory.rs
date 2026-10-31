use silex_environment::VMContext;
use silex_types::{Heap, Primitive, PrimitiveInteger, ValuePointer};
use crate::{debug, stack::Stack, Backend, ChunkManager, ChunkReader, VMError};
use super::InstructionResult;

pub fn memory_load<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, manager: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    let value = load_reference(manager, reader, context, heap)?;
    stack.push_pointer(value)?;
    Ok(InstructionResult::Nothing)
}

#[inline(always)]
pub(super) fn load_reference(manager: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut VMContext<'_, '_>, heap: &mut Heap) -> Result<ValuePointer, VMError> {
    let index = reader.read_u16()?;
    debug!("memory load at {}", index);
    let value = manager.from_register(index as usize)?;
    let memory = value.estimate_memory_usage(heap, context.memory_left())?;
    context.increase_memory_usage_unchecked(memory)?;
    Ok(value.reference_pointer(heap))
}

pub fn memory_set<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, manager: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    let index = reader.read_u16()?;
    debug!("memory set at {}", index);

    let value = stack.pop_stack()?;
    
    // If we're overwriting an existing register value, decrease its memory
    if let Some(previous) = manager.set_register(index as usize, value)? {
        let memory = previous.estimate_memory_usage(heap, context.max_memory_usage())?;
        context.decrease_memory_usage(memory);
    }

    Ok(InstructionResult::Nothing)
}

pub fn memory_pop<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, manager: &mut ChunkManager, _: &mut ChunkReader<'_>, context: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("memory pop");

    let v = manager.pop_register()?;
    let memory_usage = v.estimate_memory_usage(heap, context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack(v)?;
    Ok(InstructionResult::Nothing)
}

pub fn memory_len<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, manager: &mut ChunkManager, _: &mut ChunkReader<'_>, context: &mut VMContext<'ty, 'r>, _heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("memory len");

    let len = Primitive::Integer(PrimitiveInteger::U32(manager.registers_len() as u32));
    let memory_usage = len.get_memory_usage();
    context.increase_memory_usage(memory_usage)?;

    stack.push_stack(len.into())?;

    Ok(InstructionResult::Nothing)
}

pub fn memory_to_owned<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, _: &mut Stack<M>, manager: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("memory to owned");

    let index = reader.read_u16()?;
    let value = manager.from_register(index as _)?;
    if value.make_owned(heap)? {
        let memory = value.estimate_memory_usage(heap, context.memory_left())?;
        context.increase_memory_usage_unchecked(memory)?;
    }

    Ok(InstructionResult::Nothing)
}