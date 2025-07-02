use crate::{
    debug,
    iterator::ValueIterator,
    stack::Stack,
    Backend,
    ChunkManager,
    ChunkReader,
    Context,
    VMError
};
use xelis_types::Primitive;

use super::InstructionResult;

pub fn iterable_length<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, _: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("iterable length");

    let value = stack.pop_stack()?;
    let len = Primitive::U32(value.as_ref()?.as_vec()?.len() as u32);

    let memory_usage = len.get_memory_usage();
    context.increase_memory_usage(memory_usage)?;

    stack.push_stack_unchecked(len.into());
    Ok(InstructionResult::Nothing)
}

pub fn iterator_begin<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, manager: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("iterator begin");

    let value = stack.pop_stack()?;
    let iterator = ValueIterator::new(value)?;
    manager.add_iterator(iterator);
    Ok(InstructionResult::Nothing)
}

pub fn iterator_next<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, manager: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("iterator next");

    let addr = reader.read_u32()?;
    if let Some(value) = manager.next_iterator()? {
        let memory_usage = value.as_ref()?
            .calculate_memory_usage(context.memory_left())?;
        context.increase_memory_usage_unchecked(memory_usage)?;

        stack.push_stack(value)?;
    } else {
        reader.set_index(addr as usize)?;
    }
    Ok(InstructionResult::Nothing)
}

pub fn iterator_end<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, _: &mut Stack, manager: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("iterator end");

    manager.pop_iterator()?;
    Ok(InstructionResult::Nothing)
}