use crate::{
    iterator::PathIterator,
    stack::Stack,
    Backend,
    ChunkManager,
    Context,
    VMError
};
use xelis_types::{Path, Value, ValueCell};

use super::InstructionResult;

pub fn iterable_length<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let value = stack.pop_stack()?;
    let len = value.as_ref().as_vec()?.len();
    stack.push_stack_unchecked(Path::Owned(ValueCell::Default(Value::U32(len as u32))));
    Ok(InstructionResult::Nothing)
}

pub fn iterator_begin<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let value = stack.pop_stack()?;
    let iterator = PathIterator::new(value)?;
    manager.add_iterator(iterator);
    Ok(InstructionResult::Nothing)
}

pub fn iterator_next<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let addr = manager.read_u32()?;
    if let Some(value) = manager.next_iterator()? {
        stack.push_stack(value)?;
    } else {
        manager.set_index(addr as usize)?;
    }
    Ok(InstructionResult::Nothing)
}

pub fn iterator_end<'a>(_: &Backend<'a>, _: &mut Stack<'a>, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    manager.pop_iterator()?;
    Ok(InstructionResult::Nothing)
}