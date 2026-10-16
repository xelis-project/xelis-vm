use std::collections::VecDeque;
use indexmap::IndexMap;
use silex_environment::EnvironmentError;
use silex_types::{Primitive, ValueCell};

use crate::{debug, stack::Stack, Backend, ChunkManager, ChunkReader, VMContext, VMError};
use super::InstructionResult;

pub fn new_array<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut VMContext<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let length = reader.read_u8()?;
    debug!("new array with length {}", length);

    let mut array = VecDeque::with_capacity(length as usize);
    for _ in 0..length {
        let pop = stack.pop_stack()?;
        array.push_front(pop.into_owned().into());
    }

    let value = ValueCell::Object(array.into());
    value.calculate_depth(context.max_value_depth())?;
    let memory_usage = value.calculate_memory_usage(context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack(value.into())?;
    Ok(InstructionResult::Nothing)
}

pub fn new_range<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, _: &mut ChunkManager, _: &mut ChunkReader<'_>, context: &mut VMContext<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("new range");
    let mut end = stack.pop_stack()?.into_owned();
    let mut start = stack.pop_stack()?.into_owned();

    let value = Primitive::new_range(
        start.into_value().map_err(|_| VMError::InvalidRangeType)?,
        end.into_value().map_err(|_| VMError::InvalidRangeType)?,
    ).map_err(|_| VMError::InvalidRangeType)?;
    let memory_usage = value.get_memory_usage();
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack_unchecked(value.into());
    Ok(InstructionResult::Nothing)
}

pub fn new_map<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut VMContext<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let len = reader.read_u8()?;
    debug!("new map with length {}", len);

    let mut map = IndexMap::with_capacity(len as usize);
    for _ in 0..len {
        let value = stack.pop_stack()?;
        let key = stack.pop_stack()?.into_owned();
        if !key.is_hashable() {
            return Err(EnvironmentError::InvalidKeyType.into());
        }

        map.insert(key, value.into_owned().into());
    }

    let map = ValueCell::Map(Box::new(map));
    map.calculate_depth(context.max_value_depth())?;
    let memory_usage = map.calculate_memory_usage(context.memory_left())?;
    context.increase_memory_usage_unchecked(memory_usage)?;

    stack.push_stack_unchecked(map.into());
    Ok(InstructionResult::Nothing)
}