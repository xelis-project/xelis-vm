use std::collections::{HashMap, VecDeque};
use xelis_environment::EnvironmentError;
use xelis_types::{Primitive, ValueCell};

use crate::{stack::Stack, Backend, ChunkManager, Context, VMError};
use super::InstructionResult;

pub fn new_array<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let length = manager.read_u8()?;
    let mut array = VecDeque::with_capacity(length as usize);
    for _ in 0..length {
        let pop = stack.pop_stack()?;
        array.push_front(pop.into_owned()?);
    }

    stack.push_stack(ValueCell::Array(array.into()).into())?;
    Ok(InstructionResult::Nothing)
}

pub fn new_range<'a>(_: &Backend<'a>, stack: &mut Stack, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let end = stack.pop_stack()?.into_owned()?;
    let start = stack.pop_stack()?.into_owned()?;

    if !start.is_number() || !end.is_number() {
        return Err(VMError::InvalidRangeType);
    }

    let start_type = start.as_value()?.get_type()?;
    if start_type != end.as_value()?.get_type()? {
        return Err(VMError::InvalidRangeType);
    }

    let value = Primitive::Range(Box::new((start.into_value()?, end.into_value()?)));
    stack.push_stack_unchecked(value.into());
    Ok(InstructionResult::Nothing)
}

pub fn new_map<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let len = manager.read_u8()?;
    let mut map = HashMap::with_capacity(len as usize);
    for _ in 0..len {
        let value = stack.pop_stack()?;
        let key = stack.pop_stack()?.into_owned()?;
        if key.is_map() {
            return Err(EnvironmentError::InvalidKeyType.into());
        }

        map.insert(key, value.into_owned()?.into());
    }

    stack.push_stack_unchecked(ValueCell::Map(Box::new(map)).into());
    Ok(InstructionResult::Nothing)
}