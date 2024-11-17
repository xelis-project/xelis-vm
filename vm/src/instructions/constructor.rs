use std::collections::{HashMap, VecDeque};
use xelis_types::{EnumValueType, Path, Value};

use crate::{stack::Stack, Backend, ChunkManager, Context, VMError};
use super::InstructionResult;

pub fn new_array<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
    let length = manager.read_u32()?;
    let mut array = VecDeque::with_capacity(length as usize);
    for _ in 0..length {
        let pop = stack.pop_stack()?;
        array.push_front(pop.into_ownable());
    }

    stack.push_stack(Path::Owned(Value::Array(array.into())))?;
    Ok(InstructionResult::Nothing)
}

pub fn new_struct<'a>(backend: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
    let id = manager.read_u16()?;
    let struct_type = backend.get_struct_with_id(id as usize)?;

    let mut fields = VecDeque::new();
    for _ in 0..struct_type.fields().len() {
        fields.push_front(stack.pop_stack()?.into_ownable());
    }

    stack.push_stack(Path::Owned(Value::Struct(fields.into(), struct_type.clone())))?;
    Ok(InstructionResult::Nothing)
}

pub fn new_range<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
    let end = stack.pop_stack()?;
    let start = stack.pop_stack()?;

    if !start.as_ref().is_number() {
        return Err(VMError::InvalidRangeType);
    }

    let start_type = start.as_ref().get_type()?;
    if start_type != end.as_ref().get_type()? {
        return Err(VMError::InvalidRangeType);
    }

    let value = Value::Range(Box::new(start.into_owned()), Box::new(end.into_owned()), start_type);
    stack.push_stack_unchecked(Path::Owned(value));
    Ok(InstructionResult::Nothing)
}

pub fn new_map<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
    let len = manager.read_u32()?;
    let mut map = HashMap::new();
    for _ in 0..len {
        let value = stack.pop_stack()?;
        let key = stack.pop_stack()?;
        map.insert(key.into_owned(), value.into_ownable());
    }

    stack.push_stack_unchecked(Path::Owned(Value::Map(map)));
    Ok(InstructionResult::Nothing)
}

pub fn new_enum<'a>(backend: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
    let id = manager.read_u16()?;
    let enum_type = backend.get_enum_with_id(id as usize)?;

    let variant_id = manager.read_u8()?;
    let variant = enum_type.get_variant(variant_id)
        .ok_or(VMError::InvalidEnumVariant)?;

    let mut values = VecDeque::new();
    for _ in variant.fields() {
        values.push_front(stack.pop_stack()?.into_ownable());
    }

    stack.push_stack(Path::Owned(Value::Enum(values.into(), EnumValueType::new(enum_type.clone(), variant_id))))?;
    Ok(InstructionResult::Nothing)
}