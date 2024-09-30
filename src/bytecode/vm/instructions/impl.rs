use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use crate::{bytecode::vm::{stack::Stack, Backend, ChunkManager, VMError}, Path, Value};

use super::InstructionResult;

pub fn constant<'a>(backend: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let index = manager.read_u16()? as usize;
    let constant = backend.module.get_constant_at(index).ok_or(VMError::ConstantNotFound)?;
    stack.push_stack(Path::Borrowed(constant))?;
    Ok(InstructionResult::Nothing)
}

pub fn memory_load<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let index = manager.read_u16()?;
    let value = manager.from_register(index as usize)?
        .shareable();
    stack.push_stack(value)?;

    Ok(InstructionResult::Nothing)
}

pub fn memory_set<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let index = manager.read_u16()?;
    let value = stack.pop_stack()?;
    manager.set_register(index as usize, value);

    Ok(InstructionResult::Nothing)
}

pub fn subload<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let index = manager.read_u16()?;
    let path = stack.pop_stack()?;
    let sub = path.get_sub_variable(index as usize)?;
    stack.push_stack_unchecked(sub);

    Ok(InstructionResult::Nothing)
}

pub fn copy<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let value = stack.last_stack()?;
    stack.push_stack(value.clone())?;

    Ok(InstructionResult::Nothing)
}

pub fn pop<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    stack.pop_stack()?;
    Ok(InstructionResult::Nothing)
}

pub fn swap<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let index = manager.read_u8()?;
    stack.swap_stack(index as usize)?;
    Ok(InstructionResult::Nothing)
}

pub fn array_call<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let index = stack.pop_stack()?.into_owned().cast_to_u32()?;
    let value = stack.pop_stack()?;
    let sub = value.get_sub_variable(index as usize)?;
    stack.push_stack_unchecked(sub);
    Ok(InstructionResult::Nothing)
}

pub fn invoke_chunk<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let id = manager.read_u16()?;
    let on_value = manager.read_bool()?;
    let mut args = manager.read_u8()? as usize;
    if on_value {
        args += 1;
    }

    // We need to reverse the order of the arguments
    let inner = stack.get_inner();
    let len = inner.len();
    if len < args {
        return Err(VMError::NotEnoughArguments);
    }

    stack.get_inner()[len - args..len].reverse();

    Ok(InstructionResult::InvokeChunk(id))
}

pub fn syscall<'a>(backend: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
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

    let func = backend.environment.get_functions().get(id as usize)
        .ok_or(VMError::UnknownSysCall)?;

    let mut instance = match on_value.as_mut() {
        Some(v) => Some(v.as_mut()),
        None => None,
    };

    if let Some(v) = func.call_function(instance.as_deref_mut(), arguments.into())? {
        stack.push_stack(Path::Owned(v))?;
    }

    Ok(InstructionResult::Nothing)
}

pub fn new_array<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let length = manager.read_u32()?;
    let mut array = VecDeque::with_capacity(length as usize);
    for _ in 0..length {
        let pop = stack.pop_stack()?;
        array.push_front(Rc::new(RefCell::new(pop.into_owned())));
    }

    stack.push_stack(Path::Owned(Value::Array(array.into())))?;
    Ok(InstructionResult::Nothing)
}

pub fn new_struct<'a>(backend: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let id = manager.read_u16()?;
    let structure = backend.get_struct_with_id(id)?;
    let mut fields = VecDeque::new();
    for _ in 0..structure.fields.len() {
        fields.push_front(Rc::new(RefCell::new(stack.pop_stack()?.into_owned())));
    }

    stack.push_stack(Path::Owned(Value::Struct(id, fields.into())))?;
    Ok(InstructionResult::Nothing)
}