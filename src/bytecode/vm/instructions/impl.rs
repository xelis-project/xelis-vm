use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use crate::{bytecode::vm::{ChunkManager, VMError, VM}, Path, Value};

pub fn constant<'a>(vm: &mut VM<'a>, manager: &mut ChunkManager<'a>) -> Result<(), VMError> {
    let index = manager.read_u16()? as usize;
    let constant = vm.module.get_constant_at(index).ok_or(VMError::ConstantNotFound)?;
    vm.push_stack(Path::Borrowed(constant))
}

pub fn memory_load<'a>(vm: &mut VM<'a>, manager: &mut ChunkManager<'a>) -> Result<(), VMError> {
    let index = manager.read_u16()?;
    let value = manager.from_register(index as usize)?
        .shareable();
    vm.push_stack(value)?;

    Ok(())
}

pub fn memory_set<'a>(vm: &mut VM<'a>, manager: &mut ChunkManager<'a>) -> Result<(), VMError> {
    let index = manager.read_u16()?;
    let value = vm.pop_stack()?;
    manager.set_register(index as usize, value);

    Ok(())
}

pub fn subload<'a>(vm: &mut VM<'a>, manager: &mut ChunkManager<'a>) -> Result<(), VMError> {
    let index = manager.read_u16()?;
    let path = vm.pop_stack()?;
    let sub = path.get_sub_variable(index as usize)?;
    vm.push_stack_unchecked(sub);

    Ok(())
}

pub fn copy<'a>(vm: &mut VM<'a>, _: &mut ChunkManager<'a>) -> Result<(), VMError> {
    let value = vm.last_stack()?;
    vm.push_stack(value.clone())?;

    Ok(())
}

pub fn pop<'a>(vm: &mut VM<'a>, _: &mut ChunkManager<'a>) -> Result<(), VMError> {
    vm.pop_stack()?;
    Ok(())
}

pub fn swap<'a>(vm: &mut VM<'a>, manager: &mut ChunkManager<'a>) -> Result<(), VMError> {
    let index = manager.read_u8()?;
    vm.swap_stack(index as usize)
}

pub fn array_call<'a>(vm: &mut VM<'a>, _: &mut ChunkManager<'a>) -> Result<(), VMError> {
    let index = vm.pop_stack()?.into_owned().cast_to_u32()?;
    let value = vm.pop_stack()?;
    let sub = value.get_sub_variable(index as usize)?;
    vm.push_stack_unchecked(sub);
    Ok(())
}

pub fn syscall<'a>(vm: &mut VM<'a>, manager: &mut ChunkManager<'a>) -> Result<(), VMError> {
    let id = manager.read_u16()?;
    let on_value = manager.read_bool()?;
    let args = manager.read_u8()?;

    let mut arguments = VecDeque::with_capacity(args as usize);
    for _ in 0..args {
        arguments.push_front(vm.pop_stack()?);
    }

    let mut on_value = if on_value {
        Some(vm.pop_stack()?)
    } else {
        None
    };

    let func = vm.environment.get_functions().get(id as usize)
        .ok_or(VMError::UnknownSysCall)?;

    let mut instance = match on_value.as_mut() {
        Some(v) => Some(v.as_mut()),
        None => None,
    };

    if let Some(v) = func.call_function(instance.as_deref_mut(), arguments.into())? {
        vm.push_stack(Path::Owned(v))?;
    }

    Ok(())
}

pub fn new_array<'a>(vm: &mut VM<'a>, manager: &mut ChunkManager<'a>) -> Result<(), VMError> {
    let length = manager.read_u32()?;
    let mut array = VecDeque::with_capacity(length as usize);
    for _ in 0..length {
        let pop = vm.pop_stack()?;
        array.push_front(Rc::new(RefCell::new(pop.into_owned())));
    }

    vm.push_stack(Path::Owned(Value::Array(array.into())))
}

pub fn new_struct<'a>(vm: &mut VM<'a>, manager: &mut ChunkManager<'a>) -> Result<(), VMError> {
    let id = manager.read_u16()?;
    let structure = vm.get_struct_with_id(id)?;
    let mut fields = VecDeque::new();
    for _ in 0..structure.fields.len() {
        fields.push_front(Rc::new(RefCell::new(vm.pop_stack()?.into_owned())));
    }

    vm.push_stack(Path::Owned(Value::Struct(id, fields.into())))
}