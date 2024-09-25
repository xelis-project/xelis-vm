use std::{cell::RefCell, rc::Rc};

use crate::{
    values::Value,
    EnvironmentBuilder,
    Type
};

use super::{EnvironmentError, FnInstance, FnParams, FnReturnType};

pub fn register(env: &mut EnvironmentBuilder) {
    env.register_native_function("len", Some(Type::Array(Box::new(Type::Any))), vec![], len, 1, Some(Type::U32));
    env.register_native_function("push", Some(Type::Array(Box::new(Type::Any))), vec![Type::T], push, 1, None);
    env.register_native_function("remove", Some(Type::Array(Box::new(Type::Any))), vec![Type::U32], remove, 1, Some(Type::T));
    env.register_native_function("pop", Some(Type::Array(Box::new(Type::Any))), vec![], pop, 1, Some(Type::Optional(Box::new(Type::T))));
    env.register_native_function("slice", Some(Type::Array(Box::new(Type::Any))), vec![Type::U32, Type::U32], slice, 3, Some(Type::Array(Box::new(Type::T))));
    env.register_native_function("contains", Some(Type::Array(Box::new(Type::Any))), vec![Type::T], contains, 1, Some(Type::Bool));
    env.register_native_function("get", Some(Type::Array(Box::new(Type::Any))), vec![Type::U32], get, 1, Some(Type::Optional(Box::new(Type::T))));
    env.register_native_function("first", Some(Type::Array(Box::new(Type::Any))), vec![], first, 1, Some(Type::Optional(Box::new(Type::T))));
    env.register_native_function("last", Some(Type::Array(Box::new(Type::Any))), vec![], last, 1, Some(Type::Optional(Box::new(Type::T))));
}

// native functions
fn len(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let len = zelf?.as_vec()?.len();
    Ok(Some(Value::U32(len as u32)))
}

fn push(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let param = parameters.remove(0);
    zelf?.as_mut_vec()?.push(Rc::new(RefCell::new(param.into_owned())));
    Ok(None)
}

fn remove(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let index = parameters.remove(0).as_u32()? as usize;

    let array = zelf?.as_mut_vec()?;
    if index >= array.len() {
        return Err(EnvironmentError::OutOfBounds(index, array.len()))
    }

    Ok(Some(array.remove(index).borrow().clone()))
}

fn pop(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let array = zelf?.as_mut_vec()?;
    if let Some(value) = array.pop() {
        Ok(Some(value.borrow().clone()))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn slice(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let start = parameters.remove(0).as_u32()?;
    let end = parameters.remove(0).as_u32()?;

    let vec = zelf?.as_vec()?;
    let len = vec.len() as u32;
    if start >= len || end >= len || start >= end {
        return Err(EnvironmentError::InvalidRange(start, end))
    }

    let mut slice = Vec::new();
    for i in start..end {
        // due to SharedValue, slice are connected.
        let value = match vec.get(i as usize) {
            Some(v) => v.clone(),
            None => return Err(EnvironmentError::NoValueFoundAtIndex(i))
        };
        slice.push(value);
    }

    Ok(Some(Value::Array(slice)))
}

fn contains(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let value = parameters.remove(0);
    let handle = value.as_ref();
    let expected = handle.as_value();
    let vec = zelf?.as_vec()?;
    Ok(Some(Value::Boolean(vec.iter().find(|v| *v.borrow() == *expected).is_some())))
}

fn get(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let index = parameters.remove(0).as_u32()? as usize;
    let vec = zelf?.as_vec()?;
    if let Some(value) = vec.get(index) {
        Ok(Some(Value::Optional(Some(Box::new(value.borrow().clone())))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn first(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let vec = zelf?.as_vec()?;
    if let Some(value) = vec.first() {
        Ok(Some(Value::Optional(Some(Box::new(value.borrow().clone())))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn last(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let vec = zelf?.as_vec()?;
    if let Some(value) = vec.last() {
        Ok(Some(Value::Optional(Some(Box::new(value.borrow().clone())))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}