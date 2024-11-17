use xelis_types::{Type, Value};
use xelis_environment::{Context, EnvironmentError, FnInstance, FnParams, FnReturnType};
use super::EnvironmentBuilder;

pub fn register(env: &mut EnvironmentBuilder) {
    env.register_native_function("len", Some(Type::Array(Box::new(Type::T(0)))), vec![], len, 1, Some(Type::U32));
    env.register_native_function("push", Some(Type::Array(Box::new(Type::T(0)))), vec![Type::T(0)], push, 1, None);
    env.register_native_function("remove", Some(Type::Array(Box::new(Type::T(0)))), vec![Type::U32], remove, 1, Some(Type::T(0)));
    env.register_native_function("pop", Some(Type::Array(Box::new(Type::T(0)))), vec![], pop, 1, Some(Type::Optional(Box::new(Type::T(0)))));
    env.register_native_function("slice", Some(Type::Array(Box::new(Type::T(0)))), vec![Type::U32, Type::U32], slice, 3, Some(Type::Array(Box::new(Type::T(0)))));
    env.register_native_function("contains", Some(Type::Array(Box::new(Type::T(0)))), vec![Type::T(0)], contains, 1, Some(Type::Bool));
    env.register_native_function("get", Some(Type::Array(Box::new(Type::T(0)))), vec![Type::U32], get, 1, Some(Type::Optional(Box::new(Type::T(0)))));
    env.register_native_function("first", Some(Type::Array(Box::new(Type::T(0)))), vec![], first, 1, Some(Type::Optional(Box::new(Type::T(0)))));
    env.register_native_function("last", Some(Type::Array(Box::new(Type::T(0)))), vec![], last, 1, Some(Type::Optional(Box::new(Type::T(0)))));
}

// native functions
fn len(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let len = zelf?.as_vec()?.len();
    Ok(Some(Value::U32(len as u32)))
}

fn push(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let param = parameters.remove(0);
    zelf?.as_mut_vec()?
        .push(param.into_ownable());
    Ok(None)
}

fn remove(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let index = parameters.remove(0).as_u32()? as usize;

    let array = zelf?.as_mut_vec()?;
    if index >= array.len() {
        return Err(EnvironmentError::OutOfBounds(index, array.len()))
    }

    Ok(Some(array.remove(index).into_inner()))
}

fn pop(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let array = zelf?.as_mut_vec()?;
    if let Some(value) = array.pop() {
        Ok(Some(value.into_inner()))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn slice(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
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

fn contains(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let value = parameters.remove(0);
    let handle = value.as_ref();
    let expected = handle.as_value();
    let vec = zelf?.as_vec()?;
    Ok(Some(Value::Boolean(vec.iter().find(|v| *v.handle() == *expected).is_some())))
}

fn get(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let index = parameters.remove(0).as_u32()? as usize;
    let vec = zelf?.as_mut_vec()?;
    if let Some(value) = vec.get_mut(index) {
        Ok(Some(Value::Optional(Some(value.transform()))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn first(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let vec = zelf?.as_mut_vec()?;
    if let Some(value) = vec.first_mut() {
        Ok(Some(Value::Optional(Some(value.transform()))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn last(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let vec = zelf?.as_mut_vec()?;
    if let Some(value) = vec.last_mut() {
        Ok(Some(Value::Optional(Some(value.transform()))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}