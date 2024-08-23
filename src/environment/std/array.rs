use crate::{
    ast::{FnInstance, FnParams, FnReturnType},
    values::Value,
    EnvironmentBuilder,
    InterpreterError,
    Type
};

pub fn register(env: &mut EnvironmentBuilder) {
    env.register_native_function("len", Some(Type::Array(Box::new(Type::Any))), vec![], len, 1, Some(Type::U64));
    env.register_native_function("push", Some(Type::Array(Box::new(Type::Any))), vec![Type::T], push, 1, None);
    env.register_native_function("remove", Some(Type::Array(Box::new(Type::Any))), vec![Type::U64], remove, 1, Some(Type::T));
    env.register_native_function("pop", Some(Type::Array(Box::new(Type::Any))), vec![], pop, 1, Some(Type::Optional(Box::new(Type::T))));
    env.register_native_function("slice", Some(Type::Array(Box::new(Type::Any))), vec![Type::U64, Type::U64], slice, 3, Some(Type::Array(Box::new(Type::T))));
    env.register_native_function("contains", Some(Type::Array(Box::new(Type::Any))), vec![Type::T], contains, 1, Some(Type::Bool));
    env.register_native_function("get", Some(Type::Array(Box::new(Type::Any))), vec![Type::U64], get, 1, Some(Type::Optional(Box::new(Type::T))));
    env.register_native_function("first", Some(Type::Array(Box::new(Type::Any))), vec![], first, 1, Some(Type::Optional(Box::new(Type::T))));
    env.register_native_function("last", Some(Type::Array(Box::new(Type::Any))), vec![], last, 1, Some(Type::Optional(Box::new(Type::T))));
}

// native functions
fn len(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let len = zelf?.as_vec()?.len();
    Ok(Some(Value::U64(len as u64)))
}

fn push(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let param = parameters.remove(0);
    zelf?.as_mut_vec()?.push(param.into_owned());
    Ok(None)
}

fn remove(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let index = *parameters.remove(0).as_u64()? as usize;

    let array = zelf?.as_mut_vec()?;
    if index >= array.len() {
        return Err(InterpreterError::OutOfBounds(index, array.len()))
    }

    Ok(Some(array.remove(index)))
}

fn pop(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let array = zelf?.as_mut_vec()?;
    if let Some(value) = array.pop() {
        Ok(Some(value))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn slice(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let start = *parameters.remove(0).as_u64()?;
    let end = *parameters.remove(0).as_u64()?;

    let vec = zelf?.as_vec()?;
    let len_u64 = vec.len() as u64;
    if start >= len_u64 || end >= len_u64 || start >= end {
        return Err(InterpreterError::InvalidRange(start, end))
    }

    let mut slice = Vec::new();
    for i in start..end {
        // due to SharedValue, slice are connected.
        let value = match vec.get(i as usize) {
            Some(v) => v.clone(),
            None => return Err(InterpreterError::NoValueFoundAtIndex(i))
        };
        slice.push(value);
    }

    Ok(Some(Value::Array(slice)))
}

fn contains(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let value = parameters.remove(0);
    let vec = zelf?.as_vec()?;
    Ok(Some(Value::Boolean(vec.contains(&value))))
}

fn get(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let index = *parameters.remove(0).as_u64()? as usize;
    let vec = zelf?.as_vec()?;
    if let Some(value) = vec.get(index) {
        Ok(Some(Value::Optional(Some(Box::new(value.clone())))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn first(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let vec = zelf?.as_vec()?;
    if let Some(value) = vec.first() {
        Ok(Some(Value::Optional(Some(Box::new(value.clone())))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn last(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let vec = zelf?.as_vec()?;
    if let Some(value) = vec.last() {
        Ok(Some(Value::Optional(Some(Box::new(value.clone())))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}