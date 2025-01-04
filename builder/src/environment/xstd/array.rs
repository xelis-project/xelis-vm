use xelis_types::{Type, Value, ValueCell};
use xelis_environment::{Context, EnvironmentError, FnInstance, FnParams, FnReturnType};
use super::EnvironmentBuilder;

pub fn register(env: &mut EnvironmentBuilder) {
    env.register_native_function("len", Some(Type::Array(Box::new(Type::T(0)))), vec![], len, 1, Some(Type::U32));
    env.register_native_function("push", Some(Type::Array(Box::new(Type::T(0)))), vec![("value", Type::T(0))], push, 2, None);
    env.register_native_function("remove", Some(Type::Array(Box::new(Type::T(0)))), vec![("index", Type::U32)], remove, 5, Some(Type::T(0)));
    env.register_native_function("pop", Some(Type::Array(Box::new(Type::T(0)))), vec![], pop, 1, Some(Type::Optional(Box::new(Type::T(0)))));
    env.register_native_function("slice", Some(Type::Array(Box::new(Type::T(0)))), vec![("range", Type::Range(Box::new(Type::U32)))], slice, 5, Some(Type::Array(Box::new(Type::T(0)))));
    env.register_native_function("contains", Some(Type::Array(Box::new(Type::T(0)))), vec![("value", Type::T(0))], contains, 10, Some(Type::Bool));
    env.register_native_function("get", Some(Type::Array(Box::new(Type::T(0)))), vec![("index", Type::U32)], get, 1, Some(Type::Optional(Box::new(Type::T(0)))));
    env.register_native_function("first", Some(Type::Array(Box::new(Type::T(0)))), vec![], first, 1, Some(Type::Optional(Box::new(Type::T(0)))));
    env.register_native_function("last", Some(Type::Array(Box::new(Type::T(0)))), vec![], last, 1, Some(Type::Optional(Box::new(Type::T(0)))));
}

// native functions
fn len(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let len = zelf?.as_vec()?.len();
    Ok(Some(Value::U32(len as u32).into()))
}

fn push(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType {
    let array =  zelf?.as_mut_vec()?;
    if array.len() >= u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }

    let param = parameters.remove(0);
    let value = param.into_owned();

    // Verify the depth of the value
    value.calculate_depth(context.max_value_depth() - 1)?;

    array.push(value.into());

    Ok(None)
}

fn remove(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType {
    let index = parameters.remove(0).as_u32()? as usize;

    let array = zelf?.as_mut_vec()?;
    if index >= array.len() {
        return Err(EnvironmentError::OutOfBounds(index, array.len()))
    }

    // moving all elements after the index to the left is costly
    context.increase_gas_usage((array.len() as u64) * 5)?;

    Ok(Some(array.remove(index).into_owned()))
}

fn pop(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let array = zelf?.as_mut_vec()?;
    if let Some(value) = array.pop() {
        Ok(Some(value.into_owned()))
    } else {
        Ok(Some(ValueCell::Optional(None)))
    }
}

fn slice(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType {
    let param = parameters.remove(0);
    let range = param.as_ref();
    let (start, end, _type) = range.as_range()?;

    if *_type != Type::U32 {
        return Err(EnvironmentError::InvalidParameter)
    }

    let start = start.as_u32()?;
    let end = end.as_u32()?;

    let vec = zelf?.as_mut_vec()?;
    let len = vec.len() as u32;
    if start >= len || end >= len || start >= end {
        return Err(EnvironmentError::InvalidRange(start, end))
    }

    // we need to go through all elements in the slice, thus we increase the gas usage
    context.increase_gas_usage((vec.len() as u64) * 5)?;

    let mut slice = Vec::new();
    for i in start..end {
        // due to ValuePointer, slice are connected.
        let value = match vec.get(i as usize) {
            Some(v) => v.reference(),
            None => return Err(EnvironmentError::NoValueFoundAtIndex(i))
        };
        slice.push(value);
    }

    Ok(Some(ValueCell::Array(slice)))
}

fn contains(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType {
    let value = parameters.remove(0);
    let handle = value.as_ref();
    let expected = handle.as_value();
    let vec = zelf?.as_vec()?;

    // we need to go through all elements in the slice, thus we increase the gas usage
    context.increase_gas_usage((vec.len() as u64) * 5)?;

    Ok(Some(Value::Boolean(vec.iter().find(|v| *v.borrow() == *expected).is_some()).into()))
}

fn get(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let index = parameters.remove(0).as_u32()? as usize;
    let vec = zelf?.as_vec()?;
    if let Some(value) = vec.get(index) {
        Ok(Some(ValueCell::Optional(Some(value.reference()))))
    } else {
        Ok(Some(ValueCell::Optional(None)))
    }
}

fn first(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let vec = zelf?.as_vec()?;
    if let Some(value) = vec.first() {
        Ok(Some(ValueCell::Optional(Some(value.reference()))))
    } else {
        Ok(Some(ValueCell::Optional(None)))
    }
}

fn last(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let vec = zelf?.as_vec()?;
    if let Some(value) = vec.last() {
        Ok(Some(ValueCell::Optional(Some(value.reference()))))
    } else {
        Ok(Some(ValueCell::Optional(None)))
    }
}