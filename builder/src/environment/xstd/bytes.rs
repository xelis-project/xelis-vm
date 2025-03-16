use xelis_environment::{Context, EnvironmentError, FnInstance, FnParams, FnReturnType};
use xelis_types::{Constant, Primitive, Type, ValueCell};

use crate::EnvironmentBuilder;

pub fn register(env: &mut EnvironmentBuilder) {
    // Bytes
    env.register_native_function("len", Some(Type::Bytes), vec![], len, 1, Some(Type::U32));
    env.register_native_function("push", Some(Type::Bytes), vec![("value", Type::U8)], push, 2, None);
    env.register_native_function("remove", Some(Type::Bytes), vec![("index", Type::U32)], remove, 5, Some(Type::U8));
    env.register_native_function("pop", Some(Type::Bytes), vec![], pop, 1, Some(Type::Optional(Box::new(Type::U8))));
    env.register_native_function("slice", Some(Type::Bytes), vec![("range", Type::Range(Box::new(Type::U32)))], slice, 5, Some(Type::Bytes));
    env.register_native_function("contains", Some(Type::Bytes), vec![("value", Type::U8)], contains, 10, Some(Type::Bool));
    env.register_native_function("get", Some(Type::Bytes), vec![("index", Type::U32)], get, 1, Some(Type::Optional(Box::new(Type::U8))));
    env.register_native_function("first", Some(Type::Bytes), vec![], first, 1, Some(Type::Optional(Box::new(Type::U8))));
    env.register_native_function("last", Some(Type::Bytes), vec![], last, 1, Some(Type::Optional(Box::new(Type::U8))));

    env.register_const_function("new", Type::Bytes, vec![], |_| Ok(Constant::Bytes(Vec::new())));
    env.register_const_function("from", Type::Bytes, vec![("values", Type::Array(Box::new(Type::U8)))], |values| {
        let mut bytes = Vec::with_capacity(values.len());
        for v in values {
            bytes.push(v.to_u8()?);
        }

        Ok(Constant::Bytes(bytes))
    });
}

// native functions
fn len(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let len = zelf?.as_bytes()?.len();
    Ok(Some(Primitive::U32(len as u32).into()))
}

fn push(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let array =  zelf?.as_bytes_mut()?;
    if array.len() >= u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }

    let param = parameters.remove(0);
    let mut value = param.into_owned()?;

    array.push(value.into_value()?.to_u8()?);

    Ok(None)
}

fn remove(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType {
    let index = parameters.remove(0).as_u32()? as usize;

    let array = zelf?.as_bytes_mut()?;
    if index >= array.len() {
        return Err(EnvironmentError::OutOfBounds(index, array.len()))
    }

    // moving all elements after the index to the left is costly
    context.increase_gas_usage((array.len() as u64) * 2)?;

    Ok(Some(Primitive::U8(array.remove(index)).into()))
}

fn pop(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let array = zelf?.as_bytes_mut()?;
    if let Some(value) = array.pop() {
        Ok(Some(Primitive::U8(value).into()))
    } else {
        Ok(Some(Primitive::Null.into()))
    }
}

fn slice(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType {
    let param = parameters.remove(0);
    let range = param.as_ref()?;
    let (start, end) = range.as_range()?;

    let start = start.as_u32()?;
    let end = end.as_u32()?;

    let vec = zelf?.as_bytes()?;
    let len = vec.len() as u32;
    if start >= len || end >= len || start >= end {
        return Err(EnvironmentError::InvalidRange(start, end))
    }

    // we need to go through all elements in the slice, thus we increase the gas usage
    context.increase_gas_usage((vec.len() as u64) * 5)?;

    let slice = vec[start as usize..end as usize].into();
    Ok(Some(ValueCell::Bytes(slice)))
}

fn contains(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType {
    let value = parameters.remove(0);
    let handle = value.as_ref()?.as_u8()?;
    let vec = zelf?.as_bytes()?;

    // we need to go through all elements in the slice, thus we increase the gas usage
    context.increase_gas_usage((vec.len() as u64) * 5)?;

    Ok(Some(Primitive::Boolean(vec.contains(&handle)).into()))
}

fn get(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let index = parameters.remove(0).as_u32()? as usize;
    let vec = zelf?.as_bytes()?;
    if let Some(value) = vec.get(index).copied() {
        Ok(Some(Primitive::U8(value).into()))
    } else {
        Ok(Some(Primitive::Null.into()))
    }
}

fn first(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let vec = zelf?.as_bytes()?;
    if let Some(value) = vec.first().copied() {
        Ok(Some(Primitive::U8(value).into()))
    } else {
        Ok(Some(Primitive::Null.into()))
    }
}

fn last(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let vec = zelf?.as_bytes()?;
    if let Some(value) = vec.last().copied() {
        Ok(Some(Primitive::U8(value).into()))
    } else {
        Ok(Some(Primitive::Null.into()))
    }
}