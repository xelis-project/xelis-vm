use xelis_environment::{Context, EnvironmentError, FnInstance, FnParams, FnReturnType, FunctionHandler, SysCallResult};
use xelis_types::{Constant, Primitive, Type, ValueCell};

use crate::EnvironmentBuilder;

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    // Bytes
    env.register_native_function("len", Some(Type::Bytes), vec![], FunctionHandler::Sync(len), 1, Some(Type::U32));
    env.register_native_function("push", Some(Type::Bytes), vec![("value", Type::U8)], FunctionHandler::Sync(push), 2, None);
    env.register_native_function("remove", Some(Type::Bytes), vec![("index", Type::U32)], FunctionHandler::Sync(remove), 5, Some(Type::U8));
    env.register_native_function("pop", Some(Type::Bytes), vec![], FunctionHandler::Sync(pop), 1, Some(Type::Optional(Box::new(Type::U8))));
    env.register_native_function("slice", Some(Type::Bytes), vec![("range", Type::Range(Box::new(Type::U32)))], FunctionHandler::Sync(slice), 5, Some(Type::Bytes));
    env.register_native_function("contains", Some(Type::Bytes), vec![("value", Type::U8)], FunctionHandler::Sync(contains), 10, Some(Type::Bool));
    env.register_native_function("get", Some(Type::Bytes), vec![("index", Type::U32)], FunctionHandler::Sync(get), 1, Some(Type::Optional(Box::new(Type::U8))));
    env.register_native_function("first", Some(Type::Bytes), vec![], FunctionHandler::Sync(first), 1, Some(Type::Optional(Box::new(Type::U8))));
    env.register_native_function("last", Some(Type::Bytes), vec![], FunctionHandler::Sync(last), 1, Some(Type::Optional(Box::new(Type::U8))));
    env.register_native_function("to_array", Some(Type::Bytes), vec![], FunctionHandler::Sync(to_array), 1, Some(Type::Array(Box::new(Type::U8))));

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
fn len<M>(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType<M> {
    let len = zelf?.as_bytes()?.len();
    Ok(SysCallResult::Return(Primitive::U32(len as u32).into()))
}

fn push<M>(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType<M> {
    let array =  zelf?.as_bytes_mut()?;
    if array.len() >= u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }

    let param = parameters.remove(0);
    let mut value = param.into_owned()?;

    array.push(value.into_value()?.to_u8()?);

    Ok(SysCallResult::None)
}

fn remove<M>(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType<M> {
    let index = parameters.remove(0).as_u32()? as usize;

    let array = zelf?.as_bytes_mut()?;
    if index >= array.len() {
        return Err(EnvironmentError::OutOfBounds(index, array.len()))
    }

    // moving all elements after the index to the left is costly
    context.increase_gas_usage((array.len() as u64) * 2)?;

    Ok(SysCallResult::Return(Primitive::U8(array.remove(index)).into()))
}

fn pop<M>(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType<M> {
    let array = zelf?.as_bytes_mut()?;
    if let Some(value) = array.pop() {
        Ok(SysCallResult::Return(Primitive::U8(value).into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn slice<M>(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType<M> {
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
    Ok(SysCallResult::Return(ValueCell::Bytes(slice)))
}

fn contains<M>(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType<M> {
    let value = parameters.remove(0);
    let handle = value.as_ref()?.as_u8()?;
    let vec = zelf?.as_bytes()?;

    // we need to go through all elements in the slice, thus we increase the gas usage
    context.increase_gas_usage((vec.len() as u64) * 5)?;

    Ok(SysCallResult::Return(Primitive::Boolean(vec.contains(&handle)).into()))
}

fn get<M>(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType<M> {
    let index = parameters.remove(0).as_u32()? as usize;
    let vec = zelf?.as_bytes()?;
    if let Some(value) = vec.get(index).copied() {
        Ok(SysCallResult::Return(Primitive::U8(value).into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn first<M>(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType<M> {
    let vec = zelf?.as_bytes()?;
    if let Some(value) = vec.first().copied() {
        Ok(SysCallResult::Return(Primitive::U8(value).into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn last<M>(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType<M> {
    let vec = zelf?.as_bytes()?;
    if let Some(value) = vec.last().copied() {
        Ok(SysCallResult::Return(Primitive::U8(value).into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn to_array<M>(zelf: FnInstance, _: FnParams, context: &mut Context) -> FnReturnType<M> {
    let vec = zelf?.as_bytes()?;

    context.increase_gas_usage(vec.len() as _)?;

    let values = vec.iter()
        .map(|v| Primitive::U8(*v).into())
        .collect();

    Ok(SysCallResult::Return(ValueCell::Object(values)))
}