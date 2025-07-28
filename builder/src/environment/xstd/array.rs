use xelis_types::{Constant, Primitive, Type, ValueCell};
use xelis_environment::{Context, EnvironmentError, FnInstance, FnParams, FnReturnType, FunctionHandler, SysCallResult};
use super::EnvironmentBuilder;
use paste::paste;

macro_rules! array_number_with_size {
    ($env: expr, $op: ident, $t: ident) => {
        paste! {
            fn [<with_size_ $op>](params: Vec<Constant>) -> Result<Constant, anyhow::Error> {
                let count = params[0].as_u32()? as usize;
                let values = vec![Constant::Default(Primitive::$t(Default::default())); count];
                Ok(Constant::Array(values))
            }

            // Registering the generated function in the environment
            $env.register_const_function(
                // Function name as a string
                "with_size",
                Type::Array(Box::new(Type::$t)),
                vec![("size", Type::U32)],
                // The function ptr
                [<with_size_ $op>]
            );
        }
    };
}

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    env.register_native_function("len", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![], FunctionHandler::Sync(len), 1, Some(Type::U32));
    env.register_native_function("push", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("value", Type::T(Some(0)))], FunctionHandler::Sync(push), 2, None);
    env.register_native_function("remove", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("index", Type::U32)], FunctionHandler::Sync(remove), 5, Some(Type::T(Some(0))));
    env.register_native_function("pop", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![], FunctionHandler::Sync(pop), 1, Some(Type::Optional(Box::new(Type::T(Some(0))))));
    env.register_native_function("slice", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("range", Type::Range(Box::new(Type::U32)))], FunctionHandler::Sync(slice), 5, Some(Type::Array(Box::new(Type::T(Some(0))))));
    env.register_native_function("contains", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("value", Type::T(Some(0)))], FunctionHandler::Sync(contains), 10, Some(Type::Bool));
    env.register_native_function("get", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("index", Type::U32)], FunctionHandler::Sync(get), 1, Some(Type::Optional(Box::new(Type::T(Some(0))))));
    env.register_native_function("first", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![], FunctionHandler::Sync(first), 1, Some(Type::Optional(Box::new(Type::T(Some(0))))));
    env.register_native_function("last", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![], FunctionHandler::Sync(last), 1, Some(Type::Optional(Box::new(Type::T(Some(0))))));

    env.register_native_function("extend", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("other", Type::Array(Box::new(Type::T(Some(0)))))], FunctionHandler::Sync(extend), 5, None);
    env.register_native_function("concat", Some(Type::Array(Box::new(Type::Array(Box::new(Type::T(Some(0))))))), vec![], FunctionHandler::Sync(concat), 5, Some(Type::Array(Box::new(Type::T(Some(0))))));

    // Transform a Type::Array(U8) into bytes easily
    env.register_native_function("to_bytes", Some(Type::Array(Box::new(Type::U8))), vec![], FunctionHandler::Sync(to_bytes), 1, Some(Type::Bytes));

    // Constant function
    env.register_const_function("with", Type::Array(Box::new(Type::T(Some(0)))), vec![("size", Type::U32), ("default", Type::T(Some(0)))], const_with);

    array_number_with_size!(env, u8, U8);
    array_number_with_size!(env, u16, U16);
    array_number_with_size!(env, u32, U32);
    array_number_with_size!(env, u64, U64);
    array_number_with_size!(env, u128, U128);
    array_number_with_size!(env, u256, U256);
}

// native functions
fn len<M>(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType<M> {
    let len = zelf?.as_vec()?.len();
    Ok(SysCallResult::Return(Primitive::U32(len as u32).into()))
}

fn push<M>(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType<M> {
    let array =  zelf?.as_mut_vec()?;
    if array.len() >= u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }

    let param = parameters.remove(0);
    let depth = param.depth();
    let value = param.into_owned()?;

    value.calculate_depth(
        context.max_value_depth()
            .saturating_sub(depth.saturating_add(1))
    )?;

    array.push(value.into());

    Ok(SysCallResult::None)
}

fn remove<M>(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType<M> {
    let index = parameters.remove(0).as_u32()? as usize;

    let array = zelf?.as_mut_vec()?;
    if index >= array.len() {
        return Err(EnvironmentError::OutOfBounds(index, array.len()))
    }

    // moving all elements after the index to the left is costly
    context.increase_gas_usage((array.len() as u64) * 5)?;

    Ok(SysCallResult::Return(array.remove(index).into()))
}

fn pop<M>(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType<M> {
    let array = zelf?.as_mut_vec()?;
    if let Some(value) = array.pop() {
        Ok(SysCallResult::Return(value.into()))
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
        let value = vec.get(i as usize)
            .cloned()
            .ok_or(EnvironmentError::NoValueFoundAtIndex(i))?;
        slice.push(value);
    }

    Ok(SysCallResult::Return(ValueCell::Object(slice).into()))
}

fn contains<M>(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType<M> {
    let value = parameters.remove(0);
    let handle = value.as_ref()?;
    let vec = zelf?.as_vec()?;

    // we need to go through all elements in the slice, thus we increase the gas usage
    context.increase_gas_usage((vec.len() as u64) * 5)?;

    Ok(SysCallResult::Return(Primitive::Boolean(vec.iter().find(|v| *v.as_ref().unwrap() == *handle).is_some()).into()))
}

fn get<M>(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType<M> {
    let index = parameters.remove(0).as_u32()? as usize;
    let vec = zelf?.as_vec()?;
    if let Some(value) = vec.get(index) {
        Ok(SysCallResult::Return(value.clone().into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn first<M>(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType<M> {
    let vec = zelf?.as_vec()?;
    if let Some(value) = vec.first() {
        Ok(SysCallResult::Return(value.clone().into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn last<M>(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType<M> {
    let vec = zelf?.as_vec()?;
    if let Some(value) = vec.last() {
        Ok(SysCallResult::Return(value.clone().into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn extend<M>(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType<M> {
    let other = parameters.remove(0)
        .into_owned()?
        .to_vec()?;

    context.increase_gas_usage(other.len() as _)?;

    let vec = zelf?.as_mut_vec()?;
    if other.len() as u64 + vec.len() as u64 > u32::MAX as u64 {
        return Err(EnvironmentError::OutOfMemory)
    }

    vec.extend(other);

    Ok(SysCallResult::None)
}

fn concat<M>(zelf: FnInstance, _: FnParams, context: &mut Context) -> FnReturnType<M> {
    let vec = zelf?.as_vec()?;
    context.increase_gas_usage(vec.len() as u64)?;

    let mut result = Vec::new();
    for el in vec.iter() {
        let v = el.as_ref()?.as_vec()?;

        context.increase_gas_usage(v.len() as u64)?;
        // Check len is <= u32::MAX
        if result.len() as u64 + v.len() as u64 > u32::MAX as u64 {
            return Err(EnvironmentError::OutOfMemory)
        }

        result.extend(v.iter().cloned());
    }

    Ok(SysCallResult::Return(ValueCell::Object(result).into()))
}

fn const_with(mut params: Vec<Constant>) -> Result<Constant, anyhow::Error> {
    let default = params.remove(1);
    let count = params[0].as_u32()? as usize;
    let values = vec![default; count];
    Ok(Constant::Array(values))
}

fn to_bytes<M>(zelf: FnInstance, _: FnParams, context: &mut Context) -> FnReturnType<M> {
    let values = zelf?.as_vec()?;
    let len = values.len();

    context.increase_gas_usage(len as _)?;

    let bytes = values.iter()
        .map(|v| v.as_ref().and_then(ValueCell::as_u8))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(SysCallResult::Return(ValueCell::Bytes(bytes).into()))
}