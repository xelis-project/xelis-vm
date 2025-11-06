use xelis_bytecode::ModuleMetadata;
use xelis_environment::{
    Context,
    EnvironmentError,
    FnInstance,
    FnParams,
    FnReturnType,
    FunctionHandler,
    SysCallResult
};
use xelis_types::{Type, Primitive, ValueCell, ValuePointer};

use crate::EnvironmentBuilder;

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    let key_type = Type::T(Some(0));
    let value_type = Type::T(Some(1));
    let _type = Type::Map(Box::new(key_type.clone()), Box::new(value_type.clone()));
    env.register_native_function("len", Some(_type.clone()), vec![], FunctionHandler::Sync(len), 1, Some(Type::U32));
    env.register_native_function("contains_key", Some(_type.clone()), vec![("key", key_type.clone())], FunctionHandler::Sync(contains_key), 15, Some(Type::Bool));
    env.register_native_function("get", Some(_type.clone()), vec![("key", key_type.clone())], FunctionHandler::Sync(get), 15, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("insert", Some(_type.clone()), vec![("key", key_type.clone()), ("value", value_type.clone())], FunctionHandler::Sync(insert), 30, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("shift_remove", Some(_type.clone()), vec![("key", key_type.clone())], FunctionHandler::Sync(shift_remove), 15, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("swap_remove", Some(_type.clone()), vec![("key", key_type.clone())], FunctionHandler::Sync(swap_remove), 15, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("clear", Some(_type.clone()), vec![], FunctionHandler::Sync(clear), 5, None);
    env.register_native_function("keys", Some(_type.clone()), vec![], FunctionHandler::Sync(keys), 20, Some(Type::Array(Box::new(key_type.clone()))));
    env.register_native_function("values", Some(_type.clone()), vec![], FunctionHandler::Sync(values), 20, Some(Type::Array(Box::new(value_type.clone()))));
    env.register_native_function("entries", Some(_type.clone()), vec![], FunctionHandler::Sync(entries), 40, Some(Type::Array(Box::new(Type::Tuples(vec![key_type.clone(), value_type.clone()])))));
}

fn len<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let len = zelf?.as_map()?.len();
    Ok(SysCallResult::Return(Primitive::U32(len as u32).into()))
}

fn contains_key<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let key = parameters.remove(0);
    let k = key.as_ref();
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let contains = zelf?.as_map()?.contains_key(k);
    Ok(SysCallResult::Return(Primitive::Boolean(contains).into()))
}

fn get<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let key = parameters.remove(0);
    let k = key.as_ref();
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let value = zelf?.as_map()?
        .get(k)
        .cloned();

    Ok(SysCallResult::Return(match value {
        Some(v) => v.to_owned().into(),
        None => Primitive::Null.into(),
    }))
}

fn insert<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let param = parameters.remove(0);
    let key_depth = param.depth();
    // Key is deep cloned here
    let key = param.into_owned();
    if key.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    key.calculate_depth(
        context.max_value_depth()
            .saturating_sub(key_depth.saturating_add(1))
    )?;

    let mut zelf = zelf?;
    let map = zelf.as_mut()
        .as_mut_map()?;
    if map.len() >= u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }

    let param = parameters.remove(0);
    let value_depth = param.depth();
    let value = param.into_owned();

    value.calculate_depth(
        context.max_value_depth()
            .saturating_sub(value_depth.saturating_add(1))
    )?;

    let previous = map
        .insert(key, value.into());

    Ok(SysCallResult::Return(match previous {
        Some(v) => v.into(),
        None => Primitive::Null.into(),
    }))
}

fn shift_remove<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let key = parameters.remove(0);

    let k = key.as_ref();
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let mut zelf = zelf?;
    let map = zelf.as_mut()
        .as_mut_map()?;

    // pay based on O(N)
    context.increase_gas_usage(map.len() as _)?;

    let value = map.shift_remove(k);
    Ok(SysCallResult::Return(match value {
        Some(v) => v.into(),
        None => Primitive::Null.into(),
    }))
}

fn swap_remove<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let key = parameters.remove(0);

    let k = key.as_ref();
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let value = zelf?.as_mut_map()?
        .swap_remove(k);
    Ok(SysCallResult::Return(match value {
        Some(v) => v.into(),
        None => Primitive::Null.into(),
    }))
}

fn clear<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    zelf?.as_mut_map()?.clear();
    Ok(SysCallResult::None)
}

fn keys<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let map = zelf.as_map()?;

    // we need to go through all elements, thus we increase the gas usage
    context.increase_gas_usage((map.len() as u64) * 8)?;

    let keys = map.keys()
        .map(|key| key.clone().into())
        .collect::<Vec<_>>();

    Ok(SysCallResult::Return(ValueCell::Object(keys).into()))
}

fn values<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let map = zelf.as_map()?;

    // we need to go through all elements, thus we increase the gas usage
    context.increase_gas_usage((map.len() as u64) * 5)?;

    let values = map.values()
        .cloned()
        .collect::<Vec<_>>();

    Ok(SysCallResult::Return(ValueCell::Object(values).into()))
}

fn entries<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let map = zelf.as_map()?;

    // we need to go through all elements, thus we increase the gas usage
    context.increase_gas_usage((map.len() as u64) * 10)?;

    let entries = map.iter()
        .map(|(k, v)| ValuePointer::new(ValueCell::Object(vec![ValuePointer::new(k.clone()), v.clone()])))
        .collect::<Vec<_>>();

    Ok(SysCallResult::Return(ValueCell::Object(entries).into()))
}