use xelis_environment::{Context, EnvironmentError, FnInstance, FnParams, FnReturnType};
use xelis_types::{Type, Value, ValueCell};

use crate::EnvironmentBuilder;

pub fn register(env: &mut EnvironmentBuilder) {
    let key_type = Type::T(0);
    let value_type = Type::T(1);
    let _type = Type::Map(Box::new(key_type.clone()), Box::new(value_type.clone()));
    env.register_native_function("len", Some(_type.clone()), vec![], len, 1, Some(Type::U32));
    env.register_native_function("contains_key", Some(_type.clone()), vec![("key", key_type.clone())], contains_key, 15, Some(Type::Bool));
    env.register_native_function("get", Some(_type.clone()), vec![("key", key_type.clone())], get, 15, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("insert", Some(_type.clone()), vec![("key", key_type.clone()), ("value", value_type.clone())], insert, 30, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("remove", Some(_type.clone()), vec![("key", key_type.clone())], remove, 15, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("clear", Some(_type.clone()), vec![], clear, 5, None);
    env.register_native_function("keys", Some(_type.clone()), vec![], keys, 20, Some(Type::Array(Box::new(key_type.clone()))));
    env.register_native_function("values", Some(_type.clone()), vec![], values, 20, Some(Type::Array(Box::new(value_type.clone()))));
}

fn len(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let len = zelf?.as_map()?.len();
    Ok(Some(Value::U32(len as u32).into()))
}

fn contains_key(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let key = parameters.remove(0);
    let k = key.as_ref();
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let contains = zelf?.as_map()?.contains_key(&k);
    Ok(Some(Value::Boolean(contains).into()))
}

fn get(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let key = parameters.remove(0);
    let k = key.as_ref();
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let value = zelf?.as_map()?.get(&k).cloned();
    Ok(Some(ValueCell::Optional(value)))
}

fn insert(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType {
    let key = parameters.remove(0).into_owned();
    if key.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let map = zelf?.as_mut_map()?;
    if map.len() >= u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }

    let max_depth = context.max_value_depth() - 1;
    // Verify the depth of the key
    key.calculate_depth(max_depth)?;

    let value = parameters.remove(0);
    // Verify the depth of the value
    value.as_ref()
        .calculate_depth(max_depth)?;

    let previous = map
        .insert(key, value.into_owned().into());
    Ok(Some(ValueCell::Optional(previous)))
}

fn remove(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let key = parameters.remove(0);

    let k = key.as_ref();
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let value = zelf?.as_mut_map()?
        .remove(&k);
    Ok(Some(ValueCell::Optional(value)))
}

fn clear(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    zelf?.as_mut_map()?.clear();
    Ok(None)
}

fn keys(zelf: FnInstance, _: FnParams, context: &mut Context) -> FnReturnType {
    let map = zelf?.as_map()?;

    // we need to go through all elements, thus we increase the gas usage
    context.increase_gas_usage((map.len() as u64) * 8)?;

    let keys = map.keys()
        .map(|key| key.clone().into())
        .collect::<Vec<_>>();

    Ok(Some(ValueCell::Array(keys)))
}

fn values(zelf: FnInstance, _: FnParams, context: &mut Context) -> FnReturnType {
    let map = zelf?.as_mut_map()?;

    // we need to go through all elements, thus we increase the gas usage
    context.increase_gas_usage((map.len() as u64) * 5)?;

    let values = map.values()
        .map(|v| v.reference())
        .collect::<Vec<_>>();

    Ok(Some(ValueCell::Array(values)))
}