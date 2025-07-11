use xelis_environment::{Context, EnvironmentError, FnInstance, FnParams, FnReturnType};
use xelis_types::{Type, Primitive, ValueCell};

use crate::EnvironmentBuilder;

pub fn register(env: &mut EnvironmentBuilder) {
    let key_type = Type::T(Some(0));
    let value_type = Type::T(Some(1));
    let _type = Type::Map(Box::new(key_type.clone()), Box::new(value_type.clone()));
    env.register_native_function("len", Some(_type.clone()), vec![], len, 1, Some(Type::U32));
    env.register_native_function("contains_key", Some(_type.clone()), vec![("key", key_type.clone())], contains_key, 15, Some(Type::Bool));
    env.register_native_function("get", Some(_type.clone()), vec![("key", key_type.clone())], get, 15, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("insert", Some(_type.clone()), vec![("key", key_type.clone()), ("value", value_type.clone())], insert, 30, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("shift_remove", Some(_type.clone()), vec![("key", key_type.clone())], shift_remove, 15, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("swap_remove", Some(_type.clone()), vec![("key", key_type.clone())], swap_remove, 15, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("clear", Some(_type.clone()), vec![], clear, 5, None);
    env.register_native_function("keys", Some(_type.clone()), vec![], keys, 20, Some(Type::Array(Box::new(key_type.clone()))));
    env.register_native_function("values", Some(_type.clone()), vec![], values, 20, Some(Type::Array(Box::new(value_type.clone()))));
}

fn len(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let len = zelf?.as_map()?.len();
    Ok(Some(Primitive::U32(len as u32).into()))
}

fn contains_key(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let key = parameters.remove(0);
    let k = key.as_ref()?;
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let contains = zelf?.as_map()?.contains_key(k);
    Ok(Some(Primitive::Boolean(contains).into()))
}

fn get(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let key = parameters.remove(0);
    let k = key.as_ref()?;
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let value = zelf?.as_map()?
        .get(k)
        .cloned();

    Ok(Some(match value {
        Some(v) => v,
        None => Primitive::Null.into(),
    }))
}

fn insert(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType {
    let param = parameters.remove(0);
    let key_depth = param.depth();
    let key = param.into_owned()?;
    if key.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    key.calculate_depth(
        context.max_value_depth()
            .saturating_sub(key_depth.saturating_add(1))
    )?;

    let map = zelf?.as_mut_map()?;
    if map.len() >= u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }

    let param = parameters.remove(0);
    let value_depth = param.depth();
    let value = param.into_owned()?;

    value.calculate_depth(
        context.max_value_depth()
            .saturating_sub(value_depth.saturating_add(1))
    )?;

    let previous = map
        .insert(key, value);

    Ok(Some(match previous {
        Some(v) => v,
        None => Primitive::Null.into(),
    }))
}

fn shift_remove(zelf: FnInstance, mut parameters: FnParams, context: &mut Context) -> FnReturnType {
    let key = parameters.remove(0);

    let k = key.as_ref()?;
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let map = zelf?.as_mut_map()?;
    // pay based on O(N)
    context.increase_gas_usage(map.len() as _)?;

    let value = map.shift_remove(k);
    Ok(Some(match value {
        Some(v) => v,
        None => Primitive::Null.into(),
    }))
}

fn swap_remove(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let key = parameters.remove(0);

    let k = key.as_ref()?;
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let value = zelf?.as_mut_map()?
        .swap_remove(k);
    Ok(Some(match value {
        Some(v) => v,
        None => Primitive::Null.into(),
    }))
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

    Ok(Some(ValueCell::Object(keys)))
}

fn values(zelf: FnInstance, _: FnParams, context: &mut Context) -> FnReturnType {
    let map = zelf?.as_mut_map()?;

    // we need to go through all elements, thus we increase the gas usage
    context.increase_gas_usage((map.len() as u64) * 5)?;

    let values = map.values()
        .map(|v| v.clone())
        .collect::<Vec<_>>();

    Ok(Some(ValueCell::Object(values)))
}