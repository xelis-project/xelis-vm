use xelis_environment::{Context, EnvironmentError, FnInstance, FnParams, FnReturnType};
use xelis_types::{Type, Value, ValuePointer};

use crate::EnvironmentBuilder;

pub fn register(env: &mut EnvironmentBuilder) {
    let key_type = Type::T(0);
    let value_type = Type::T(1);
    let _type = Type::Map(Box::new(key_type.clone()), Box::new(value_type.clone()));
    env.register_native_function("len", Some(_type.clone()), vec![], len, 1, Some(Type::U32));
    env.register_native_function("contains_key", Some(_type.clone()), vec![key_type.clone()], contains_key, 10, Some(Type::Bool));
    env.register_native_function("get", Some(_type.clone()), vec![key_type.clone()], get, 10, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("insert", Some(_type.clone()), vec![key_type.clone(), Type::T(1)], insert, 30, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("remove", Some(_type.clone()), vec![key_type.clone()], remove, 10, Some(Type::Optional(Box::new(value_type.clone()))));
    env.register_native_function("clear", Some(_type.clone()), vec![], clear, 5, None);
    env.register_native_function("keys", Some(_type.clone()), vec![], keys, 200, Some(Type::Array(Box::new(key_type.clone()))));
    env.register_native_function("values", Some(_type.clone()), vec![], values, 50, Some(Type::Array(Box::new(value_type.clone()))));
}

fn len(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let len = zelf?.as_map()?.len();
    Ok(Some(Value::U32(len as u32)))
}

fn contains_key(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let key = parameters.remove(0);
    let k = key.as_ref();
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let contains = zelf?.as_map()?.contains_key(&k);
    Ok(Some(Value::Boolean(contains)))
}

fn get(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let key = parameters.remove(0);
    let k = key.as_ref();
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let value = zelf?.as_map()?.get(&k).cloned();
    Ok(Some(Value::Optional(value)))
}

fn insert(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let key = parameters.remove(0).into_owned();
    if key.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let value = parameters.remove(0);
    let previous = zelf?.as_mut_map()?
        .insert(key, value.into_pointer());
    Ok(Some(Value::Optional(previous)))
}

fn remove(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let key = parameters.remove(0);

    let k = key.as_ref();
    if k.is_map() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let value = zelf?.as_mut_map()?
        .remove(&k);
    Ok(Some(Value::Optional(value)))
}

fn clear(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    zelf?.as_mut_map()?.clear();
    Ok(None)
}

fn keys(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let keys = zelf?.as_map()?
        .keys()
        .map(|key| ValuePointer::Owned(Box::new(key.clone())))
        .collect::<Vec<_>>();

    Ok(Some(Value::Array(keys)))
}

fn values(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let values = zelf?.as_mut_map()?
        .values_mut()
        .map(ValuePointer::transform)
        .collect::<Vec<_>>();

    Ok(Some(Value::Array(values)))
}