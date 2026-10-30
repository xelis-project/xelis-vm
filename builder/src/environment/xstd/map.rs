use silex_environment::{
    VMContext,
    EnvironmentError,
    FnInstance,
    FnParams,
    FnReturnType,
    FunctionHandler,
    SysCallResult,
    ModuleMetadata,
};
use silex_types::{PrimitiveInteger, Type, Primitive, RuntimeValue, HeapError};

use crate::EnvironmentBuilder;

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    let key_type = Type::T(Some(0));
    let value_type = Type::T(Some(1));
    let _type = Type::Map(Box::new(key_type.clone()), Box::new(value_type.clone()));
    env.register_native_function_with_comment("len", Some(_type.clone()), vec![], FunctionHandler::Sync(len), 1, Some(Type::U32), "Returns the number of entries in the map.");
    env.register_native_function_with_comment("contains_key", Some(_type.clone()), vec![("key", key_type.clone())], FunctionHandler::Sync(contains_key), 15, Some(Type::Bool), "Returns true when the map contains the key.");
    env.register_native_function_with_comment("get", Some(_type.clone()), vec![("key", key_type.clone())], FunctionHandler::Sync(get), 15, Some(Type::Optional(Box::new(value_type.clone()))), "Returns the value for the key, or null when absent.");
    env.register_native_function_with_comment("insert", Some(_type.clone()), vec![("key", key_type.clone()), ("value", value_type.clone())], FunctionHandler::Sync(insert), 30, Some(Type::Optional(Box::new(value_type.clone()))), "Inserts a key-value pair and returns the previous value, or null.");
    env.register_native_function_with_comment("shift_remove", Some(_type.clone()), vec![("key", key_type.clone())], FunctionHandler::Sync(shift_remove), 15, Some(Type::Optional(Box::new(value_type.clone()))), "Removes a key while preserving map order and returns its value, or null.");
    env.register_native_function_with_comment("swap_remove", Some(_type.clone()), vec![("key", key_type.clone())], FunctionHandler::Sync(swap_remove), 15, Some(Type::Optional(Box::new(value_type.clone()))), "Removes a key by swapping with the last entry and returns its value, or null.");
    env.register_native_function_with_comment("clear", Some(_type.clone()), vec![], FunctionHandler::Sync(clear), 5, None, "Removes all entries from the map.");
    env.register_native_function_with_comment("keys", Some(_type.clone()), vec![], FunctionHandler::Sync(keys), 20, Some(Type::Array(Box::new(key_type.clone()))), "Returns all keys in insertion order.");
    env.register_native_function_with_comment("values", Some(_type.clone()), vec![], FunctionHandler::Sync(values), 20, Some(Type::Array(Box::new(value_type.clone()))), "Returns all values in insertion order.");
    env.register_native_function_with_comment("entries", Some(_type.clone()), vec![], FunctionHandler::Sync(entries), 40, Some(Type::Array(Box::new(Type::Tuples(vec![key_type.clone(), value_type.clone()])))), "Returns all key-value pairs in insertion order.");
}

fn len<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, _resources) = context.split_heap();
    let len = zelf?.as_ref(heap).as_map()?.len();
    Ok(SysCallResult::Return(Primitive::Integer(PrimitiveInteger::U32(len as u32)).into()))
}

fn contains_key<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, _resources) = context.split_heap();
    let key = parameters.remove(0);
    let k = heap.snapshot(key.as_ref(heap))?;
    if !k.is_hashable() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let contains = zelf?.as_ref(heap).as_map()?.contains_key(&k);
    Ok(SysCallResult::Return(Primitive::Boolean(contains).into()))
}

fn get<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, _resources) = context.split_heap();
    let key = parameters.remove(0);
    let k = heap.snapshot(key.as_ref(heap))?;
    if !k.is_hashable() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let value = zelf?.as_ref(heap).as_map()?
        .get(&k)
        .cloned();

    Ok(SysCallResult::Return(match value {
        Some(v) => v.to_owned(heap)?.into(),
        None => Primitive::Null.into(),
    }))
}

fn insert<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let param = parameters.remove(0);
    let key_depth = param.depth();
    // Key is deep cloned here
    let key = param.into_owned(heap)?;
    let key = heap.snapshot(&key)?;
    if !key.is_hashable() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    key.calculate_depth(resources.max_value_depth()
            .saturating_sub(key_depth.saturating_add(1)))?;

    let mut zelf = zelf?;
    let len = zelf.as_ref(heap).as_map()?.len();

    if len >= u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }

    let param = parameters.remove(0);
    let value_depth = param.depth();
    let value = param.into_owned(heap)?;

    value.calculate_depth(heap, resources.max_value_depth()
            .saturating_sub(value_depth.saturating_add(1)))?;

    let value = heap.insert(value)?;
    let previous = zelf.as_mut(heap).as_mut_map()?.insert(key, value);

    Ok(SysCallResult::Return(match previous {
        Some(v) => v.into(),
        None => Primitive::Null.into(),
    }))
}

fn shift_remove<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    // We make it owned in case the key contains our only reference to itself
    let key = parameters.remove(0)
        .into_owned(heap)?;
    let key = heap.snapshot(&key)?;

    if !key.is_hashable() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let mut zelf = zelf?;
    let map = zelf.as_mut(heap)
        .as_mut_map()?;

    // pay based on O(N)
    resources.increase_gas_usage(map.len() as _)?;

    let value = map.shift_remove(&key);
    Ok(SysCallResult::Return(match value {
        Some(v) => v.into(),
        None => Primitive::Null.into(),
    }))
}

fn swap_remove<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    // We make it owned in case the key contains our only reference to itself
    let key = parameters.remove(0)
        .into_owned(heap)?;
    let key = heap.snapshot(&key)?;

    if !key.is_hashable() {
        return Err(EnvironmentError::InvalidKeyType);
    }
    resources.increase_gas_usage(key.calculate_memory_usage(resources.memory_left())? as u64)?;

    let value = zelf?.as_mut(heap)
        .as_mut_map()?
        .swap_remove(&key);

    Ok(SysCallResult::Return(match value {
        Some(v) => v.into(),
        None => Primitive::Null.into(),
    }))
}

fn clear<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let mut zelf = zelf?;
    let map = zelf.as_mut(heap).as_mut_map()?;
    resources.increase_gas_usage(map.len() as u64)?;
    map.clear();

    Ok(SysCallResult::None)
}

fn keys<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let zelf = zelf?;
    let map = zelf.as_ref(heap).as_map()?;

    // we need to go through all elements, thus we increase the gas usage
    resources.increase_gas_usage((map.len() as u64) * 8)?;

    let keys: Vec<_> = map.keys().cloned().collect();
    let keys = keys.iter().map(|key| { let key = heap.import(key)?; heap.insert(key) }).collect::<Result<_, HeapError>>()?;

    Ok(SysCallResult::Return(RuntimeValue::Object(keys).into()))
}

fn values<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let zelf = zelf?;
    let map = zelf.as_ref(heap).as_map()?;

    // we need to go through all elements, thus we increase the gas usage
    resources.increase_gas_usage((map.len() as u64) * 5)?;

    let values = map.values()
        .cloned()
        .collect::<Vec<_>>();

    Ok(SysCallResult::Return(RuntimeValue::Object(values).into()))
}

fn entries<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let zelf = zelf?;
    let map = zelf.as_ref(heap).as_map()?;

    // we need to go through all elements, thus we increase the gas usage
    resources.increase_gas_usage((map.len() as u64) * 10)?;

    let pairs: Vec<_> = map.iter().map(|(key, value)| (key.clone(), *value)).collect();
    let entries = pairs.into_iter().map(|(key, value)| {
        let key = heap.import(&key)?;
        let key = heap.insert(key)?;
        heap.insert(RuntimeValue::Object(vec![key, value]))
    }).collect::<Result<_, HeapError>>()?;

    Ok(SysCallResult::Return(RuntimeValue::Object(entries).into()))
}
