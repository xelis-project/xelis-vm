use crate::environment::xstd::native;
use silex_types::Heap;
use silex_environment::{
    VMContext,
    EnvironmentError,
    FnInstance,
    FnParams,
    FnReturnType,
    SysCallResult,
    ModuleMetadata,
};
use silex_types::{PrimitiveInteger, Type, Primitive, RuntimeValue};

use crate::EnvironmentBuilder;

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    let key_type = Type::T(Some(0));
    let value_type = Type::T(Some(1));
    let _type = Type::Map(Box::new(key_type.clone()), Box::new(value_type.clone()));
    env.register_native_function_with_comment("len", Some(_type.clone()), vec![], native(len), 1, Some(Type::U32), "Returns the number of entries in the map.");
    env.register_native_function_with_comment("contains_key", Some(_type.clone()), vec![("key", key_type.clone())], native(contains_key), 15, Some(Type::Bool), "Returns true when the map contains the key.");
    env.register_native_function_with_comment("get", Some(_type.clone()), vec![("key", key_type.clone())], native(get), 15, Some(Type::Optional(Box::new(value_type.clone()))), "Returns the value for the key, or null when absent.");
    env.register_native_function_with_comment("insert", Some(_type.clone()), vec![("key", key_type.clone()), ("value", value_type.clone())], native(insert), 30, Some(Type::Optional(Box::new(value_type.clone()))), "Inserts a key-value pair and returns the previous value, or null.");
    env.register_native_function_with_comment("shift_remove", Some(_type.clone()), vec![("key", key_type.clone())], native(shift_remove), 15, Some(Type::Optional(Box::new(value_type.clone()))), "Removes a key while preserving map order and returns its value, or null.");
    env.register_native_function_with_comment("swap_remove", Some(_type.clone()), vec![("key", key_type.clone())], native(swap_remove), 15, Some(Type::Optional(Box::new(value_type.clone()))), "Removes a key by swapping with the last entry and returns its value, or null.");
    env.register_native_function_with_comment("clear", Some(_type.clone()), vec![], native(clear), 5, None, "Removes all entries from the map.");
    env.register_native_function_with_comment("keys", Some(_type.clone()), vec![], native(keys), 20, Some(Type::Array(Box::new(key_type.clone()))), "Returns all keys in insertion order.");
    env.register_native_function_with_comment("values", Some(_type.clone()), vec![], native(values), 20, Some(Type::Array(Box::new(value_type.clone()))), "Returns all values in insertion order.");
    env.register_native_function_with_comment("entries", Some(_type.clone()), vec![], native(entries), 40, Some(Type::Array(Box::new(Type::Tuples(vec![key_type.clone(), value_type.clone()])))), "Returns all key-value pairs in insertion order.");
}

fn len<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let len = zelf?.as_ref(heap).as_map()?.len();
    Ok(SysCallResult::Return(Primitive::Integer(PrimitiveInteger::U32(len as u32)).into()))
}

fn contains_key<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let key = &parameters[0];
    let k = heap.snapshot_stack(key)?;
    if !k.is_hashable() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let contains = zelf?.as_ref(heap).as_map()?.contains_key(&k);
    Ok(SysCallResult::Return(Primitive::Boolean(contains).into()))
}

fn get<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let key = &parameters[0];
    let k = heap.snapshot_stack(key)?;
    if !k.is_hashable() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let value = zelf?.as_ref(heap).as_map()?
        .get(&k);

    Ok(SysCallResult::Return(match value {
        Some(v) => v.clone_value(heap).into(),
        None => Primitive::Null.into(),
    }))
}

fn insert<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    // Export directly to immutable owned key data; no detached runtime copy
    // is needed before crossing this boundary.
    let key = heap.export(parameters.remove(0))?;
    if !key.is_hashable() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let zelf = zelf?;
    let len = zelf.as_ref(heap).as_map()?.len();

    if len >= u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }

    let param = parameters.remove(0);
    let value = param.into_owned(heap)?;

    let value = heap.insert(value);
    let previous = zelf.as_mut(heap).as_mut_map()?.insert(key, value);

    Ok(SysCallResult::Return(match previous {
        Some(v) => v.into(),
        None => Primitive::Null.into(),
    }))
}

fn shift_remove<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    // Export consumes the argument while preserving any remaining aliases.
    let key = heap.export(parameters.remove(0))?;

    if !key.is_hashable() {
        return Err(EnvironmentError::InvalidKeyType);
    }

    let zelf = zelf?;
    let map = zelf.as_mut(heap)
        .as_mut_map()?;

    // pay based on O(N)
    context.increase_gas_usage(map.len() as _)?;

    let value = map.shift_remove(&key);
    Ok(SysCallResult::Return(match value {
        Some(v) => v.into(),
        None => Primitive::Null.into(),
    }))
}

fn swap_remove<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    // Export consumes the argument while preserving any remaining aliases.
    let key = heap.export(parameters.remove(0))?;

    if !key.is_hashable() {
        return Err(EnvironmentError::InvalidKeyType);
    }
    context.increase_gas_usage(key.calculate_memory_usage(context.memory_left())? as u64)?;

    let value = zelf?.as_mut(heap)
        .as_mut_map()?
        .swap_remove(&key);

    Ok(SysCallResult::Return(match value {
        Some(v) => v.into(),
        None => Primitive::Null.into(),
    }))
}

fn clear<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let zelf = zelf?;
    let map = zelf.as_mut(heap).as_mut_map()?;
    context.increase_gas_usage(map.len() as u64)?;
    map.clear();

    Ok(SysCallResult::None)
}

fn keys<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    // Temporarily move the runtime map out so its keys can be borrowed while
    // allocating runtime copies, without first cloning an OwnedValue tree.
    let keys = heap.with_value(zelf?, |value, heap| -> Result<_, EnvironmentError> {
        let map = value.as_map()?;
        context.increase_gas_usage((map.len() as u64) * 8)?;
        let keys = map.keys().map(|key| {
            let key = heap.import(key);
            heap.insert(key)
        }).collect();
        Ok(keys)
    })?;

    Ok(SysCallResult::Return(RuntimeValue::Object(keys).into()))
}

fn values<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let zelf = zelf?;
    let map = zelf.as_ref(heap).as_map()?;

    // we need to go through all elements, thus we increase the gas usage
    context.increase_gas_usage((map.len() as u64) * 5)?;

    let values = map.values()
        .cloned()
        .collect::<Vec<_>>();

    Ok(SysCallResult::Return(RuntimeValue::Object(values).into()))
}

fn entries<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let entries = heap.with_value(zelf?, |value, heap| -> Result<_, EnvironmentError> {
        let map = value.as_map()?;
        context.increase_gas_usage((map.len() as u64) * 10)?;
        let entries = map.iter().map(|(key, value)| {
            let key = heap.import(key);
            let key = heap.insert(key);
            heap.insert(RuntimeValue::Object(vec![key, value.clone()]))
        }).collect();
        Ok(entries)
    })?;

    Ok(SysCallResult::Return(RuntimeValue::Object(entries).into()))
}
