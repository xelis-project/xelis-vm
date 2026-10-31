use crate::environment::xstd::native;
use silex_types::Heap;
use matriochka::ResultExt as _;
use silex_environment::{VMContext, EnvironmentError, FnInstance, FnParams, FnReturnType, SysCallResult, ModuleMetadata};
use silex_types::{PrimitiveInteger, OwnedValue, Primitive, Type, RuntimeValue, ValueError};

use crate::EnvironmentBuilder;

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    // Bytes
    env.register_native_function_with_comment("len", Some(Type::Bytes), vec![], native(len), 1, Some(Type::U32), "Returns the number of bytes.");
    env.register_native_function_with_comment("push", Some(Type::Bytes), vec![("byte", Type::U8)], native(push), 2, None, "Appends one byte to the end.");
    env.register_native_function_with_comment("remove", Some(Type::Bytes), vec![("index", Type::U32)], native(remove), 5, Some(Type::U8), "Removes and returns the byte at the index, shifting later bytes left.");
    env.register_native_function_with_comment("pop", Some(Type::Bytes), vec![], native(pop), 1, Some(Type::Optional(Box::new(Type::U8))), "Removes and returns the last byte, or null when empty.");
    env.register_native_function_with_comment("slice", Some(Type::Bytes), vec![("range", Type::Range(Box::new(Type::U32)))], native(slice), 5, Some(Type::Bytes), "Returns a copy of the bytes in the range.");
    env.register_native_function_with_comment("contains", Some(Type::Bytes), vec![("byte", Type::U8)], native(contains), 10, Some(Type::Bool), "Returns true when the byte sequence contains the byte.");
    env.register_native_function_with_comment("get", Some(Type::Bytes), vec![("index", Type::U32)], native(get), 1, Some(Type::Optional(Box::new(Type::U8))), "Returns the byte at the index, or null if out of bounds.");
    env.register_native_function_with_comment("first", Some(Type::Bytes), vec![], native(first), 1, Some(Type::Optional(Box::new(Type::U8))), "Returns the first byte, or null when empty.");
    env.register_native_function_with_comment("last", Some(Type::Bytes), vec![], native(last), 1, Some(Type::Optional(Box::new(Type::U8))), "Returns the last byte, or null when empty.");
    env.register_native_function_with_comment("to_array", Some(Type::Bytes), vec![], native(to_array), 1, Some(Type::Array(Box::new(Type::U8))), "Converts the bytes to an array of u8 values.");
    env.register_native_function_with_comment("split_off", Some(Type::Bytes), vec![("index", Type::U32)], native(split_off), 5, Some(Type::Bytes), "Splits the bytes at the index, keeping the first part and returning the second.");
    env.register_native_function_with_comment("extend", Some(Type::Bytes), vec![("other", Type::Bytes)], native(extend), 5, None, "Appends another byte sequence to the end.");
    env.register_native_function_with_comment("truncate", Some(Type::Bytes), vec![("size", Type::U32)], native(truncate), 5, None, "Shortens the byte sequence to the requested size.");

    env.register_native_function_with_comment("to_hex", Some(Type::Bytes), vec![], native(to_hex), 1, Some(Type::String), "Encodes the bytes as a lowercase hexadecimal string.");
    env.register_static_function_with_comment("from_hex", Type::Bytes, vec![("hex_string", Type::String)], native(from_hex), 1, Some(Type::Bytes), "Decodes a hexadecimal string into bytes.");

    env.register_const_function_with_comment("new", Type::Bytes, vec![], |_| Ok(OwnedValue::Bytes(Vec::new())), "Creates an empty byte sequence at compile time.");
    env.register_const_function_with_comment("from", Type::Bytes, vec![("values", Type::Array(Box::new(Type::U8)))], |values| {
        let mut bytes = Vec::with_capacity(values.len());
        for v in values {
            bytes.push(v.to_u8()?);
        }

        Ok(OwnedValue::Bytes(bytes))
    }, "Creates a byte sequence from an array of u8 values at compile time.");
}

// native functions
fn len<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let len = zelf?.as_bytes(heap)?.len();
    Ok(SysCallResult::Return(Primitive::Integer(PrimitiveInteger::U32(len as u32)).into()))
}

fn push<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let param = parameters.remove(0);
    let mut value = param.into_owned(heap)?;

    let zelf = zelf?;
    let array = zelf.as_mut(heap).as_bytes_mut()?;
    if array.len() >= u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }

    array.push(value.into_value()?.to_u8()?);

    Ok(SysCallResult::None)
}

fn remove<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let index = parameters.remove(0).into_owned(heap)?.as_u32()? as usize;

    let zelf = zelf?;
    let array = zelf.as_mut(heap).as_bytes_mut()?;
    if index >= array.len() {
        return Err(EnvironmentError::OutOfBounds(index, array.len()))
    }

    // moving all elements after the index to the left is costly
    context.increase_gas_usage((array.len() - index) as u64 * 2)?;

    Ok(SysCallResult::Return(Primitive::Integer(PrimitiveInteger::U8(array.remove(index))).into()))
}

fn pop<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let zelf = zelf?;
    let array = zelf.as_mut(heap).as_bytes_mut()?;
    if let Some(value) = array.pop() {
        Ok(SysCallResult::Return(Primitive::Integer(PrimitiveInteger::U8(value)).into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn slice<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let param = &parameters[0];
    let range = param.as_ref(heap);
    let range = range.as_range()?;
    let start = Primitive::Integer(range.start()).as_u32()?;
    let end = Primitive::Integer(range.end()).as_u32()?;

    let zelf = zelf?;
    let vec = zelf.as_mut(heap).as_bytes_mut()?;
    let len = vec.len() as u32;
    if start >= len || end >= len || start >= end {
        return Err(EnvironmentError::InvalidRange(start, end))
    }

    // we need to allocate all elements in the new slice, thus we increase the gas usage
    context.increase_gas_usage((end - start) as u64)?;

    let slice = vec[start as usize..end as usize].into();
    Ok(SysCallResult::Return(RuntimeValue::Bytes(slice).into()))
}

fn split_off<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let index = parameters.remove(0).into_owned(heap)?.as_u32()? as usize;

    let zelf = zelf?;
    let vec = zelf.as_mut(heap).as_bytes_mut()?;
    if index > vec.len() {
        return Err(EnvironmentError::OutOfBounds(index, vec.len()))
    }

    // we need to allocate all elements in the new slice, thus we increase the gas usage
    context.increase_gas_usage((vec.len() - index) as u64)?;

    let second = vec.split_off(index);
    Ok(SysCallResult::Return(RuntimeValue::Bytes(second).into()))
}

fn extend<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let param = parameters.remove(0);
    let value = param.into_owned(heap)?;

    let zelf = zelf?;
    let array = zelf.as_mut(heap).as_bytes_mut()?;
    let other = value.as_bytes()?;

    // we need to allocate all elements in the other slice, thus we increase the gas usage
    context.increase_gas_usage(other.len() as u64)?;

    array.extend_from_slice(other);

    Ok(SysCallResult::None)
}

fn truncate<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let size = parameters.remove(0).into_owned(heap)?.as_u32()? as usize;

    let zelf = zelf?;
    let vec = zelf.as_mut(heap).as_bytes_mut()?;
    if size > vec.len() {
        return Err(EnvironmentError::OutOfBounds(size, vec.len()))
    }

    context.increase_gas_usage((vec.len() - size) as u64)?;
    vec.truncate(size);

    Ok(SysCallResult::None)
}

fn contains<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let value = &parameters[0];
    let handle = value.as_ref(heap).as_u8()?;
    let zelf = zelf?;
    let vec = zelf.as_bytes(heap)?;

    // we need to go through all elements in the slice, thus we increase the gas usage
    context.increase_gas_usage(vec.len() as u64)?;

    Ok(SysCallResult::Return(Primitive::Boolean(vec.contains(&handle)).into()))
}

fn get<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let index = parameters.remove(0).into_owned(heap)?.as_u32()? as usize;
    let zelf = zelf?;
    let vec = zelf.as_bytes(heap)?;
    if let Some(value) = vec.get(index).copied() {
        Ok(SysCallResult::Return(Primitive::Integer(PrimitiveInteger::U8(value)).into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn first<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let zelf = zelf?;
    let vec = zelf.as_bytes(heap)?;
    if let Some(value) = vec.first().copied() {
        Ok(SysCallResult::Return(Primitive::Integer(PrimitiveInteger::U8(value)).into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn last<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let zelf = zelf?;
    let vec = zelf.as_bytes(heap)?;
    if let Some(value) = vec.last().copied() {
        Ok(SysCallResult::Return(Primitive::Integer(PrimitiveInteger::U8(value)).into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn to_array<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let zelf = zelf?;
    let vec = zelf.as_bytes(heap)?;

    context.increase_gas_usage((vec.len() as u64) * 2)?;

    let values = heap.with_value(zelf, |value, heap| {
        Ok::<_, ValueError>(value.as_bytes()?
            .iter()
            .map(|byte| heap.insert(Primitive::Integer(PrimitiveInteger::U8(*byte)).into()))
            .collect())
    })?;

    Ok(SysCallResult::Return(RuntimeValue::Object(values).into()))
}

fn from_hex<M>(_: FnInstance, params: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let value = params[0].as_string(heap)?;

    context.increase_gas_usage(value.len() as _)?;

    let values = hex::decode(&value)
        .context("decoding bytes from hexadecimal string")?;

    Ok(SysCallResult::Return(RuntimeValue::Bytes(values).into()))
}

fn to_hex<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let zelf = zelf?;
    let bytes = zelf.as_bytes(heap)?;

    context.increase_gas_usage(bytes.len() as _)?;

    let encoded = hex::encode(&bytes);
    Ok(SysCallResult::Return(Primitive::String(encoded).into()))
}
