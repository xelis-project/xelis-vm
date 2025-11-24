use xelis_environment::{Context, FunctionHandler};
use xelis_types::{Type, Primitive, ValueCell};
use super::{
    FnInstance,
    FnParams,
    FnReturnType,
    EnvironmentBuilder,
    SysCallResult,
    ModuleMetadata
};

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    // String
    env.register_native_function("len", Some(Type::String), vec![], FunctionHandler::Sync(len), 1, Some(Type::U32));
    env.register_native_function("trim", Some(Type::String), vec![], FunctionHandler::Sync(trim), 1, Some(Type::String));
    env.register_native_function("contains", Some(Type::String), vec![("value", Type::String)], FunctionHandler::Sync(contains), 1, Some(Type::Bool));
    env.register_native_function("contains_ignore_case", Some(Type::String), vec![("value", Type::String)], FunctionHandler::Sync(contains_ignore_case), 1, Some(Type::Bool));
    env.register_native_function("to_uppercase", Some(Type::String), vec![], FunctionHandler::Sync(to_uppercase), 1, Some(Type::String));
    env.register_native_function("to_lowercase", Some(Type::String), vec![], FunctionHandler::Sync(to_lowercase), 1, Some(Type::String));
    env.register_native_function("to_bytes", Some(Type::String), vec![], FunctionHandler::Sync(to_bytes), 5, Some(Type::Bytes));
    env.register_native_function("index_of", Some(Type::String), vec![("value", Type::String)], FunctionHandler::Sync(index_of), 3, Some(Type::Optional(Box::new(Type::U32))));
    env.register_native_function("last_index_of", Some(Type::String), vec![("value", Type::String)], FunctionHandler::Sync(last_index_of), 3, Some(Type::Optional(Box::new(Type::U32))));
    env.register_native_function("replace", Some(Type::String), vec![("from", Type::String), ("to", Type::String)], FunctionHandler::Sync(replace), 5, Some(Type::String));
    env.register_native_function("starts_with", Some(Type::String), vec![("value", Type::String)], FunctionHandler::Sync(starts_with), 3, Some(Type::Bool));
    env.register_native_function("ends_with", Some(Type::String), vec![("value", Type::String)], FunctionHandler::Sync(ends_with), 3, Some(Type::Bool));
    env.register_native_function("split", Some(Type::String), vec![("at", Type::String)], FunctionHandler::Sync(split), 5, Some(Type::Array(Box::new(Type::String))));
    env.register_native_function("char_at", Some(Type::String), vec![("index", Type::U32)], FunctionHandler::Sync(char_at), 1, Some(Type::Optional(Box::new(Type::String))));

    env.register_native_function("is_empty", Some(Type::String), vec![], FunctionHandler::Sync(is_empty), 1, Some(Type::Bool));
    env.register_native_function("matches", Some(Type::String), vec![("pattern", Type::String)], FunctionHandler::Sync(string_matches), 50, Some(Type::Array(Box::new(Type::String))));
    env.register_native_function("substring", Some(Type::String), vec![("value", Type::U32)], FunctionHandler::Sync(string_substring), 3, Some(Type::Optional(Box::new(Type::String))));
    env.register_native_function("substring_range", Some(Type::String), vec![("value", Type::U32), ("value", Type::U32)], FunctionHandler::Sync(string_substring_range), 3, Some(Type::Optional(Box::new(Type::String))));

    // from utf8
    env.register_static_function(
        "from_utf8",
        Type::String,
        vec![("bytes", Type::Bytes)],
        FunctionHandler::Sync(string_from_utf8),
        5,
        Some(Type::Optional(Box::new(Type::String)))
    );

    env.register_static_function(
        "from_utf8_lossy",
        Type::String,
        vec![("bytes", Type::Bytes)],
        FunctionHandler::Sync(string_from_utf8_lossy),
        5,
        Some(Type::String)
    );
}

fn len<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let len = zelf?.as_string()?.len();
    Ok(SysCallResult::Return(Primitive::U32(len as u32).into()))
}

fn trim<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let s = zelf?.as_string()?.trim().to_string();
    Ok(SysCallResult::Return(Primitive::String(s).into()))
}

fn contains<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let param = parameters.remove(0);
    let handle = param.as_ref();
    let value = handle.as_string()?;

    let contains = zelf?
        .as_string()?
        .contains(value);

    Ok(SysCallResult::Return(Primitive::Boolean(contains).into()))
}

fn contains_ignore_case<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let param = parameters.remove(0);
    let handle = param.as_ref();
    let value = handle.as_string()?.to_lowercase();
    let s: String = zelf?.as_string()?.to_lowercase();
    Ok(SysCallResult::Return(Primitive::Boolean(s.contains(&value)).into()))
}

fn to_uppercase<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let s: String = zelf?.as_string()?.to_uppercase();
    Ok(SysCallResult::Return(Primitive::String(s).into()))
}

fn to_lowercase<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let s: String = zelf?.as_string()?.to_lowercase();
    Ok(SysCallResult::Return(Primitive::String(s).into()))
}

fn to_bytes<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let s = zelf.as_ref()
        .as_string()?;

    let values = s.as_bytes();
    context.increase_gas_usage(values.len() as _)?;

    let bytes = values.to_vec();
    Ok(SysCallResult::Return(ValueCell::Bytes(bytes).into()))
}

fn index_of<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let s = zelf.as_ref()
        .as_string()?;

    let param = parameters.remove(0);
    let handle = param.as_ref();
    let value = handle.as_string()?;
    if let Some(index) = s.find(value) {
        let inner = Primitive::U32(index as u32).into();
        Ok(SysCallResult::Return(inner))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn last_index_of<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let s = zelf.as_ref()
        .as_string()?;

    let param = parameters.remove(0);
    let handle = param.as_ref();
    let value = handle.as_string()?;
    if let Some(index) = s.rfind(value) {
        let inner = Primitive::U32(index as u32).into();
        Ok(SysCallResult::Return(inner))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn replace<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let s = zelf.as_ref()
        .as_string()?;

    let param1 = parameters.remove(0);
    let param2 = parameters.remove(0);
    let handle1 = param1.as_ref();
    let handle2 = param2.as_ref();
    let old = handle1.as_string()?;
    let new = handle2.as_string()?;
    let s = s.replace(old, new);
    Ok(SysCallResult::Return(Primitive::String(s).into()))
}

fn starts_with<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let s = zelf.as_ref()
        .as_string()?;

        let param = parameters.remove(0);
    let handle = param.as_ref();
    let value = handle.as_string()?;
    Ok(SysCallResult::Return(Primitive::Boolean(s.starts_with(value)).into()))
}

fn ends_with<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let s = zelf.as_ref()
        .as_string()?;

    let param = parameters.remove(0);
    let handle = param.as_ref();
    let value = handle.as_string()?;
    Ok(SysCallResult::Return(Primitive::Boolean(s.ends_with(value)).into()))
}

fn split<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let s = zelf.as_ref()
        .as_string()?;

    let param = parameters.remove(0);
    let handle = param.as_ref();
    let value = handle.as_string()?;
    let values = s.split(value)
        .map(|s| Primitive::String(s.to_string()).into())
        .collect();

    Ok(SysCallResult::Return(ValueCell::Object(values).into()))
}

fn char_at<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let param =  parameters.remove(0);
    let index = param.as_u32()? as usize;

    let zelf = zelf?;
    let s = zelf.as_ref()
        .as_string()?;

    if let Some(c) = s.chars().nth(index) {
        let inner = Primitive::String(c.to_string()).into();
        Ok(SysCallResult::Return(inner))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn is_empty<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let s = zelf.as_ref()
        .as_string()?;

    Ok(SysCallResult::Return(Primitive::Boolean(s.is_empty()).into()))
}

fn string_matches<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let s = zelf.as_ref()
        .as_string()?;

    context.increase_gas_usage(s.len() as u64 * 5)?;

    let param = parameters.remove(0);
    let handle = param.as_ref();
    let value = handle.as_string()?;

    let m = s.matches(value);
    Ok(SysCallResult::Return(ValueCell::Object(m.map(|s| Primitive::String(s.to_string()).into()).collect()).into()))
}

fn string_substring<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let s = zelf.as_ref()
        .as_string()?;

    let param = parameters.remove(0);
    let start = param.as_u32()? as usize;
    if let Some(s) = s.get(start..) {
        let inner = Primitive::String(s.to_owned()).into();
        Ok(SysCallResult::Return(inner))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn string_substring_range<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let s = zelf.as_ref()
        .as_string()?;

    let param1 = parameters.remove(0);
    let param2 = parameters.remove(0);
    let start = param1.as_u32()? as usize;
    let end = param2.as_u32()? as usize;
    if let Some(s) = s.get(start..end) {
        let inner = Primitive::String(s.to_owned()).into();
        Ok(SysCallResult::Return(inner))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn string_from_utf8<M>(_: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let bytes = parameters[0]
        .as_bytes()?;

    context.increase_gas_usage(bytes.len() as u64)?;

    let str = String::from_utf8(bytes.clone())
        .ok()
        .map(|s| Primitive::String(s))
        .unwrap_or_default();

    Ok(SysCallResult::Return(str.into()))
}

fn string_from_utf8_lossy<M>(_: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let bytes = parameters[0]
        .as_bytes()?;

    context.increase_gas_usage(bytes.len() as u64)?;

    let str = String::from_utf8_lossy(&bytes).into_owned();

    Ok(SysCallResult::Return(Primitive::String(str).into()))
}