use crate::{
    ast::{FnInstance, FnParams, FnReturnType},
    values::Value,
    EnvironmentBuilder,
    Type
};

pub fn register(env: &mut EnvironmentBuilder) {
    // String
    env.register_native_function("len", Some(Type::String), vec![], len, 1, Some(Type::U64));
    env.register_native_function("trim", Some(Type::String), vec![], trim, 1, Some(Type::String));
    env.register_native_function("contains", Some(Type::String), vec![Type::String], contains, 1, Some(Type::Bool));
    env.register_native_function("contains_ignore_case", Some(Type::String), vec![Type::String], contains_ignore_case, 1, Some(Type::Bool));
    env.register_native_function("to_uppercase", Some(Type::String), vec![], to_uppercase, 1, Some(Type::String));
    env.register_native_function("to_lowercase", Some(Type::String), vec![], to_lowercase, 1, Some(Type::String));
    env.register_native_function("to_bytes", Some(Type::String), vec![], to_bytes, 5, Some(Type::Array(Box::new(Type::U8))));
    env.register_native_function("index_of", Some(Type::String), vec![Type::String], index_of, 3, Some(Type::Optional(Box::new(Type::U64))));
    env.register_native_function("last_index_of", Some(Type::String), vec![Type::String], last_index_of, 3, Some(Type::Optional(Box::new(Type::U64))));
    env.register_native_function("replace", Some(Type::String), vec![Type::String, Type::String], replace, 5, Some(Type::String));
    env.register_native_function("starts_with", Some(Type::String), vec![Type::String], starts_with, 3, Some(Type::Bool));
    env.register_native_function("ends_with", Some(Type::String), vec![Type::String], ends_with, 3, Some(Type::Bool));
    env.register_native_function("split", Some(Type::String), vec![Type::String], split, 5, Some(Type::Array(Box::new(Type::String))));
    env.register_native_function("char_at", Some(Type::String), vec![Type::U64], char_at, 1, Some(Type::Optional(Box::new(Type::String))));

    env.register_native_function("is_empty", Some(Type::String), vec![], is_empty, 1, Some(Type::Bool));
    env.register_native_function("matches", Some(Type::String), vec![Type::String], string_matches, 50, Some(Type::Array(Box::new(Type::String))));
    env.register_native_function("substring", Some(Type::String), vec![Type::U64], string_substring, 3, Some(Type::Optional(Box::new(Type::String))));
    env.register_native_function("substring", Some(Type::String), vec![Type::U64, Type::U64], string_substring_range, 3, Some(Type::Optional(Box::new(Type::String))));   
}

fn len(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    Ok(Some(Value::U64(s.len() as u64)))
}

fn trim(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let s = zelf?.as_string()?.trim().to_string();
    Ok(Some(Value::String(s)))
}

fn contains(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let param = parameters.remove(0);
    let value = param.as_string()?;
    let s: &String = zelf?.as_string()?;
    Ok(Some(Value::Boolean(s.contains(value))))
}

fn contains_ignore_case(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let param = parameters.remove(0);
    let value = param.as_string()?.to_lowercase();
    let s: String = zelf?.as_string()?.to_lowercase();
    Ok(Some(Value::Boolean(s.contains(&value))))
}

fn to_uppercase(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let s: String = zelf?.as_string()?.to_uppercase();
    Ok(Some(Value::String(s)))
}

fn to_lowercase(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let s: String = zelf?.as_string()?.to_lowercase();
    Ok(Some(Value::String(s)))
}

fn to_bytes(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let s: &String = zelf?.as_string()?;

    let mut bytes = Vec::new();
    for b in s.as_bytes() {
        bytes.push(Value::U8(*b));
    }

    Ok(Some(Value::Array(bytes)))
}

fn index_of(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let param = parameters.remove(0);
    let value = param.as_string()?;
    if let Some(index) = s.find(value) {
        Ok(Some(Value::Optional(Some(Box::new(Value::U64(index as u64))))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn last_index_of(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let param = parameters.remove(0);
    let value = param.as_string()?;
    if let Some(index) = s.rfind(value) {
        Ok(Some(Value::Optional(Some(Box::new(Value::U64(index as u64))))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn replace(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let param1 = parameters.remove(0);
    let param2 = parameters.remove(0);
    let old = param1.as_string()?;
    let new = param2.as_string()?;
    let s = s.replace(old, new);
    Ok(Some(Value::String(s)))
}

fn starts_with(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let param = parameters.remove(0);
    let value = param.as_string()?;
    Ok(Some(Value::Boolean(s.starts_with(value))))
}

fn ends_with(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let param = parameters.remove(0);
    let value = param.as_string()?;
    Ok(Some(Value::Boolean(s.ends_with(value))))
}

fn split(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let param = parameters.remove(0);
    let value = param.as_string()?;
    let values = s.split(value)
        .map(|s| Value::String(s.to_string()))
        .collect();

    Ok(Some(Value::Array(values)))
}

fn char_at(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let param =  parameters.remove(0);
    let index = *param.as_u64()? as usize;
    let s: &String = zelf?.as_string()?;
    if let Some(c) = s.chars().nth(index) {
        Ok(Some(Value::Optional(Some(Box::new(Value::String(c.to_string()))))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn is_empty(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    Ok(Some(Value::Boolean(s.is_empty())))
}

fn string_matches(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let param = parameters.remove(0);
    let value = param.as_string()?;
    let m = s.matches(value);
    Ok(Some(Value::Array(m.map(|s| Value::String(s.to_string())).collect())))
}

fn string_substring(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let param = parameters.remove(0);
    let start = *param.as_u64()? as usize;
    if let Some(s) = s.get(start..) {
        Ok(Some(Value::Optional(Some(Box::new(Value::String(s.to_string()))))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn string_substring_range(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let param1 = parameters.remove(0);
    let param2 = parameters.remove(0);
    let start = *param1.as_u64()? as usize;
    let end = *param2.as_u64()? as usize;
    if let Some(s) = s.get(start..end) {
        Ok(Some(Value::Optional(Some(Box::new(Value::String(s.to_string()))))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}