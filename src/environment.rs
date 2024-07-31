use crate::{
    functions::{FnInstance, FnReturnType, FunctionType, NativeFunction, OnCallFn},
    interpreter::InterpreterError,
    mapper::FunctionMapper,
    types::{Struct, Type},
    values::{Value, ValueVariant},
    IdentifierType
};
use std::collections::HashMap;

pub struct EnvironmentBuilder {
    functions_mapper: FunctionMapper,
    functions: HashMap<IdentifierType, FunctionType>,
    structures: HashMap<IdentifierType, Struct>
}

impl EnvironmentBuilder {
    pub fn register_native_function(&mut self, name: &str, for_type: Option<Type>, parameters: Vec<Type>, on_call: OnCallFn, cost: u64, return_type: Option<Type>) {
        let id = self.functions_mapper.register((name.to_owned(), for_type.clone(), parameters.clone())).unwrap();
        self.functions.insert(id, FunctionType::Native(NativeFunction::new(for_type, parameters, on_call, cost, return_type)));
    }

    pub fn build(self) -> (Environment, FunctionMapper) {
        (Environment {
            functions: self.functions,
            structures: self.structures
        }, self.functions_mapper)
    }
}

impl Default for EnvironmentBuilder {
    fn default() -> Self {
        let mut env = Self {
            functions_mapper: FunctionMapper::new(),
            functions: HashMap::new(),
            structures: HashMap::new()
        };

        env.register_native_function("println", None, vec![Type::Any], println, 1, None);

        // Array
        env.register_native_function("len", Some(Type::Array(Box::new(Type::Any))), vec![], len, 1, Some(Type::U64));
        env.register_native_function("push", Some(Type::Array(Box::new(Type::Any))), vec![Type::T], push, 1, None); // TODO Any
        env.register_native_function("remove", Some(Type::Array(Box::new(Type::Any))), vec![Type::T], remove, 1, Some(Type::T));
        env.register_native_function("slice", Some(Type::Array(Box::new(Type::Any))), vec![Type::U64, Type::U64], slice, 3, Some(Type::Array(Box::new(Type::T))));
        env.register_native_function("contains", Some(Type::Array(Box::new(Type::Any))), vec![Type::T], array_contains, 1, Some(Type::Bool));
        env.register_native_function("get", Some(Type::Array(Box::new(Type::Any))), vec![Type::U64], array_get, 1, Some(Type::Optional(Box::new(Type::T))));

        // Optional
        env.register_native_function("is_none", Some(Type::Optional(Box::new(Type::Any))), vec![], is_none, 1, Some(Type::Bool));
        env.register_native_function("is_some", Some(Type::Optional(Box::new(Type::Any))), vec![], is_some, 1, Some(Type::Bool));
        env.register_native_function("take", Some(Type::Optional(Box::new(Type::Any))), vec![], optional_take, 1, Some(Type::T));

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
        env.register_native_function("substring", Some(Type::String), vec![Type::U64, Type::U64], string_subtring_range, 3, Some(Type::Optional(Box::new(Type::String))));

        env
    }
}

pub struct Environment {
    functions: HashMap<IdentifierType, FunctionType>,
    structures: HashMap<IdentifierType, Struct>
}

impl Environment {
    pub fn new() -> (Self, FunctionMapper) {
        let builder = EnvironmentBuilder::default();
        builder.build()
    }

    pub fn get_functions(&self) -> &HashMap<IdentifierType, FunctionType> {
        &self.functions
    }

    // pub fn get_structure(&self, name: &String) -> Option<&Struct> {
    //     self.structures.get(name)
    // }

    pub fn get_structures(&self) -> &HashMap<IdentifierType, Struct> {
        &self.structures
    }
}

// native functions
fn len(zelf: FnInstance, _: Vec<Value>) -> FnReturnType {
    let len = match zelf? {
        Value::Array(values) => values.len(),
        Value::String(s) => s.len(),
        v => return Err(InterpreterError::InvalidValue(v.clone(), Type::String))
    };
    Ok(Some(Value::U64(len as u64)))
}

fn push(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let param = parameters.remove(0);
    zelf?.as_mut_vec()?.push(ValueVariant::Value(param));
    Ok(None)
}

fn remove(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let index = parameters.remove(0).to_u64()? as usize;

    let array = zelf?.as_mut_vec()?;
    if index >= array.len() {
        return Err(InterpreterError::OutOfBounds(index, array.len()))
    }

    Ok(Some(array.remove(index).into_value()))
}

fn slice(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let start = parameters.remove(0).to_u64()?;
    let end = parameters.remove(0).to_u64()?;

    let vec: &Vec<ValueVariant> = zelf?.as_vec()?;
    let len_u64 = vec.len() as u64;
    if start >= len_u64 || end >= len_u64 || start >= end {
        return Err(InterpreterError::InvalidRange(start, end))
    }

    let mut slice: Vec<ValueVariant> = Vec::new();
    for i in start..end {
        // due to SharedValue, slice are connected.
        let value = match vec.get(i as usize) {
            Some(v) => v.clone(),
            None => return Err(InterpreterError::NoValueFoundAtIndex(i))
        };
        slice.push(value);
    }

    Ok(Some(Value::Array(slice)))
}

fn array_contains(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let value = parameters.remove(0);
    let vec: &Vec<ValueVariant> = zelf?.as_vec()?;
    Ok(Some(Value::Boolean(vec.contains(&ValueVariant::Value(value)))))
}

fn array_get(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let index = parameters.remove(0).to_u64()? as usize;
    let vec: &Vec<ValueVariant> = zelf?.as_vec()?;
    if let Some(value) = vec.get(index) {
        Ok(Some(Value::Optional(Some(Box::new(value.clone_value())))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn println(_: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let param = parameters.remove(0);
    println!("{}", param);

    Ok(None)
}

// string

fn trim(zelf: FnInstance, _: Vec<Value>) -> FnReturnType {
    let s = zelf?.as_string()?.trim().to_string();
    Ok(Some(Value::String(s)))
}

fn contains(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let value = parameters.remove(0).to_string()?;
    let s: &String = zelf?.as_string()?;
    Ok(Some(Value::Boolean(s.contains(&value))))
}

fn contains_ignore_case(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let value = parameters.remove(0).to_string()?.to_lowercase();
    let s: String = zelf?.as_string()?.to_lowercase();
    Ok(Some(Value::Boolean(s.contains(&value))))
}

fn to_uppercase(zelf: FnInstance, _: Vec<Value>) -> FnReturnType {
    let s: String = zelf?.as_string()?.to_uppercase();
    Ok(Some(Value::String(s)))
}

fn to_lowercase(zelf: FnInstance, _: Vec<Value>) -> FnReturnType {
    let s: String = zelf?.as_string()?.to_lowercase();
    Ok(Some(Value::String(s)))
}

fn to_bytes(zelf: FnInstance, _: Vec<Value>) -> FnReturnType {
    let s: &String = zelf?.as_string()?;

    let mut bytes: Vec<ValueVariant> = Vec::new();
    for b in s.as_bytes() {
        bytes.push(ValueVariant::Value(Value::U8(*b)));
    }

    Ok(Some(Value::Array(bytes)))
}

fn index_of(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let value = parameters.remove(0).to_string()?;
    if let Some(index) = s.find(&value) {
        Ok(Some(Value::Optional(Some(Box::new(Value::U64(index as u64))))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn last_index_of(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let value = parameters.remove(0).to_string()?;
    if let Some(index) = s.rfind(&value) {
        Ok(Some(Value::Optional(Some(Box::new(Value::U64(index as u64))))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn replace(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let old = parameters.remove(0).to_string()?;
    let new = parameters.remove(0).to_string()?;
    let s = s.replace(&old, &new);
    Ok(Some(Value::String(s)))
}

fn starts_with(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let value = parameters.remove(0).to_string()?;
    Ok(Some(Value::Boolean(s.starts_with(&value))))
}

fn ends_with(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let value = parameters.remove(0).to_string()?;
    Ok(Some(Value::Boolean(s.ends_with(&value))))
}

fn split(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let value = parameters.remove(0).to_string()?;
    let values: Vec<ValueVariant> = s.split(&value)
        .map(|s| ValueVariant::Value(Value::String(s.to_string())))
        .collect();

    Ok(Some(Value::Array(values)))
}

fn char_at(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let index = parameters.remove(0).to_u64()? as usize;
    let s: &String = zelf?.as_string()?;
    if let Some(c) = s.chars().nth(index) {
        Ok(Some(Value::Optional(Some(Box::new(Value::String(c.to_string()))))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn is_empty(zelf: FnInstance, _: Vec<Value>) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    Ok(Some(Value::Boolean(s.is_empty())))
}

fn string_matches(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let value = parameters.remove(0).to_string()?;
    let m = s.matches(&value);
    Ok(Some(Value::Array(m.map(|s| ValueVariant::Value(Value::String(s.to_string()))).collect())))
}

fn string_substring(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let start = parameters.remove(0).to_u64()? as usize;
    if let Some(s) = s.get(start..) {
        Ok(Some(Value::Optional(Some(Box::new(Value::String(s.to_string()))))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

fn string_subtring_range(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let s: &String = zelf?.as_string()?;
    let start = parameters.remove(0).to_u64()? as usize;
    let end = parameters.remove(0).to_u64()? as usize;
    if let Some(s) = s.get(start..end) {
        Ok(Some(Value::Optional(Some(Box::new(Value::String(s.to_string()))))))
    } else {
        Ok(Some(Value::Optional(None)))
    }
}

// Optional

fn is_none(zelf: FnInstance, _: Vec<Value>) -> FnReturnType {
    Ok(Some(Value::Boolean(zelf?.as_optional(&Type::T)?.is_none())))
}

fn is_some(zelf: FnInstance, _: Vec<Value>) -> FnReturnType {
    Ok(Some(Value::Boolean(zelf?.as_optional(&Type::T)?.is_some())))
}

fn optional_take(zelf: FnInstance, _: Vec<Value>) -> FnReturnType {
    Ok(Some(zelf?.take_from_optional(&Type::T)?))
}