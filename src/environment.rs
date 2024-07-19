use crate::{
    functions::{FnInstance, FnReturnType, FunctionType, NativeFunction, OnCallFn},
    interpreter::InterpreterError,
    mapper::FunctionMapper,
    types::{Struct, Type, Value},
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
        println!("Registering native function: {}", name);
        let id = self.functions_mapper.register((name.to_owned(), for_type.clone())).unwrap();
        self.functions.insert(id, FunctionType::Native(NativeFunction::new(for_type, parameters, on_call, cost, return_type)));
    }

    // pub fn register_structure(&mut self, name: String, structure: Struct) -> Option<Struct> {
    //     self.structures.insert(name, structure)
    // }

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
        env.register_native_function("len", Some(Type::Array(Box::new(Type::Any))), vec![], len, 1, Some(Type::Int));
        env.register_native_function("push", Some(Type::Array(Box::new(Type::Any))), vec![Type::Any], push, 1, None); // TODO Any
        env.register_native_function("remove", Some(Type::Array(Box::new(Type::Any))), vec![], remove, 1, Some(Type::Any));
        env.register_native_function("slice", Some(Type::Array(Box::new(Type::Any))), vec![Type::Int, Type::Int], slice, 3, Some(Type::Array(Box::new(Type::Any))));

        // String
        env.register_native_function("len", Some(Type::String), vec![], len, 1, Some(Type::Int));
        env.register_native_function("trim", Some(Type::String), vec![], trim, 1, Some(Type::String));
        env.register_native_function("contains", Some(Type::String), vec![Type::String], contains, 1, Some(Type::Boolean));
        env.register_native_function("contains_ignore_case", Some(Type::String), vec![Type::String], contains_ignore_case, 1, Some(Type::Boolean));
        env.register_native_function("to_uppercase", Some(Type::String), vec![], to_uppercase, 1, Some(Type::String));
        env.register_native_function("to_lowercase", Some(Type::String), vec![], to_lowercase, 1, Some(Type::String));
        env.register_native_function("to_bytes", Some(Type::String), vec![], to_bytes, 5, Some(Type::Array(Box::new(Type::Byte))));

        // env.register_native_function("index_of", Some(Type::String), vec![Type::String], println, 3, Some(Type::Int));
        // env.register_native_function("last_index_of", Some(Type::String), vec![Type::String], println, 3, Some(Type::Int));
        // env.register_native_function("replace", Some(Type::String), vec![Type::String, Type::String], println, 3, Some(Type::String));
        // env.register_native_function("starts_with", Some(Type::String), vec![Type::String], println, 3, Some(Type::Boolean));
        // env.register_native_function("ends_with", Some(Type::String), vec![Type::String], println, 3, Some(Type::Boolean));
        // env.register_native_function("split", Some(Type::String), vec![Type::String], println, 3, Some(Type::Array(Box::new(Type::String))));
        // env.register_native_function("char_at", Some(Type::String), vec![Type::Int], println, 1, Some(Type::String));

        // env.register_native_function("is_empty", Some(Type::String), vec![], println, 1, Some(Type::Boolean));
        // env.register_native_function("matches", Some(Type::String), vec![Type::String], println, 25, Some(Type::Boolean));
        // env.register_native_function("substring", Some(Type::String), vec![Type::Int], println, 3, Some(Type::String));
        // env.register_native_function("substring", Some(Type::String), vec![Type::Int, Type::Int], println, 3, Some(Type::String));
        // env.register_native_function("format", Some(Type::String), vec![Type::String, Type::Array(Box::new(Type::String))], println, 5, Some(Type::String));

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
    Ok(Some(Value::Int(len as u64)))
}

fn push(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let param = parameters.remove(0);
    zelf?.as_mut_vec()?.push(param);
    Ok(None)
}

fn remove(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let index = parameters.remove(0).to_int()? as usize;
    Ok(Some(zelf?.as_mut_vec()?.remove(index)))
}

fn slice(zelf: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let start = parameters.remove(0).to_int()?;
    let end = parameters.remove(0).to_int()?;

    let vec: &Vec<Value> = zelf?.as_vec()?;
    let len_u64 = vec.len() as u64;
    if start >= len_u64 || end >= len_u64 || start >= end {
        return Err(InterpreterError::InvalidRange(start, end))
    }

    let mut slice: Vec<Value> = Vec::new();
    for i in start..end {
        let value = match vec.get(i as usize) {
            Some(v) => v.clone(), // due to RefValue, slice are connected.
            None => return Err(InterpreterError::NoValueFoundAtIndex(i))
        };
        slice.push(value);
    }

    Ok(Some(Value::Array(slice)))
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

    let mut bytes: Vec<Value> = Vec::new();
    for byte in s.as_bytes() {
        bytes.push(Value::Byte(*byte));
    }

    Ok(Some(Value::Array(bytes)))
}
