use crate::functions::{FunctionType, NativeFunction, FnInstance, FnReturnType};
use crate::types::{Type, Value, Struct};
use crate::interpreter::InterpreterError;
use std::collections::HashMap;

pub struct Environment {
    functions: Vec<FunctionType>,
    structures: HashMap<String, Struct>
}

impl Environment {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            structures: HashMap::new()
        }
    }

    pub fn default() -> Self {
        let mut env = Self::new();

        env.register_native_function(NativeFunction::new("println".to_owned(), None, vec![Type::Any], println, 1, None));

        // Array
        env.register_native_function(NativeFunction::new("len".to_owned(), Some(Type::Array(Box::new(Type::Any))), vec![], len, 1, Some(Type::Int)));
        env.register_native_function(NativeFunction::new("push".to_owned(), Some(Type::Array(Box::new(Type::Any))), vec![Type::Any], push, 1, None)); // TODO Any
        env.register_native_function(NativeFunction::new("remove".to_owned(), Some(Type::Array(Box::new(Type::Any))), vec![], remove, 1, Some(Type::Any)));
        env.register_native_function(NativeFunction::new("slice".to_owned(), Some(Type::Array(Box::new(Type::Any))), vec![Type::Int, Type::Int], slice, 3, Some(Type::Array(Box::new(Type::Any)))));

        // String
        env.register_native_function(NativeFunction::new("len".to_owned(), Some(Type::String), vec![], len, 1, Some(Type::Int)));
        env.register_native_function(NativeFunction::new("trim".to_owned(), Some(Type::String), vec![], trim, 1, Some(Type::String)));
        env.register_native_function(NativeFunction::new("contains".to_owned(), Some(Type::String), vec![Type::String], contains, 1, Some(Type::Boolean)));
        env.register_native_function(NativeFunction::new("contains_ignore_case".to_owned(), Some(Type::String), vec![Type::String], contains_ignore_case, 1, Some(Type::Boolean)));
        env.register_native_function(NativeFunction::new("to_uppercase".to_owned(), Some(Type::String), vec![], to_uppercase, 1, Some(Type::String)));
        env.register_native_function(NativeFunction::new("to_lowercase".to_owned(), Some(Type::String), vec![], to_lowercase, 1, Some(Type::String)));
        env.register_native_function(NativeFunction::new("to_bytes".to_owned(), Some(Type::String), vec![], to_bytes, 5, Some(Type::Array(Box::new(Type::Byte)))));

        env.register_native_function(NativeFunction::new("index_of".to_owned(), Some(Type::String), vec![Type::String], println, 3, Some(Type::Int)));
        env.register_native_function(NativeFunction::new("last_index_of".to_owned(), Some(Type::String), vec![Type::String], println, 3, Some(Type::Int)));
        env.register_native_function(NativeFunction::new("replace".to_owned(), Some(Type::String), vec![Type::String, Type::String], println, 3, Some(Type::String)));
        env.register_native_function(NativeFunction::new("starts_with".to_owned(), Some(Type::String), vec![Type::String], println, 3, Some(Type::Boolean)));
        env.register_native_function(NativeFunction::new("ends_with".to_owned(), Some(Type::String), vec![Type::String], println, 3, Some(Type::Boolean)));
        env.register_native_function(NativeFunction::new("split".to_owned(), Some(Type::String), vec![Type::String], println, 3, Some(Type::Array(Box::new(Type::String)))));
        env.register_native_function(NativeFunction::new("char_at".to_owned(), Some(Type::String), vec![Type::Int], println, 1, Some(Type::String)));

        env.register_native_function(NativeFunction::new("is_empty".to_owned(), Some(Type::String), vec![], println, 1, Some(Type::Boolean)));
        env.register_native_function(NativeFunction::new("matches".to_owned(), Some(Type::String), vec![Type::String], println, 25, Some(Type::Boolean)));
        env.register_native_function(NativeFunction::new("substring".to_owned(), Some(Type::String), vec![Type::Int], println, 3, Some(Type::String)));
        env.register_native_function(NativeFunction::new("substring".to_owned(), Some(Type::String), vec![Type::Int, Type::Int], println, 3, Some(Type::String)));
        env.register_native_function(NativeFunction::new("format".to_owned(), Some(Type::String), vec![Type::String, Type::Array(Box::new(Type::String))], println, 5, Some(Type::String)));

        env
    }

    pub fn register_native_function(&mut self, function: NativeFunction) {
        self.functions.push(FunctionType::Native(function));
    }

    pub fn register_structure(&mut self, structure: Struct) -> Option<Struct> {
        self.structures.insert(structure.name.clone(), structure)
    }

    pub fn get_functions(&self) -> &Vec<FunctionType> {
        &self.functions
    }

    pub fn get_structure(&self, name: &String) -> Option<&Struct> {
        self.structures.get(name)
    }

    pub fn get_structures(&self) -> &HashMap<String, Struct> {
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
