use crate::types::{Type, Value, Struct};
use crate::functions::NativeFunction;

pub struct Environment {
    functions: Vec<NativeFunction>,
    structures: Vec<Struct>
}

impl Environment {
    pub fn new(functions: Vec<NativeFunction>, structures: Vec<Struct>) -> Self {
        Self {
            functions,
            structures
        }
    }

    pub fn default() -> Self {
        let mut functions = Vec::new();
        let /*mut*/ structures = Vec::new();
        
        functions.push(NativeFunction::new("println".to_owned(), None, vec![Type::Any], println, None));

        // Array
        functions.push(NativeFunction::new("len".to_owned(), Some(Type::Array(Box::new(Type::Any))), vec![], len, Some(Type::Int)));
        functions.push(NativeFunction::new("push".to_owned(), Some(Type::Array(Box::new(Type::Any))), vec![Type::Any], push, None));
        functions.push(NativeFunction::new("remove".to_owned(), Some(Type::Array(Box::new(Type::Any))), vec![], remove, Some(Type::Any)));
        functions.push(NativeFunction::new("slice".to_owned(), Some(Type::Array(Box::new(Type::Any))), vec![Type::Int, Type::Int], slice, None));

        // String TODO
        /*functions.push(NativeFunction::new("len".to_owned(), Some(Type::String), vec![], len, Some(Type::Int)));
        functions.push(NativeFunction::new("trim".to_owned(), Some(Type::String), vec![], println, Some(Type::String)));
        functions.push(NativeFunction::new("contains".to_owned(), Some(Type::String), vec![Type::String], println, Some(Type::Boolean)));
        functions.push(NativeFunction::new("contains_ignore_case".to_owned(), Some(Type::String), vec![Type::String], println, Some(Type::Boolean)));
        functions.push(NativeFunction::new("index_of".to_owned(), Some(Type::String), vec![Type::String], println, Some(Type::Int)));
        functions.push(NativeFunction::new("last_index_of".to_owned(), Some(Type::String), vec![Type::String], println, Some(Type::Int)));
        functions.push(NativeFunction::new("replace".to_owned(), Some(Type::String), vec![Type::String, Type::String], println, Some(Type::String)));
        functions.push(NativeFunction::new("starts_with".to_owned(), Some(Type::String), vec![Type::String], println, Some(Type::Boolean)));
        functions.push(NativeFunction::new("ends_with".to_owned(), Some(Type::String), vec![Type::String], println, Some(Type::Boolean)));
        functions.push(NativeFunction::new("split".to_owned(), Some(Type::String), vec![Type::String], println, Some(Type::Array(Box::new(Type::String)))));
        functions.push(NativeFunction::new("char_at".to_owned(), Some(Type::String), vec![Type::Int], println, Some(Type::String)));
        functions.push(NativeFunction::new("to_upper".to_owned(), Some(Type::String), vec![], println, Some(Type::String)));
        functions.push(NativeFunction::new("to_lower".to_owned(), Some(Type::String), vec![], println, Some(Type::String)));
        functions.push(NativeFunction::new("is_empty".to_owned(), Some(Type::String), vec![], println, Some(Type::Boolean)));
        functions.push(NativeFunction::new("matches".to_owned(), Some(Type::String), vec![Type::String], println, Some(Type::Boolean)));
        functions.push(NativeFunction::new("substring".to_owned(), Some(Type::String), vec![Type::Int], println, Some(Type::String)));
        functions.push(NativeFunction::new("substring".to_owned(), Some(Type::String), vec![Type::Int, Type::Int], println, Some(Type::String)));
        functions.push(NativeFunction::new("format".to_owned(), Some(Type::String), vec![Type::String, Type::Array(Box::new(Type::String))], println, Some(Type::String)));
        */
        Self::new(functions, structures)
    }

    pub fn consume(self) -> (Vec<NativeFunction>, Vec<Struct>) {
        (self.functions, self.structures)
    }
}

// native functions
fn len(zelf: &mut Value, _: Vec<Value>) -> Option<Value> {
    if let Value::Array(values) = zelf {
        return Some(Value::Int(values.len() as u64))
    }

    None
}

fn push(zelf: &mut Value, mut parameters: Vec<Value>) -> Option<Value> {
    let param = parameters.remove(0);
    if let Value::Array(ref mut values) = zelf {
        values.push(param)
    }

    None
}

fn remove(zelf: &mut Value, mut parameters: Vec<Value>) -> Option<Value> {
    let param = parameters.remove(0);
    if let Value::Array(ref mut values) = zelf {
        if let Value::Int(index) = param {
            return Some(values.remove(index as usize))
        }
    }

    None
}

fn slice(zelf: &mut Value, mut parameters: Vec<Value>) -> Option<Value> {
    let start = parameters.remove(0);
    let end = parameters.remove(1);

    None
}

fn println(_: &mut Value, mut parameters: Vec<Value>) -> Option<Value> {
    let param = parameters.remove(0);
    println!("{}", param);

    None
}