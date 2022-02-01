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

        // Array
        functions.push(NativeFunction::new("len".to_owned(), Some(Type::Array(Box::new(Type::Any))), vec![], len, Some(Type::Int)));
        functions.push(NativeFunction::new("push".to_owned(), Some(Type::Array(Box::new(Type::Any))), vec![Type::Any], push, None));
        functions.push(NativeFunction::new("remove".to_owned(), Some(Type::Array(Box::new(Type::Any))), vec![], remove, Some(Type::Any)));
        functions.push(NativeFunction::new("println".to_owned(), None, vec![Type::Any], println, None));

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

fn println(_: &mut Value, mut parameters: Vec<Value>) -> Option<Value> {
    let param = parameters.remove(0);
    println!("{}", param);

    None
}