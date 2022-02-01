use crate::types::{Type, Value, Struct};
use crate::functions::{NativeFunction, OnCallFn};

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
        let mut structures = Vec::new();

        Self::new(functions, structures)
    }

    pub fn consume(self) -> (Vec<NativeFunction>, Vec<Struct>) {
        (self.functions, self.structures)
    }
}