mod builder;
pub mod std;

pub use builder::EnvironmentBuilder;

use crate::{ast::FunctionType, types::Struct, NoHashMap};

pub struct Environment {
    functions: NoHashMap<FunctionType>,
    structures: NoHashMap<Struct>
}

impl Default for Environment {
    fn default() -> Self {
        let builder = EnvironmentBuilder::default();
        builder.build()
    }
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            functions: NoHashMap::default(),
            structures: NoHashMap::default()
        }
    }

    pub fn get_functions(&self) -> &NoHashMap<FunctionType> {
        &self.functions
    }

    pub fn get_structures(&self) -> &NoHashMap<Struct> {
        &self.structures
    }
}