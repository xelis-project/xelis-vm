mod builder;
pub mod std;
mod error;
mod function;


use ::std::{collections::HashMap, hash::BuildHasherDefault};

pub use builder::EnvironmentBuilder;
pub use error::EnvironmentError;
pub use function::*;

use crate::{
    ast::Operator,
    types::Struct,
    NoOpHasher
};

pub struct Environment {
    functions: Vec<NativeFunction>,
    structures: Vec<Struct>,
    operators: HashMap<Operator, u64, BuildHasherDefault<NoOpHasher>>
}

impl Default for Environment {
    fn default() -> Self {
        let builder = EnvironmentBuilder::default();
        builder.build()
    }
}

impl Environment {
    // Create a new environment
    pub fn new() -> Self {
        Environment {
            functions: Vec::new(),
            structures: Vec::new(),
            operators: HashMap::with_hasher(BuildHasherDefault::default())
        }
    }

    // Get all the registered functions
    #[inline(always)]
    pub fn get_functions(&self) -> &Vec<NativeFunction> {
        &self.functions
    }

    // Get all the registered structures
    #[inline(always)]
    pub fn get_structures(&self) -> &Vec<Struct> {
        &self.structures
    }

    // Get the gas used of an operator
    #[inline(always)]
    pub fn get_operator_cost(&self, operator: &Operator) -> Option<u64> {
        self.operators.get(operator).copied()
    }
}