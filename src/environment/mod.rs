mod builder;
pub mod std;

use ::std::{collections::HashMap, hash::BuildHasherDefault};

pub use builder::EnvironmentBuilder;

use crate::{ast::{Function, Operator}, types::Struct, NoHashMap, NoOpHasher};

pub struct Environment {
    functions: NoHashMap<Function>,
    structures: NoHashMap<Struct>,
    operators: HashMap<Operator, u64, BuildHasherDefault<NoOpHasher>>
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
            structures: NoHashMap::default(),
            operators: HashMap::with_hasher(BuildHasherDefault::default())
        }
    }

    pub fn get_functions(&self) -> &NoHashMap<Function> {
        &self.functions
    }

    pub fn get_structures(&self) -> &NoHashMap<Struct> {
        &self.structures
    }

    // Get the gas used of an operator
    pub fn get_operator_cost(&self, operator: &Operator) -> Option<u64> {
        self.operators.get(operator).copied()
    }
}