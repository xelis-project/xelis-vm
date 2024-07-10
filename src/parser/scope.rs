use std::collections::HashMap;
use crate::{types::Type, VariableIdentifier};

// Scope is used to store variables
// Variables are stored in a HashMap with a unique id
#[derive(Debug, Clone)]
pub struct Scope {
    variables: HashMap<VariableIdentifier, Type>
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: HashMap::new()
        }
    }

    pub fn insert(&mut self, key: VariableIdentifier, var_type: Type) {
        self.variables.insert(key, var_type);
    }

    pub fn get(&self, key: &VariableIdentifier) -> Option<&Type> {
        self.variables.get(key)
    }
}

impl Default for Scope {
    fn default() -> Self {
        Scope::new()
    }
}