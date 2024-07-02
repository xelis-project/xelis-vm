use std::collections::HashMap;
use crate::types::Type;
use super::VariableId;

// Scope is used to store variables
// Variables are stored in a HashMap with a unique id
#[derive(Debug, Clone)]
pub struct Scope {
    variables: HashMap<VariableId, Type>
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: HashMap::new()
        }
    }

    pub fn insert(&mut self, key: VariableId, var_type: Type) {
        self.variables.insert(key, var_type);
    }

    pub fn get(&self, key: &VariableId) -> Option<&Type> {
        self.variables.get(key)
    }
}

impl Default for Scope {
    fn default() -> Self {
        Scope::new()
    }
}