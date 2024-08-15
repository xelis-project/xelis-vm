use crate::{types::Type, IdentifierType, NoHashMap};

// Scope is used to store variables
// Variables are stored in a HashMap with a unique id
#[derive(Debug, Clone)]
pub struct Scope {
    variables: NoHashMap<Type>
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            variables: NoHashMap::default()
        }
    }

    // Insert a new variable in the scope
    pub fn insert(&mut self, key: IdentifierType, var_type: Type) {
        self.variables.insert(key, var_type);
    }

    // Get a variable from the scope
    pub fn get(&self, key: &IdentifierType) -> Option<&Type> {
        self.variables.get(key)
    }
}

impl Default for Scope {
    fn default() -> Self {
        Scope::new()
    }
}