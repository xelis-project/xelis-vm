use std::collections::HashMap;

use crate::{ParserError, VariableIdentifier};

// VariableMapper is used to store the mapping between variable names and their identifiers
// So we can reduce the memory footprint of the interpreter by using an incremented id
#[derive(Debug)]
pub struct VariableMapper {
    next_id: u64,
    mappings: HashMap<String, VariableIdentifier>
}

impl VariableMapper {
    // Create a new VariableMapper
    pub fn new() -> Self {
        Self {
            next_id: 0,
            mappings: HashMap::new()
        }
    }

    // Get the identifier of a variable name
    pub fn get(&self, name: &String) -> Result<VariableIdentifier, ParserError> {
        self.mappings.get(name).copied().ok_or_else(|| ParserError::MappingNotFound)
    }

    pub fn has_variable(&self, name: &String) -> bool {
        self.mappings.contains_key(name)
    }

    // Register a new variable name
    pub fn register(&mut self, name: String) -> VariableIdentifier {
        let id = self.next_id;
        self.mappings.insert(name, id);
        self.next_id += 1;
        id
    }
}