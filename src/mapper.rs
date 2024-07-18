use std::{collections::HashMap, hash::Hash};

use crate::{types::Type, IdentifierType, ParserError};

pub type IdMapper = Mapper<String>;

pub type FunctionMapper = Mapper<(String, Option<Type>)>;

// VariableMapper is used to store the mapping between variable names and their identifiers
// So we can reduce the memory footprint of the interpreter by using an incremented id
#[derive(Debug, Clone)]
pub struct Mapper<T: Clone + Eq + Hash> {
    next_id: IdentifierType,
    mappings: HashMap<T, IdentifierType>
}

impl<T: Clone + Eq + Hash> Default for Mapper<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone + Eq + Hash> Mapper<T> {
    // Create a new VariableMapper
    pub fn new() -> Self {
        Self {
            next_id: 0,
            mappings: HashMap::new()
        }
    }

    // Get the identifier of a variable name
    pub fn get(&self, name: &T) -> Result<IdentifierType, ParserError> {
        self.mappings.get(name).copied().ok_or_else(|| ParserError::MappingNotFound)
    }

    // Check if a variable name is already registered
    pub fn has_variable(&self, name: &T) -> bool {
        self.mappings.contains_key(name)
    }

    // Register a new variable name
    pub fn register(&mut self, name: T) -> IdentifierType {
        let id = self.next_id;
        self.mappings.insert(name, id);
        self.next_id += 1;
        id
    }
}

impl FunctionMapper {
    pub fn get_compatible(&self, key: (String, Option<Type>)) -> Result<IdentifierType, ParserError> {
        match self.get(&key) {
            Ok(id) => Ok(id),
            Err(e) => match key.1 {
                Some(t) => {
                    let new_type = if t.is_array() {
                        Type::Array(Box::new(Type::Any))
                    } else {
                        Type::Any
                    };

                    self.get(&(key.0, Some(new_type)))
                },
                None => Err(e)
            }
        }
    }
}