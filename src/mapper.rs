use std::{collections::HashMap, fmt::Debug, hash::Hash};
use crate::{functions::Signature, IdentifierType, ParserError};

pub type IdMapper = Mapper<String>;
pub type FunctionMapper = Mapper<Signature>;

// VariableMapper is used to store the mapping between variable names and their identifiers
// So we can reduce the memory footprint of the interpreter by using an incremented id
#[derive(Debug, Clone)]
pub struct Mapper<T: Clone + Eq + Hash> {
    next_id: IdentifierType,
    mappings: HashMap<T, IdentifierType>
}

impl<T: Clone + Eq + Hash + Debug> Default for Mapper<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T: Clone + Eq + Hash + Debug> Mapper<T> {
    // Create a new VariableMapper
    pub fn new() -> Self {
        Self {
            next_id: 0,
            mappings: HashMap::new()
        }
    }

    // Get the identifier of a variable name
    pub fn get(&self, name: &T) -> Result<IdentifierType, ParserError> {
        self.mappings.get(name).copied().ok_or_else(|| ParserError::MappingNotFound(format!("{:?}", name)))
    }

    // Check if a variable name is already registered
    pub fn has_variable(&self, name: &T) -> bool {
        self.mappings.contains_key(name)
    }

    // Register a new variable name
    pub fn register(&mut self, name: T) -> Result<IdentifierType, ParserError> {
        let id = self.next_id;
        if let Some(v) = self.mappings.insert(name, id) {
            return Err(ParserError::MappingExists(v));
        }

        self.next_id += 1;
        Ok(id)
    }
}

impl FunctionMapper {
    pub fn get_compatible(&self, key: Signature) -> Result<IdentifierType, ParserError> {
        if let Ok(id) = self.get(&key) {
            return Ok(id);
        }

        for (signature, id) in self.mappings.iter().filter(|(s, _)| s.get_name() == key.get_name()) {
            let params = signature.get_parameters().iter().zip(key.get_parameters()).all(|(s, k)| s.is_compatible_with(k));
            let on_type = match (signature.get_on_type(), key.get_on_type()) {
                (Some(s), Some(k)) => s.is_compatible_with(k),
                (None, None) => true,
                _ => false
            };

            if params && on_type {
                return Ok(id.clone());
            }
        }

        Err(ParserError::MappingNotFound(format!("{:?}", key)))
    }
}