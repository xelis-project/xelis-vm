use std::{collections::HashMap, fmt::Debug, hash::Hash};

use crate::{types::Type, IdentifierType, ParserError};

pub type IdMapper = Mapper<String>;

type FunctionSignature = (String, Option<Type>, Vec<Type>);
pub type FunctionMapper = Mapper<FunctionSignature>;

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
    pub fn get_compatible(&self, key: FunctionSignature) -> Result<IdentifierType, ParserError> {
        match self.get(&key) {
            Ok(id) => Ok(id),
            Err(e) => match key.1 {
                Some(t) => {
                    let new_type = match t {
                        Type::Array(_) => Type::Array(Box::new(Type::Any)),
                        Type::Optional(_) => Type::Optional(Box::new(Type::Any)),
                        _ => t
                    };

                    self.get(&(key.0, Some(new_type), key.2))
                },
                None => Err(e)
            }
        }
    }
}