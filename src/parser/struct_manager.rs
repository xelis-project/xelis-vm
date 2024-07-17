use std::collections::HashMap;

use crate::{types::{Struct, Type}, ParserError, IdentifierType, mapper::IdMapper};

#[derive(Debug)]
pub struct StructBuilder {
    pub fields: HashMap<IdentifierType, Type>,
    pub mapper: IdMapper
}

pub struct StructManager {
    // All structs registered in the manager
    structures: HashMap<IdentifierType, StructBuilder>,
    // mapper to map each string name into a unique identifier
    mapper: IdMapper
}

impl StructManager {
    // Create a new struct manager
    pub fn new() -> Self {
        StructManager {
            structures: HashMap::new(),
            mapper: IdMapper::new()
        }
    }

    // Get all the names of the structures
    pub fn inner(&self) -> &HashMap<IdentifierType, StructBuilder> {
        &self.structures
    }

    // register a new struct in the manager
    pub fn add(&mut self, name: String, builder: StructBuilder) -> Result<(), ParserError> {
        if self.mapper.has_variable(&name) {
            return Err(ParserError::StructNameAlreadyUsed(name));
        }

        let id = self.mapper.register(name);
        self.structures.insert(id, builder);

        Ok(())
    }

    // Check if a struct exists
    pub fn has(&self, name: &IdentifierType) -> bool {
        self.structures.contains_key(name)
    }

    // Get the mapping of a struct by name
    pub fn get_mapping(&self, name: &String) -> Result<IdentifierType, ParserError> {
        self.mapper.get(name)
    }

    // Get a struct by name
    pub fn get(&self, name: &IdentifierType) -> Result<&StructBuilder, ParserError> {
        self.structures.get(name).ok_or_else(|| ParserError::StructNotFound(name.clone()))
    }

    // Convert the struct manager into a hashmap of structs
    pub fn finalize(self) -> HashMap<IdentifierType, Struct> {
        self.structures.into_iter().map(|(k, v)| (k, Struct { fields: v.fields })).collect()
    }
}