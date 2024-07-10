use std::collections::HashMap;

use crate::{types::{Struct, Type}, ParserError, VariableIdentifier};

use super::variable_mapper::VariableMapper;

#[derive(Debug)]
pub struct StructBuilder {
    pub fields: HashMap<VariableIdentifier, Type>,
    pub mapper: VariableMapper
}

pub struct StructManager {
    structures: HashMap<String, StructBuilder>
}

impl StructManager {
    // Create a new struct manager
    pub fn new() -> Self {
        StructManager {
            structures: HashMap::new()
        }
    }

    // Get all the names of the structures
    pub fn inner(&self) -> &HashMap<String, StructBuilder> {
        &self.structures
    }

    // register a new struct in the manager
    pub fn add(&mut self, name: String, builder: StructBuilder) -> Result<(), ParserError> {
        if self.structures.contains_key(&name) {
            return Err(ParserError::StructNameAlreadyUsed(name));
        }

        self.structures.insert(name, builder);

        Ok(())
    }

    // Check if a struct exists
    pub fn has(&self, name: &String) -> bool {
        self.structures.contains_key(name)
    }

    // Get a struct by name
    pub fn get(&self, name: &String) -> Result<&StructBuilder, ParserError> {
        self.structures.get(name).ok_or_else(|| ParserError::StructNotFound(name.clone()))
    }

    // Convert the struct manager into a hashmap of structs
    pub fn finalize(self) -> HashMap<String, Struct> {
        self.structures.into_iter().map(|(k, v)| (k, Struct { fields: v.fields })).collect()
    }
}