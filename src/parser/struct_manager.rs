use std::collections::{HashMap, HashSet};

use crate::{types::Type, ParserError, VariableIdentifier};

use super::variable_mapper::VariableMapper;

#[derive(Debug)]
pub struct StructBuilder {
    pub fields: HashMap<VariableIdentifier, Type>,
    pub mapper: VariableMapper
}

pub struct StructManager {
    keys: HashSet<String>,
    structures: HashMap<String, StructBuilder>
}

impl StructManager {
    pub fn new() -> Self {
        StructManager {
            keys: HashSet::new(),
            structures: HashMap::new()
        }
    }

    pub fn keys(&self) -> &HashSet<String> {
        &self.keys
    }

    pub fn add(&mut self, name: String, builder: StructBuilder) -> Result<(), ParserError> {
        if !self.keys.insert(name.clone()) {
            return Err(ParserError::StructNameAlreadyUsed(name));
        }

        self.structures.insert(name, builder);
        Ok(())
    }

    pub fn has(&self, name: &String) -> bool {
        self.structures.contains_key(name)
    }

    pub fn get(&self, name: &String) -> Result<&StructBuilder, ParserError> {
        self.structures.get(name).ok_or_else(|| ParserError::StructNotFound(name.clone()))
    }
}