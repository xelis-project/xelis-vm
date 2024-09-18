use std::{borrow::Cow, collections::HashMap};

use crate::{mapper::IdMapper, types::{Struct, Type}, IdentifierType, NoHashMap, ParserError};

#[derive(Debug, Clone)]
pub struct StructBuilder<'a> {
    pub fields: Vec<Type>,
    pub mapper: IdMapper<'a>
}

pub struct StructManager<'a> {
    // All structs registered in the manager
    structures: NoHashMap<StructBuilder<'a>>,
    // mapper to map each string name into a unique identifier
    mapper: IdMapper<'a>
}

impl<'a> StructManager<'a> {
    // Create a new struct manager
    pub fn new() -> Self {
        StructManager {
            structures: HashMap::default(),
            mapper: IdMapper::new()
        }
    }

    // Get all the names of the structures
    pub fn inner(&self) -> &NoHashMap<StructBuilder> {
        &self.structures
    }

    // register a new struct in the manager
    pub fn add(&mut self, name: String, builder: StructBuilder<'a>) -> Result<(), ParserError<'a>> {
        let name = Cow::Owned(name);
        if self.mapper.has_variable(&name) {
            return Err(ParserError::StructNameAlreadyUsed(name.into_owned()));
        }

        let id = self.mapper.register(name)?;
        self.structures.insert(id, builder);

        Ok(())
    }

    // Check if a struct exists
    pub fn has(&self, name: &IdentifierType) -> bool {
        self.structures.contains_key(name)
    }

    // Get the mapping of a struct by name
    pub fn get_mapping(&self, name: &str) -> Result<IdentifierType, ParserError<'a>> {
        self.mapper.get(name)
    }

    // Get a struct by name
    pub fn get(&self, name: &IdentifierType) -> Result<&StructBuilder<'a>, ParserError<'a>> {
        self.structures.get(name).ok_or_else(|| ParserError::StructNotFound(name.clone()))
    }

    // Convert the struct manager into a hashmap of structs
    pub fn finalize(self) -> NoHashMap<Struct> {
        self.structures.into_iter().map(|(k, v)| (k, Struct { fields: v.fields })).collect()
    }
}