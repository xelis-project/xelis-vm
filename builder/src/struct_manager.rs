use std::borrow::Cow;
use xelis_types::{
    IdentifierType,
    StructType, Type,
};
use crate::{
    BuilderError,
    IdMapper
};

#[derive(Debug, Clone)]
pub struct StructBuilder<'a> {
    inner: StructType,
    fields_names: Vec<&'a str>
}

impl<'a> StructBuilder<'a> {
    // Get the index of a field by name
    pub fn get_id_for_field(&self, name: &str) -> Option<IdentifierType> {
        self.fields_names.iter().position(|k| *k == name).map(|v| v as IdentifierType)
    }

    pub fn inner(&self) -> &StructType {
        &self.inner
    }
}

#[derive(Debug)]
pub struct StructManager<'a> {
    parent: Option<&'a StructManager<'a>>,
    // All structs registered in the manager
    structures: Vec<StructBuilder<'a>>,
    // mapper to map each string name into a unique identifier
    mapper: IdMapper<'a>
}

impl<'a> StructManager<'a> {
    // Create a new struct manager
    pub fn new() -> Self {
        StructManager {
            parent: None,
            structures: Vec::new(),
            mapper: IdMapper::new()
        }
    }

    pub fn with_parent(parent: &'a StructManager<'a>) -> Self {
        StructManager {
            parent: Some(parent),
            structures: Vec::new(),
            mapper: IdMapper::with_parent(&parent.mapper)
        }
    }

    // register a new struct in the manager
    pub fn add(&mut self, name: Cow<'a, str>, fields: Vec<(&'a str, Type)>) -> Result<(), BuilderError> {
        if self.mapper.has_variable(&name) {
            return Err(BuilderError::StructNameAlreadyUsed);
        }

        let id = self.mapper.register(name)?;
        let inner = StructType::new(id, fields.iter().map(|(_, t)| t.clone()).collect());
        self.structures.push(StructBuilder {
            inner,
            fields_names: fields.iter().map(|(n, _)| *n).collect()
        });

        Ok(())
    }

    // Same as `add` but returns its identifier and the final struct
    pub fn build_struct(&mut self, name: Cow<'a, str>, fields: Vec<(&'a str, Type)>) -> Result<StructType, BuilderError> {
        if self.mapper.has_variable(&name) {
            return Err(BuilderError::StructNameAlreadyUsed);
        }

        let id = self.mapper.register(name)?;
        let inner = StructType::new(id, fields.iter().map(|(_, t)| t.clone()).collect());
        self.structures.push(StructBuilder {
            inner: inner.clone(),
            fields_names: fields.iter().map(|(n, _)| *n).collect()
        });

        Ok(inner)
    }

    pub fn get_by_id(&self, id: &IdentifierType) -> Result<&StructBuilder<'a>, BuilderError> {
        if let Some(parent) = self.parent {
            if let Ok(s) = parent.get_by_id(id) {
                return Ok(s);
            }
        }

        self.structures.iter().find(|b| b.inner.id() == *id).ok_or(BuilderError::StructNotFound)
    }

    // Get a struct by name
    pub fn get_by_name(&self, name: &str) -> Result<&StructBuilder<'a>, BuilderError> {
        let id = self.mapper.get(name)?;
        self.get_by_id(&id)
    }

    pub fn get_by_ref(&self, _type: &StructType) -> Result<&StructBuilder<'a>, BuilderError> {
        if let Some(parent) = self.parent {
            if let Ok(s) = parent.get_by_ref(_type) {
                return Ok(s);
            }
        }

        self.structures.iter().find(|v| &v.inner == _type).ok_or(BuilderError::StructNotFound)   
    }

    // Convert the struct manager into a list of structs
    pub fn finalize(self) -> Vec<StructType> {
        self.structures.into_iter().map(|builder| builder.inner).collect()
    }
}