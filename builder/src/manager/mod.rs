mod r#struct;
mod r#enum;
mod opaque;

pub use r#struct::*;
pub use r#enum::*;
pub use opaque::*;

use std::borrow::Cow;
use xelis_types::IdentifierType;
use crate::{
    BuilderError,
    IdMapper
};

pub trait Builder {
    type Type: Clone + Eq;
    type Data;

    fn build_with(id: IdentifierType, name: impl Into<Cow<'static, str>>, data: Self::Data) -> Self;

    fn get_type(&self) -> &Self::Type;

    fn names<'a>(&'a self) -> impl Iterator<Item = &'a str>;

    fn get_id_for_sub(&self, name: &str) -> Option<IdentifierType> {
        self.names().position(|k| k == name).map(|v| v as IdentifierType)
    }

    fn to_type(&self) -> Self::Type;

    fn type_id(&self) -> IdentifierType;
}

#[derive(Debug)]
pub struct TypeManager<'a, T: Builder> {
    parent: Option<&'a Self>,
    // All structs registered in the manager
    types: Vec<T>,
    // mapper to map each string name into a unique identifier
    mapper: IdMapper<'a>,
}

impl<'a, T: Builder> TypeManager<'a, T> {
    // Create a new struct manager
    pub fn new() -> Self {
        Self {
            parent: None,
            types: Vec::new(),
            mapper: IdMapper::new()
        }
    }

    pub fn with_parent(parent: &'a Self) -> Self {
        Self {
            parent: Some(parent),
            types: Vec::new(),
            mapper: IdMapper::with_parent(&parent.mapper),
        }
    }

    pub fn is_defined(&self, ty: &T::Type) -> bool {
        self.types.iter().any(|v| v.get_type() == ty)
    }

    fn build_internal(&mut self, name: impl Into<Cow<'static, str>>, data: T::Data) -> Result<T, BuilderError> {
        let name = name.into();
        if self.mapper.has_variable(&name) {
            return Err(BuilderError::StructNameAlreadyUsed);
        }

        // if its a Cow::Borrowed, only the reference is cloned
        let id = self.mapper.register(name.clone())?;
        let inner = T::build_with(id, name, data);

        Ok(inner)
    }

    // register a new struct in the manager
    pub fn add(&mut self, name: impl Into<Cow<'static, str>>, data: T::Data) -> Result<(), BuilderError> {
        let builder = self.build_internal(name, data)?;
        self.types.push(builder);

        Ok(())
    }

    // Same as `add` but returns its identifier and the final struct
    pub fn build(&mut self, name: impl Into<Cow<'static, str>>, data: T::Data) -> Result<T::Type, BuilderError> {
        let builder = self.build_internal(name, data)?;
        let inner = builder.get_type().clone();
        self.types.push(builder);

        Ok(inner)
    }

    pub fn get_by_id(&self, id: &IdentifierType) -> Result<&T, BuilderError> {
        if let Some(parent) = self.parent {
            if let Ok(s) = parent.get_by_id(id) {
                return Ok(s);
            }
        }

        self.types.iter().find(|b| b.type_id() == *id).ok_or(BuilderError::TypeNotFound)
    }

    // Get a struct by name
    pub fn get_by_name(&self, name: &str) -> Result<&T, BuilderError> {
        let id = self.mapper.get(name)?;
        self.get_by_id(&id)
    }

    pub fn get_by_ref(&self, _type: &T::Type) -> Result<&T, BuilderError> {
        if let Some(parent) = self.parent {
            if let Ok(s) = parent.get_by_ref(_type) {
                return Ok(s);
            }
        }

        self.types.iter().find(|v| v.get_type() == _type).ok_or(BuilderError::TypeNotFound)   
    }

    pub fn get_name_by_ref<'b>(&'b self, _type: &T::Type) -> Result<&'b Cow<'a, str>, BuilderError> {
        if let Some(parent) = self.parent {
            if let Ok(s) = parent.get_name_by_ref(_type) {
                return Ok(s);
            }
        }

        // We need to find its id to get the name from the mapper
        let id = self.get_by_ref(_type)?.type_id();
        self.mapper.get_by_id(id).ok_or(BuilderError::TypeNotFound)
    }

    // Convert the struct manager into a list of structs
    pub fn finalize<B: FromIterator<T::Type>>(&self) -> B {
        self.types.iter().map(T::to_type).collect()
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.types.iter()
    }
}