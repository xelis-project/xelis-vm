mod r#struct;
mod r#enum;

pub use r#struct::*;
pub use r#enum::*;

use std::borrow::Cow;
use xelis_types::IdentifierType;
use crate::{
    BuilderError,
    IdMapper
};

pub trait BuilderType<D> {
    fn with(id: IdentifierType, data: Vec<D>) -> Self;

    fn type_id(&self) -> IdentifierType;
}

pub trait Builder<'a> {
    type Data;
    type BuilderType: BuilderType<Self::Data>;
    type Type: Clone + Eq;

    fn new(inner: Self::BuilderType, names: Vec<&'a str>) -> Self;

    fn get_type(&self) -> &Self::Type;

    fn names(&self) -> &Vec<&'a str>;

    fn builder_type(&self) -> &Self::BuilderType;

    fn get_id_for_field(&self, name: &str) -> Option<IdentifierType> {
        self.names().iter().position(|k| *k == name).map(|v| v as IdentifierType)
    }

    fn to_type(&self) -> Self::Type;

    fn type_id(&self) -> IdentifierType;
}

#[derive(Debug)]
pub struct TypeManager<'a, T: Builder<'a>> {
    parent: Option<&'a Self>,
    // All structs registered in the manager
    types: Vec<T>,
    // mapper to map each string name into a unique identifier
    mapper: IdMapper<'a>,
}

impl<'a, T: Builder<'a>> TypeManager<'a, T> {
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

    fn build_internal(&mut self, name: Cow<'a, str>, fields: Vec<(&'a str, T::Data)>) -> Result<T, BuilderError> {
        if self.mapper.has_variable(&name) {
            return Err(BuilderError::StructNameAlreadyUsed);
        }

        let (fields_names, fields_types) = split_vec(fields);
        let id = self.mapper.register(name)?;
        let inner = T::BuilderType::with(id, fields_types);

        Ok(T::new(
            inner,
            fields_names
        ))
    }
    // register a new struct in the manager
    pub fn add(&mut self, name: Cow<'a, str>, fields: Vec<(&'a str, T::Data)>) -> Result<(), BuilderError> {
        let builder = self.build_internal(name, fields)?;
        self.types.push(builder);

        Ok(())
    }

    // Same as `add` but returns its identifier and the final struct
    pub fn build(&mut self, name: Cow<'a, str>, fields: Vec<(&'a str, T::Data)>) -> Result<T::Type, BuilderError> {
        let builder = self.build_internal(name, fields)?;
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

        self.types.iter().find(|b| b.type_id() == *id).ok_or(BuilderError::StructNotFound)
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

        self.types.iter().find(|v| v.get_type() == _type).ok_or(BuilderError::StructNotFound)   
    }

    // Convert the struct manager into a list of structs
    pub fn finalize<B: FromIterator<T::Type>>(&self) -> B {
        self.types.iter().map(T::to_type).collect()
    }
}

fn split_vec<A, B>(input: Vec<(A, B)>) -> (Vec<A>, Vec<B>) {
    let mut vec_a = Vec::with_capacity(input.len());
    let mut vec_b = Vec::with_capacity(input.len());

    for (a, b) in input {
        vec_a.push(a);
        vec_b.push(b);
    }

    (vec_a, vec_b)
}