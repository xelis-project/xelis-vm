use std::{any::TypeId, collections::HashMap};

use xelis_types::{IdentifierType, NoHashMap, Opaque, OpaqueType};
use crate::{BuilderError, Mapper};

pub struct OpaqueManager<'a> {
    mapper: Mapper<'a, &'static str>,
    types: NoHashMap<OpaqueType>,
    lookup: HashMap<TypeId, OpaqueType>,
}

impl<'a> OpaqueManager<'a> {
    pub fn new() -> Self {
        Self {
            mapper: Mapper::new(),
            types: NoHashMap::default(),
            lookup: HashMap::default(),
        }
    }

    // Register a new opaque type
    pub fn build<T: Opaque>(&mut self, name: &'static str, allow_external_input: bool) -> Result<OpaqueType, BuilderError> {
        let id = self.mapper.register(name)?;
        let ty = OpaqueType::with(id, name, allow_external_input);
        self.types.insert(id, ty.clone());
        self.lookup.insert(TypeId::of::<T>(), ty.clone());

        Ok(ty)
    }

    // Get an opaque type by its registered name
    pub fn get_by_name(&self, name: &str) -> Option<&OpaqueType> {
        self.mapper.get(name)
            .ok()
            .and_then(|id| self.types.get(&id))
    }

    // Get an opaque type by its id
    pub fn get_by_id(&self, id: &IdentifierType) -> Option<&OpaqueType> {
        self.types.get(id)
    }

    pub fn get_name_of(&self, ty: &OpaqueType) -> Option<&str> {
        self.mapper.get_by_id(ty.id())
            .map(|v| *v)
    }

    pub fn iter(&self) -> impl Iterator<Item = &OpaqueType> {
        self.types.values()
    }

    pub fn get_by_type_id(&self, ty: &TypeId) -> Option<&OpaqueType> {
        self.lookup.get(&ty)
    }
}