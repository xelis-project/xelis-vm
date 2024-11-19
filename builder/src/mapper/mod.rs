mod function;

use std::{
    borrow::{Borrow, Cow},
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
};
use xelis_types::IdentifierType;
use crate::BuilderError;

pub use function::*;

pub type IdMapper<'a> = Mapper<'a, Cow<'a, str>>;

// VariableMapper is used to store the mapping between variable names and their identifiers
// So we can reduce the memory footprint of the interpreter by using an incremented id
#[derive(Debug, Clone)]
pub struct Mapper<'a, T: Clone + Eq + Hash> {
    parent: Option<&'a Mapper<'a, T>>,
    next_id: IdentifierType,
    mappings: HashMap<T, IdentifierType>
}

impl<'a, T: Clone + Eq + Hash + Debug> Default for Mapper<'a, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, T: Clone + Eq + Hash + Debug> Mapper<'a, T> {
    // Create a new VariableMapper
    pub fn new() -> Self {
        Self {
            parent: None,
            next_id: 0,
            mappings: HashMap::new()
        }
    }

    // Create a new VariableMapper starting at a specific id
    pub fn with_parent(parent: &'a Self) -> Self {
        Self {
            next_id: parent.get_next_id(),
            parent: Some(parent),
            mappings: HashMap::new()
        }
    }

    pub fn get_next_id(&self) -> IdentifierType {
        self.next_id
    }

    // Get the identifier of a variable name
    pub fn get<K: ?Sized + Debug>(&self, name: &K) -> Result<IdentifierType, BuilderError>
    where
        T: Borrow<K>,
        K: Eq + Hash
    {
        if let Some(parent) = self.parent {
            if let Ok(id) = parent.get(name) {
                return Ok(id);
            }
        }

        self.mappings.get(name).copied().ok_or(BuilderError::MappingNotFound)
    }

    // Get the variable name of an identifier
    pub fn get_by_id(&self, id: IdentifierType) -> Option<&T> {
        if let Some(parent) = self.parent {
            if let Some(name) = parent.get_by_id(id) {
                return Some(name);
            }
        }

        self.mappings.iter().find_map(|(k, v)| if *v == id { Some(k) } else { None })
    }

    // Check if a variable name is already registered
    pub fn has_variable<K: ?Sized>(&self, name: &K) -> bool
    where
        T: Borrow<K>,
        K: Eq + Hash
    {
        if let Some(parent) = self.parent {
            if parent.has_variable(name) {
                return true;
            }
        }
        self.mappings.contains_key(name)
    }

    // Register a new variable name
    pub fn register(&mut self, name: T) -> Result<IdentifierType, BuilderError> {
        if self.get(&name).is_ok() {
            return Err(BuilderError::MappingExists);
        }

        let id = self.next_id;
        self.mappings.insert(name, id);

        self.next_id += 1;
        Ok(id)
    }

    pub fn count(&self) -> usize {
        self.parent.map_or(0, |p| p.count()) + self.mappings.len()
    }
}
