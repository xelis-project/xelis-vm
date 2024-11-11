use std::{
    borrow::{Borrow, Cow},
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
    mem::take
};
use xelis_ast::{Expression, Signature};
use xelis_types::IdentifierType;

use crate::BuilderError;

pub type IdMapper<'a> = Mapper<'a, Cow<'a, str>>;
pub type FunctionMapper<'a> = Mapper<'a, Signature>;

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

impl<'a> FunctionMapper<'a> {
    pub fn get_compatible(&self, key: Signature, expressions: &mut [Expression]) -> Result<IdentifierType, BuilderError> {
        // First check if we have the exact signature
        if let Ok(id) = self.get(&key) {
            return Ok(id);
        }

        // Lets find a compatible signature
        'main: for (signature, id) in self.mappings.iter().filter(|(s, _)| s.get_name() == key.get_name() && s.get_parameters().len() == key.get_parameters().len()) {
            for (i, (a, b)) in signature.get_parameters().iter().zip(key.get_parameters()).enumerate() {
                if !a.is_compatible_with(b) {
                    // They are not the same numbers, lets see if they are hardcoded values
                    if b.is_castable_to(a) {
                        if let Expression::Value(value) = &mut expressions[i] {
                            let owned = take(value);
                            *value = owned.checked_cast_to_primitive_type(a)?;
                            continue;
                        }
                    }
                    continue 'main;
                }
            }

            let on_type = match (signature.get_on_type(), key.get_on_type()) {
                (Some(s), Some(k)) => s.is_compatible_with(k),
                (None, None) => true,
                _ => false
            };

            if on_type {
                return Ok(id.clone());
            }
        }

        if let Some(parent) = self.parent {
            return parent.get_compatible(key, expressions);
        }

        Err(BuilderError::MappingNotFound)
    }
}