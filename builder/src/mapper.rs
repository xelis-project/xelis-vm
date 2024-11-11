use std::{
    borrow::{Borrow, Cow},
    collections::HashMap,
    fmt::Debug,
    hash::Hash,
};
use xelis_ast::{Expression, Signature};
use xelis_types::{IdentifierType, Type};

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
            let on_type = match (signature.get_on_type(), key.get_on_type()) {
                (Some(s), Some(k)) => s.is_compatible_with(k),
                (None, None) => true,
                _ => false
            };

            if !on_type {
                continue;
            }

            let mut updated_expressions = Vec::new();
            for (i, (a, b)) in signature.get_parameters().iter().zip(key.get_parameters()).enumerate() {
                let mut cast_to_type = key.get_on_type()
                    .as_ref()
                    .map(Type::get_inner_type)
                    .filter(|t| b.is_castable_to(t));

                if cast_to_type.is_none() && !a.is_compatible_with(b) {
                    // If our parameter is castable to the signature parameter, cast it
                    if b.is_castable_to(a) {
                        cast_to_type = Some(a);
                    } else {
                        continue 'main;
                    }
                }

                // If cast is needed, cast it, if we fail, we continue to the next signature
                if let Some(a) = cast_to_type {
                    // We can only cast hardcoded values
                    if let Expression::Value(value) = &expressions[i] {
                        let cloned = value.clone();
                        let v = cloned.checked_cast_to_primitive_type(a)?;
                        updated_expressions.push(Expression::Value(v));
                        continue;
                    } else {
                        continue 'main;
                    }
                }
            }

            for (i, expr) in updated_expressions.into_iter().enumerate() {
                expressions[i] = expr;
            }

            return Ok(id.clone());
        }

        if let Some(parent) = self.parent {
            return parent.get_compatible(key, expressions);
        }

        Err(BuilderError::MappingNotFound)
    }
}