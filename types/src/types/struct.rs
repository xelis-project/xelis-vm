use std::{hash::{Hash, Hasher}, sync::Arc};
use serde::{Deserialize, Serialize};

use crate::IdentifierType;
use super::Type;

// Represents a struct in the language
#[derive(Clone, Eq, Debug, Serialize, Deserialize)]
pub struct Struct {
    // Unique identifier for serialization
    id: IdentifierType,
    // Fields of the struct
    fields: Vec<Type>
}

impl Hash for Struct {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for Struct {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct StructType(Arc<Struct>);

impl StructType {
    /// Create a new struct type
    pub fn new(id: IdentifierType, fields: Vec<Type>) -> Self {
        Self(Arc::new(Struct { id, fields }))
    }

    /// Get the unique identifier of the struct
    #[inline(always)]
    pub fn id(&self) -> IdentifierType {
        self.0.id
    }

    /// Get the fields of the struct
    #[inline(always)]
    pub fn fields(&self) -> &Vec<Type> {
        &self.0.fields
    }
}

impl Serialize for StructType {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}

// Deserialize the struct type by providing only the identifier
impl<'a> Deserialize<'a> for StructType {
    fn deserialize<D: serde::Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(Self(Arc::new(Struct {
            id: IdentifierType::deserialize(deserializer)?,
            fields: Vec::new()
        })))
    }
}