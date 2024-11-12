use std::rc::Rc;
use crate::IdentifierType;
use super::Type;

// Represents a struct in the language
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Struct {
    // Unique identifier for serialization
    id: IdentifierType,
    // Fields of the struct
    fields: Vec<Type>
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct StructType(Rc<Struct>);

impl StructType {
    /// Create a new struct type
    pub fn new(id: IdentifierType, fields: Vec<Type>) -> Self {
        Self(Rc::new(Struct { id, fields }))
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