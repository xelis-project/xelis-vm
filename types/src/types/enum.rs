use std::rc::Rc;

use crate::IdentifierType;
use super::Type;

// Represents a variant of an enum
// This is similar to a struct
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct EnumVariant(pub Vec<Type>);

// Represents an enum like in Rust with variants
// Support up to 255 variants
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct Enum {
    id: IdentifierType,
    variants: Vec<EnumVariant>,
}

// Selected enum variant with associated type
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct EnumType(Rc<Enum>);

// Represents the type of an enum variant
// This is embed in the value to determine easily which variant it is
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct EnumValueType {
    enum_type: EnumType,
    variant_id: u8
}

impl EnumType {
    // Create a new enum type
    pub fn new(id: IdentifierType, variants: Vec<EnumVariant>) -> Self {
        Self(Rc::new(Enum { id, variants }))
    }

    // Get the unique identifier of the enum
    #[inline(always)]
    pub fn id(&self) -> IdentifierType {
        self.0.id
    }

    // Get the variants of the enum
    #[inline(always)]
    pub fn variants(&self) -> &Vec<EnumVariant> {
        &self.0.variants
    }
}

impl EnumValueType {
    // Create a new enum value type
    pub fn new(enum_type: EnumType, variant_id: u8) -> Self {
        Self { enum_type, variant_id }
    }

    // Get the unique identifier of the enum
    #[inline(always)]
    pub fn id(&self) -> IdentifierType {
        self.enum_type.id()
    }

    // Get the enum type
    #[inline(always)]
    pub fn enum_type(&self) -> &EnumType {
        &self.enum_type
    }

    // Get the variant id
    #[inline(always)]
    pub fn variant_id(&self) -> u8 {
        self.variant_id
    }
}