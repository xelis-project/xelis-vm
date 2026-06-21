use std::{borrow::Cow, fmt, hash::{Hash, Hasher}, sync::Arc};

use serde::{Deserialize, Serialize};

use crate::IdentifierType;
use super::{fmt_generics, fmt_type_with_generics, Type};

// Represents a variant of an enum
// Supports both tuple-style (unnamed) and struct-style (named) fields
#[derive(Clone, Hash, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct EnumVariant {
    // Fields of the variant (name is empty for tuple-style)
    fields: Vec<(Cow<'static, str>, Type)>,
    // Whether this is a tuple-style variant (e.g., Some(T)) vs struct-style (e.g., Some { value: T })
    is_tuple: bool,
}

impl EnumVariant {
    // Create a new struct-style variant with named fields
    #[inline(always)]
    pub fn new(fields: Vec<(Cow<'static, str>, Type)>) -> Self {
        Self { fields, is_tuple: false }
    }

    // Create a new tuple-style variant with unnamed fields
    #[inline(always)]
    pub fn new_tuple(types: Vec<Type>) -> Self {
        // For tuple variants, we generate field names like "0", "1", "2", etc.
        let fields = types.into_iter()
            .enumerate()
            .map(|(i, ty)| (Cow::Owned(i.to_string()), ty))
            .collect();
        Self { fields, is_tuple: true }
    }

    #[inline(always)]
    pub fn fields(&self) -> &[(Cow<'static, str>, Type)] {
        &self.fields
    }

    // Check if this is a tuple-style variant
    #[inline(always)]
    pub fn is_tuple(&self) -> bool {
        self.is_tuple
    }

    // Get the types only (useful for tuple-style variants)
    #[inline(always)]
    pub fn types(&self) -> impl Iterator<Item = &Type> {
        self.fields.iter().map(|(_, ty)| ty)
    }
}

impl From<Vec<(Cow<'static, str>, Type)>> for EnumVariant {
    fn from(value: Vec<(Cow<'static, str>, Type)>) -> Self {
        Self::new(value)
    }
}

impl From<Vec<(&'static str, Type)>> for EnumVariant {
    fn from(value: Vec<(&'static str, Type)>) -> Self {
        Self::new(value.into_iter().map(|(k, v)| (Cow::Borrowed(k), v)).collect())
    }
}

// Allow creating tuple variants from a Vec of types
impl From<Vec<Type>> for EnumVariant {
    fn from(value: Vec<Type>) -> Self {
        Self::new_tuple(value)
    }
}

// Represents an enum like in Rust with variants
// Support up to 255 variants
#[derive(Clone, Eq, Debug, Serialize, Deserialize)]
pub struct Enum {
    id: IdentifierType,
    name: Cow<'static, str>,
    // Generic parameter names (e.g., ["T", "E"] for Result<T, E>)
    generics: Vec<Cow<'static, str>>,
    variants: Vec<(Cow<'static, str>, EnumVariant)>,
}

impl Hash for Enum {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}

impl PartialEq for Enum {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

// Selected enum variant with associated type
#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct EnumType(Arc<Enum>);

// Represents the type of an enum variant
// This is embed in the value to determine easily which variant it is
#[derive(Clone, Hash, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct EnumValueType {
    enum_type: EnumType,
    variant_id: u8
}

impl EnumType {
    // Create a new enum type
    pub fn new(id: IdentifierType, name: impl Into<Cow<'static, str>>, variants: Vec<(Cow<'static, str>, EnumVariant)>) -> Self {
        Self(Arc::new(Enum { id, name: name.into(), generics: Vec::new(), variants }))
    }

    // Create a new enum type with generic parameters
    pub fn new_with_generics(
        id: IdentifierType,
        name: impl Into<Cow<'static, str>>,
        generics: Vec<Cow<'static, str>>,
        variants: Vec<(Cow<'static, str>, EnumVariant)>
    ) -> Self {
        Self(Arc::new(Enum { id, name: name.into(), generics, variants }))
    }

    // Get the unique identifier of the enum
    #[inline(always)]
    pub fn id(&self) -> IdentifierType {
        self.0.id
    }

    // Get the name of the enum
    #[inline(always)]
    pub fn name(&self) -> &str {
        &self.0.name
    }

    // Get the generic parameter names
    #[inline(always)]
    pub fn generics(&self) -> &Vec<Cow<'static, str>> {
        &self.0.generics
    }

    // Get the variants of the enum
    #[inline(always)]
    pub fn variants(&self) -> &Vec<(Cow<'static, str>, EnumVariant)> {
        &self.0.variants
    }

    // Get a variant by its id
    #[inline(always)]
    pub fn get_variant(&self, id: u8) -> Option<&(Cow<'static, str>, EnumVariant)> {
        self.0.variants.get(id as usize)
    }
}

impl fmt::Display for EnumType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "enum {}", self.name())?;
        fmt_generics(f, self.generics())?;
        write!(f, " {{ ")?;

        for (index, (name, variant)) in self.variants().iter().enumerate() {
            if index > 0 {
                write!(f, ", ")?;
            }

            write!(f, "{}", name)?;
            if !variant.fields().is_empty() {
                if variant.is_tuple() {
                    write!(f, "(")?;
                    for (field_index, ty) in variant.types().enumerate() {
                        if field_index > 0 {
                            write!(f, ", ")?;
                        }
                        fmt_type_with_generics(f, ty, self.generics())?;
                    }
                    write!(f, ")")?;
                } else {
                    write!(f, " {{ ")?;
                    for (field_index, (field_name, ty)) in variant.fields().iter().enumerate() {
                        if field_index > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}: ", field_name)?;
                        fmt_type_with_generics(f, ty, self.generics())?;
                    }
                    write!(f, " }}")?;
                }
            }
        }

        write!(f, " }}")
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

impl Serialize for EnumType {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}

impl<'a> Deserialize<'a> for EnumType {
    fn deserialize<D: serde::Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(Self(Arc::new(Enum::deserialize(deserializer)?)))
    }
}
