use crate::{
    values::Value,
    IdentifierType,
};
use std::{
    collections::{HashMap, HashSet},
    hash::{BuildHasher, Hash}
};

// Represents a struct in the language
#[derive(Clone, PartialEq, Debug)]
pub struct Struct {
    pub fields: Vec<Type>
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Type {
    Any,
    T,

    U8,
    U16,
    U32,
    U64,
    U128,

    String,
    Bool,
    Struct(IdentifierType),
    Array(Box<Type>),
    Optional(Box<Type>)
}

impl Type {
    // transform a byte into a primitive type
    pub fn primitive_type_from_byte(byte: u8) -> Option<Self> {
        match byte {
            0 => Some(Type::U8),
            1 => Some(Type::U16),
            2 => Some(Type::U32),
            3 => Some(Type::U64),
            4 => Some(Type::U128),
            5 => Some(Type::Bool),
            6 => Some(Type::String),
            _ => None
        }
    }

    // get the byte representation of the primitive type
    pub fn primitive_byte(&self) -> Option<u8> {
        match self {
            Type::U8 => Some(0),
            Type::U16 => Some(1),
            Type::U32 => Some(2),
            Type::U64 => Some(3),
            Type::U128 => Some(4),
            Type::Bool => Some(5),
            Type::String => Some(6),
            _ => None
        }
    }

    // check if the type is a primitive type
    pub fn is_primitive(&self) -> bool {
        self.primitive_byte().is_some()
    }

    pub fn from_value<H: HasKey<IdentifierType>>(value: &Value, structures: &H) -> Option<Self> {
        let _type = match value {
            Value::Null => return None,
            Value::U8(_) => Type::U8,
            Value::U16(_) => Type::U16,
            Value::U32(_) => Type::U32,
            Value::U64(_) => Type::U64,
            Value::U128(_) => Type::U128,
            Value::String(_) => Type::String,
            Value::Boolean(_) => Type::Bool,
            Value::Optional(value) => Type::Optional(Box::new(Type::from_value(value.as_ref()?, structures)?)),
            Value::Array(values) => Type::Array(Box::new(Type::from_value(&values.first()?.borrow(), structures)?)),
            Value::Struct(name, _) => if structures.has(name) {
                Type::Struct(name.clone())
            } else {
                return None
            }
        };

        Some(_type)
    }

    pub fn get_inner_type(&self) -> &Type {
        match &self {
            Type::Array(ref _type) => _type,
            Type::Optional(ref _type) => _type,
            _ => &self
        }
    }

    pub fn allow_null(&self) -> bool {
        match self {
            Type::Optional(_) => true,
            _ => false
        }
    }

    pub fn is_compatible_with(&self, other: &Type) -> bool {
        match other {
            Type::Any | Type::T => true,
            Type::Array(sub_type) => match self {
                Type::Array(sub) => sub.is_compatible_with(sub_type.as_ref()),
                _ => *self == *other || self.is_compatible_with(sub_type.as_ref()),
            },
            Type::Optional(sub_type) => match self {
                Type::Optional(sub) => sub.is_compatible_with(sub_type.as_ref()),
                _ => *self == *other || self.is_compatible_with(sub_type.as_ref()),
            },
            o => *o == *self || *self == Type::T || *self == Type::Any
        }
    }

    // check if the type can be casted to another type
    pub fn is_castable_to(&self, other: &Type) -> bool {
        match self {
            Type::U8 => match other {
                Type::U16 | Type::U32 | Type::U64 | Type::U128 | Type::String => true,
                _ => false
            },
            Type::U16 => match other {
                Type::U8 | Type::U32 | Type::U64 | Type::U128 | Type::String => true,
                _ => false
            },
            Type::U32 => match other {
                Type::U8 | Type::U16 | Type::U64 | Type::U128 | Type::String => true,
                _ => false
            },
            Type::U64 => match other {
                Type::U8 | Type::U16 | Type::U32 | Type::U128 | Type::String => true,
                _ => false
            },
            Type::U128 => match other {
                Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::String => true,
                _ => false
            },
            Type::Bool => match other {
                Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U128 | Type::String => true,
                _ => false
            },
            _ => false
        }
    }

    // Check if current type can be casted to another type without loss of data
    pub fn is_castable_to_no_loss(&self, other: &Type) -> bool {
        match self {
            Type::U8 => match other {
                Type::U16 | Type::U32 | Type::U64 | Type::U128 => true,
                _ => false
            },
            Type::U16 => match other {
                Type::U32 | Type::U64 | Type::U128 => true,
                _ => false
            },
            Type::U32 => match other {
                Type::U64 | Type::U128 => true,
                _ => false
            },
            Type::U64 => match other {
                Type::U128 => true,
                _ => false
            },
            _ => false
        }
    }

    pub fn is_array(&self) -> bool {
        match &self {
            Type::Array(_) => true,
            _ => false
        }
    }

    pub fn is_struct(&self) -> bool {
        match &self {
            Type::Struct(_) => true,
            _ => false
        }
    }

    pub fn is_number(&self) -> bool {
        match &self {
            Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U128 => true,
            _ => false
        }
    }

    pub fn is_optional(&self) -> bool {
        match &self {
            Type::Optional(_) => true,
            _ => false
        }
    }
}

pub trait HasKey<K> {
    fn has(&self, key: &K) -> bool;
}

impl<K: Hash + Eq> HasKey<K> for HashSet<K> {
    fn has(&self, key: &K) -> bool {
        self.contains(key)
    }
}

impl<K: Hash + Eq, V, S: BuildHasher> HasKey<K> for HashMap<K, V, S> {
    fn has(&self, key: &K) -> bool {
        self.contains_key(key)
    }
}