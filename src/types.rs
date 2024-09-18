use crate::{
    parser::StructManager,
    values::Value,
    IdentifierType,
    ast::Token
};
use std::{
    collections::{hash_map::RandomState, HashMap, HashSet},
    hash::{BuildHasher, Hash}
};

// Represents a struct in the language
#[derive(Clone, PartialEq, Debug)]
pub struct Struct {
    pub fields: Vec<Type>
}

pub struct RefMap<'a, K, V, S = RandomState> {
    maps: Vec<&'a HashMap<K, V, S>>
}

// link multiple maps in one struct for read-only
impl<'a, K, V, S: BuildHasher> RefMap<'a, K, V, S> {
    pub fn new() -> Self {
        RefMap {
            maps: vec![]
        }
    }

    pub fn get(&self, key: &K) -> Option<&V>
    where K: Hash + Eq {
        for map in &self.maps {
            match map.get(key) {
                Some(v) => return Some(v),
                None => {}
            }
        }
        None
    }

    pub fn link_maps(&mut self, maps: &[&'a HashMap<K, V, S>]) {
        for map in maps {
            self.link_map(map);
        }
    }

    pub fn link_map(&mut self, map: &'a HashMap<K, V, S>) {
        self.maps.push(map);
    }
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
    pub(crate) fn from_token(s: Token, struct_manager: &StructManager) -> Option<Self> {
        let value: Self = match s {
            Token::U8 => Type::U8,
            Token::U16 => Type::U16,
            Token::U32 => Type::U32,
            Token::U64 => Type::U64,
            Token::U128 => Type::U128,
            Token::Bool => Type::Bool,
            Token::String => Type::String,
            Token::Optional(token) => Type::Optional(Box::new(Type::from_token(*token, struct_manager)?)),
            Token::Identifier(s) => {
                if let Ok(id) = struct_manager.get_mapping(&s) {
                    Type::Struct(id)
                } else {
                    return None
                }
            }
            _ => return None
        };

        Some(value)
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

impl<K: Hash + Eq, V> HasKey<K> for RefMap<'_, K, V> {
    fn has(&self, key: &K) -> bool {
        self.get(key).is_some()
    }
}