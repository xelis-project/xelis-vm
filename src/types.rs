use crate::{
    parser::StructManager,
    values::Value,
    IdentifierType,
    Token
};
use std::{
    collections::{HashMap, HashSet, hash_map::RandomState},
    hash::Hash
};

// Represents a struct in the language
#[derive(Clone, PartialEq, Debug)]
pub struct Struct {
    pub fields: HashMap<IdentifierType, Type>
}

pub struct RefMap<'a, K, V = RandomState> {
    maps: Vec<&'a HashMap<K, V>>
}

// link multiple maps in one struct for read-only
impl<'a, K, V> RefMap<'a, K, V> {
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

    pub fn link_maps(&mut self, maps: Vec<&'a HashMap<K, V>>) {
        for map in maps {
            self.link_map(map);
        }
    }

    pub fn link_map(&mut self, map: &'a HashMap<K, V>) {
        self.maps.push(map);
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub enum Type {
    Any,
    T,

    Byte,
    Short,
    Int,
    Long,

    String,
    Boolean,
    Struct(IdentifierType),
    Array(Box<Type>),
    Optional(Box<Type>)
}

impl Type {
    pub(crate) fn from_token(s: Token, struct_manager: &StructManager) -> Option<Self> {
        let value: Self = match s {
            Token::Byte => Type::Byte,
            Token::Short => Type::Short,
            Token::Int => Type::Int,
            Token::Long => Type::Long,
            Token::Boolean => Type::Boolean,
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
            Value::Byte(_) => Type::Byte,
            Value::Short(_) => Type::Short,
            Value::Int(_) => Type::Int,
            Value::Long(_) => Type::Long,
            Value::String(_) => Type::String,
            Value::Boolean(_) => Type::Boolean,
            Value::Optional(value) => Type::Optional(Box::new(Type::from_value(value.as_ref()?, structures)?)),
            Value::Array(values) => Type::Array(Box::new(Type::from_value(&values.first()?.get_value(), structures)?)),
            Value::Struct(name, _) => if structures.has(name) {
                Type::Struct(name.clone())
            } else {
                return None
            }
        };

        Some(_type)
    }

    pub fn get_array_type(&self) -> &Type {
        match &self {
            Type::Array(ref _type) => _type,
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
            Type::Byte => match other {
                Type::Short | Type::Int | Type::Long | Type::String => true,
                _ => false
            },
            Type::Short => match other {
                Type::Byte | Type::Int | Type::Long | Type::String => true,
                _ => false
            },
            Type::Int => match other {
                Type::Byte | Type::Short | Type::Long | Type::String => true,
                _ => false
            },
            Type::Long => match other {
                Type::Byte | Type::Short | Type::Int | Type::String => true,
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
            Type::Byte | Type::Short | Type::Int | Type::Long => true,
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

impl<K: Hash + Eq, V> HasKey<K> for HashMap<K, V> {
    fn has(&self, key: &K) -> bool {
        self.contains_key(key)
    }
}

impl<K: Hash + Eq, V> HasKey<K> for RefMap<'_, K, V> {
    fn has(&self, key: &K) -> bool {
        self.get(key).is_some()
    }
}