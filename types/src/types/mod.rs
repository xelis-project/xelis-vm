mod r#struct;
mod r#enum;
mod func;

pub mod opaque;

use std::{fmt, hash::{Hash, Hasher}};
use indexmap::Equivalent;
use serde::{Deserialize, Serialize};

use crate::{values::Primitive, Constant};
use opaque::OpaqueType;

pub use r#struct::*;
pub use r#enum::*;
pub use func::*;

#[derive(Clone, Hash, PartialEq, Eq, Debug, Serialize, Deserialize)]
#[serde(tag = "type", content = "value")]
#[serde(rename_all = "snake_case")]
pub enum Type {
    // Any Type is accepted
    Any,
    // T is a generic type, inner byte is for its position
    // if byte is None, it means the global type itself
    T(Option<u8>),

    U8,
    U16,
    U32,
    U64,
    U128,
    U256,

    String,
    Bool,

    Bytes,

    Tuples(Vec<Type>),
    Array(Box<Type>),
    Optional(Box<Type>),
    Range(Box<Type>),
    Map(Box<Type>, Box<Type>),

    Struct(StructType),
    Enum(EnumType),
    Opaque(OpaqueType),
    Function(FnType)
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum DefinedType {
    Struct(StructType),
    Enum(EnumValueType),
    Tuples(Vec<Type>),
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
            5 => Some(Type::U256),
            6 => Some(Type::Bool),
            7 => Some(Type::String),
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
            Type::U256 => Some(5),
            Type::Bool => Some(6),
            Type::String => Some(7),
            _ => None
        }
    }

    // check if the type has an inner type
    pub fn has_inner_type(&self) -> bool {
        match self {
            Type::Array(_) | Type::Optional(_) | Type::Range(_) | Type::Map(_, _) => true,
            _ => false
        }
    }

    // check if the type is a primitive type
    pub fn is_primitive(&self) -> bool {
        self.primitive_byte().is_some()
    }

    // Get a type from a value
    pub fn from_value(value: &Primitive) -> Option<Self> {
        Some(match value {
            Primitive::Null => return None,
            Primitive::U8(_) => Type::U8,
            Primitive::U16(_) => Type::U16,
            Primitive::U32(_) => Type::U32,
            Primitive::U64(_) => Type::U64,
            Primitive::U128(_) => Type::U128,
            Primitive::U256(_) => Type::U256,
            Primitive::String(_) => Type::String,
            Primitive::Boolean(_) => Type::Bool,
            Primitive::Range(range) => Type::Range(Box::new(Self::from_value(&range.0)?)),
            Primitive::Opaque(_) => return None,
        })
    }

    // Get a type from a value type
    pub fn from_value_type(value_type: &Constant) -> Option<Self> {
        Some(match value_type {
            Constant::Default(v) => Self::from_value(v)?,
            Constant::Array(values) => Type::Array(Box::new(Type::from_value_type(values.first()?)?)),
            Constant::Map(map) => {
                let (key, value) = map.iter().next()?;
                let key = Type::from_value_type(&key)?;
                let value = Type::from_value_type(&value)?;
                Type::Map(Box::new(key), Box::new(value))
            },
            Constant::Bytes(_) => Type::Bytes,
            Constant::Typed(_, ty) => match ty {
                DefinedType::Enum(ty) => Type::Enum(ty.enum_type().clone()),
                DefinedType::Struct(ty) => Type::Struct(ty.clone()),
                DefinedType::Tuples(ty) => Type::Tuples(ty.clone()),
            }
        })
    }

    // get the inner type of the type or fallback to self
    pub fn get_inner_type(&self) -> &Type {
        match &self {
            Type::Array(ref _type) => _type,
            Type::Optional(ref _type) => _type,
            Type::Range(ref _type) => _type,
            _ => &self
        }
    }

    // get the generic type with the given id
    pub fn get_generic_type(&self, id: u8) -> Option<&Type> {
        match id {
            0 => match &self {
                Type::Map(key, _) => Some(key.as_ref()),
                Type::Array(inner) => Some(inner.as_ref()),
                Type::Optional(inner) => Some(inner.as_ref()),
                Type::Range(inner) => Some(inner.as_ref()),
                _ => None
            },
            1 => match &self {
                Type::Map(_, value) => Some(value.as_ref()),
                _ => None
            }
            _ => None
        }
    }

    // check if the type allow to have a null value
    pub fn allow_null(&self) -> bool {
        match self {
            Type::Optional(_) => true,
            _ => false
        }
    }

    // check if the type is a generic type
    pub fn is_generic(&self) -> bool {
        match self {
            Type::T(_) | Type::Any => true,
            _ => false
        }
    }

    // Is generic type "any"
    pub fn is_any(&self) -> bool {
        matches!(self, Type::Any)
    }

    // Our current self type may be a generic type
    // We have an instance of this type to know the real types instead of the "generic" types
    // We have to verify that they are exactly the same as other
    pub fn is_generic_compatible_with(&self, instance: &Type, other: &Type) -> bool {
        match self {
            Type::T(Some(id)) => instance.get_generic_type(*id).map_or(false, |t| t == other),
            Type::T(None) => instance == self,
            Type::Any => true,
            // No need to use instance because we don't have generic here
            _ => self.is_compatible_with(other)
        }
    }

    pub fn map_generic_type(&self, replacement: Option<&Type>) -> Self {
        match self {
            Type::T(Some(id)) => replacement.and_then(|ty| ty.get_generic_type(*id).cloned()).unwrap_or_else(|| self.clone()),
            Type::Optional(inner) => Type::Optional(Box::new(inner.map_generic_type(replacement))),
            Type::Array(inner) => Type::Array(Box::new(inner.map_generic_type(replacement))),
            Type::Range(inner) => Type::Range(Box::new(inner.map_generic_type(replacement))),
            Type::Map(k, v) => Type::Map(
                Box::new(k.map_generic_type(replacement)),
                Box::new(v.map_generic_type(replacement)),
            ),
            Type::Tuples(types) => Type::Tuples(types.iter().map(|t| t.map_generic_type(replacement)).collect()),
            Type::Function(f) => Type::Function(f.map_generic_type(replacement)),
            _ => self.clone(),
        }
    }

    // check if the type contains a sub type
    pub fn contains_sub_type(&self) -> bool {
        match self {
            Type::Array(_) | Type::Optional(_) | Type::Range(_) | Type::Map(_, _) => true,
            _ => false
        }
    }

    // check if the type is an enum
    pub fn is_enum(&self) -> bool {
        match self {
            Type::Enum(_) => true,
            _ => false
        }
    }

    // check if the type is a map
    pub fn is_map(&self) -> bool {
        match self {
            Type::Map(_, _) => true,
            _ => false
        }
    }

    // check if the type is a tuples
    pub fn is_tuples(&self) -> bool {
        match self {
            Type::Tuples(_) => true,
            _ => false
        }
    }

    // Same as is_compatible_with, but allow for null/value assignation with Optional types
    pub fn is_assign_compatible_with(&self, other: &Type) -> bool {
        if other.is_any() || self.is_any() {
            return true
        }

        match self {
            Self::Optional(v) => match other {
                Self::Optional(b) => v.is_assign_compatible_with(b),
                _ => **v == *other || v.is_assign_compatible_with(other)
            },
            Self::Array(v) => match other {
                Self::Array(v2) => v.is_assign_compatible_with(v2),
                _ => self.is_compatible_with(other)
            },
            Self::Range(v) => match other {
                Self::Range(v2) => v.is_assign_compatible_with(v2),
                _ => self.is_compatible_with(other)
            },
            Self::Map(k, v) => match other {
                Self::Map(k2, v2) => k.is_assign_compatible_with(k2) && v.is_assign_compatible_with(v2),
                _ => self.is_compatible_with(other)
            }
            Self::Function(f) => match other {
                Self::Function(f2) if f2.return_type().is_some() && f.return_type().is_some() => f.return_type()
                    .expect("f type")
                    .is_assign_compatible_with(
                        f2.return_type().expect("f2 type")
                    ),
                _ => self.is_compatible_with(other)
            },
            Self::Tuples(tuples) => match other {
                Self::Tuples(tuples2) => tuples.iter().zip(tuples2.iter()).all(|(a, b)| a.is_assign_compatible_with(b)),
                _ => self.is_compatible_with(other)
            },
            _ => other.is_compatible_with(self)
        }
    }

    // check if the type is compatible with another type
    pub fn is_compatible_with(&self, other: &Type) -> bool {
        if other.is_any() || self.is_any() {
            return true
        }

        match other {
            Type::Range(inner) => match self {
                Type::Range(inner2) => inner2.is_generic_compatible_with(other, inner),
                Type::Any | Type::T(None) => true,
                Type::T(Some(_)) => self.is_generic_compatible_with(other, inner),
                _ => false
            },
            Type::Enum(e) => match self {
                Type::Enum(e2) => e == e2,
                _ => self.is_generic() || other.is_compatible_with(self)
            },
            Type::Struct(a) => match self {
                Type::Struct(b) => a == b,
                _ => self.is_generic() || other.is_compatible_with(self)
            },
            Type::Opaque(a) => match self {
                Type::Opaque(b) => a == b,
                _ => self.is_generic() || other.is_compatible_with(self)
            },
            Type::Any | Type::T(None) => true,
            Type::Array(sub_type) => match self {
                Type::Array(sub) => sub.is_generic_compatible_with(other, sub_type.as_ref()),
                Type::Any | Type::T(None) => true,
                Type::T(Some(_)) => self.is_generic_compatible_with(other, sub_type.as_ref()),
                _ => *self == *other
            },
            Type::Optional(sub_type) => match self {
                Type::Optional(sub) => sub.is_generic_compatible_with(other, sub_type.as_ref()),
                Type::Any | Type::T(None) => true,
                Type::T(Some(_)) => self.is_generic_compatible_with(other, sub_type.as_ref()),
                _ => *self == *other
            },
            Type::Map(k, v) => match self {
                Type::Map(k2, v2) => k2.is_generic_compatible_with(other, k) && v2.is_generic_compatible_with(other, v),
                Type::Any | Type::T(None) => true,
                _ => *self == *other
            },
            Type::Tuples(types) => match self {
                Type::Tuples(types2) => types.iter().zip(types2.iter()).all(|(a, b)| a.is_compatible_with(b)),
                Type::Any | Type::T(None) => true,
                _ => *self == *other
            }
            _ => *self == *other || self.is_generic(),
        }
    }

    // check if the type can be casted to another type
    pub fn is_castable_to(&self, other: &Type) -> bool {
        match self {
            Type::U8 => match other {
                Type::U16 | Type::U32 | Type::U64 | Type::U128 | Type::U256 | Type::String => true,
                _ => false
            },
            Type::U16 => match other {
                Type::U8 | Type::U32 | Type::U64 | Type::U128 | Type::U256 | Type::String => true,
                _ => false
            },
            Type::U32 => match other {
                Type::U8 | Type::U16 | Type::U64 | Type::U128 | Type::U256 | Type::String => true,
                _ => false
            },
            Type::U64 => match other {
                Type::U8 | Type::U16 | Type::U32 | Type::U128 | Type::U256 | Type::String => true,
                _ => false
            },
            Type::U128 => match other {
                Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U256 | Type::String => true,
                _ => false
            },
            Type::U256 => match other {
                Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U128 | Type::String => true,
                _ => false
            },
            Type::Bool => match other {
                Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U128 | Type::U256 | Type::String => true,
                _ => false
            },
            Type::Range(inner) => match other {
                Type::Range(inner2) => inner.is_castable_to(inner2),
                _ => false
            },
            Type::Optional(inner) => match other {
                Type::Optional(inner2) => inner.is_castable_to(inner2),
                _ => inner.is_compatible_with(other)
            },
            _ => false
        }
    }

    // Check if current type can be casted to another type without loss of data
    pub fn is_castable_to_no_loss(&self, other: &Type) -> bool {
        match self {
            Type::U8 => match other {
                Type::U16 | Type::U32 | Type::U64 | Type::U128 | Type::U256 => true,
                _ => false
            },
            Type::U16 => match other {
                Type::U32 | Type::U64 | Type::U128 | Type::U256 => true,
                _ => false
            },
            Type::U32 => match other {
                Type::U64 | Type::U128 | Type::U256 => true,
                _ => false
            },
            Type::U64 => match other {
                Type::U128 | Type::U256 => true,
                _ => false
            },
            Type::U128 => match other {
                Type::U256 => true,
                _ => false
            }
            _ => false
        }
    }

    pub fn is_iterable(&self) -> bool {
        match self {
            Type::Array(_) => true,
            Type::Range(_) => true,
            Type::Bytes => true,
            _ => false
        }
    }

    pub fn support_array_call(&self) -> bool {
        match &self {
            Type::Array(_) => true,
            Type::Bytes => true,
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
            Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::U128 | Type::U256 => true,
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

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Any => write!(f, "any"),
            Type::T(None) => write!(f, "T"),
            Type::T(Some(id)) => write!(f, "T{}", id),
            Type::U8 => write!(f, "u8"),
            Type::U16 => write!(f, "u16"),
            Type::U32 => write!(f, "u32"),
            Type::U64 => write!(f, "u64"),
            Type::U128 => write!(f, "u128"),
            Type::U256 => write!(f, "u256"),
            Type::String => write!(f, "string"),
            Type::Bool => write!(f, "bool"),
            Type::Bytes => write!(f, "bytes"),
            Type::Tuples(types) => {
                write!(f, "(")?;
                for (i, t) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", t)?;
                }
                write!(f, ")")
            }
            Type::Struct(ty) => write!(f, "{}", ty.name()),
            Type::Array(_type) => write!(f, "{}[]", _type),
            Type::Optional(_type) => write!(f, "optional<{}>", _type),
            Type::Range(_type) => write!(f, "range<{}>", _type),
            Type::Map(key, value) => write!(f, "map<{}, {}>", key, value),
            Type::Enum(ty) => write!(f, "{}", ty.name()),
            Type::Opaque(ty) => write!(f, "{}", ty.name()),
            Type::Function(ty) => write!(f, "{}", ty)
        }
    }
}

// TypeId is used to identify a type
// We can retrieve a type from a TypeId
// if its stored in an IndexMap
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct TypeId(pub u16);

impl Hash for TypeId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Equivalent<EnumType> for TypeId {
    fn equivalent(&self, key: &EnumType) -> bool {
        key.id() == self.0
    }
}

impl Equivalent<StructType> for TypeId {
    fn equivalent(&self, key: &StructType) -> bool {
        key.id() == self.0
    }
}

#[cfg(test)]
mod tests {
    use std::hash::{DefaultHasher, Hash, Hasher};
    use super::*;

    #[test]
    fn test_compatibility_between_types() {
        assert!(Type::Optional(Box::new(Type::Bool)).is_assign_compatible_with(&Type::Bool));
        assert!(!Type::Optional(Box::new(Type::Bool)).is_compatible_with(&Type::Bool));
        assert!(!Type::Bool.is_compatible_with(&Type::Optional(Box::new(Type::Bool))));

        let struct_type = StructType::new(0, "Foo", Vec::new());
        assert!(Type::Optional(Box::new(Type::Struct(struct_type.clone()))).is_assign_compatible_with(&Type::Struct(struct_type.clone())));
    }

    #[test]
    fn test_compatibility_deep_types() {
        let expected = Type::Array(Box::new(Type::Tuples(vec![Type::String, Type::U64])));
        let ty = Type::Array(Box::new(Type::Tuples(vec![Type::T(Some(0)), Type::T(Some(1))])));

        assert!(expected.is_compatible_with(&ty));
        assert!(expected.is_assign_compatible_with(&ty));
    }

    #[test]
    fn test_mapping_generic_map() {
        let expected = Type::Map(Box::new(Type::U64), Box::new(Type::String));
        let got = Type::Map(Box::new(Type::T(Some(0))), Box::new(Type::T(Some(1))))
            .map_generic_type(Some(&expected));

        assert!(expected == got);
    }

    #[test]
    fn test_mapping_generic_range() {
        let expected = Type::Range(Box::new(Type::U32));
        let got = Type::Range(Box::new(Type::U32))
            .map_generic_type(Some(&Type::Range(Box::new(Type::U64))));

        assert!(expected == got);

        let expected = Type::Range(Box::new(Type::U32));
        let got = Type::Range(Box::new(Type::U32))
            .map_generic_type(Some(&Type::Array(Box::new(Type::U64))));

        assert!(expected == got);
    }

    #[test]
    fn test_type_id_equivalent() {
        let id = TypeId(1);
        let struct_type = StructType::new(1, "Foo", vec![]);
        assert!(id.equivalent(&struct_type));

        // Also test hash
        let mut left_hasher = DefaultHasher::new();
        id.hash(&mut left_hasher);
        let left_hash = left_hasher.finish();

        let mut right_hasher = DefaultHasher::new();
        struct_type.id().hash(&mut right_hasher);
        let right_hash = right_hasher.finish();
        assert_eq!(left_hash, right_hash);
    }
}