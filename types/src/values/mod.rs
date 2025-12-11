mod error;
mod cell;
mod constant;

use std::{
    borrow::Cow,
    cmp::Ordering,
    hash::{Hash, Hasher}
};
use crate::opaque::OpaqueWrapper;

use super::{
    Type,
    U256
};

pub use cell::*;
pub use error::*;
pub use constant::*;
use schemars::JsonSchema;
use serde::{Deserialize, Deserializer, Serialize, Serializer};

// Helper functions for U64 string serialization/deserialization
// JSON parsers may have issues with large integers above 2^53
fn serialize_u64_as_string<S>(value: &u64, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str(&value.to_string())
}

fn deserialize_u64_from_string<'de, D>(deserializer: D) -> Result<u64, D::Error>
where
    D: Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    s.parse::<u64>().map_err(serde::de::Error::custom)
}

// Helper functions for U128 string serialization
// See https://github.com/serde-rs/json/issues/740
fn serialize_u128_as_string<S>(value: &u128, serializer: S) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str(&value.to_string())
}

fn deserialize_u128_from_string<'de, D>(deserializer: D) -> Result<u128, D::Error>
where
    D: Deserializer<'de>,
{
    let s = String::deserialize(deserializer)?;
    s.parse::<u128>().map_err(serde::de::Error::custom)
}

macro_rules! checked_cast {
    ($self: expr, $type: expr) => {
        match $self {
            Primitive::U8(n) => n.try_into().map_err(|_| ValueError::CastError),
            Primitive::U16(n) => n.try_into().map_err(|_| ValueError::CastError),
            Primitive::U32(n) => n.try_into().map_err(|_| ValueError::CastError),
            Primitive::U64(n) => n.try_into().map_err(|_| ValueError::CastError),
            Primitive::U128(n) => n.try_into().map_err(|_| ValueError::CastError),
            Primitive::U256(n) => n.try_into().map_err(|_| ValueError::CastError),
            Primitive::Boolean(n) => n.try_into().map_err(|_| ValueError::CastError),
            _ => Err(ValueError::InvalidCastType($type))
        }
    };
}

// This enum is dedicated for constants values / parser
#[derive(Debug, Clone, Eq, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case", tag = "type", content = "value")]
pub enum Primitive {
    Null,
    Boolean(bool),
    // number types
    U8(u8),
    U16(u16),
    U32(u32),
    #[serde(serialize_with = "serialize_u64_as_string", deserialize_with = "deserialize_u64_from_string")]
    U64(u64),
    #[serde(serialize_with = "serialize_u128_as_string", deserialize_with = "deserialize_u128_from_string")]
    U128(u128),
    U256(U256),
    String(String),
    Range(Box<(Primitive, Primitive)>),

    // Opaque Type injected by the environment
    Opaque(OpaqueWrapper)
}

impl PartialEq for Primitive {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Primitive::Null, Primitive::Null) => true,
            (Primitive::U8(a), Primitive::U8(b)) => a == b,
            (Primitive::U16(a), Primitive::U16(b)) => a == b,
            (Primitive::U32(a), Primitive::U32(b)) => a == b,
            (Primitive::U64(a), Primitive::U64(b)) => a == b,
            (Primitive::U128(a), Primitive::U128(b)) => a == b,
            (Primitive::U256(a), Primitive::U256(b)) => a == b,
            (Primitive::String(a), Primitive::String(b)) => a == b,
            (Primitive::Boolean(a), Primitive::Boolean(b)) => a == b,
            (Primitive::Range(a), Primitive::Range(b)) => a == b,
            (Primitive::Opaque(a), Primitive::Opaque(b)) => a == b,
            _ => false
        }
    }
}

impl PartialOrd for Primitive {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Primitive::U8(a), Primitive::U8(b)) => a.partial_cmp(b),
            (Primitive::U16(a), Primitive::U16(b)) => a.partial_cmp(b),
            (Primitive::U32(a), Primitive::U32(b)) => a.partial_cmp(b),
            (Primitive::U64(a), Primitive::U64(b)) => a.partial_cmp(b),
            (Primitive::U128(a), Primitive::U128(b)) => a.partial_cmp(b),
            (Primitive::U256(a), Primitive::U256(b)) => a.partial_cmp(b),
            (Primitive::String(a), Primitive::String(b)) => a.partial_cmp(b),
            (Primitive::Boolean(a), Primitive::Boolean(b)) => a.partial_cmp(b),
            _ => None
        }
    }
}

impl Hash for Primitive {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Primitive::Null => 0.hash(state),
            Primitive::U8(n) => {
                1u8.hash(state);
                n.hash(state);
            },
            Primitive::U16(n) => {
                2u8.hash(state);
                n.hash(state);
            },
            Primitive::U32(n) => {
                3u8.hash(state);
                n.hash(state);
            },
            Primitive::U64(n) => {
                4u8.hash(state);
                n.hash(state);
            },
            Primitive::U128(n) => {
                5u8.hash(state);
                n.hash(state);
            },
            Primitive::U256(n) => {
                6u8.hash(state);
                n.hash(state);
            },
            Primitive::String(n) => {
                7u8.hash(state);
                n.hash(state);
            },
            Primitive::Boolean(n) => {
                8u8.hash(state);
                n.hash(state);
            },
            Primitive::Range(b) => {
                9u8.hash(state);
                b.hash(state);
            },
            Primitive::Opaque(n) => {
                10u8.hash(state);
                n.hash(state);
            }
        }
    }
}

impl Default for Primitive {
    fn default() -> Self {
        Primitive::Null
    }
}

impl Primitive {
    pub fn get_memory_usage(&self) -> usize {
        match self {
            Primitive::Null => 1,
            Primitive::U8(_) => 1,
            Primitive::U16(_) => 2,
            Primitive::U32(_) => 4,
            Primitive::U64(_) => 8,
            Primitive::U128(_) => 16,
            Primitive::U256(_) => 32,
            Primitive::String(s) => 8 + s.len(),
            Primitive::Boolean(_) => 1,
            Primitive::Range(b) => 16 + b.0.get_memory_usage() + b.1.get_memory_usage(),
            Primitive::Opaque(_) => 8
        }
    }

    #[inline]
    pub fn is_null(&self) -> bool {
        match &self {
            Primitive::Null => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        match &self {
            Primitive::String(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn as_u8(&self) -> Result<u8, ValueError> {
        match self {
            Primitive::U8(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U8))
        }
    }

    #[inline]
    pub fn as_u16(&self) -> Result<u16, ValueError> {
        match self {
            Primitive::U16(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U16))
        }
    }

    #[inline]
    pub fn as_u32(&self) -> Result<u32, ValueError> {
        match self {
            Primitive::U32(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U32))
        }
    }

    #[inline]
    pub fn as_u64(&self) -> Result<u64, ValueError> {
        match self {
            Primitive::U64(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U64))
        }
    }

    #[inline]
    pub fn as_u128(&self) -> Result<u128, ValueError> {
        match self {
            Primitive::U128(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U128))
        }
    }

    #[inline]
    pub fn as_u256(&self) -> Result<U256, ValueError> {
        match self {
            Primitive::U256(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U256))
        }
    }

    #[inline]
    pub fn as_string(&self) -> Result<&String, ValueError> {
        match self {
            Primitive::String(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::String))
        }
    }

    #[inline]
    pub fn as_bool(&self) -> Result<bool, ValueError> {
        match self {
            Primitive::Boolean(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Bool))
        }
    }

    #[inline]
    pub fn to_u8(self) -> Result<u8, ValueError> {
        match self {
            Primitive::U8(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U8))
        }
    }

    #[inline]
    pub fn to_u16(self) -> Result<u16, ValueError> {
        match self {
            Primitive::U16(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U16))
        }
    }

    #[inline]
    pub fn to_u32(self) -> Result<u32, ValueError> {
        match self {
            Primitive::U32(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U32))
        }
    }

    #[inline]
    pub fn to_u64(self) -> Result<u64, ValueError> {
        match self {
            Primitive::U64(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U64))
        }
    }

    #[inline]
    pub fn to_u128(self) -> Result<u128, ValueError> {
        match self {
            Primitive::U128(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U128))
        }
    }

    #[inline]
    pub fn to_u256(self) -> Result<U256, ValueError> {
        match self {
            Primitive::U256(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U256))
        }
    }

    #[inline]
    pub fn to_string(self) -> Result<String, ValueError> {
        match self {
            Primitive::String(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::String))
        }
    }

    #[inline]
    pub fn to_bool(self) -> Result<bool, ValueError> {
        match self {
            Primitive::Boolean(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Bool))
        }
    }

    #[inline]
    pub fn as_range(&self) -> Result<(&Primitive, &Primitive), ValueError> {
        match self {
            Primitive::Range(range) => Ok((&range.0, &range.1)),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Range(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn to_range(self) -> Result<(Primitive, Primitive), ValueError> {
        match self {
            Primitive::Range(range) => Ok((range.0, range.1)),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Range(Box::new(Type::Any))))
        }
    }

    // Check if the value is a number
    #[inline]
    pub fn is_number(&self) -> bool {
        match self {
            Primitive::U8(_) | Primitive::U16(_) | Primitive::U32(_) | Primitive::U64(_) | Primitive::U128(_) | Primitive::U256(_) => true,
            _ => false
        }
    }

    pub fn as_opaque(&self) -> Result<&OpaqueWrapper, ValueError> {
        match self {
            Primitive::Opaque(opaque) => Ok(opaque),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    pub fn as_opaque_mut(&mut self) -> Result<&mut OpaqueWrapper, ValueError> {
        match self {
            Primitive::Opaque(opaque) => Ok(opaque),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    pub fn to_opaque(self) -> Result<OpaqueWrapper, ValueError> {
        match self {
            Primitive::Opaque(opaque) => Ok(opaque),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    // Increment the value
    pub fn increment(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Primitive::U8(n) => *n += 1,
            Primitive::U16(n) => *n += 1,
            Primitive::U32(n) => *n += 1,
            Primitive::U64(n) => *n += 1,
            Primitive::U128(n) => *n += 1,
            Primitive::U256(n) => *n += U256::ONE,
            _ => return Err(ValueError::OperationNotNumberType)
        })
    }

    // Decrement the value
    pub fn decrement(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Primitive::U8(n) => *n -= 1,
            Primitive::U16(n) => *n -= 1,
            Primitive::U32(n) => *n -= 1,
            Primitive::U64(n) => *n -= 1,
            Primitive::U128(n) => *n -= 1,
            Primitive::U256(n) => *n -= U256::ONE,
            _ => return Err(ValueError::OperationNotNumberType)
        })
    }

    // Cast value to string
    #[inline]
    pub fn cast_to_string(self) -> Result<String, ValueError> {
        match self {
            Primitive::U8(n) => Ok(n.to_string()),
            Primitive::U16(n) => Ok(n.to_string()),
            Primitive::U32(n) => Ok(n.to_string()),
            Primitive::U64(n) => Ok(n.to_string()),
            Primitive::U128(n) => Ok(n.to_string()),
            Primitive::U256(n) => Ok(n.to_string()),
            Primitive::String(s) => Ok(s),
            Primitive::Boolean(b) => Ok(b.to_string()),
            _ => Err(ValueError::InvalidCastType(Type::String))
        }
    }

    // transform the value into as string
    pub fn as_string_formatted<'a>(&'a self) -> Result<Cow<'a, str>, ValueError> {
        match self {
            Primitive::String(s) => Ok(Cow::Borrowed(s)),
            Primitive::U8(n) => Ok(Cow::Owned(n.to_string())),
            Primitive::U16(n) => Ok(Cow::Owned(n.to_string())),
            Primitive::U32(n) => Ok(Cow::Owned(n.to_string())),
            Primitive::U64(n) => Ok(Cow::Owned(n.to_string())),
            Primitive::U128(n) => Ok(Cow::Owned(n.to_string())),
            Primitive::U256(n) => Ok(Cow::Owned(n.to_string())),
            Primitive::Boolean(b) => Ok(Cow::Owned(b.to_string())),
            _ => Err(ValueError::InvalidCastType(Type::String))
        }
    }

    // Cast the value to the expected type
    pub fn mut_checked_cast_to_primitive_type(&mut self, expected: &Type) -> Result<(), ValueError> {
        let take = std::mem::take(self);
        let value = take.checked_cast_to_primitive_type(expected)?;
        *self = value;
        Ok(())
    }

    // Cast without loss in the expected type
    #[inline]
    pub fn checked_cast_to_primitive_type(self, expected: &Type) -> Result<Primitive, ValueError> {
        match expected {
            Type::U8 => self.checked_cast_to_u8().map(Primitive::U8),
            Type::U16 => self.checked_cast_to_u16().map(Primitive::U16),
            Type::U32 => self.checked_cast_to_u32().map(Primitive::U32),
            Type::U64 => self.checked_cast_to_u64().map(Primitive::U64),
            Type::U128 => self.checked_cast_to_u128().map(Primitive::U128),
            Type::U256 => self.checked_cast_to_u256().map(Primitive::U256),
            Type::String => self.cast_to_string().map(Primitive::String),
            Type::Bool => self.cast_to_bool().map(Primitive::Boolean),
            Type::Range(inner) => {
                let (start, end) = self.to_range()?;
                let start = start.checked_cast_to_primitive_type(inner)?;
                let end = end.checked_cast_to_primitive_type(inner)?;
                Ok(Primitive::Range(Box::new((start, end))))
            },
            _ => Err(ValueError::InvalidCastType(expected.clone()))
        }
    }

    // Cast to u8, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u8(self) -> Result<u8, ValueError> {
        checked_cast!(self, Type::U8)
    }

    // Cast to u16, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u16(self) -> Result<u16, ValueError> {
        checked_cast!(self, Type::U16)
    }

    // Cast to u32, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u32(self) -> Result<u32, ValueError> {
        checked_cast!(self, Type::U32)
    }

    // Cast to u64, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u64(self) -> Result<u64, ValueError> {
        checked_cast!(self, Type::U64)
    }

    // Cast to u128, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u128(self) -> Result<u128, ValueError> {
        checked_cast!(self, Type::U128)
    }

    // Cast to u256, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u256(self) -> Result<U256, ValueError> {
        checked_cast!(self, Type::U256)
    }

    // Cast value to bool
    #[inline]
    pub fn cast_to_bool(self) -> Result<bool, ValueError> {
        match self {
            Primitive::U8(n) => Ok(n != 0),
            Primitive::U16(n) => Ok(n != 0),
            Primitive::U32(n) => Ok(n != 0),
            Primitive::U64(n) => Ok(n != 0),
            Primitive::U128(n) => Ok(n != 0),
            Primitive::U256(n) => Ok(!n.is_zero()),
            Primitive::Boolean(b) => Ok(b),
            _ => Err(ValueError::InvalidCastType(Type::Bool))
        }
    }

    // Cast value to u8
    #[inline]
    pub fn cast_to_u8(self) -> Result<u8, ValueError> {
        match self {
            Primitive::U8(n) => Ok(n),
            Primitive::U16(n) => Ok(n as u8),
            Primitive::U32(n) => Ok(n as u8),
            Primitive::U64(n) => Ok(n as u8),
            Primitive::U128(n) => Ok(n as u8),
            Primitive::U256(n) => Ok(n.low_u64() as u8),
            Primitive::Boolean(b) => Ok(b as u8),
            _ => Err(ValueError::InvalidCastType(Type::U8))
        }
    }

    // Cast value to u16
    #[inline]
    pub fn cast_to_u16(self) -> Result<u16, ValueError> {
        match self {
            Primitive::U8(n) => Ok(n as u16),
            Primitive::U16(n) => Ok(n),
            Primitive::U32(n) => Ok(n as u16),
            Primitive::U64(n) => Ok(n as u16),
            Primitive::U128(n) => Ok(n as u16),
            Primitive::U256(n) => Ok(n.low_u64() as u16),
            Primitive::Boolean(b) => Ok(b as u16),
            _ => Err(ValueError::InvalidCastType(Type::U16))
        }
    }

    // Cast value to u32
    #[inline]
    pub fn cast_to_u32(self) -> Result<u32, ValueError> {
        match self {
            Primitive::U8(n) => Ok(n as u32),
            Primitive::U16(n) => Ok(n as u32),
            Primitive::U32(n) => Ok(n),
            Primitive::U64(n) => Ok(n as u32),
            Primitive::U128(n) => Ok(n as u32),
            Primitive::U256(n) => Ok(n.low_u64() as u32),
            Primitive::Boolean(b) => Ok(b as u32),
            _ => Err(ValueError::InvalidCastType(Type::U16))
        }
    }

    // Cast value to u64
    #[inline]
    pub fn cast_to_u64(self) -> Result<u64, ValueError> {
        match self {
            Primitive::U8(n) => Ok(n as u64),
            Primitive::U16(n) => Ok(n as u64),
            Primitive::U32(n) => Ok(n as u64),
            Primitive::U64(n) => Ok(n),
            Primitive::U128(n) => Ok(n as u64),
            Primitive::U256(n) => Ok(n.low_u64()),
            Primitive::Boolean(b) => Ok(b as u64),
            _ => Err(ValueError::InvalidCastType(Type::U64))
        }
    }

    // Cast value to u128
    #[inline]
    pub fn cast_to_u128(self) -> Result<u128, ValueError> {
        match self {
            Primitive::U8(n) => Ok(n as u128),
            Primitive::U16(n) => Ok(n as u128),
            Primitive::U32(n) => Ok(n as u128),
            Primitive::U64(n) => Ok(n as u128),
            Primitive::U128(n) => Ok(n),
            Primitive::U256(n) => Ok(n.low_u128()),
            Primitive::Boolean(b) => Ok(b as u128),
            _ => Err(ValueError::InvalidCastType(Type::U128))
        }
    }

    // Cast value to u256
    #[inline]
    pub fn cast_to_u256(self) -> Result<U256, ValueError> {
        match self {
            Primitive::U8(n) => Ok(U256::from(n)),
            Primitive::U16(n) => Ok(U256::from(n)),
            Primitive::U32(n) => Ok(U256::from(n)),
            Primitive::U64(n) => Ok(U256::from(n)),
            Primitive::U128(n) => Ok(U256::from(n)),
            Primitive::U256(n) => Ok(n),
            Primitive::Boolean(b) => Ok(U256::from(b as u8)),
            _ => Err(ValueError::InvalidCastType(Type::U256))
        }
    }

    // Retrieve the type of a value
    // Returns an error if it can't be determined
    #[inline]
    pub fn get_type(&self) -> Result<Type, ValueError> {
        Type::from_value(self).ok_or(ValueError::UnknownType)
    }
}

impl std::fmt::Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Primitive::Null => write!(f, "null"),
            Primitive::U8(v) => write!(f, "{}", v),
            Primitive::U16(v) => write!(f, "{}", v),
            Primitive::U32(v) => write!(f, "{}", v),
            Primitive::U64(v) => write!(f, "{}", v),
            Primitive::U128(v) => write!(f, "{}", v),
            Primitive::U256(v) => write!(f, "{}", v),
            Primitive::String(s) => write!(f, "{}", s),
            Primitive::Boolean(b) => write!(f, "{}", b),
            Primitive::Range(range) => write!(f, "{}..{}", range.0, range.1),
            Primitive::Opaque(o) => write!(f, "{}", o)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_u128_serialization_as_string() {
        // Test that U128 is serialized as a string
        let value = Primitive::U128(340282366920938463463374607431768211455u128); // max u128
        
        // Serialize to JSON
        let json = serde_json::to_string(&value).expect("Failed to serialize");
        
        // Should contain the value as a string, not a number
        assert!(json.contains("\"340282366920938463463374607431768211455\""), 
            "U128 should be serialized as a string, got: {}", json);
        
        // Deserialize back
        let deserialized: Primitive = serde_json::from_str(&json).expect("Failed to deserialize");
        
        // Should match the original value
        assert_eq!(value, deserialized, "Deserialized value should match original");
    }

    #[test]
    fn test_u128_roundtrip() {
        let test_values = vec![
            0u128,
            1u128,
            u64::MAX as u128,
            u64::MAX as u128 + 1,
            u128::MAX,
        ];

        for val in test_values {
            let primitive = Primitive::U128(val);
            let json = serde_json::to_string(&primitive).expect("Failed to serialize");
            let deserialized: Primitive = serde_json::from_str(&json).expect("Failed to deserialize");
            assert_eq!(primitive, deserialized, "Roundtrip failed for value: {}", val);
        }
    }

    #[test]
    fn test_u64_deserialize_from_string() {
        // U64 should deserialize from string input
        let json = r#"{"type":"u64","value":"12345678901234"}"#;
        let deserialized: Primitive = serde_json::from_str(json).expect("Failed to deserialize from string");
        assert_eq!(deserialized, Primitive::U64(12345678901234u64));
    }

    #[test]
    fn test_u64_serialize_as_string() {
        // U64 should serialize as a string
        let value = Primitive::U64(12345678901234u64);
        let json = serde_json::to_string(&value).expect("Failed to serialize");
        // Should contain the value as a string
        assert!(json.contains("\"12345678901234\""), "U64 should be serialized as string, got: {}", json);
    }

    #[test]
    fn test_u64_roundtrip() {
        let test_values = vec![0u64, 1u64, u64::MAX];

        for val in test_values {
            let primitive = Primitive::U64(val);
            let json = serde_json::to_string(&primitive).expect("Failed to serialize");
            let deserialized: Primitive = serde_json::from_str(&json).expect("Failed to deserialize");
            assert_eq!(primitive, deserialized, "Roundtrip failed for value: {}", val);
        }
    }
}
