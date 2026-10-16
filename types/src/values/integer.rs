use super::Primitive;
use crate::{NumberType, Type, ValueError, U256};
use schemars::JsonSchema;
use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
};

/// An integer value with its exact VM width.
#[derive(Debug, Clone, Copy, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case", tag = "type", content = "value")]
pub enum PrimitiveInteger {
    U8(u8),
    U16(u16),
    U32(u32),
    #[serde(
        serialize_with = "super::serialize_u64_as_string",
        deserialize_with = "super::deserialize_u64_from_string"
    )]
    U64(u64),
    #[serde(
        serialize_with = "super::serialize_u128_as_string",
        deserialize_with = "super::deserialize_u128_from_string"
    )]
    U128(u128),
    U256(U256),
}
impl PrimitiveInteger {
    pub fn number_type(&self) -> NumberType {
        match self {
            Self::U8(..) => NumberType::U8,
            Self::U16(..) => NumberType::U16,
            Self::U32(..) => NumberType::U32,
            Self::U64(..) => NumberType::U64,
            Self::U128(..) => NumberType::U128,
            Self::U256(..) => NumberType::U256,
        }
    }
    pub fn get_type(&self) -> Type {
        match self {
            Self::U8(..) => Type::U8,
            Self::U16(..) => Type::U16,
            Self::U32(..) => Type::U32,
            Self::U64(..) => Type::U64,
            Self::U128(..) => Type::U128,
            Self::U256(..) => Type::U256,
        }
    }
    pub fn get_memory_usage(&self) -> usize {
        match self {
            Self::U8(..) => 1,
            Self::U16(..) => 2,
            Self::U32(..) => 4,
            Self::U64(..) => 8,
            Self::U128(..) => 16,
            Self::U256(..) => 32,
        }
    }
    pub fn increment(&mut self) {
        match self {
            Self::U8(n) => *n = n.wrapping_add(1),
            Self::U16(n) => *n = n.wrapping_add(1),
            Self::U32(n) => *n = n.wrapping_add(1),
            Self::U64(n) => *n = n.wrapping_add(1),
            Self::U128(n) => *n = n.wrapping_add(1),
            Self::U256(n) => *n = n.wrapping_add(U256::ONE),
        }
    }
    pub fn add(self, other: &Self) -> Result<Self, ValueError> {
        match (self, other) {
            (Self::U8(a), Self::U8(b)) => Ok(Self::U8(a.wrapping_add(*b))),
            (Self::U16(a), Self::U16(b)) => Ok(Self::U16(a.wrapping_add(*b))),
            (Self::U32(a), Self::U32(b)) => Ok(Self::U32(a.wrapping_add(*b))),
            (Self::U64(a), Self::U64(b)) => Ok(Self::U64(a.wrapping_add(*b))),
            (Self::U128(a), Self::U128(b)) => Ok(Self::U128(a.wrapping_add(*b))),
            (Self::U256(a), Self::U256(b)) => Ok(Self::U256(a.wrapping_add(*b))),
            _ => Err(ValueError::OperationNotNumberType),
        }
    }
    pub fn sub(self, other: &Self) -> Result<Self, ValueError> {
        match (self, other) {
            (Self::U8(a), Self::U8(b)) => Ok(Self::U8(a.wrapping_sub(*b))),
            (Self::U16(a), Self::U16(b)) => Ok(Self::U16(a.wrapping_sub(*b))),
            (Self::U32(a), Self::U32(b)) => Ok(Self::U32(a.wrapping_sub(*b))),
            (Self::U64(a), Self::U64(b)) => Ok(Self::U64(a.wrapping_sub(*b))),
            (Self::U128(a), Self::U128(b)) => Ok(Self::U128(a.wrapping_sub(*b))),
            (Self::U256(a), Self::U256(b)) => Ok(Self::U256(a.wrapping_sub(*b))),
            _ => Err(ValueError::OperationNotNumberType),
        }
    }
    pub fn decrement(&mut self) {
        match self {
            Self::U8(n) => *n = n.wrapping_sub(1),
            Self::U16(n) => *n = n.wrapping_sub(1),
            Self::U32(n) => *n = n.wrapping_sub(1),
            Self::U64(n) => *n = n.wrapping_sub(1),
            Self::U128(n) => *n = n.wrapping_sub(1),
            Self::U256(n) => *n = n.wrapping_sub(U256::ONE),
        }
    }
    pub fn checked_cast_to_primitive_type(self, expected: &Type) -> Result<Primitive, ValueError> {
        Primitive::Integer(self).checked_cast_to_primitive_type(expected)
    }
    pub fn to_u32(self) -> Result<u32, ValueError> {
        Primitive::Integer(self).to_u32()
    }
}
impl Hash for PrimitiveInteger {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::U8(n) => {
                1u8.hash(state);
                n.hash(state);
            }
            Self::U16(n) => {
                2u8.hash(state);
                n.hash(state);
            }
            Self::U32(n) => {
                3u8.hash(state);
                n.hash(state);
            }
            Self::U64(n) => {
                4u8.hash(state);
                n.hash(state);
            }
            Self::U128(n) => {
                5u8.hash(state);
                n.hash(state);
            }
            Self::U256(n) => {
                6u8.hash(state);
                n.hash(state);
            }
        }
    }
}
impl fmt::Display for PrimitiveInteger {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U8(n) => n.fmt(f),
            Self::U16(n) => n.fmt(f),
            Self::U32(n) => n.fmt(f),
            Self::U64(n) => n.fmt(f),
            Self::U128(n) => n.fmt(f),
            Self::U256(n) => n.fmt(f),
        }
    }
}
impl PartialOrd for PrimitiveInteger {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::U8(a), Self::U8(b)) => a.partial_cmp(b),
            (Self::U16(a), Self::U16(b)) => a.partial_cmp(b),
            (Self::U32(a), Self::U32(b)) => a.partial_cmp(b),
            (Self::U64(a), Self::U64(b)) => a.partial_cmp(b),
            (Self::U128(a), Self::U128(b)) => a.partial_cmp(b),
            (Self::U256(a), Self::U256(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}
impl From<PrimitiveInteger> for Primitive {
    fn from(value: PrimitiveInteger) -> Self {
        Self::Integer(value)
    }
}
