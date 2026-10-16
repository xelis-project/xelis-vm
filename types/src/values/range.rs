use super::{Primitive, PrimitiveInteger};
use crate::{NumberType, Type, ValueError, U256};
use schemars::{JsonSchema, Schema, SchemaGenerator};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::{
    borrow::Cow,
    fmt,
    hash::{Hash, Hasher},
    ops::Range,
};

/// A half-open integer range. Both endpoints always have the same width.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimitiveRange {
    U8(Range<u8>),
    U16(Range<u16>),
    U32(Range<u32>),
    U64(Range<u64>),
    U128(Range<u128>),
    U256(Range<U256>),
}
impl PrimitiveRange {
    pub fn new(start: PrimitiveInteger, end: PrimitiveInteger) -> Result<Self, ValueError> {
        match (start, end) {
            (PrimitiveInteger::U8(start), PrimitiveInteger::U8(end)) => Ok(Self::U8(start..end)),
            (PrimitiveInteger::U16(start), PrimitiveInteger::U16(end)) => Ok(Self::U16(start..end)),
            (PrimitiveInteger::U32(start), PrimitiveInteger::U32(end)) => Ok(Self::U32(start..end)),
            (PrimitiveInteger::U64(start), PrimitiveInteger::U64(end)) => Ok(Self::U64(start..end)),
            (PrimitiveInteger::U128(start), PrimitiveInteger::U128(end)) => {
                Ok(Self::U128(start..end))
            }
            (PrimitiveInteger::U256(start), PrimitiveInteger::U256(end)) => {
                Ok(Self::U256(start..end))
            }
            _ => Err(ValueError::InvalidRange),
        }
    }
    pub fn start(&self) -> PrimitiveInteger {
        match self {
            Self::U8(range) => PrimitiveInteger::U8(range.start),
            Self::U16(range) => PrimitiveInteger::U16(range.start),
            Self::U32(range) => PrimitiveInteger::U32(range.start),
            Self::U64(range) => PrimitiveInteger::U64(range.start),
            Self::U128(range) => PrimitiveInteger::U128(range.start),
            Self::U256(range) => PrimitiveInteger::U256(range.start),
        }
    }
    pub fn end(&self) -> PrimitiveInteger {
        match self {
            Self::U8(range) => PrimitiveInteger::U8(range.end),
            Self::U16(range) => PrimitiveInteger::U16(range.end),
            Self::U32(range) => PrimitiveInteger::U32(range.end),
            Self::U64(range) => PrimitiveInteger::U64(range.end),
            Self::U128(range) => PrimitiveInteger::U128(range.end),
            Self::U256(range) => PrimitiveInteger::U256(range.end),
        }
    }
    pub fn number_type(&self) -> NumberType {
        self.start().number_type()
    }
    pub fn get_memory_usage(&self) -> usize {
        // Preserve the VM's existing range accounting independently of Rust layout.
        16 + self.start().get_memory_usage() + self.end().get_memory_usage()
    }
    pub fn checked_cast_to_primitive_type(
        self,
        expected: &Type,
    ) -> Result<Self, ValueError> {
        let start = self
            .start()
            .checked_cast_to_primitive_type(expected)?
            .to_integer()?;
        let end = self
            .end()
            .checked_cast_to_primitive_type(expected)?
            .to_integer()?;
        Self::new(start, end)
    }
}
impl TryFrom<(Primitive, Primitive)> for PrimitiveRange {
    type Error = ValueError;
    fn try_from((start, end): (Primitive, Primitive)) -> Result<Self, Self::Error> {
        Self::new(start.to_integer()?, end.to_integer()?)
    }
}
impl Hash for PrimitiveRange {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Keep the legacy pair-of-primitives hash, without an extra enum tag.
        self.start().hash(state);
        self.end().hash(state);
    }
}
impl fmt::Display for PrimitiveRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start(), self.end())
    }
}

impl Serialize for PrimitiveRange {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        (self.start(), self.end()).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for PrimitiveRange {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let (start, end) = <(PrimitiveInteger, PrimitiveInteger)>::deserialize(deserializer)?;
        Self::new(start, end).map_err(serde::de::Error::custom)
    }
}
impl JsonSchema for PrimitiveRange {
    fn schema_name() -> Cow<'static, str> {
        "PrimitiveRange".into()
    }
    fn json_schema(generator: &mut SchemaGenerator) -> Schema {
        <(PrimitiveInteger, PrimitiveInteger)>::json_schema(generator)
    }
}
