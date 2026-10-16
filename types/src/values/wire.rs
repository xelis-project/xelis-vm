//! Preserve the serialized representation while separating runtime integers and opaques.
use super::{Primitive, PrimitiveInteger, PrimitiveRange};
use crate::{CellArray, CellMap, Constant, ConstantValueCell, DefinedType, OpaqueWrapper, ValueCell, ValueError, U256};
use indexmap::IndexMap;
use schemars::{JsonSchema, Schema, SchemaGenerator};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::borrow::Cow;

// Keep the original enum ordering: serializers may use variant indices as well as tags.
// Cow borrows payloads during serialization and owns them during deserialization.
macro_rules! primitive_wire {
    ($name:ident $(, $opaque:ident)?) => {
        #[derive(Serialize, Deserialize, JsonSchema)]
        #[serde(rename="Primitive", rename_all="snake_case", tag="type", content="value")]
        pub(crate) enum $name<'a> {
            Null,
            Boolean(bool),
            U8(u8),
            U16(u16),
            U32(u32),
            #[serde(serialize_with="super::serialize_u64_as_string", deserialize_with="super::deserialize_u64_from_string")]
            U64(u64),
            #[serde(serialize_with="super::serialize_u128_as_string", deserialize_with="super::deserialize_u128_from_string")]
            U128(u128),
            U256(U256),
            String(Cow<'a, str>),
            Range(Cow<'a, PrimitiveRange>),
            $($opaque(Cow<'a, OpaqueWrapper>),)?
        }
        impl<'a> From<&'a Primitive> for $name<'a> {
            fn from(value: &'a Primitive) -> Self {
                match value {
                    Primitive::Null => Self::Null,
                    Primitive::Boolean(v) => Self::Boolean(*v),
                    Primitive::String(v) => Self::String(Cow::Borrowed(v)),
                    Primitive::Range(v) => Self::Range(Cow::Borrowed(v)),
                    Primitive::Integer(PrimitiveInteger::U8(n)) => Self::U8(*n),
                    Primitive::Integer(PrimitiveInteger::U16(n)) => Self::U16(*n),
                    Primitive::Integer(PrimitiveInteger::U32(n)) => Self::U32(*n),
                    Primitive::Integer(PrimitiveInteger::U64(n)) => Self::U64(*n),
                    Primitive::Integer(PrimitiveInteger::U128(n)) => Self::U128(*n),
                    Primitive::Integer(PrimitiveInteger::U256(n)) => Self::U256(*n),
                }
            }
        }
        impl $name<'_> {
            fn into_primitive(self) -> Result<Primitive, ValueError> {
                Ok(match self {
                    Self::Null => Primitive::Null,
                    Self::Boolean(v) => Primitive::Boolean(v),
                    Self::String(v) => Primitive::String(v.into_owned()),
                    Self::Range(range) => Primitive::Range(Box::new(range.into_owned())),
                    Self::U8(n) => Primitive::Integer(PrimitiveInteger::U8(n)),
                    Self::U16(n) => Primitive::Integer(PrimitiveInteger::U16(n)),
                    Self::U32(n) => Primitive::Integer(PrimitiveInteger::U32(n)),
                    Self::U64(n) => Primitive::Integer(PrimitiveInteger::U64(n)),
                    Self::U128(n) => Primitive::Integer(PrimitiveInteger::U128(n)),
                    Self::U256(n) => Primitive::Integer(PrimitiveInteger::U256(n)),
                    $(Self::$opaque(_) => return Err(ValueError::ExpectedValue),)?
                })
            }
        }
    };
}
primitive_wire!(PrimitiveWire);
primitive_wire!(LegacyPrimitiveWire, Opaque);

impl Serialize for Primitive {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        PrimitiveWire::from(self).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for Primitive {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        PrimitiveWire::deserialize(deserializer)?
            .into_primitive()
            .map_err(serde::de::Error::custom)
    }
}
impl JsonSchema for Primitive {
    fn schema_name() -> Cow<'static, str> {
        Cow::Borrowed("Primitive")
    }
    fn json_schema(generator: &mut SchemaGenerator) -> Schema {
        PrimitiveWire::json_schema(generator)
    }
}

#[derive(Serialize)]
#[serde(
    rename = "ValueCell",
    rename_all = "snake_case",
    tag = "type",
    content = "value"
)]
enum ValueCellRef<'a> {
    Primitive(LegacyPrimitiveWire<'a>),
    #[serde(with = "hex::serde")]
    Bytes(&'a Vec<u8>),
    Object(&'a CellArray),
    #[serde(serialize_with = "super::cell::serde_map::serialize")]
    Map(&'a Box<CellMap>),
}

#[derive(Deserialize)]
#[serde(
    rename = "ValueCell",
    rename_all = "snake_case",
    tag = "type",
    content = "value"
)]
enum ValueCellOwned {
    Primitive(LegacyPrimitiveWire<'static>),
    #[serde(with = "hex::serde")]
    Bytes(Vec<u8>),
    Object(CellArray),
    #[serde(deserialize_with = "super::cell::serde_map::deserialize")]
    Map(Box<CellMap>),
}
impl Serialize for ValueCell {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Self::Primitive(p) => ValueCellRef::Primitive(LegacyPrimitiveWire::from(p)),
            Self::Opaque(o) => {
                ValueCellRef::Primitive(LegacyPrimitiveWire::Opaque(Cow::Borrowed(o)))
            }
            Self::Bytes(v) => ValueCellRef::Bytes(v),
            Self::Object(v) => ValueCellRef::Object(v),
            Self::Map(v) => ValueCellRef::Map(v),
        }
        .serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for ValueCell {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(match ValueCellOwned::deserialize(deserializer)? {
            ValueCellOwned::Primitive(LegacyPrimitiveWire::Opaque(o)) => {
                Self::Opaque(o.into_owned())
            }
            ValueCellOwned::Primitive(p) => {
                Self::Primitive(p.into_primitive().map_err(serde::de::Error::custom)?)
            }
            ValueCellOwned::Bytes(v) => Self::Bytes(v),
            ValueCellOwned::Object(v) => Self::Object(v),
            ValueCellOwned::Map(v) => Self::Map(v),
        })
    }
}

#[derive(Serialize)]
#[serde(
    rename = "ConstantValueCell",
    rename_all = "snake_case",
    tag = "type",
    content = "value"
)]
enum ConstantValueCellRef<'a> {
    Primitive(LegacyPrimitiveWire<'a>),
    #[serde(with = "hex::serde")]
    Bytes(&'a Vec<u8>),
    Object(&'a Vec<ConstantValueCell>),
    #[serde(serialize_with = "super::cell::constant_cell::serialize_map")]
    Map(&'a IndexMap<ConstantValueCell, ConstantValueCell>),
}

#[derive(Deserialize)]
#[serde(
    rename = "ConstantValueCell",
    rename_all = "snake_case",
    tag = "type",
    content = "value"
)]
enum ConstantValueCellOwned {
    Primitive(LegacyPrimitiveWire<'static>),
    #[serde(with = "hex::serde")]
    Bytes(Vec<u8>),
    Object(Vec<ConstantValueCell>),
    #[serde(deserialize_with = "super::cell::constant_cell::deserialize_map")]
    Map(IndexMap<ConstantValueCell, ConstantValueCell>),
}
impl Serialize for ConstantValueCell {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Self::Primitive(p) => ConstantValueCellRef::Primitive(LegacyPrimitiveWire::from(p)),
            Self::Opaque(o) => {
                ConstantValueCellRef::Primitive(LegacyPrimitiveWire::Opaque(Cow::Borrowed(o)))
            }
            Self::Bytes(v) => ConstantValueCellRef::Bytes(v),
            Self::Object(v) => ConstantValueCellRef::Object(v),
            Self::Map(v) => ConstantValueCellRef::Map(v),
        }
        .serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for ConstantValueCell {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(match ConstantValueCellOwned::deserialize(deserializer)? {
            ConstantValueCellOwned::Primitive(LegacyPrimitiveWire::Opaque(o)) => {
                Self::Opaque(o.into_owned())
            }
            ConstantValueCellOwned::Primitive(p) => {
                Self::Primitive(p.into_primitive().map_err(serde::de::Error::custom)?)
            }
            ConstantValueCellOwned::Bytes(v) => Self::Bytes(v),
            ConstantValueCellOwned::Object(v) => Self::Object(v),
            ConstantValueCellOwned::Map(v) => Self::Map(v),
        })
    }
}

#[derive(Serialize)]
#[serde(
    rename = "Constant",
    rename_all = "snake_case",
    tag = "type",
    content = "value"
)]
enum ConstantRef<'a> {
    Primitive(LegacyPrimitiveWire<'a>),
    Array(&'a Vec<Constant>),
    Bytes(&'a Vec<u8>),
    Map(&'a IndexMap<Constant, Constant>),
    Typed(&'a Vec<Constant>, &'a DefinedType),
}

#[derive(Deserialize)]
#[serde(
    rename = "Constant",
    rename_all = "snake_case",
    tag = "type",
    content = "value"
)]
enum ConstantOwned {
    Primitive(LegacyPrimitiveWire<'static>),
    Array(Vec<Constant>),
    Bytes(Vec<u8>),
    Map(IndexMap<Constant, Constant>),
    Typed(Vec<Constant>, DefinedType),
}
impl Serialize for Constant {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        match self {
            Self::Primitive(p) => ConstantRef::Primitive(LegacyPrimitiveWire::from(p)),
            Self::Opaque(o) => {
                ConstantRef::Primitive(LegacyPrimitiveWire::Opaque(Cow::Borrowed(o)))
            }
            Self::Array(v) => ConstantRef::Array(v),
            Self::Bytes(v) => ConstantRef::Bytes(v),
            Self::Map(v) => ConstantRef::Map(v),
            Self::Typed(v, ty) => ConstantRef::Typed(v, ty),
        }
        .serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for Constant {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(match ConstantOwned::deserialize(deserializer)? {
            ConstantOwned::Primitive(LegacyPrimitiveWire::Opaque(o)) => {
                Self::Opaque(o.into_owned())
            }
            ConstantOwned::Primitive(p) => {
                Self::Primitive(p.into_primitive().map_err(serde::de::Error::custom)?)
            }
            ConstantOwned::Array(v) => Self::Array(v),
            ConstantOwned::Bytes(v) => Self::Bytes(v),
            ConstantOwned::Map(v) => Self::Map(v),
            ConstantOwned::Typed(v, ty) => Self::Typed(v, ty),
        })
    }
}

// A range endpoint uses the same numeric encoding as a standalone primitive.
impl Serialize for PrimitiveInteger {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        Primitive::Integer(*self).serialize(serializer)
    }
}
impl<'de> Deserialize<'de> for PrimitiveInteger {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Primitive::deserialize(deserializer)?
            .to_integer()
            .map_err(serde::de::Error::custom)
    }
}
