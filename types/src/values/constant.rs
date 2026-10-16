use std::{fmt, hash::{Hash, Hasher}};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use crate::{PrimitiveInteger, PrimitiveRange, DefinedType, Opaque, OpaqueWrapper, Type, U256};
use super::{Primitive, ValueError};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constant {
    Primitive(Primitive),
    Opaque(OpaqueWrapper),
    Array(Vec<Constant>),
    // Bytes represents a Vec<u8>
    Bytes(Vec<u8>),

    // Map cannot be used as a key in another map
    Map(IndexMap<Constant, Constant>),
    Typed(Vec<Constant>, DefinedType),
}

// Wrapper to drop the value without stackoverflow
#[derive(Debug, Hash, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ConstantWrapper(pub Constant);

impl Drop for ConstantWrapper {
    fn drop(&mut self) {
        if matches!(self.0, Constant::Primitive(_) | Constant::Opaque(_)) {
            return
        }

        let mut stack = vec![std::mem::take(&mut self.0)];
        while let Some(value) = stack.pop() {
            match value {
                Constant::Primitive(_) | Constant::Opaque(_) => {},
                Constant::Array(values) => stack.extend(values),
                Constant::Bytes(_) => {},
                Constant::Map(map) => stack.extend(map.into_iter().flat_map(|(k, v)| [k, v])),
                Constant::Typed(fields, _) => stack.extend(fields),
            }
        }
    }
}

impl Hash for Constant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Fast path
        if let Self::Primitive(v) = self {
            v.hash(state);
            return;
        }

        if let Self::Opaque(value) = self {
            10u8.hash(state);
            value.hash(state);
            return;
        }

        let mut stack = vec![self];
        while let Some(value) = stack.pop() {
            match value {
                Self::Primitive(v) => v.hash(state),
                Self::Opaque(v) => {
                    10u8.hash(state);
                    v.hash(state);
                },
                Self::Array(values) => {
                    12u8.hash(state);
                    stack.extend(values);
                },
                Self::Bytes(values) => {
                    13u8.hash(state);
                    values.hash(state);
                }
                Self::Map(map) => {
                    14u8.hash(state);
                    stack.extend(map.iter().flat_map(|(k, v)| [k, v]))
                },
                Self::Typed(fields, ty) => {
                    15u8.hash(state);
                    stack.extend(fields);
                    ty.hash(state);
                }
            }
        }
    }
}

impl Default for Constant {
    fn default() -> Self {
        Self::Primitive(Default::default())
    }
}

impl From<Primitive> for Constant {
    fn from(value: Primitive) -> Self {
        Self::Primitive(value)
    }
}

impl Constant {
    #[inline]
    pub fn is_null(&self) -> bool {
        match &self {
            Self::Primitive(Primitive::Null) => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        match &self {
            Self::Primitive(Primitive::String(_)) => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_map(&self) -> bool {
        match &self {
            Self::Map(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn as_u8(&self) -> Result<u8, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U8(n))) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U8))
        }
    }

    #[inline]
    pub fn as_u16(&self) -> Result<u16, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U16(n))) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U16))
        }
    }

    #[inline]
    pub fn as_u32(&self) -> Result<u32, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U32(n))) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U32))
        }
    }

    #[inline]
    pub fn as_u64(&self) -> Result<u64, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U64(n))) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U64))
        }
    }

    #[inline]
    pub fn as_u128(&self) -> Result<u128, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U128(n))) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U128))
        }
    }

    #[inline]
    pub fn as_u256(&self) -> Result<U256, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U256(n))) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U256))
        }
    }

    #[inline]
    pub fn as_string(&self) -> Result<&String, ValueError> {
        match self {
            Self::Primitive(Primitive::String(n)) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::String))
        }
    }

    #[inline]
    pub fn as_bool(&self) -> Result<bool, ValueError> {
        match self {
            Self::Primitive(Primitive::Boolean(n)) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::Bool))
        }
    }

    #[inline]
    pub fn as_map(&self) -> Result<&IndexMap<Self, Self>, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedMap)
        }
    }

    #[inline]
    pub fn as_mut_map(&mut self) -> Result<&mut IndexMap<Self, Self>, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedMap)
        }
    }

    #[inline]
    pub fn as_vec<'a>(&'a self) -> Result<&'a Vec<Self>, ValueError> {
        match self {
            Self::Array(n) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn as_mut_vec<'a>(&'a mut self) -> Result<&'a mut Vec<Self>, ValueError> {
        match self {
            Self::Array(n) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn as_optional(&self) -> Result<Option<&Self>, ValueError> {
        match self {
            Self::Primitive(Primitive::Null) => Ok(None),
            v => Ok(Some(v))
        }
    }

    #[inline]
    pub fn is_serializable(&self) -> bool {
        match self {
            Self::Opaque(op) => op.is_serializable(),
            _ => true
        }
    }

    #[inline]
    pub fn to_u8(self) -> Result<u8, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U8(n))) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U8))
        }
    }

    #[inline]
    pub fn to_u16(self) -> Result<u16, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U16(n))) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U16))
        }
    }

    #[inline]
    pub fn to_u32(self) -> Result<u32, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U32(n))) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U32))
        }
    }

    #[inline]
    pub fn to_u64(self) -> Result<u64, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U64(n))) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U64))
        }
    }

    #[inline]
    pub fn to_u128(self) -> Result<u128, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U128(n))) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U128))
        }
    }

    #[inline]
    pub fn to_u256(self) -> Result<U256, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U256(n))) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U256))
        }
    }

    #[inline]
    pub fn to_string(self) -> Result<String, ValueError> {
        match self {
            Self::Primitive(Primitive::String(n)) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::String))
        }
    }

    #[inline]
    pub fn to_bool(self) -> Result<bool, ValueError> {
        match self {
            Self::Primitive(Primitive::Boolean(n)) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::Bool))
        }
    }

    #[inline]
    pub fn to_vec(self) -> Result<Vec<Self>, ValueError> {
        match self {
            Self::Array(n) => Ok(n),
            Self::Typed(values, _) => Ok(values),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn as_range(&self) -> Result<&PrimitiveRange, ValueError> {
        self.as_value().and_then(Primitive::as_range)
    }

    #[inline]
    pub fn to_range(self) -> Result<PrimitiveRange, ValueError> {
        self.into_value().and_then(Primitive::to_range)
    }

    // Check if the value is a number
    #[inline]
    pub fn is_number(&self) -> bool {
        match self {
            Self::Primitive(v) => v.is_number(),
            _ => false
        }
    }

    // Increment the value
    pub fn increment(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Self::Primitive(v) => v.increment()?,
            _ => return Err(ValueError::OperationNotNumberType)
        })
    }

    // Decrement the value
    pub fn decrement(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Self::Primitive(v) => v.decrement()?,
            _ => return Err(ValueError::OperationNotNumberType)
        })
    }

    // Cast value to string
    #[inline]
    pub fn cast_to_string(self) -> Result<String, ValueError> {
        match self {
            Self::Primitive(v) => v.cast_to_string(),
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
    pub fn checked_cast_to_primitive_type(self, expected: &Type) -> Result<Self, ValueError> {
        match expected {
            Type::U8 => self.checked_cast_to_u8().map(|value| Primitive::Integer(PrimitiveInteger::U8(value))),
            Type::U16 => self.checked_cast_to_u16().map(|value| Primitive::Integer(PrimitiveInteger::U16(value))),
            Type::U32 => self.checked_cast_to_u32().map(|value| Primitive::Integer(PrimitiveInteger::U32(value))),
            Type::U64 => self.checked_cast_to_u64().map(|value| Primitive::Integer(PrimitiveInteger::U64(value))),
            Type::U128 => self.checked_cast_to_u128().map(|value| Primitive::Integer(PrimitiveInteger::U128(value))),
            Type::U256 => self.checked_cast_to_u256().map(|value| Primitive::Integer(PrimitiveInteger::U256(value))),
            Type::String => self.cast_to_string().map(Primitive::String),
            Type::Bool => self.cast_to_bool().map(Primitive::Boolean),
            Type::Optional(inner) => {
                if self.is_null() {
                    return Ok(self)
                } else {
                    return self.checked_cast_to_primitive_type(inner)
                }
            },
            Type::Range(inner) => {
                self.to_range()?.checked_cast_to_primitive_type(inner)
                    .map(|range| Primitive::Range(Box::new(range)))
            },
            _ => Err(ValueError::InvalidCastType(expected.clone()))
        }.map(Self::Primitive)
    }

    // Cast to u8, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u8(self) -> Result<u8, ValueError> {
        self.into_value().and_then(Primitive::checked_cast_to_u8)
    }

    // Cast to u16, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u16(self) -> Result<u16, ValueError> {
        self.into_value().and_then(Primitive::checked_cast_to_u16)
    }

    // Cast to u32, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u32(self) -> Result<u32, ValueError> {
        self.into_value().and_then(Primitive::checked_cast_to_u32)
    }

    // Cast to u64, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u64(self) -> Result<u64, ValueError> {
        self.into_value().and_then(Primitive::checked_cast_to_u64)
    }

    // Cast to u128, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u128(self) -> Result<u128, ValueError> {
        self.into_value().and_then(Primitive::checked_cast_to_u128)
    }

    // Cast to u256, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u256(self) -> Result<U256, ValueError> {
        self.into_value().and_then(Primitive::checked_cast_to_u256)
    }

    // Cast value to bool
    #[inline]
    pub fn cast_to_bool(self) -> Result<bool, ValueError> {
        self.into_value().and_then(Primitive::cast_to_bool)
    }

    // Cast value to u8
    #[inline]
    pub fn cast_to_u8(self) -> Result<u8, ValueError> {
        self.into_value().and_then(Primitive::cast_to_u8)
    }

    // Cast value to u16
    #[inline]
    pub fn cast_to_u16(self) -> Result<u16, ValueError> {
        self.into_value().and_then(Primitive::cast_to_u16)
    }

    // Cast value to u32
    #[inline]
    pub fn cast_to_u32(self) -> Result<u32, ValueError> {
        self.into_value().and_then(Primitive::cast_to_u32)
    }

    // Cast value to u64
    #[inline]
    pub fn cast_to_u64(self) -> Result<u64, ValueError> {
        self.into_value().and_then(Primitive::cast_to_u64)
    }

    // Cast value to u128
    #[inline]
    pub fn cast_to_u128(self) -> Result<u128, ValueError> {
        self.into_value().and_then(Primitive::cast_to_u128)
    }

    // Cast value to u256
    #[inline]
    pub fn cast_to_u256(self) -> Result<U256, ValueError> {
        self.into_value().and_then(Primitive::cast_to_u256)
    }

    #[inline(always)]
    pub fn as_value(&self) -> Result<&Primitive, ValueError> {
        match self {
            Self::Primitive(v) => Ok(v),
            _ => Err(ValueError::InvalidValueType(self.clone(), Type::Any))
        }
    }

    #[inline(always)]
    pub fn into_value(self) -> Result<Primitive, ValueError> {
        match self {
            Self::Primitive(v) => Ok(v),
            _ => Err(ValueError::InvalidValueType(self, Type::Any))
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use fmt::Write;

        // Use iterative formatting to avoid stack overflow on deep nesting
        enum FormatItem<'a> {
            Value(&'a Constant),
            OpenArray,
            CloseArray,
            OpenMap,
            CloseMap,
            OpenTyped(&'a DefinedType),
            CloseTyped,
            MapSeparator,
            ArraySeparator,
            FieldIndex(usize),
        }

        let mut output = String::new();
        let mut stack = vec![FormatItem::Value(self)];
        let mut first_in_container = Vec::new();

        while let Some(item) = stack.pop() {
            match item {
                FormatItem::Value(constant) => {
                    match constant {
                        Self::Primitive(v) => {
                            write!(&mut output, "{}", v).map_err(|_| fmt::Error)?;
                        }
                        Self::Opaque(v) => write!(&mut output, "{}", v).map_err(|_| fmt::Error)?,
                        Self::Bytes(bytes) => {
                            write!(&mut output, "bytes{:?}", bytes).map_err(|_| fmt::Error)?;
                        }
                        Self::Array(values) => {
                            if values.is_empty() {
                                output.push_str("[]");
                            } else {
                                stack.push(FormatItem::CloseArray);
                                first_in_container.push(true);
                                for value in values.iter().rev() {
                                    stack.push(FormatItem::Value(value));
                                    stack.push(FormatItem::ArraySeparator);
                                }
                                stack.push(FormatItem::OpenArray);
                            }
                        }
                        Self::Map(map) => {
                            if map.is_empty() {
                                output.push_str("map{}");
                            } else {
                                stack.push(FormatItem::CloseMap);
                                first_in_container.push(true);
                                for (k, v) in map.iter().rev() {
                                    stack.push(FormatItem::Value(v));
                                    stack.push(FormatItem::MapSeparator);
                                    stack.push(FormatItem::Value(k));
                                    stack.push(FormatItem::ArraySeparator);
                                }
                                stack.push(FormatItem::OpenMap);
                            }
                        }
                        Self::Typed(fields, _type) => {
                            stack.push(FormatItem::CloseTyped);
                            first_in_container.push(true);
                            for (idx, value) in fields.iter().enumerate().rev() {
                                stack.push(FormatItem::Value(value));
                                stack.push(FormatItem::MapSeparator);
                                stack.push(FormatItem::FieldIndex(idx));
                                stack.push(FormatItem::ArraySeparator);
                            }
                            stack.push(FormatItem::OpenTyped(_type));
                        }
                    }
                }
                FormatItem::OpenArray => output.push('['),
                FormatItem::CloseArray => {
                    output.push(']');
                    first_in_container.pop();
                }
                FormatItem::OpenMap => output.push_str("map{"),
                FormatItem::CloseMap => {
                    output.push('}');
                    first_in_container.pop();
                }
                FormatItem::OpenTyped(_type) => {
                    use std::fmt::Write;
                    write!(&mut output, "{:?} {{", _type).map_err(|_| fmt::Error)?;
                }
                FormatItem::CloseTyped => {
                    output.push('}');
                    first_in_container.pop();
                }
                FormatItem::MapSeparator => output.push_str(": "),
                FormatItem::FieldIndex(idx) => {
                    use std::fmt::Write;
                    write!(&mut output, "{}", idx).map_err(|_| fmt::Error)?;
                }
                FormatItem::ArraySeparator => {
                    if let Some(first) = first_in_container.last_mut() {
                        if *first {
                            *first = false;
                        } else {
                            output.push_str(", ");
                        }
                    }
                }
            }
        }

        write!(f, "{}", output)
    }
}

impl<T: Opaque> From<T> for Constant {
    fn from(value: T) -> Self {
        Self::Opaque(value.into())
    }
}

impl From<OpaqueWrapper> for Constant {
    fn from(value: OpaqueWrapper) -> Self {
        Self::Opaque(value)
    }
}

impl From<PrimitiveInteger> for Constant {
    fn from(value: PrimitiveInteger) -> Self { Primitive::Integer(value).into() }
}

#[cfg(test)]
mod tests {
    use std::hash::DefaultHasher;
    use super::*;

    #[test]
    fn test_huge_depth() {
        let mut map = Constant::Map(Default::default());
        for _ in 0..100000 {
            let mut m = IndexMap::new();
            m.insert(Constant::Primitive(Primitive::Integer(PrimitiveInteger::U8(0))), map);

            map = Constant::Map(m);
        }

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        map.hash(&mut hasher);

        // To drop it without stackoverflow
        let _wrapped = ConstantWrapper(map);
        drop(_wrapped);
    }

    #[test]
    fn test_std_hash_map_as_key() {
        let mut map = Constant::Map(Default::default());
        for _ in 0..5000 {
            let mut m = IndexMap::new();
            m.insert(map, Constant::Primitive(Primitive::Integer(PrimitiveInteger::U8(0))));
            map = Constant::Map(m);
        }

        let mut hasher = DefaultHasher::new();
        map.hash(&mut hasher);

        // To drop it without stackoverflow
        let _wrapped = ConstantWrapper(map);
        drop(_wrapped);
    }

    #[test]
    fn test_display_deep_nesting() {
        // Test that Display doesn't stack overflow on deeply nested constants
        let mut map = Constant::Map(Default::default());
        for _ in 0..1000 {
            let mut m = IndexMap::new();
            m.insert(Constant::Primitive(Primitive::Integer(PrimitiveInteger::U8(0))), map);
            map = Constant::Map(m);
        }

        let display = format!("{}", map);
        assert!(display.starts_with("map{"));
        assert!(display.ends_with("}"));

        // Test with deeply nested arrays
        let mut array = Constant::Array(vec![Constant::Primitive(Primitive::Integer(PrimitiveInteger::U64(42)))]);
        for _ in 0..1000 {
            array = Constant::Array(vec![array]);
        }

        let display = format!("{}", array);
        assert!(display.starts_with("["));
        assert!(display.ends_with("]"));
    }
}