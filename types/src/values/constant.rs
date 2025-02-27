use std::{fmt, hash::{Hash, Hasher}};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

use crate::{DefinedType, Type, U256};
use super::{Value, ValueCell, ValueError};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "type", content = "value")]
pub enum Constant {
    Default(Value),
    Array(Vec<Constant>),

    // Use box directly because the range are primitive only
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
        if matches!(self.0, Constant::Default(_)) {
            return
        }

        let mut stack = vec![std::mem::take(&mut self.0)];
        while let Some(value) = stack.pop() {
            match value {
                Constant::Default(_) => {},
                Constant::Array(values) => stack.extend(values),
                Constant::Map(map) => stack.extend(map.into_iter().flat_map(|(k, v)| [k, v])),
                Constant::Typed(fields, _) => stack.extend(fields),
            }
        }
    }
}

impl Hash for Constant {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut stack = vec![self];
        while let Some(value) = stack.pop() {
            match value {
                Self::Default(v) => v.hash(state),
                Self::Array(values) => {
                    12u8.hash(state);
                    values.iter().for_each(|f| stack.push(f));
                },
                Self::Map(map) => {
                    13u8.hash(state);
                    for (key, value) in map {
                        stack.push(&key);
                        stack.push(&value);
                    }
                },
                Self::Typed(fields, ty) => {
                    14u8.hash(state);
                    fields.iter().for_each(|f| stack.push(f));
                    ty.hash(state);
                }
            }
        }
    }
}

impl Default for Constant {
    fn default() -> Self {
        Self::Default(Default::default())
    }
}

impl From<Value> for Constant {
    fn from(value: Value) -> Self {
        Self::Default(value)
    }
}

impl TryFrom<ValueCell> for Constant {
    type Error = ValueError;

    fn try_from(cell: ValueCell) -> Result<Self, Self::Error> {
        Ok(match cell {
            ValueCell::Default(v) => Self::Default(v),
            ValueCell::Array(values) => Self::Array(values.into_iter().map(|v| v.into_owned()?.try_into()).collect::<Result<Vec<_>, _>>()?),
            ValueCell::Map(map) => {
                let m = map.into_iter()
                    .map(|(k, v)| Ok((k.into_owned()?.try_into()?, v.into_owned()?.try_into()?)))
                    .collect::<Result<IndexMap<_, _>, _>>()?;
                Self::Map(m)
            },
            ValueCell::Typed(values, ty) => {
                let values = values.into_iter()
                    .map(|v| v.into_owned()?.try_into())
                    .collect::<Result<Vec<_>, _>>()?;
                Self::Typed(values, ty)
            }
        })
    }
}

impl Constant {
    #[inline]
    pub fn is_null(&self) -> bool {
        match &self {
            Self::Default(Value::Null) => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        match &self {
            Self::Default(Value::String(_)) => true,
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
            Self::Default(Value::U8(n)) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U8))
        }
    }

    #[inline]
    pub fn as_u16(&self) -> Result<u16, ValueError> {
        match self {
            Self::Default(Value::U16(n)) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U16))
        }
    }

    #[inline]
    pub fn as_u32(&self) -> Result<u32, ValueError> {
        match self {
            Self::Default(Value::U32(n)) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U32))
        }
    }

    #[inline]
    pub fn as_u64(&self) -> Result<u64, ValueError> {
        match self {
            Self::Default(Value::U64(n)) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U64))
        }
    }

    #[inline]
    pub fn as_u128(&self) -> Result<u128, ValueError> {
        match self {
            Self::Default(Value::U128(n)) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U128))
        }
    }

    #[inline]
    pub fn as_u256(&self) -> Result<U256, ValueError> {
        match self {
            Self::Default(Value::U256(n)) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U256))
        }
    }

    #[inline]
    pub fn as_string(&self) -> Result<&String, ValueError> {
        match self {
            Self::Default(Value::String(n)) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::String))
        }
    }

    #[inline]
    pub fn as_bool(&self) -> Result<bool, ValueError> {
        match self {
            Self::Default(Value::Boolean(n)) => Ok(*n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::Bool))
        }
    }

    #[inline]
    pub fn as_map(&self) -> Result<&IndexMap<Self, Self>, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedStruct)
        }
    }

    #[inline]
    pub fn as_mut_map(&mut self) -> Result<&mut IndexMap<Self, Self>, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedStruct)
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
            Self::Default(Value::Null) => Ok(None),
            v => Ok(Some(v))
        }
    }

    #[inline]
    pub fn is_serializable(&self) -> bool {
        match self {
            Self::Default(Value::Opaque(op)) => op.is_serializable(),
            _ => true
        }
    }

    #[inline]
    pub fn to_u8(self) -> Result<u8, ValueError> {
        match self {
            Self::Default(Value::U8(n)) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U8))
        }
    }

    #[inline]
    pub fn to_u16(self) -> Result<u16, ValueError> {
        match self {
            Self::Default(Value::U16(n)) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U16))
        }
    }

    #[inline]
    pub fn to_u32(self) -> Result<u32, ValueError> {
        match self {
            Self::Default(Value::U32(n)) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U32))
        }
    }

    #[inline]
    pub fn to_u64(self) -> Result<u64, ValueError> {
        match self {
            Self::Default(Value::U64(n)) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U64))
        }
    }

    #[inline]
    pub fn to_u128(self) -> Result<u128, ValueError> {
        match self {
            Self::Default(Value::U128(n)) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U128))
        }
    }

    #[inline]
    pub fn to_u256(self) -> Result<U256, ValueError> {
        match self {
            Self::Default(Value::U256(n)) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::U256))
        }
    }

    #[inline]
    pub fn to_string(self) -> Result<String, ValueError> {
        match self {
            Self::Default(Value::String(n)) => Ok(n),
            v => Err(ValueError::InvalidValueType(v.clone(), Type::String))
        }
    }

    #[inline]
    pub fn to_bool(self) -> Result<bool, ValueError> {
        match self {
            Self::Default(Value::Boolean(n)) => Ok(n),
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
    pub fn as_range(&self) -> Result<(&Value, &Value, &Type), ValueError> {
        self.as_value().and_then(Value::as_range)
    }

    #[inline]
    pub fn to_range(self) -> Result<(Value, Value, Type), ValueError> {
        self.into_value().and_then(Value::to_range)
    }

    // Check if the value is a number
    #[inline]
    pub fn is_number(&self) -> bool {
        match self {
            Self::Default(v) => v.is_number(),
            _ => false
        }
    }

    // Increment the value
    pub fn increment(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Self::Default(v) => v.increment()?,
            _ => return Err(ValueError::OperationNotNumberType)
        })
    }

    // Decrement the value
    pub fn decrement(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Self::Default(v) => v.decrement()?,
            _ => return Err(ValueError::OperationNotNumberType)
        })
    }

    // Cast value to string
    #[inline]
    pub fn cast_to_string(self) -> Result<String, ValueError> {
        match self {
            Self::Default(v) => v.cast_to_string(),
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
            Type::U8 => self.checked_cast_to_u8().map(Value::U8),
            Type::U16 => self.checked_cast_to_u16().map(Value::U16),
            Type::U32 => self.checked_cast_to_u32().map(Value::U32),
            Type::U64 => self.checked_cast_to_u64().map(Value::U64),
            Type::U128 => self.checked_cast_to_u128().map(Value::U128),
            Type::U256 => self.checked_cast_to_u256().map(Value::U256),
            Type::String => self.cast_to_string().map(Value::String),
            Type::Bool => self.cast_to_bool().map(Value::Boolean),
            Type::Optional(inner) => {
                if self.is_null() {
                    return Ok(self)
                } else {
                    return self.checked_cast_to_primitive_type(inner)
                }
            },
            Type::Range(inner) => {
                let (start, end, _) = self.to_range()?;
                let start = start.checked_cast_to_primitive_type(inner)?;
                let end = end.checked_cast_to_primitive_type(inner)?;
                Ok(Value::Range(Box::new(start), Box::new(end), *inner.clone()))
            },
            _ => Err(ValueError::InvalidCastType(expected.clone()))
        }.map(Self::Default)
    }

    // Cast to u8, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u8(self) -> Result<u8, ValueError> {
        self.into_value().and_then(Value::checked_cast_to_u8)
    }

    // Cast to u16, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u16(self) -> Result<u16, ValueError> {
        self.into_value().and_then(Value::checked_cast_to_u16)
    }

    // Cast to u32, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u32(self) -> Result<u32, ValueError> {
        self.into_value().and_then(Value::checked_cast_to_u32)
    }

    // Cast to u64, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u64(self) -> Result<u64, ValueError> {
        self.into_value().and_then(Value::checked_cast_to_u64)
    }

    // Cast to u128, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u128(self) -> Result<u128, ValueError> {
        self.into_value().and_then(Value::checked_cast_to_u128)
    }

    // Cast to u256, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u256(self) -> Result<U256, ValueError> {
        self.into_value().and_then(Value::checked_cast_to_u256)
    }

    // Cast value to bool
    #[inline]
    pub fn cast_to_bool(self) -> Result<bool, ValueError> {
        self.into_value().and_then(Value::cast_to_bool)
    }

    // Cast value to u8
    #[inline]
    pub fn cast_to_u8(self) -> Result<u8, ValueError> {
        self.into_value().and_then(Value::cast_to_u8)
    }

    // Cast value to u16
    #[inline]
    pub fn cast_to_u16(self) -> Result<u16, ValueError> {
        self.into_value().and_then(Value::cast_to_u16)
    }

    // Cast value to u32
    #[inline]
    pub fn cast_to_u32(self) -> Result<u32, ValueError> {
        self.into_value().and_then(Value::cast_to_u32)
    }

    // Cast value to u64
    #[inline]
    pub fn cast_to_u64(self) -> Result<u64, ValueError> {
        self.into_value().and_then(Value::cast_to_u64)
    }

    // Cast value to u128
    #[inline]
    pub fn cast_to_u128(self) -> Result<u128, ValueError> {
        self.into_value().and_then(Value::cast_to_u128)
    }

    // Cast value to u256
    #[inline]
    pub fn cast_to_u256(self) -> Result<U256, ValueError> {
        self.into_value().and_then(Value::cast_to_u256)
    }

    #[inline(always)]
    pub fn as_value(&self) -> Result<&Value, ValueError> {
        match self {
            Self::Default(v) => Ok(v),
            _ => Err(ValueError::InvalidValueType(self.clone(), Type::Any))
        }
    }

    #[inline(always)]
    pub fn into_value(self) -> Result<Value, ValueError> {
        match self {
            Self::Default(v) => Ok(v),
            _ => Err(ValueError::InvalidValueType(self, Type::Any))
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Default(v) => write!(f, "{}", v),
            Self::Array(values) => {
                let s: Vec<String> = values.iter().map(|v| format!("{}", v)).collect();
                write!(f, "[{}]", s.join(", "))
            },
            Self::Map(map) => {
                let s: Vec<String> = map.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                write!(f, "map{}{}{}", "{", s.join(", "), "}")
            },
            Self::Typed(fields, _type) => {
                let s: Vec<String> = fields.iter().enumerate().map(|(k, v)| format!("{}: {}", k, v)).collect();
                write!(f, "{:?} {} {} {}", _type, "{", s.join(", "), "}")
            }
        }
    }
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
            m.insert(Constant::Default(Value::U8(0)), map);

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
            m.insert(map, Constant::Default(Value::U8(0)));
            map = Constant::Map(m);
        }

        let mut hasher = DefaultHasher::new();
        map.hash(&mut hasher);

        // To drop it without stackoverflow
        let _wrapped = ConstantWrapper(map);
        drop(_wrapped);
    }
}