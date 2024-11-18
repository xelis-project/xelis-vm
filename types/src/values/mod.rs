mod pointer;
mod error;

use std::{
    cmp::Ordering,
    collections::{HashMap, HashSet},
    hash::{Hash, Hasher}, ptr,
};
use super::{
    Type,
    EnumValueType,
    StructType,
    U256
};

pub use pointer::*;
pub use error::*;

macro_rules! checked_cast {
    ($self: expr, $type: expr) => {
        match $self {
            Value::U8(n) => n.try_into().map_err(|_| ValueError::CastError),
            Value::U16(n) => n.try_into().map_err(|_| ValueError::CastError),
            Value::U32(n) => n.try_into().map_err(|_| ValueError::CastError),
            Value::U64(n) => n.try_into().map_err(|_| ValueError::CastError),
            Value::U128(n) => n.try_into().map_err(|_| ValueError::CastError),
            Value::U256(n) => n.try_into().map_err(|_| ValueError::CastError),
            Value::Boolean(n) => n.try_into().map_err(|_| ValueError::CastError),
            _ => Err(ValueError::InvalidCastType($type))
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Null,
    // number types
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    U256(U256),

    String(String),
    Boolean(bool),
    Struct(Vec<ValuePointer>, StructType),
    Array(Vec<ValuePointer>),
    Optional(Option<ValuePointer>),
    // Use box directly because the range are primitive only
    Range(Box<Value>, Box<Value>, Type),
    // Map cannot be used as a key in another map
    Map(HashMap<Value, ValuePointer>),
    Enum(Vec<ValuePointer>, EnumValueType),
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::U8(a), Value::U8(b)) => a.partial_cmp(b),
            (Value::U16(a), Value::U16(b)) => a.partial_cmp(b),
            (Value::U32(a), Value::U32(b)) => a.partial_cmp(b),
            (Value::U64(a), Value::U64(b)) => a.partial_cmp(b),
            (Value::U128(a), Value::U128(b)) => a.partial_cmp(b),
            (Value::U256(a), Value::U256(b)) => a.partial_cmp(b),
            _ => None
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash_with_tracked_pointers(state, &mut HashSet::new());
    }
}

impl Default for Value {
    fn default() -> Self {
        Value::Null
    }
}

impl Value {
    fn hash_with_tracked_pointers<H: Hasher>(&self, state: &mut H, tracked_pointers: &mut HashSet<*const Value>) {
        if !tracked_pointers.insert(ptr::from_ref(self)) {
            // Cyclic reference detected
            return;
        }

        match self {
            Value::Null => 0.hash(state),
            Value::U8(n) => {
                1.hash(state);
                n.hash(state);
            },
            Value::U16(n) => {
                2.hash(state);
                n.hash(state);
            },
            Value::U32(n) => {
                3.hash(state);
                n.hash(state);
            },
            Value::U64(n) => {
                4.hash(state);
                n.hash(state);
            },
            Value::U128(n) => {
                5.hash(state);
                n.hash(state);
            },
            Value::U256(n) => {
                6.hash(state);
                n.hash(state);
            },
            Value::String(n) => {
                7.hash(state);
                n.hash(state);
            },
            Value::Boolean(n) => {
                8.hash(state);
                n.hash(state);
            },
            Value::Struct(fields, struct_type) => {
                9.hash(state);
                fields.iter().for_each(|f| f.handle().hash_with_tracked_pointers(state, tracked_pointers));
                struct_type.hash(state);
            },
            Value::Array(values) => {
                10.hash(state);
                values.iter().for_each(|f| f.handle().hash_with_tracked_pointers(state, tracked_pointers));
            },
            Value::Optional(opt) => {
                11.hash(state);
                if let Some(value) = opt {
                    value.handle().hash_with_tracked_pointers(state, tracked_pointers);
                }
            },
            Value::Range(start, end, range_type) => {
                12.hash(state);
                start.hash_with_tracked_pointers(state, tracked_pointers);
                end.hash_with_tracked_pointers(state, tracked_pointers);
                range_type.hash(state);
            },
            Value::Map(map) => {
                13.hash(state);
                for (key, value) in map {
                    key.hash_with_tracked_pointers(state, tracked_pointers);
                    value.handle().hash_with_tracked_pointers(state, tracked_pointers);
                }
            },
            Value::Enum(fields, enum_type) => {
                14.hash(state);
                fields.iter().for_each(|f| f.handle().hash_with_tracked_pointers(state, tracked_pointers));
                enum_type.hash(state);
            }
        }
    }

    #[inline]
    pub fn is_null(&self) -> bool {
        match &self {
            Value::Null => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        match &self {
            Value::String(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_map(&self) -> bool {
        match &self {
            Value::Map(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn as_u8(&self) -> Result<u8, ValueError> {
        match self {
            Value::U8(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U8))
        }
    }

    #[inline]
    pub fn as_u16(&self) -> Result<u16, ValueError> {
        match self {
            Value::U16(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U16))
        }
    }

    #[inline]
    pub fn as_u32(&self) -> Result<u32, ValueError> {
        match self {
            Value::U32(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U32))
        }
    }

    #[inline]
    pub fn as_u64(&self) -> Result<u64, ValueError> {
        match self {
            Value::U64(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U64))
        }
    }

    #[inline]
    pub fn as_u128(&self) -> Result<u128, ValueError> {
        match self {
            Value::U128(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U128))
        }
    }

    #[inline]
    pub fn as_u256(&self) -> Result<U256, ValueError> {
        match self {
            Value::U256(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U256))
        }
    }

    #[inline]
    pub fn as_string(&self) -> Result<&String, ValueError> {
        match self {
            Value::String(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::String))
        }
    }

    #[inline]
    pub fn as_bool(&self) -> Result<bool, ValueError> {
        match self {
            Value::Boolean(n) => Ok(*n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Bool))
        }
    }

    #[inline]
    pub fn as_map(&self) -> Result<&HashMap<Value, ValuePointer>, ValueError> {
        match self {
            Value::Map(map) => Ok(map),
            v => Err(ValueError::InvalidStructValue(v.clone()))
        }
    }

    #[inline]
    pub fn as_mut_map(&mut self) -> Result<&mut HashMap<Value, ValuePointer>, ValueError> {
        match self {
            Value::Map(map) => Ok(map),
            v => Err(ValueError::InvalidStructValue(v.clone()))
        }
    }

    #[inline]
    pub fn as_vec<'a>(&'a self) -> Result<&'a Vec<ValuePointer>, ValueError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn as_mut_vec<'a>(&'a mut self) -> Result<&'a mut Vec<ValuePointer>, ValueError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn as_optional(&self, expected: &Type) -> Result<&Option<ValuePointer>, ValueError> {
        match self {
            Value::Null => Ok(&None),
            Value::Optional(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Optional(Box::new(expected.clone()))))
        }
    }

    #[inline]
    pub fn take_from_optional(&mut self, expected: &Type) -> Result<ValuePointer, ValueError> {
        match self {
            Value::Optional(opt) => opt.take().ok_or(ValueError::OptionalIsNull),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Optional(Box::new(expected.clone()))))
        }
    }

    #[inline]
    pub fn take_optional(&mut self) -> Result<Option<ValuePointer>, ValueError> {
        match self {
            Value::Optional(opt) => Ok(opt.take()),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Optional(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn to_u8(self) -> Result<u8, ValueError> {
        match self {
            Value::U8(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U8))
        }
    }

    #[inline]
    pub fn to_u16(self) -> Result<u16, ValueError> {
        match self {
            Value::U16(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U16))
        }
    }

    #[inline]
    pub fn to_u32(self) -> Result<u32, ValueError> {
        match self {
            Value::U32(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U32))
        }
    }

    #[inline]
    pub fn to_u64(self) -> Result<u64, ValueError> {
        match self {
            Value::U64(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U64))
        }
    }

    #[inline]
    pub fn to_u128(self) -> Result<u128, ValueError> {
        match self {
            Value::U128(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U128))
        }
    }

    #[inline]
    pub fn to_u256(self) -> Result<U256, ValueError> {
        match self {
            Value::U256(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::U256))
        }
    }

    #[inline]
    pub fn to_string(self) -> Result<String, ValueError> {
        match self {
            Value::String(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::String))
        }
    }

    #[inline]
    pub fn to_bool(self) -> Result<bool, ValueError> {
        match self {
            Value::Boolean(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Bool))
        }
    }

    #[inline]
    pub fn to_map(self) -> Result<Vec<ValuePointer>, ValueError> {
        match self {
            Value::Struct(fields, _) => Ok(fields),
            v => Err(ValueError::InvalidStructValue(v.clone()))
        }
    }

    #[inline]
    pub fn to_vec(self) -> Result<Vec<ValuePointer>, ValueError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }
    #[inline]

    pub fn to_sub_vec(self) -> Result<Vec<ValuePointer>, ValueError> {
        match self {
            Value::Array(values) => Ok(values),
            Value::Struct(fields, _) => Ok(fields),
            _ => Err(ValueError::SubValue)
        }
    }

    #[inline]
    pub fn as_sub_vec(&self) -> Result<&Vec<ValuePointer>, ValueError> {
        match self {
            Value::Array(values) => Ok(values),
            Value::Struct(fields, _) => Ok(fields),
            _ => Err(ValueError::SubValue)
        }
    }

    #[inline]
    pub fn as_mut_sub_vec(&mut self) -> Result<&mut Vec<ValuePointer>, ValueError> {
        match self {
            Value::Array(values) => Ok(values),
            Value::Struct(fields, _) => Ok(fields),
            _ => Err(ValueError::SubValue)
        }
    }

    #[inline]
    pub fn as_range(&self) -> Result<(&Value, &Value, &Type), ValueError> {
        match self {
            Value::Range(start, end, _type) => Ok((start, end, _type)),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Range(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn to_range(self) -> Result<(Value, Value, Type), ValueError> {
        match self {
            Value::Range(start, end, _type) => Ok((*start, *end, _type)),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Range(Box::new(Type::Any))))
        }
    }

    // Check if the value is a number
    #[inline]
    pub fn is_number(&self) -> bool {
        match self {
            Value::U8(_) | Value::U16(_) | Value::U32(_) | Value::U64(_) | Value::U128(_) | Value::U256(_) => true,
            _ => false
        }
    }

    // Increment the value
    pub fn increment(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Value::U8(n) => *n += 1,
            Value::U16(n) => *n += 1,
            Value::U32(n) => *n += 1,
            Value::U64(n) => *n += 1,
            Value::U128(n) => *n += 1,
            Value::U256(n) => *n += U256::ONE,
            _ => return Err(ValueError::OperationNotNumberType)
        })
    }

    // Decrement the value
    pub fn decrement(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Value::U8(n) => *n -= 1,
            Value::U16(n) => *n -= 1,
            Value::U32(n) => *n -= 1,
            Value::U64(n) => *n -= 1,
            Value::U128(n) => *n -= 1,
            Value::U256(n) => *n -= U256::ONE,
            _ => return Err(ValueError::OperationNotNumberType)
        })
    }

    // Cast value to string
    #[inline]
    pub fn cast_to_string(self) -> Result<String, ValueError> {
        match self {
            Value::U8(n) => Ok(n.to_string()),
            Value::U16(n) => Ok(n.to_string()),
            Value::U32(n) => Ok(n.to_string()),
            Value::U64(n) => Ok(n.to_string()),
            Value::U128(n) => Ok(n.to_string()),
            Value::U256(n) => Ok(n.to_string()),
            Value::String(s) => Ok(s),
            Value::Boolean(b) => Ok(b.to_string()),
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
    pub fn checked_cast_to_primitive_type(self, expected: &Type) -> Result<Value, ValueError> {
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
                if let Value::Null = self {
                    Ok(Value::Optional(None))
                } else {
                    self.checked_cast_to_primitive_type(inner)
                        .map(|v| Value::Optional(Some(ValuePointer::Owned(Box::new(v)))))
                }
            }
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
            Value::U8(n) => Ok(n != 0),
            Value::U16(n) => Ok(n != 0),
            Value::U32(n) => Ok(n != 0),
            Value::U64(n) => Ok(n != 0),
            Value::U128(n) => Ok(n != 0),
            Value::U256(n) => Ok(!n.is_zero()),
            Value::Boolean(b) => Ok(b),
            _ => Err(ValueError::InvalidCastType(Type::Bool))
        }
    }

    // Cast value to u8
    #[inline]
    pub fn cast_to_u8(self) -> Result<u8, ValueError> {
        match self {
            Value::U8(n) => Ok(n),
            Value::U16(n) => Ok(n as u8),
            Value::U32(n) => Ok(n as u8),
            Value::U64(n) => Ok(n as u8),
            Value::U128(n) => Ok(n as u8),
            Value::U256(n) => Ok(n.low_u64() as u8),
            Value::Boolean(b) => Ok(b as u8),
            _ => Err(ValueError::InvalidCastType(Type::U8))
        }
    }

    // Cast value to u16
    #[inline]
    pub fn cast_to_u16(self) -> Result<u16, ValueError> {
        match self {
            Value::U8(n) => Ok(n as u16),
            Value::U16(n) => Ok(n),
            Value::U32(n) => Ok(n as u16),
            Value::U64(n) => Ok(n as u16),
            Value::U128(n) => Ok(n as u16),
            Value::U256(n) => Ok(n.low_u64() as u16),
            Value::Boolean(b) => Ok(b as u16),
            _ => Err(ValueError::InvalidCastType(Type::U16))
        }
    }

    // Cast value to u32
    #[inline]
    pub fn cast_to_u32(self) -> Result<u32, ValueError> {
        match self {
            Value::U8(n) => Ok(n as u32),
            Value::U16(n) => Ok(n as u32),
            Value::U32(n) => Ok(n),
            Value::U64(n) => Ok(n as u32),
            Value::U128(n) => Ok(n as u32),
            Value::U256(n) => Ok(n.low_u64() as u32),
            Value::Boolean(b) => Ok(b as u32),
            _ => Err(ValueError::InvalidCastType(Type::U16))
        }
    }

    // Cast value to u64
    #[inline]
    pub fn cast_to_u64(self) -> Result<u64, ValueError> {
        match self {
            Value::U8(n) => Ok(n as u64),
            Value::U16(n) => Ok(n as u64),
            Value::U32(n) => Ok(n as u64),
            Value::U64(n) => Ok(n),
            Value::U128(n) => Ok(n as u64),
            Value::U256(n) => Ok(n.low_u64()),
            Value::Boolean(b) => Ok(b as u64),
            _ => Err(ValueError::InvalidCastType(Type::U64))
        }
    }

    // Cast value to u128
    #[inline]
    pub fn cast_to_u128(self) -> Result<u128, ValueError> {
        match self {
            Value::U8(n) => Ok(n as u128),
            Value::U16(n) => Ok(n as u128),
            Value::U32(n) => Ok(n as u128),
            Value::U64(n) => Ok(n as u128),
            Value::U128(n) => Ok(n),
            Value::U256(n) => Ok(n.low_u128()),
            Value::Boolean(b) => Ok(b as u128),
            _ => Err(ValueError::InvalidCastType(Type::U128))
        }
    }

    // Cast value to u256
    #[inline]
    pub fn cast_to_u256(self) -> Result<U256, ValueError> {
        match self {
            Value::U8(n) => Ok(U256::from(n)),
            Value::U16(n) => Ok(U256::from(n)),
            Value::U32(n) => Ok(U256::from(n)),
            Value::U64(n) => Ok(U256::from(n)),
            Value::U128(n) => Ok(U256::from(n)),
            Value::U256(n) => Ok(n),
            Value::Boolean(b) => Ok(U256::from(b as u8)),
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

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::U8(v) => write!(f, "{}", v),
            Value::U16(v) => write!(f, "{}", v),
            Value::U32(v) => write!(f, "{}", v),
            Value::U64(v) => write!(f, "{}", v),
            Value::U128(v) => write!(f, "{}", v),
            Value::U256(v) => write!(f, "{}", v),
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Struct(fields, _type) => {
                let s: Vec<String> = fields.iter().enumerate().map(|(k, v)| format!("{}: {}", k, v.handle())).collect();
                write!(f, "{:?} {} {} {}", _type, "{", s.join(", "), "}")
            },
            Value::Array(values) => {
                let s: Vec<String> = values.iter().map(|v| format!("{}", v.handle())).collect();
                write!(f, "[{}]", s.join(", "))
            },
            Value::Optional(value) => match value.as_ref() {
                Some(value) => write!(f, "optional<{}>", match value {
                    ValuePointer::Owned(v) => v.to_string(),
                    ValuePointer::Shared(v) => v.borrow().to_string()
                }),
                None => write!(f, "optional<null>")
            },
            Value::Range(start, end, _type) => write!(f, "range<{}: {}..{}>", _type, start, end),
            Value::Map(map) => {
                let s: Vec<String> = map.iter().map(|(k, v)| format!("{}: {}", k, v.handle())).collect();
                write!(f, "map{}{}{}", "{", s.join(", "), "}")
            },
            Value::Enum(fields, enum_type) => {
                let s: Vec<String> = fields.iter().enumerate().map(|(k, v)| format!("{}: {}", k, v.handle())).collect();
                write!(f, "enum{:?} {} {} {}", enum_type, "{", s.join(", "), "}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stackoverflow_std_hash() {
        // Create a map that contains itself
        let map = InnerValue::new(Value::Map(HashMap::new()));
        let cloned = {
            let mut m = map.borrow_mut();
            m.as_mut_map()
            .unwrap()
            .insert(Value::U8(10), ValuePointer::Shared(map.clone()));
            m.clone()
        };

        let mut inner_map: HashMap<Value, ValuePointer> = HashMap::new();
        inner_map.insert(cloned, ValuePointer::Owned(Box::new(Value::U8(10))));
    }
}