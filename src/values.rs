use std::{
    borrow::Borrow,
    ops::{Deref, DerefMut}
};

use crate::{types::Type, IdentifierType, InterpreterError, NoHashMap};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Null,
    // number types
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),

    String(String),
    Boolean(bool),
    Struct(IdentifierType, NoHashMap<Value>),
    Array(Vec<Value>),
    Optional(Option<Box<Value>>)
}

#[derive(Debug)]
pub enum MutValue<'a> {
    Owned(Value),
    Borrowed(&'a mut Value)
}

impl<'a> Into<MutValue<'a>> for Value {
    fn into(self) -> MutValue<'a> {
        MutValue::Owned(self)
    }
}

impl<'a> From<&'a mut Value> for MutValue<'a> {
    fn from(value: &'a mut Value) -> Self {
        MutValue::Borrowed(value)
    }
}

impl<'a> AsMut<Value> for MutValue<'a> {
    fn as_mut(&mut self) -> &mut Value {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v,
        }
    }
}

impl<'a> MutValue<'a> {
    pub fn into_owned(self) -> Value {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v.clone(),
        }
    }

    pub fn to_owned(&self) -> Value {
        match self {
            Self::Owned(v) => v.clone(),
            Self::Borrowed(v) => (**v).clone(),
        }
    }

    pub fn as_value(&'a self) -> &'a Value {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v,
        }
    }

    pub fn as_mut_value(&'a mut self) -> &'a mut Value {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v,
        }
    }
}

impl Borrow<Value> for MutValue<'_> {
    fn borrow(&self) -> &Value {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v,
        }
    }
}

impl Deref for MutValue<'_> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v,
        }
    }
}

impl DerefMut for MutValue<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v,
        }
    }
}

impl Value {
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
    pub fn as_u8(&self) -> Result<&u8, InterpreterError> {
        match self {
            Value::U8(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::U8))
        }
    }

    #[inline]
    pub fn as_u16(&self) -> Result<&u16, InterpreterError> {
        match self {
            Value::U16(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::U16))
        }
    }

    #[inline]
    pub fn as_u32(&self) -> Result<&u32, InterpreterError> {
        match self {
            Value::U32(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::U32))
        }
    }

    #[inline]
    pub fn as_u64(&self) -> Result<&u64, InterpreterError> {
        match self {
            Value::U64(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::U64))
        }
    }

    #[inline]
    pub fn as_u128(&self) -> Result<&u128, InterpreterError> {
        match self {
            Value::U128(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::U128))
        }
    }

    #[inline]
    pub fn as_string(&self) -> Result<&String, InterpreterError> {
        match self {
            Value::String(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::String))
        }
    }

    #[inline]
    pub fn as_bool(&self) -> Result<&bool, InterpreterError> {
        match self {
            Value::Boolean(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Bool))
        }
    }

    #[inline]
    pub fn as_map(&self) -> Result<&NoHashMap<Value>, InterpreterError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(InterpreterError::InvalidStructValue(v.clone()))
        }
    }

    #[inline]
    pub fn as_mut_map(&mut self) -> Result<&mut NoHashMap<Value>, InterpreterError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(InterpreterError::InvalidStructValue(v.clone()))
        }
    }

    #[inline]
    pub fn as_vec<'a>(&'a self) -> Result<&'a Vec<Value>, InterpreterError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn as_mut_vec<'a>(&'a mut self) -> Result<&'a mut Vec<Value>, InterpreterError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn as_optional(&self, expected: &Type) -> Result<&Option<Box<Value>>, InterpreterError> {
        match self {
            Value::Null => Ok(&None),
            Value::Optional(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Optional(Box::new(expected.clone()))))
        }
    }

    #[inline]
    pub fn take_from_optional(&mut self, expected: &Type) -> Result<Value, InterpreterError> {
        match self {
            Value::Optional(opt) => Ok(*opt.take().ok_or(InterpreterError::OptionalIsNull)?),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Optional(Box::new(expected.clone()))))
        }
    }

    #[inline]
    pub fn take_optional(&mut self) -> Result<Option<Box<Value>>, InterpreterError> {
        match self {
            Value::Optional(opt) => Ok(opt.take()),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Optional(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn to_u8(self) -> Result<u8, InterpreterError> {
        match self {
            Value::U8(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::U8))
        }
    }

    #[inline]
    pub fn to_u16(self) -> Result<u16, InterpreterError> {
        match self {
            Value::U16(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::U16))
        }
    }

    #[inline]
    pub fn to_u32(self) -> Result<u32, InterpreterError> {
        match self {
            Value::U32(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::U32))
        }
    }

    #[inline]
    pub fn to_u64(self) -> Result<u64, InterpreterError> {
        match self {
            Value::U64(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::U64))
        }
    }

    #[inline]
    pub fn to_u128(self) -> Result<u128, InterpreterError> {
        match self {
            Value::U128(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::U128))
        }
    }

    #[inline]
    pub fn to_string(self) -> Result<String, InterpreterError> {
        match self {
            Value::String(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::String))
        }
    }

    #[inline]
    pub fn to_bool(self) -> Result<bool, InterpreterError> {
        match self {
            Value::Boolean(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Bool))
        }
    }

    #[inline]
    pub fn to_map(self) -> Result<NoHashMap<Value>, InterpreterError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(InterpreterError::InvalidStructValue(v.clone()))
        }
    }

    #[inline]
    pub fn to_vec(self) -> Result<Vec<Value>, InterpreterError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    // Check if the value is a number
    #[inline]
    pub fn is_number(&self) -> bool {
        match self {
            Value::U8(_) | Value::U16(_) | Value::U32(_) | Value::U64(_) | Value::U128(_) => true,
            _ => false
        }
    }

    // Cast value to string
    #[inline]
    pub fn cast_to_string(self) -> Result<String, InterpreterError> {
        match self {
            Value::U8(n) => Ok(n.to_string()),
            Value::U16(n) => Ok(n.to_string()),
            Value::U32(n) => Ok(n.to_string()),
            Value::U64(n) => Ok(n.to_string()),
            Value::U128(n) => Ok(n.to_string()),
            Value::String(s) => Ok(s),
            Value::Boolean(b) => Ok(b.to_string()),
            _ => Err(InterpreterError::InvalidCastType(Type::String))
        }
    }

    // Cast value to u8
    #[inline]
    pub fn cast_to_u8(self) -> Result<u8, InterpreterError> {
        match self {
            Value::U8(n) => Ok(n),
            Value::U16(n) => Ok(n as u8),
            Value::U32(n) => Ok(n as u8),
            Value::U64(n) => Ok(n as u8),
            Value::U128(n) => Ok(n as u8),
            Value::Boolean(b) => Ok(b as u8),
            _ => Err(InterpreterError::InvalidCastType(Type::U8))
        }
    }

    // Cast value to u16
    #[inline]
    pub fn cast_to_u16(self) -> Result<u16, InterpreterError> {
        match self {
            Value::U8(n) => Ok(n as u16),
            Value::U16(n) => Ok(n),
            Value::U32(n) => Ok(n as u16),
            Value::U64(n) => Ok(n as u16),
            Value::U128(n) => Ok(n as u16),
            Value::Boolean(b) => Ok(b as u16),
            _ => Err(InterpreterError::InvalidCastType(Type::U16))
        }
    }

    // Cast value to u32
    #[inline]
    pub fn cast_to_u32(self) -> Result<u32, InterpreterError> {
        match self {
            Value::U8(n) => Ok(n as u32),
            Value::U16(n) => Ok(n as u32),
            Value::U32(n) => Ok(n),
            Value::U64(n) => Ok(n as u32),
            Value::U128(n) => Ok(n as u32),
            Value::Boolean(b) => Ok(b as u32),
            _ => Err(InterpreterError::InvalidCastType(Type::U16))
        }
    }

    // Cast value to u64
    #[inline]
    pub fn cast_to_u64(self) -> Result<u64, InterpreterError> {
        match self {
            Value::U8(n) => Ok(n as u64),
            Value::U16(n) => Ok(n as u64),
            Value::U32(n) => Ok(n as u64),
            Value::U64(n) => Ok(n),
            Value::U128(n) => Ok(n as u64),
            Value::Boolean(b) => Ok(b as u64),
            _ => Err(InterpreterError::InvalidCastType(Type::U64))
        }
    }

    // Cast value to u128
    #[inline]
    pub fn cast_to_u128(self) -> Result<u128, InterpreterError> {
        match self {
            Value::U8(n) => Ok(n as u128),
            Value::U16(n) => Ok(n as u128),
            Value::U32(n) => Ok(n as u128),
            Value::U64(n) => Ok(n as u128),
            Value::U128(n) => Ok(n),
            Value::Boolean(b) => Ok(b as u128),
            _ => Err(InterpreterError::InvalidCastType(Type::U128))
        }
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
            Value::String(s) => write!(f, "{}", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Struct(name, fields) => {
                let s: Vec<String> = fields.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                write!(f, "{} {} {} {}", name, "{", s.join(", "), "}")
            },
            Value::Array(values) => {
                let s: Vec<String> = values.iter().map(|v| format!("{}", v)).collect();
                write!(f, "[{}]", s.join(", "))
            },
            Value::Optional(value) => match value.as_ref() {
                Some(value) => write!(f, "Optional({})", value),
                None => write!(f, "Optional(null)")
            },
        }
    }
}
