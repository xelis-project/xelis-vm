use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    ops::{Deref, DerefMut},
    rc::Rc
};

use crate::{types::Type, IdentifierType, InterpreterError};

pub type SharableValue = Rc<RefCell<Value>>;

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
    Struct(IdentifierType, HashMap<IdentifierType, ValueVariant>),
    Array(Vec<ValueVariant>),
    Optional(Option<Box<Value>>)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValueVariant {
    Value(Value),
    Reference(SharableValue)
}

pub enum ValueHandle<'a> {
    Value(&'a Value),
    Reference(Ref<'a, Value>)
}

impl Deref for ValueHandle<'_> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Value(v) => v,
            Self::Reference(v) => &**v,
        }
    }
}

#[derive(Debug)]
pub enum ValueHandleMut<'a> {
    Value(&'a mut Value),
    Reference(RefMut<'a, Value>)
}

impl Deref for ValueHandleMut<'_> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Value(v) => v,
            Self::Reference(v) => &**v,
        }
    }
}

impl DerefMut for ValueHandleMut<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::Value(v) => v,
            Self::Reference(v) => &mut **v,
        }
    }
}

impl ValueVariant {
    pub fn get_sharable(&mut self) -> SharableValue {
        match self {
            ValueVariant::Value(value) => {
                let value = std::mem::replace(value, Value::Null);
                let shared = Rc::new(RefCell::new(value));
                *self = ValueVariant::Reference(shared.clone());
                shared
            },
            ValueVariant::Reference(value) => value.clone()
        }
    }

    pub fn clone_value(&self) -> Value {
        match self {
            ValueVariant::Value(value) => value.clone(),
            ValueVariant::Reference(value) => value.borrow().clone()
        }
    }

    pub fn into_value(self) -> Value {
        match self {
            ValueVariant::Value(value) => value,
            ValueVariant::Reference(value) => value.borrow().clone()
        }
    }

    pub fn get_value<'a>(&'a self) -> ValueHandle<'a> {
        match self {
            ValueVariant::Value(value) => ValueHandle::Value(value),
            ValueVariant::Reference(ref value) => ValueHandle::Reference(value.borrow())
        }
    }

    pub fn get_mut_value<'a>(&'a mut self) -> ValueHandleMut<'a> {
        match self {
            ValueVariant::Value(value) => ValueHandleMut::Value(value),
            ValueVariant::Reference(ref mut value) => ValueHandleMut::Reference(value.borrow_mut())
        }
    }
}

impl Into<ValueVariant> for Value {
    fn into(self) -> ValueVariant {
        ValueVariant::Value(self)
    }
}

impl Into<ValueVariant> for SharableValue {
    fn into(self) -> ValueVariant {
        ValueVariant::Reference(self)
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
    pub fn as_map(&self) -> Result<&HashMap<IdentifierType, ValueVariant>, InterpreterError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(InterpreterError::InvalidStructValue(v.clone()))
        }
    }

    #[inline]
    pub fn as_mut_map(&mut self) -> Result<&mut HashMap<IdentifierType, ValueVariant>, InterpreterError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(InterpreterError::InvalidStructValue(v.clone()))
        }
    }

    #[inline]
    pub fn as_vec(&self) -> Result<&Vec<ValueVariant>, InterpreterError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn as_mut_vec(&mut self) -> Result<&mut Vec<ValueVariant>, InterpreterError> {
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
    pub fn to_map(self) -> Result<HashMap<IdentifierType, ValueVariant>, InterpreterError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(InterpreterError::InvalidStructValue(v.clone()))
        }
    }

    #[inline]
    pub fn to_vec(self) -> Result<Vec<ValueVariant>, InterpreterError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    // Check if the value is a number
    #[inline]
    pub fn is_number(&self) -> bool {
        match self {
            Value::U8(_) | Value::U16(_) | Value::U64(_) | Value::U128(_) => true,
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

impl std::fmt::Display for ValueVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueVariant::Value(value) => write!(f, "{}", value),
            ValueVariant::Reference(value) => write!(f, "Reference({})", value.borrow())
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
