use std::{cell::RefCell, rc::Rc};

use crate::{types::Type, IdentifierType};

pub type InnerValue = Rc<RefCell<Value>>;

macro_rules! checked_cast {
    ($self: expr, $type: expr) => {
        match $self {
            Value::U8(n) => n.try_into().map_err(|_| ValueError::CastError),
            Value::U16(n) => n.try_into().map_err(|_| ValueError::CastError),
            Value::U32(n) => n.try_into().map_err(|_| ValueError::CastError),
            Value::U64(n) => n.try_into().map_err(|_| ValueError::CastError),
            Value::U128(n) => n.try_into().map_err(|_| ValueError::CastError),
            Value::Boolean(n) => n.try_into().map_err(|_| ValueError::CastError),
            _ => Err(ValueError::InvalidCastType($type))
        }
    };
}

#[derive(Debug)]
pub enum ValueError {
    InvalidValue(Value, Type),
    InvalidStructValue(Value),
    InvalidCastType(Type),
    OperationNotNumberType,
    SubValue,
    OptionalIsNull,
    OutOfBounds(usize, usize),
    CastError,
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

    String(String),
    Boolean(bool),
    Struct(IdentifierType, Vec<InnerValue>),
    Array(Vec<InnerValue>),
    Optional(Option<Box<Value>>)
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
    pub fn as_map(&self) -> Result<&Vec<InnerValue>, ValueError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(ValueError::InvalidStructValue(v.clone()))
        }
    }

    #[inline]
    pub fn as_mut_map(&mut self) -> Result<&mut Vec<InnerValue>, ValueError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(ValueError::InvalidStructValue(v.clone()))
        }
    }

    #[inline]
    pub fn as_vec<'a>(&'a self) -> Result<&'a Vec<InnerValue>, ValueError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn as_mut_vec<'a>(&'a mut self) -> Result<&'a mut Vec<InnerValue>, ValueError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn as_optional(&self, expected: &Type) -> Result<&Option<Box<Value>>, ValueError> {
        match self {
            Value::Null => Ok(&None),
            Value::Optional(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Optional(Box::new(expected.clone()))))
        }
    }

    #[inline]
    pub fn take_from_optional(&mut self, expected: &Type) -> Result<Value, ValueError> {
        match self {
            Value::Optional(opt) => Ok(*opt.take().ok_or(ValueError::OptionalIsNull)?),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Optional(Box::new(expected.clone()))))
        }
    }

    #[inline]
    pub fn take_optional(&mut self) -> Result<Option<Box<Value>>, ValueError> {
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
    pub fn to_map(self) -> Result<Vec<InnerValue>, ValueError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(ValueError::InvalidStructValue(v.clone()))
        }
    }

    #[inline]
    pub fn to_vec(self) -> Result<Vec<InnerValue>, ValueError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(ValueError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }
    #[inline]

    pub fn to_sub_vec(self) -> Result<Vec<InnerValue>, ValueError> {
        match self {
            Value::Array(values) => Ok(values),
            Value::Struct(_, fields) => Ok(fields),
            _ => Err(ValueError::SubValue)
        }
    }

    #[inline]
    pub fn as_sub_vec(&self) -> Result<&Vec<InnerValue>, ValueError> {
        match self {
            Value::Array(values) => Ok(values),
            Value::Struct(_, fields) => Ok(fields),
            _ => Err(ValueError::SubValue)
        }
    }

    #[inline]
    pub fn as_mut_sub_vec(&mut self) -> Result<&mut Vec<InnerValue>, ValueError> {
        match self {
            Value::Array(values) => Ok(values),
            Value::Struct(_, fields) => Ok(fields),
            _ => Err(ValueError::SubValue)
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

    pub fn increment(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Value::U8(n) => *n += 1,
            Value::U16(n) => *n += 1,
            Value::U32(n) => *n += 1,
            Value::U64(n) => *n += 1,
            Value::U128(n) => *n += 1,
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
            Value::String(s) => Ok(s),
            Value::Boolean(b) => Ok(b.to_string()),
            _ => Err(ValueError::InvalidCastType(Type::String))
        }
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
            Type::String => self.cast_to_string().map(Value::String),
            Type::Bool => self.cast_to_bool().map(Value::Boolean),
            Type::Optional(inner) => {
                if let Value::Null = self {
                    Ok(Value::Optional(None))
                } else {
                    self.checked_cast_to_primitive_type(inner).map(|v| Value::Optional(Some(Box::new(v))))
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

    // Cast value to bool
    #[inline]
    pub fn cast_to_bool(self) -> Result<bool, ValueError> {
        match self {
            Value::U8(n) => Ok(n != 0),
            Value::U16(n) => Ok(n != 0),
            Value::U32(n) => Ok(n != 0),
            Value::U64(n) => Ok(n != 0),
            Value::U128(n) => Ok(n != 0),
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
            Value::Boolean(b) => Ok(b as u128),
            _ => Err(ValueError::InvalidCastType(Type::U128))
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
                let s: Vec<String> = fields.iter().enumerate().map(|(k, v)| format!("{}: {}", k, v.borrow())).collect();
                write!(f, "{} {} {} {}", name, "{", s.join(", "), "}")
            },
            Value::Array(values) => {
                let s: Vec<String> = values.iter().map(|v| format!("{}", v.borrow())).collect();
                write!(f, "[{}]", s.join(", "))
            },
            Value::Optional(value) => match value.as_ref() {
                Some(value) => write!(f, "Optional({})", value),
                None => write!(f, "Optional(null)")
            },
        }
    }
}