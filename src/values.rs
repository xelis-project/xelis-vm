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
    Byte(u8),
    Short(u16),
    Int(u64),
    Long(u128),

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
    pub fn is_null(&self) -> bool {
        match &self {
            Value::Null => true,
            _ => false
        }
    }

    pub fn as_byte(&self) -> Result<&u8, InterpreterError> {
        match self {
            Value::Byte(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Byte))
        }
    }

    pub fn as_short(&self) -> Result<&u16, InterpreterError> {
        match self {
            Value::Short(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Short))
        }
    }

    pub fn as_int(&self) -> Result<&u64, InterpreterError> {
        match self {
            Value::Int(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Int))
        }
    }

    pub fn as_long(&self) -> Result<&u128, InterpreterError> {
        match self {
            Value::Long(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Long))
        }
    }

    pub fn as_string(&self) -> Result<&String, InterpreterError> {
        match self {
            Value::String(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::String))
        }
    }

    pub fn as_bool(&self) -> Result<&bool, InterpreterError> {
        match self {
            Value::Boolean(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Boolean))
        }
    }

    pub fn as_map(&self) -> Result<&HashMap<IdentifierType, ValueVariant>, InterpreterError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(InterpreterError::InvalidStructValue(v.clone()))
        }
    }

    pub fn as_mut_map(&mut self) -> Result<&mut HashMap<IdentifierType, ValueVariant>, InterpreterError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(InterpreterError::InvalidStructValue(v.clone()))
        }
    }

    pub fn as_vec(&self) -> Result<&Vec<ValueVariant>, InterpreterError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    pub fn as_mut_vec(&mut self) -> Result<&mut Vec<ValueVariant>, InterpreterError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    pub fn as_optional(&self, expected: &Type) -> Result<&Option<Box<Value>>, InterpreterError> {
        match self {
            Value::Null => Ok(&None),
            Value::Optional(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Optional(Box::new(expected.clone()))))
        }
    }

    pub fn take_from_optional(&mut self, expected: &Type) -> Result<Value, InterpreterError> {
        match self {
            Value::Optional(opt) => Ok(*opt.take().ok_or(InterpreterError::OptionalIsNull)?),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Optional(Box::new(expected.clone()))))
        }
    }

    pub fn to_byte(self) -> Result<u8, InterpreterError> {
        match self {
            Value::Byte(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Byte))
        }
    }

    pub fn to_short(self) -> Result<u16, InterpreterError> {
        match self {
            Value::Short(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Short))
        }
    }

    pub fn to_int(self) -> Result<u64, InterpreterError> {
        match self {
            Value::Int(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Int))
        }
    }

    pub fn to_long(self) -> Result<u128, InterpreterError> {
        match self {
            Value::Long(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Long))
        }
    }

    pub fn to_string(self) -> Result<String, InterpreterError> {
        match self {
            Value::String(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::String))
        }
    }

    pub fn to_bool(self) -> Result<bool, InterpreterError> {
        match self {
            Value::Boolean(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Boolean))
        }
    }

    pub fn to_map(self) -> Result<HashMap<IdentifierType, ValueVariant>, InterpreterError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(InterpreterError::InvalidStructValue(v.clone()))
        }
    }

    pub fn to_vec(self) -> Result<Vec<ValueVariant>, InterpreterError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    // Check if the value is a number
    pub fn is_number(&self) -> bool {
        match self {
            Value::Byte(_) | Value::Short(_) | Value::Int(_) | Value::Long(_) => true,
            _ => false
        }
    }

    // Cast value to string
    pub fn cast_to_string(self) -> Result<String, InterpreterError> {
        match self {
            Value::Byte(n) => Ok(n.to_string()),
            Value::Short(n) => Ok(n.to_string()),
            Value::Int(n) => Ok(n.to_string()),
            Value::Long(n) => Ok(n.to_string()),
            Value::String(s) => Ok(s),
            Value::Boolean(b) => Ok(b.to_string()),
            _ => Err(InterpreterError::InvalidCastType(Type::String))
        }
    }

    // Cast value to byte
    pub fn cast_to_byte(self) -> Result<u8, InterpreterError> {
        match self {
            Value::Byte(n) => Ok(n),
            Value::Short(n) => Ok(n as u8),
            Value::Int(n) => Ok(n as u8),
            Value::Long(n) => Ok(n as u8),
            _ => Err(InterpreterError::InvalidCastType(Type::Byte))
        }
    }

    // Cast value to short
    pub fn cast_to_short(self) -> Result<u16, InterpreterError> {
        match self {
            Value::Byte(n) => Ok(n as u16),
            Value::Short(n) => Ok(n),
            Value::Int(n) => Ok(n as u16),
            Value::Long(n) => Ok(n as u16),
            _ => Err(InterpreterError::InvalidCastType(Type::Short))
        }
    }

    // Cast value to int
    pub fn cast_to_int(self) -> Result<u64, InterpreterError> {
        match self {
            Value::Byte(n) => Ok(n as u64),
            Value::Short(n) => Ok(n as u64),
            Value::Int(n) => Ok(n),
            Value::Long(n) => Ok(n as u64),
            _ => Err(InterpreterError::InvalidCastType(Type::Int))
        }
    }

    // Cast value to long
    pub fn cast_to_long(self) -> Result<u128, InterpreterError> {
        match self {
            Value::Byte(n) => Ok(n as u128),
            Value::Short(n) => Ok(n as u128),
            Value::Int(n) => Ok(n as u128),
            Value::Long(n) => Ok(n),
            _ => Err(InterpreterError::InvalidCastType(Type::Long))
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
            Value::Byte(v) => write!(f, "{}", v),
            Value::Short(v) => write!(f, "{}", v),
            Value::Int(v) => write!(f, "{}", v),
            Value::Long(v) => write!(f, "{}", v),
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
