use crate::interpreter::InterpreterError;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum Value {
    Null,

    // number types
    Byte(u8),
    Short(u16),
    Int(u64),
    Long(u128),

    String(String),
    Boolean(bool),
    Struct(String, HashMap<String, Value>),
    Array(Vec<Value>)
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

    pub fn as_map(&self) -> Result<&HashMap<String, Value>, InterpreterError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(InterpreterError::InvalidStructValue(v.clone()))
        }
    }

    pub fn as_vec(&self) -> Result<&Vec<Value>, InterpreterError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    pub fn as_mut_vec(&mut self) -> Result<&mut Vec<Value>, InterpreterError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
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

    pub fn to_map(self) -> Result<HashMap<String, Value>, InterpreterError> {
        match self {
            Value::Struct(_, fields) => Ok(fields),
            v => Err(InterpreterError::InvalidStructValue(v.clone()))
        }
    }

    pub fn to_vec(self) -> Result<Vec<Value>, InterpreterError> {
        match self {
            Value::Array(n) => Ok(n),
            v => Err(InterpreterError::InvalidValue(v.clone(), Type::Array(Box::new(Type::Any))))
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            Value::Byte(_) | Value::Short(_) | Value::Int(_) | Value::Long(_) => true,
            _ => false
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
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Struct {
    pub name: String,
    pub fields: HashMap<String, Type> // order doesn't matter in fields, so use a HashMap for future
}

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Null,
    Any,
    //T, // TODO T must be same as for type

    Byte,
    Short,
    Int,
    Long,

    String,
    Boolean,
    Struct(Struct),
    Array(Box<Type>)
}

impl Type {
    pub fn from_string(s: &String, structures: &HashMap<String, Struct>) -> Option<Self> {
        let value: Self = match s.as_str() {
            "byte" => Type::Byte,
            "short" => Type::Short,
            "int" => Type::Int,
            "long" => Type::Long,
            "bool" => Type::Boolean,
            "string" => Type::String,
            _ => {
                let structure = structures.get(s)?;
                if structure.name == *s {
                    Type::Struct(structure.clone())
                } else {
                    return None
                }
            }
        };

        Some(value)
    }

    pub fn from_value(value: &Value, structures: &HashMap<String, Struct>) -> Option<Self> {
        let _type = match value {
            Value::Null => Type::Null,
            Value::Byte(_) => Type::Byte,
            Value::Short(_) => Type::Short,
            Value::Int(_) => Type::Int,
            Value::Long(_) => Type::Long,
            Value::String(_) => Type::String,
            Value::Boolean(_) => Type::Boolean,
            Value::Array(values) => match values.get(0) {
                Some(v) => Type::Array(Box::new(Type::from_value(v, structures)?)),
                None => Type::Array(Box::new(Type::Null)) // we can't determine precisely the type
            },
            Value::Struct(name, _) => Type::Struct(structures.get(name)?.clone())
        };

        Some(_type)
    }

    pub fn get_array_type(&self) -> &Type {
        match &self {
            Type::Array(ref _type) => _type,
            _ => &self
        }
    }

    pub fn is_compatible_with(&self, other: &Type) -> bool {
        match other {
            Type::Any => true,
            Type::Array(sub_type) => match sub_type.as_ref() {
                Type::Any => self.is_array(),
                o => *o == *self
            },
            o => *o == *self 
        }
    }

    pub fn is_array(&self) -> bool {
        match &self {
            Type::Array(_) => true,
            _ => false
        }
    }

    pub fn is_struct(&self) -> bool {
        match &self {
            Type::Struct(_) => true,
            _ => false
        }
    }

    pub fn is_number(&self) -> bool {
        match &self {
            Type::Byte | Type::Short | Type::Int | Type::Long => true,
            _ => false
        }
    }
}