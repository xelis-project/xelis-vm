use std::collections::HashMap;

#[derive(Debug)]
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

#[derive(Clone, PartialEq, Debug)]
pub struct Struct {
    pub name: String,
    pub fields: HashMap<String, Type> // order doesn't matter in fields, so use a HashMap for future
}

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
    Null,

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
            Value::Struct(name, _) => Type::Struct(structures.get(name)?.clone()),
            _ => return None
        };

        Some(_type)
    }

    fn get_type_from_array(_type: &Type) -> &Type {
        match _type {
            Type::Array(ref array) => Type::get_type_from_array(array),
            _ => _type
        }
    }

    pub fn get_real_type(&self) -> &Type {
        Type::get_type_from_array(&self)
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
}