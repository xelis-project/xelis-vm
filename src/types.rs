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
    Struct(HashMap<String, Value>),
    Array(Vec<Value>)
}

#[derive(Clone, PartialEq, Debug)]
pub struct Struct {
    pub name: String,
    pub fields: HashMap<String, Type> // order doesn't matter in fields, so use a HashMap for future
}

#[derive(Clone, PartialEq, Debug)]
pub enum Type {
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
}