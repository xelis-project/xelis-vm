use std::{fmt, iter};

use serde::{Deserialize, Serialize};

use crate::Type;

#[derive(Clone, Hash, Debug, Serialize, Deserialize)]
pub struct FnType {
    on_type: Option<Box<Type>>,
    on_instance: bool,
    parameters: Vec<Type>,
    return_type: Option<Box<Type>>
}

impl PartialEq for FnType {
    fn eq(&self, other: &Self) -> bool {
        if !(self.on_type == other.on_type
            && self.on_instance == other.on_instance) {
            return false
        }

        match (&self.return_type, &other.return_type) {
            (Some(a), Some(b)) if !a.is_compatible_with(b) => return false,
            (None, Some(_)) | (Some(_), None) => return false,
            _ => {}
        }

        // We have to check the params len
        // SPECIAL CASE: If one of the function has singular Type::Any array as parameter,
        // it means it can accept any number of parameters
        if (self.parameters.len() == 1 && self.parameters[0] == Type::Array(Box::new(Type::Any)))
            || (other.parameters.len() == 1 && other.parameters[0] == Type::Array(Box::new(Type::Any))) {
            return true
        }

        if self.parameters.len() != other.parameters.len() {
            return false
        }

        for (a, b) in self.parameters.iter().zip(other.parameters.iter()) {
            if !a.is_compatible_with(b) {
                return false
            }
        }

        true
    }
}

impl Eq for FnType {}

impl FnType {
    pub fn new(on_type: Option<Type>, on_instance: bool, parameters: Vec<Type>, return_type: Option<Type>) -> Self {
        Self {
            on_type: on_type.map(Box::new),
            on_instance,
            parameters,
            return_type: return_type.map(Box::new)
        }
    }

    pub fn map_generic_type(&self, ty: Option<&Type>) -> Self {
        Self {
            on_type: self.on_type.as_ref().map(|t| Box::new(t.map_generic_type(ty))),
            on_instance: self.on_instance,
            parameters: self.parameters.iter().map(|t| t.map_generic_type(ty)).collect(),
            return_type: self.return_type.as_ref().map(|t| Box::new(t.map_generic_type(ty))),
        }
    }

    pub fn on_type(&self) -> Option<&Type> {
        self.on_type.as_deref()
    }

    pub fn on_instance(&self) -> bool {
        self.on_instance
    }

    // All functions parameters
    pub fn parameters(&self) -> &Vec<Type> {
        &self.parameters
    }

    pub fn return_type(&self) -> Option<&Type> {
        self.return_type.as_deref()
    }
}

impl fmt::Display for FnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self.parameters.iter()
            .map(Some)
            .chain(iter::once(self.on_type()))
            .filter_map(|v| v.map(Type::to_string))
            .collect::<Vec<_>>()
            .join(", ");

        match self.return_type() {
            Some(ty) => write!(f, "fn({}) -> {}", params, ty),
            None => write!(f, "fn({})", params),
        }
    }
}