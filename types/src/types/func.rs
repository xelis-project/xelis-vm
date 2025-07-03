use std::{fmt, iter};

use serde::{Deserialize, Serialize};

use crate::Type;

#[derive(Clone, Hash, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct FnType {
    on_type: Option<Box<Type>>,
    on_instance: bool,
    parameters: Vec<Type>,
    return_type: Option<Box<Type>>
}

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
            Some(ty) => write!(f, "fn({} -> {})", params, ty),
            None => write!(f, "fn({})", params),
        }
    }
}