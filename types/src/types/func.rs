use std::{fmt, iter, ops::Deref};

use serde::{Deserialize, Serialize};

use crate::Type;


#[derive(Clone, Hash, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct ClosureType {
    parameters: Vec<Type>,
    return_type: Option<Box<Type>>
}

fn has_variadic_parameters(parameters: &[Type]) -> bool {
    matches!(parameters, [Type::Array(inner)] if inner.as_ref() == &Type::Any)
}

fn return_types_are_compatible(left: Option<&Type>, right: Option<&Type>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => left.is_compatible_with(right),
        (None, None) => true,
        _ => false,
    }
}

fn parameters_are_compatible(left: &[Type], right: &[Type]) -> bool {
    if has_variadic_parameters(left) || has_variadic_parameters(right) {
        return true;
    }

    left.len() == right.len()
        && left.iter()
            .zip(right)
            .all(|(left, right)| left.is_compatible_with(right))
}

impl ClosureType {
    pub fn new(parameters: Vec<Type>, return_type: Option<Type>) -> Self {
        Self {
            parameters,
            return_type: return_type.map(Box::new)
        }
    }

    pub fn map_generic_type(&self, ty: Option<&Type>) -> Self {
        Self {
            parameters: self.parameters.iter().map(|t| t.map_generic_type(ty)).collect(),
            return_type: self.return_type.as_ref().map(|t| Box::new(t.map_generic_type(ty))),
        }
    }

    pub fn parameters(&self) -> &Vec<Type> {
        &self.parameters
    }

    pub fn return_type(&self) -> Option<&Type> {
        self.return_type.as_deref()
    }

    // Compatibility is intentionally separate from structural equality.
    // Generic and variadic signatures may be compatible without being equal.
    pub fn is_compatible_with(&self, other: &Self) -> bool {
        return_types_are_compatible(self.return_type(), other.return_type())
            && parameters_are_compatible(&self.parameters, &other.parameters)
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct FnType {
    // Registered on a specific type
    // may be a static function if on_instance is false
    on_type: Option<Box<Type>>,
    on_instance: bool,
    closure: ClosureType,
}

impl Deref for FnType {
    type Target = ClosureType;

    fn deref(&self) -> &Self::Target {
        &self.closure
    }
}

impl AsRef<ClosureType> for FnType {
    fn as_ref(&self) -> &ClosureType {
        &self.closure
    }
}

impl FnType {
    pub fn new(on_type: Option<Type>, on_instance: bool, parameters: Vec<Type>, return_type: Option<Type>) -> Self {
        Self {
            on_type: on_type.map(Box::new),
            on_instance,
            closure: ClosureType::new(parameters, return_type)
        }
    }

    pub fn map_generic_type(&self, ty: Option<&Type>) -> Self {
        Self {
            on_type: self.on_type.as_ref().map(|t| Box::new(t.map_generic_type(ty))),
            on_instance: self.on_instance,
            closure: self.closure.map_generic_type(ty),
        }
    }

    pub fn on_type(&self) -> Option<&Type> {
        self.on_type.as_deref()
    }

    pub fn on_instance(&self) -> bool {
        self.on_instance
    }

    pub fn is_compatible_with(&self, other: &Self) -> bool {
        self.on_type == other.on_type
            && self.on_instance == other.on_instance
            && self.closure.is_compatible_with(&other.closure)
    }

    // Instance methods expose their receiver as the closure's first parameter.
    pub fn is_compatible_with_closure(&self, other: &ClosureType) -> bool {
        if !return_types_are_compatible(self.return_type(), other.return_type()) {
            return false;
        }

        if !self.on_instance {
            return parameters_are_compatible(&self.closure.parameters, &other.parameters);
        }

        let (Some(on_type), Some(receiver)) = (self.on_type(), other.parameters.first()) else {
            return false;
        };

        on_type.is_compatible_with(receiver)
            && parameters_are_compatible(&self.closure.parameters, &other.parameters[1..])
    }
}

impl fmt::Display for ClosureType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self.parameters.iter()
            .map(Some)
            .filter_map(|v| v.map(Type::to_string))
            .collect::<Vec<_>>()
            .join(", ");

        match self.return_type() {
            Some(ty) => write!(f, "closure({}) -> {}", params, ty),
            None => write!(f, "closure({})", params),
        }
    }
}

impl fmt::Display for FnType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // If on_instance is true, the on_type should be the first parameter
        let params = iter::once(self.on_type.as_deref().filter(|_| self.on_instance))
            .chain(self.parameters.iter().map(Some))
            .filter_map(|v| v.map(Type::to_string))
            .collect::<Vec<_>>()
            .join(", ");

        match self.return_type() {
            Some(ty) => write!(f, "fn({}) -> {}", params, ty),
            None => write!(f, "fn({})", params),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_closure_display() {
        let closure = ClosureType::new(
            vec![Type::U64, Type::String],
            Some(Type::Bool)
        );
        assert_eq!(format!("{}", closure), "closure(u64, string) -> bool");

        let closure_no_return = ClosureType::new(
            vec![Type::U32],
            None
        );
        assert_eq!(format!("{}", closure_no_return), "closure(u32)");
    }

    #[test]
    fn test_fn_display_static() {
        // Static function (not on instance)
        let fn_type = FnType::new(
            None,
            false,
            vec![Type::U64, Type::String],
            Some(Type::Bool)
        );
        assert_eq!(format!("{}", fn_type), "fn(u64, string) -> bool");
    }

    #[test]
    fn test_fn_display_instance_method() {
        // Instance method - on_type should appear first
        let fn_type = FnType::new(
            Some(Type::String),
            true,
            vec![Type::U64, Type::Bool],
            Some(Type::U32)
        );
        assert_eq!(format!("{}", fn_type), "fn(string, u64, bool) -> u32");
    }

    #[test]
    fn test_fn_display_static_with_on_type() {
        // Static method with on_type (on_instance = false)
        // on_type should NOT appear in parameters
        let fn_type = FnType::new(
            Some(Type::String),
            false,
            vec![Type::U64],
            Some(Type::Bool)
        );
        assert_eq!(format!("{}", fn_type), "fn(u64) -> bool");
    }
}