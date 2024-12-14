mod hash;
mod r#type;

use core::fmt;
use std::{
    any::{Any, TypeId},
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
};
use serde::Serialize;
use serde_json::Value;

use crate::ValueError;

pub use hash::DynHash;
pub use r#type::OpaqueType;

pub trait Opaque: Any + Debug + DynHash {
    fn get_type(&self) -> TypeId;

    fn clone_box(&self) -> Box<dyn Opaque>;

    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Opaque")
    }

    fn is_equal(&self, _: &dyn Opaque) -> bool {
        false
    }

    fn as_any(&self) -> &dyn Any;

    fn to_json(&self) -> Value;
}

impl Hash for dyn Opaque {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.dyn_hash(state);
    }
}

/// An Opaque value that can be used to store any type
/// This allow environments to provide custom types to the VM
#[derive(Debug, Hash)]
pub struct OpaqueWrapper(Box<dyn Opaque>);

impl OpaqueWrapper {
    pub fn new<T: Opaque>(value: T) -> Self {
        Self(Box::new(value))
    }

    pub fn get_type(&self) -> OpaqueType {
        OpaqueType::from(self.0.get_type())
    }

    pub fn as_ref<T: Opaque>(&self) -> Result<&T, ValueError> {
        self.0.as_any()
            .downcast_ref::<T>()
            .ok_or(ValueError::InvalidOpaqueTypeMismatch)
    }
}

impl Serialize for OpaqueWrapper {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0
            .to_json()
            .serialize(serializer)
    }
}

impl Clone for OpaqueWrapper {
    fn clone(&self) -> Self {
        Self(self.0.clone_box())
    }
}

impl PartialEq for OpaqueWrapper {
    fn eq(&self, other: &Self) -> bool {
        if self.0.get_type() != other.0.get_type() {
            return false;
        }

        self.0.is_equal(other.0.as_ref())
    }
}

impl Display for OpaqueWrapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.display(f)
    }
}

impl Eq for OpaqueWrapper {}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Clone, Hash)]
    struct CustomOpaque {
        value: i32,
    }

    impl Opaque for CustomOpaque {
        fn get_type(&self) -> TypeId {
            TypeId::of::<CustomOpaque>()
        }

        fn clone_box(&self) -> Box<dyn Opaque> {
            Box::new(self.clone())
        }

        fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "CustomOpaque({})", self.value)
        }

        fn as_any(&self) -> &dyn Any {
            self
        }

        fn to_json(&self) -> Value {
            Value::Number(self.value.into())
        }
    }

    #[test]
    fn test_opaque_wrapper() {
        let opaque = OpaqueWrapper::new(CustomOpaque { value: 42 });
        assert_eq!(opaque.to_string(), "CustomOpaque(42)");
        assert_eq!(opaque.get_type().as_type_id(), TypeId::of::<CustomOpaque>());
    }
}