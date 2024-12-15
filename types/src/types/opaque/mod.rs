mod r#type;

use core::fmt;
use std::{
    any::TypeId,
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
};
use serde::{ser::SerializeStruct, Deserialize, Serialize};
use serde_json::Value;

use crate::ValueError;

pub use r#type::OpaqueType;

pub mod traits;
use traits::*;

pub trait Opaque: DynHash + DynEq + JSONHelper + Debug + Sync + Send {
    fn get_type(&self) -> TypeId;

    fn clone_box(&self) -> Box<dyn Opaque>;

    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Opaque")
    }
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

impl PartialEq for OpaqueWrapper {
    fn eq(&self, other: &Self) -> bool {
        let other = other.0.as_eq();
        self.0.as_ref()
            .is_equal(other)
    }
}

impl Eq for OpaqueWrapper {}

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

    pub fn as_mut<T: Opaque>(&mut self) -> Result<&mut T, ValueError> {
        self.0.as_any_mut()
            .downcast_mut::<T>()
            .ok_or(ValueError::InvalidOpaqueTypeMismatch)
    }
}

impl Serialize for OpaqueWrapper {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut structure = serializer.serialize_struct("OpaqueWrapper", 2)?;
        structure.serialize_field("type", &self.0.get_type_name())?;

        let value = if self.0.is_supported() {
            self.0.serialize_json()
                .map_err(serde::ser::Error::custom)?
        } else {
            Value::String("JSON not supported".to_owned())
        };

        structure.serialize_field("value", &value)?;
        structure.end()
    }
}

impl<'a> Deserialize<'a> for OpaqueWrapper {
    fn deserialize<D: serde::Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        struct OpaqueWrapperData {
            #[serde(rename = "type")]
            type_: String,
            value: Value,
        }

        let data = OpaqueWrapperData::deserialize(deserializer)?;
        let type_name = data.type_;
        let value = data.value;

        let registry = JSON_REGISTRY.read()
            .map_err(serde::de::Error::custom)?;

        let deserialize_fn = registry.get(&type_name.as_str())
            .ok_or(serde::de::Error::custom("Unknown type"))?;

        deserialize_fn(value)
            .map_err(serde::de::Error::custom)
    }
}

impl Clone for OpaqueWrapper {
    fn clone(&self) -> Self {
        Self(self.0.clone_box())
    }
}

impl Display for OpaqueWrapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.display(f)
    }
}

#[cfg(test)]
mod tests {
    use crate::register_opaque;
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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
    }

    #[test]
    fn test_opaque_wrapper() {
        let opaque = OpaqueWrapper::new(CustomOpaque { value: 42 });
        assert_eq!(opaque.to_string(), "CustomOpaque(42)");
        assert_eq!(opaque.get_type().as_type_id(), TypeId::of::<CustomOpaque>());

        // Test hashing
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        opaque.hash(&mut hasher);

        // Test equality
        let opaque2 = opaque.clone();
        assert_eq!(opaque, opaque2);
    }

    #[test]
    fn test_opaque_serde() {
        register_opaque!("CustomOpaque", CustomOpaque);

        let opaque = OpaqueWrapper::new(CustomOpaque { value: 42 });
        let json = serde_json::to_string(&opaque).unwrap();
        assert_eq!(json, r#"{"type":"CustomOpaque","value":{"value":42}}"#);

        let opaque2: OpaqueWrapper = serde_json::from_str(&json).unwrap();
        assert_eq!(opaque, opaque2);
    }
}