use core::fmt;
use std::{
    any::{Any, TypeId},
    fmt::{Debug, Display},
    hash::{Hash, Hasher},
};
use serde::Serialize;
use serde_json::Value;

use crate::ValueError;

pub trait Opaque: Any + Debug + Send + Sync {
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

    fn try_hash(&self, _: &mut dyn Hasher) {}
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct OpaqueType(TypeId);

impl OpaqueType {
    pub fn new<T: Opaque>() -> Self {
        Self(TypeId::of::<T>())
    }
}

/// An Opaque value that can be used to store any type
/// This allow environments to provide custom types to the VM
#[derive(Debug)]
pub struct OpaqueWrapper(Box<dyn Opaque>);

impl OpaqueWrapper {
    pub fn new<T: Opaque>(value: T) -> Self {
        Self(Box::new(value))
    }

    pub fn get_type(&self) -> OpaqueType {
        OpaqueType(self.0.type_id())
    }

    pub fn as_ref<T: Opaque>(&self) -> Result<&T, ValueError> {
        self.0.as_any()
            .downcast_ref::<T>()
            .ok_or(ValueError::InvalidOpaqueTypeMismatch)
    }
}

impl Hash for OpaqueWrapper {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.try_hash(state)
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
        self.0.is_equal(other.0.as_ref())
    }
}

impl Display for OpaqueWrapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.display(f)
    }
}

impl Eq for OpaqueWrapper {}
