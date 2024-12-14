use core::fmt;
use std::{any::{Any, TypeId}, fmt::{Debug, Display}, sync::Arc};

pub trait Opaque: Any + Debug {
    fn get_type(&self) -> TypeId;

    fn clone_box(&self) -> Box<dyn Opaque>;

    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Opaque")
    }

    fn is_equal(&self, _: &dyn Opaque) -> bool {
        false
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct OpaqueType(Arc<TypeId>);

impl OpaqueType {
    pub fn new<T: Opaque + 'static>() -> Self {
        Self(Arc::new(TypeId::of::<T>()))
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

    pub fn get_type(&self) -> TypeId {
        self.0.type_id()
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
