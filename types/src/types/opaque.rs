use core::fmt;
use std::{any::{Any, TypeId}, fmt::{Debug, Display}, sync::Arc};

pub trait OpaqueValue: Any + Debug {
    fn as_any(&self) -> &dyn Any;

    fn clone_box(&self) -> Box<dyn OpaqueValue>;

    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Opaque")
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct OpaqueType(Arc<TypeId>);

impl OpaqueType {
    pub fn new<T: OpaqueValue + 'static>() -> Self {
        Self(Arc::new(TypeId::of::<T>()))
    }
}

/// An Opaque value that can be used to store any type
/// This allow environments to provide custom types to the VM
#[derive(Debug)]
pub struct Opaque(pub Box<dyn OpaqueValue>);

impl Clone for Opaque {
    fn clone(&self) -> Self {
        Opaque(self.0.clone_box())
    }
}

impl PartialEq for Opaque {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_any().type_id() == other.0.as_any().type_id()
    }
}

impl Display for Opaque {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.display(f)
    }
}

impl Eq for Opaque {}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug)]
    struct TestType;

    impl OpaqueValue for TestType {
        fn as_any(&self) -> &dyn Any {
            self
        }

        fn clone_box(&self) -> Box<dyn OpaqueValue> {
            Box::new(TestType)
        }
    }

    #[test]
    fn test_opaque() {
        let opaque = Opaque(Box::new(TestType));
        assert_eq!(opaque.0.as_any().type_id(), TestType.type_id());
    }
}