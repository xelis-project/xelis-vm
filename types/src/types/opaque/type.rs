use std::any::TypeId;
use crate::opaque::Opaque;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct OpaqueType(TypeId);

impl OpaqueType {
    pub fn new<T: Opaque + 'static>() -> Self {
        Self(TypeId::of::<T>())
    }

    pub fn from(type_id: TypeId) -> Self {
        Self(type_id)
    }

    pub fn as_type_id(&self) -> TypeId {
        self.0
    }
}