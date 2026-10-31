use std::{fmt, any::TypeId};
use super::{Opaque, OpaqueValue, RuntimeOpaque, OpaqueWrapper, DynSerializable, DynHash, DynEq};
use crate::{Heap, HeapError, OpaqueChildMut, ValueError};

#[derive(Debug)]
enum RuntimeOpaqueData {
    Portable(OpaqueWrapper),
    Local(Box<dyn RuntimeOpaque>),
}

/// Runtime opaque ownership never crosses the execution boundary.
#[derive(Debug)]
pub struct RuntimeOpaqueWrapper {
    inner: RuntimeOpaqueData,
}

impl RuntimeOpaqueWrapper {
    /// Wrap portable host data for runtime use.
    #[inline]
    pub fn new<T: Opaque>(value: T) -> Self {
        Self {
            inner: RuntimeOpaqueData::Portable(OpaqueWrapper::new(value))
        }
    }

    /// Get the concrete type identity.
    #[inline]
    pub fn get_type_id(&self) -> TypeId {
        self.inner().get_type()
    }

    pub fn as_ref<T: OpaqueValue>(&self) -> Result<&T, ValueError> {
        self.inner().as_any()
            .downcast_ref::<T>()
            .ok_or_else(|| ValueError::InvalidOpaqueTypeMismatch)
    }

    pub fn as_mut<T: OpaqueValue>(&mut self) -> Result<&mut T, ValueError> {
        self.inner_mut().as_any_mut()
            .downcast_mut::<T>()
            .ok_or_else(|| ValueError::InvalidOpaqueTypeMismatch)
    }

    pub fn into_inner<T: OpaqueValue>(self) -> Result<T, ValueError> {
        let value = match self.inner {
            RuntimeOpaqueData::Portable(value) => value.inner.into_any(),
            RuntimeOpaqueData::Local(value) => value.into_any(),
        };
        value.downcast::<T>()
            .map(|value| *value)
            .map_err(|_| ValueError::InvalidOpaqueTypeMismatch)
    }

    #[inline]
    pub fn as_serializable(&self) -> Option<&dyn DynSerializable> {
        self.inner().as_serializable()
    }

    #[inline]
    pub fn is_serializable(&self) -> bool {
        self.as_serializable().is_some()
    }

    #[inline]
    pub fn is_json_serializable(&self) -> bool {
        self.inner().is_json_supported()
    }

    /// Whether the value can be used as a map key (hashing and equality required).
    #[inline]
    pub fn is_hashable(&self) -> bool {
        self.as_hashable().is_some() && self.as_eq().is_some()
    }

    #[inline]
    pub fn as_hashable(&self) -> Option<&dyn DynHash> {
        self.inner().as_hashable()
    }

    #[inline]
    pub fn as_eq(&self) -> Option<&dyn DynEq> {
        self.inner().as_eq()
    }

    #[inline]
    pub fn get_size(&self) -> usize {
        self.inner().get_size()
    }

    #[inline]
    pub fn inner(&self) -> &dyn OpaqueValue {
        match &self.inner {
            RuntimeOpaqueData::Portable(value) => value.inner(),
            RuntimeOpaqueData::Local(value) => value.as_ref(),
        }
    }

    fn inner_mut(&mut self) -> &mut dyn OpaqueValue {
        match &mut self.inner {
            RuntimeOpaqueData::Portable(value) => value.inner.as_mut(),
            RuntimeOpaqueData::Local(value) => value.as_mut(),
        }
    }

    #[inline]
    pub fn validate(&self) -> Result<(), ValueError> {
        self.inner().validate()
    }
}

impl RuntimeOpaqueWrapper {
    pub fn local(value: impl RuntimeOpaque + 'static) -> Self {
        Self {
            inner: RuntimeOpaqueData::Local(Box::new(value)),
        }
    }

    pub fn deep_clone_in(&self, heap: &mut Heap) -> Result<Self, HeapError> {
        let inner = match &self.inner {
            RuntimeOpaqueData::Portable(value) => RuntimeOpaqueData::Portable(value.deep_clone()),
            RuntimeOpaqueData::Local(value) => RuntimeOpaqueData::Local(value.deep_clone_in(heap)?),
        };
        Ok(Self { inner })
    }

    pub(crate) fn visit_children_mut(&mut self, visitor: &mut dyn FnMut(OpaqueChildMut<'_>) -> Result<(), HeapError>) -> Result<bool, HeapError> {
        match &mut self.inner {
            RuntimeOpaqueData::Portable(_) => Ok(false),
            RuntimeOpaqueData::Local(value) => value.visit_children_mut(visitor),
        }
    }

    pub fn into_portable(self) -> Result<OpaqueWrapper, HeapError> {
        match self.inner {
            RuntimeOpaqueData::Portable(value) => Ok(value),
            RuntimeOpaqueData::Local(_) => Err(HeapError::NonPortableValue),
        }
    }

    pub fn to_portable(&self) -> Result<OpaqueWrapper, HeapError> {
        match &self.inner {
            RuntimeOpaqueData::Portable(value) => Ok(value.deep_clone()),
            RuntimeOpaqueData::Local(_) => Err(HeapError::NonPortableValue),
        }
    }
}

impl Clone for RuntimeOpaqueWrapper {
    fn clone(&self) -> Self {
        let inner = match &self.inner {
            RuntimeOpaqueData::Portable(value) => RuntimeOpaqueData::Portable(value.clone()),
            RuntimeOpaqueData::Local(value) => RuntimeOpaqueData::Local(value.clone_runtime()),
        };
        Self { inner }
    }
}

impl PartialEq for RuntimeOpaqueWrapper {
    fn eq(&self, other: &Self) -> bool {
        if std::ptr::eq(self, other) {
            return true;
        }
        self.get_type_id() == other.get_type_id() && match (self.as_eq(), other.as_eq()) {
            (Some(left), Some(right)) => left.is_equal(right),
            _ => false,
        }
    }
}

impl From<OpaqueWrapper> for RuntimeOpaqueWrapper {
    fn from(value: OpaqueWrapper) -> Self {
        Self {
            inner: RuntimeOpaqueData::Portable(value),
        }
    }
}

impl<T: Opaque> From<T> for RuntimeOpaqueWrapper {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl fmt::Display for RuntimeOpaqueWrapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner().display(f)
    }
}
