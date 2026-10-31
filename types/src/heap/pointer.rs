use std::fmt::{Debug, Formatter, Result as FmtResult};
use super::{RuntimeValue, Heap, HeapError, HeapSlot};

/// An owning, execution-local handle to a stable slot.
/// Owning handles cannot independently migrate or be shared across threads.
/// Only the private VM execution future transfers the complete owner family.
///
/// ```compile_fail
/// use silex_types::ValuePointer;
/// fn send<T: Send>() {}
/// send::<ValuePointer>();
/// ```
///
/// ```compile_fail
/// use silex_types::ValuePointer;
/// fn sync<T: Sync>() {}
/// sync::<ValuePointer>();
/// ```
#[derive(PartialEq, Eq, Hash, Clone)]
pub struct ValuePointer {
    // Drop takes the owner before enqueueing reclamation.
    slot: Option<HeapSlot>,
}

impl ValuePointer {
    #[inline]
    pub(super) fn new(slot: HeapSlot) -> Self {
        Self { slot: Some(slot) }
    }

    #[inline]
    pub(super) fn slot(&self) -> &HeapSlot {
        self.slot.as_ref().expect("live heap handle")
    }

    #[inline]
    pub(super) fn is_empty(&self) -> bool {
        self.slot.is_none()
    }

    #[inline]
    pub(crate) fn take(&mut self) -> Result<Self, HeapError> {
        let slot = self.slot.take().ok_or(HeapError::InvalidChildren)?;
        Ok(Self::new(slot))
    }

    #[inline]
    pub fn as_ref<'a>(&'a self, heap: &'a Heap) -> &'a RuntimeValue {
        heap.get(self)
    }

    #[inline]
    pub fn as_mut<'a>(&'a self, heap: &'a mut Heap) -> &'a mut RuntimeValue {
        heap.get_mut(self)
    }

    #[inline]
    pub fn ptr_eq(&self, other: &Self) -> bool {
        self == other
    }

    /// Clone the payload, retaining any shared child handles.
    #[inline]
    pub fn clone_value(&self, heap: &Heap) -> RuntimeValue {
        heap.get(self).clone()
    }

    #[inline]
    pub fn into_owned(self, heap: &mut Heap) -> Result<RuntimeValue, HeapError> {
        let value = heap.take(self);
        heap.detach(value)
    }
}

impl Debug for ValuePointer {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_tuple("ValuePointer").field(&self.slot.as_ref().map(|slot| slot.as_ptr())).finish()
    }
}

impl Drop for ValuePointer {
    #[inline]
    fn drop(&mut self) {
        let Some(slot) = self.slot.take() else {
            return;
        };

        // One owner belongs to the heap and one to this dropping handle.
        if slot.strong_count() == 2 {
            if let Some(storage) = slot.storage.upgrade() {
                storage.reclaim(slot);
            }
        }
    }
}