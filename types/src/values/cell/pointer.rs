use super::{RuntimeValue, Heap, HeapError};

/// An index into a VM heap. Slots are not reused until the heap is cleared.
/// The caller must resolve this handle only against its originating heap and
/// discard it before that heap is cleared or replaced.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValuePointer {
    pub(super) index: u32,
}

pub type ValueId = ValuePointer;

impl ValuePointer {
    #[inline]
    pub fn as_ref<'a>(&self, heap: &'a Heap) -> &'a RuntimeValue {
        heap.get(*self).expect("value handle index is out of bounds for the heap")
    }

    #[inline]
    pub fn as_mut<'a>(&self, heap: &'a mut Heap) -> &'a mut RuntimeValue {
        heap.get_mut(*self).expect("value handle index is out of bounds for the heap")
    }

    /// Compare slot indices; both handles must refer to the same heap.
    pub fn ptr_eq(self, other: Self) -> bool {
        self == other
    }

    pub fn to_owned(&self, heap: &Heap) -> Result<RuntimeValue, HeapError> {
        Ok(heap.get(*self)?.clone_ref())
    }

    pub fn into_owned(self, heap: &mut Heap) -> Result<RuntimeValue, HeapError> {
        let value = self.to_owned(heap)?;
        heap.detach(value)
    }

    pub fn raw_owned(&self, heap: &Heap) -> Result<RuntimeValue, HeapError> {
        self.to_owned(heap)
    }
}
