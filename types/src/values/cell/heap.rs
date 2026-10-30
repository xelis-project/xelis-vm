use std::mem;
use super::{RuntimeValue, ValuePointer, HeapError};

/// Owns runtime cells for the duration of a VM execution.
/// All references borrow this heap; exclusive access requires `&mut Heap`.
/// Slots stay allocated until the heap is cleared or dropped.
/// Handles contain only slot indices; callers must resolve them against the
/// originating heap and discard them before clearing or replacing that heap.
///
/// ```compile_fail
/// use silex_types::{PrimitiveInteger, Heap};
///
/// let mut heap = Heap::new();
/// let id = heap.insert(PrimitiveInteger::U64(1).into()).unwrap();
/// let reader = heap.get(id).unwrap();
/// let writer = heap.get_mut(id).unwrap();
///
/// println!("{reader:?} {writer:?}");
/// ```
#[derive(Debug, Default)]
pub struct Heap {
    cells: Vec<RuntimeValue>,
}

impl Heap {
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn insert(&mut self, mut value: RuntimeValue) -> Result<ValuePointer, HeapError> {
        self.insert_from(&mut value)
    }

    pub(super) fn insert_from(&mut self, value: &mut RuntimeValue) -> Result<ValuePointer, HeapError> {
        let index = u32::try_from(self.cells.len()).map_err(|_| HeapError::CapacityExceeded)?;
        self.cells.push(mem::take(value));
        Ok(ValuePointer { index })
    }

    /// Resolve a handle, rejecting indices outside the heap.
    #[inline]
    pub fn get(&self, id: ValuePointer) -> Result<&RuntimeValue, HeapError> {
        self.cells.get(id.index as usize).ok_or(HeapError::InvalidPointer)
    }

    #[inline]
    pub fn get_mut(&mut self, id: ValuePointer) -> Result<&mut RuntimeValue, HeapError> {
        self.cells.get_mut(id.index as usize).ok_or(HeapError::InvalidPointer)
    }

    /// Drop all cells and reset the slot indices.
    /// The caller must discard every handle into this heap before clearing it.
    /// Previously issued indices may be reused by subsequent insertions.
    #[inline]
    pub fn clear(&mut self) {
        *self = Self::new();
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.cells.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.cells.is_empty()
    }
}
