use std::{
    cell::UnsafeCell, hash::{Hash, Hasher}, mem, ops::Deref, rc::{Rc, Weak},
};
use crate::RuntimeValue;
use super::StorageCell;

#[derive(Debug, Clone)]
pub(crate) struct HeapSlot(Rc<Slot>);

impl HeapSlot {
    #[inline]
    pub fn new(storage: &Rc<StorageCell>, value: RuntimeValue) -> Self {
        Self(Rc::new(Slot::new(Rc::downgrade(storage), value)))
    }

    #[inline(always)]
    pub fn as_ptr(&self) -> *const Slot {
        Rc::as_ptr(&self.0)
    }

    #[inline(always)]
    pub fn strong_count(&self) -> usize {
        Rc::strong_count(&self.0)
    }
}

impl Hash for HeapSlot {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state);
    }
}

impl PartialEq for HeapSlot {
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr().eq(&other.as_ptr())
    }
}

impl Eq for HeapSlot {}

impl Deref for HeapSlot {
    type Target = Slot;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Debug)]
pub(crate) struct Slot {
    pub value: UnsafeCell<RuntimeValue>,
    pub storage: Weak<StorageCell>,
}

impl Slot {
    pub fn new(storage: Weak<StorageCell>, value: RuntimeValue) -> Self {
        Self {
            value: UnsafeCell::new(value),
            storage,
        }
    }

    pub fn get_mut(&self) -> &mut RuntimeValue {
        unsafe { self.value.get().as_mut_unchecked() }
    }

    pub fn get(&self) -> &RuntimeValue {
        unsafe { self.value.get().as_ref_unchecked() }
    }

    pub fn take(&self) -> RuntimeValue {
        mem::take(self.get_mut())
    }
}
