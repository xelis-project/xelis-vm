use std::{
    cell::UnsafeCell,
};
use super::slot::HeapSlot;
use hashbrown::HashSet;

#[derive(Debug, Default)]
pub(crate) struct StorageCell(UnsafeCell<HeapStorage>);

impl StorageCell {
    pub fn get(&self) -> &HeapStorage {
        unsafe { self.0.get().as_ref_unchecked() }
    }

    pub fn get_mut(&self) -> &mut HeapStorage {
        unsafe { self.0.get().as_mut_unchecked() }
    }

    pub fn reclaim(&self, slot: HeapSlot) {
        {
            let storage = self.get_mut();
            storage.dead.push(slot);
            if storage.draining {
                return;
            }
            storage.draining = true;
        }

        loop {
            let slot = self.get_mut().dead.pop();
            let Some(slot) = slot else {
                break;
            };
            self.get_mut().slots.remove(&slot);
        }

        let storage = self.get_mut();
        if storage.slots.capacity() > 64 && storage.slots.len() < storage.slots.capacity() / 4 {
            storage.slots.shrink_to_fit();
        }
        if storage.dead.capacity() > 64 {
            storage.dead.shrink_to_fit();
        }
        storage.draining = false;
    }
}

/// Private execution state. Heap's unsafe construction contract confines this
/// storage and every related handle to one sequential execution at a time.
#[derive(Debug, Default)]
pub(crate) struct HeapStorage {
    pub slots: HashSet<HeapSlot>,
    // accumulator of all dead slots
    dead: Vec<HeapSlot>,
    draining: bool,
}

impl Drop for HeapStorage {
    fn drop(&mut self) {
        // Clear payloads to break cycles and invalidate escaped handles.
        // Storage's weak references cannot upgrade during destruction.
        for slot in &self.slots {
            slot.take();
        }
    }
}