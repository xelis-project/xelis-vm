mod storage;
mod slot;
mod error;
mod pointer;

#[cfg(test)]
mod tests;

use std::{mem, rc::Rc};
use indexmap::IndexMap;

use crate::{OpaqueChildMut, OwnedValue, RuntimeOpaqueWrapper, RuntimeValue, StackValue, ValueError};
use storage::StorageCell;
use slot::HeapSlot;
pub use pointer::ValuePointer;

pub use error::HeapError;

/// Execution-local heap with stable slots and owning, automatically released handles.
/// Payload references borrow both the heap and an owning handle. Only the heap
/// may mutate payloads; dropping a final handle may destroy an unborrowed payload.
/// The last handle releases its payload and removes its slot from storage.
/// Owning pointers keep allocations alive, so no slot indices or generations
/// are required.
///
/// ```compile_fail
/// use silex_types::Heap;
/// let heap = Heap::new();
/// ```
///
/// ```compile_fail
/// use silex_types::Heap;
/// let heap = Heap::default();
/// ```
///
/// ```compile_fail
/// use silex_types::{Heap, RuntimeValue};
/// let mut heap = unsafe { Heap::new() };
/// let id = heap.insert(RuntimeValue::default());
/// let borrowed = heap.get(&id);
/// drop(id);
/// println!("{borrowed:?}");
/// ```
///
/// ```compile_fail
/// use silex_types::{Heap, RuntimeValue};
/// let mut heap = unsafe { Heap::new() };
/// let id = heap.insert(RuntimeValue::default());
/// let borrowed = heap.get_mut(&id);
/// drop(id);
/// println!("{borrowed:?}");
/// ```
///
/// ```compile_fail
/// use silex_types::{Heap, RuntimeValue};
/// let mut heap = unsafe { Heap::new() };
/// let id = heap.insert(RuntimeValue::default());
/// let borrowed = heap.get(&id);
/// heap.clear();
/// println!("{borrowed:?}");
/// ```
#[derive(Debug)]
pub struct Heap {
    storage: Rc<StorageCell>,
}


impl Heap {
    /// Create a heap for one sequential execution.
    ///
    /// # Safety
    /// The caller must serialize all access to this heap and every related
    /// runtime handle, including Clone and Drop. Handles must be resolved only
    /// through their originating heap and only before it is cleared or dropped.
    /// Native functions, callbacks,
    /// opaque values, constants, and external state must not operate on aliases
    /// concurrently. This obligation also covers handles left after clear/drop.
    /// An async execution may migrate threads only when all aliases remain
    /// confined to that execution and are inactive elsewhere during migration.
    /// Any foreign payload retained during migration must itself be Send.
    /// Traversals require an acyclic runtime graph. VM writes detach values
    /// before storing them below existing values. Opaque child visitors must
    /// expose every runtime child that needs detachment.
    /// Cross-thread handoffs must establish synchronization, as with an executor
    /// scheduling the future or a thread spawn/join transferring ownership.
    pub unsafe fn new() -> Self {
        Self {
            storage: Rc::default(),
        }
    }

    #[inline]
    pub fn insert(&mut self, mut value: RuntimeValue) -> ValuePointer {
        self.insert_from(&mut value)
    }

    pub(super) fn insert_from(&mut self, value: &mut RuntimeValue) -> ValuePointer {
        let slot = HeapSlot::new(&self.storage, mem::take(value));

        let storage = self.storage.get_mut();
        storage.slots.insert(slot.clone());

        ValuePointer::new(slot)
    }

    /// Resolve a live handle belonging to this heap.
    #[inline]
    pub fn get<'a>(&'a self, id: &'a ValuePointer) -> &'a RuntimeValue {
        // The borrowed handle prevents final release. Mutations require an
        // exclusive heap borrow, excluding these shared payload borrows.
        id.slot().get()
    }

    /// Mutably resolve a live handle belonging to this heap.
    #[inline]
    pub fn get_mut<'a>(&'a mut self, id: &'a ValuePointer) -> &'a mut RuntimeValue {
        // The exclusive heap borrow excludes other payload borrows; the handle
        // prevents destruction by drops of any other owners.
        id.slot().get_mut()
    }

    pub fn take(&mut self, id: ValuePointer) -> RuntimeValue {
        let slot = id.slot();
        // One owner belongs to storage and one to this consumed handle.
        if slot.strong_count() == 2 {
            slot.take()
        } else {
            slot.get().clone()
        }
    }

    pub fn replace_stack(&mut self, target: &mut StackValue, value: RuntimeValue) {
        *target.as_mut(self) = value;
    }

    /// Temporarily move a value out so the operation can also borrow the heap.
    /// Restore the slot when the operation returns, including ordinary errors.
    pub fn with_value<R>(&mut self, target: &mut StackValue, operation: impl FnOnce(&mut RuntimeValue, &mut Heap) -> R) -> R {
        let mut value = mem::take(target.as_mut(self));
        let result = operation(&mut value, self);
        self.replace_stack(target, value);
        result
    }

    #[inline]
    pub fn clear(&mut self) {
        // Replacing storage inherits this heap's existing confinement contract.
        self.storage = Rc::default();
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.storage.get().slots.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Import portable data into fresh slots in this heap.
    #[inline]
    pub fn import(&mut self, value: &OwnedValue) -> RuntimeValue {
        value.to_runtime_value(self)
    }

    /// Consume portable input, moving buffers and map keys into runtime storage.
    pub fn import_owned(&mut self, mut root: OwnedValue) -> RuntimeValue {
        match &mut root {
            OwnedValue::Primitive(value) => return RuntimeValue::Primitive(mem::take(value)),
            OwnedValue::Bytes(value) => return RuntimeValue::Bytes(mem::take(value)),
            OwnedValue::Opaque(value) => return RuntimeValue::Opaque(value.deep_clone().into()),
            _ => {}
        }

        enum Work {
            Visit(OwnedValue),
            Array(usize),
            Map(Vec<OwnedValue>),
        }
        let mut work = vec![Work::Visit(root)];
        let mut results = Vec::new();
        while let Some(item) = work.pop() {
            match item {
                Work::Visit(mut value) => match &mut value {
                    OwnedValue::Primitive(value) => results.push(RuntimeValue::Primitive(mem::take(value))),
                    OwnedValue::Bytes(value) => results.push(RuntimeValue::Bytes(mem::take(value))),
                    OwnedValue::Opaque(value) => results.push(RuntimeValue::Opaque(value.deep_clone().into())),
                    OwnedValue::Object(values) => {
                        work.push(Work::Array(values.len()));
                        work.extend(mem::take(values).into_iter().rev().map(Work::Visit));
                    }
                    OwnedValue::Map(values) => {
                        let (keys, values): (Vec<_>, Vec<_>) = mem::take(values).into_iter().unzip();
                        work.push(Work::Map(keys));
                        work.extend(values.into_iter().rev().map(Work::Visit));
                    }
                }
                Work::Array(len) => {
                    let values = results.drain(results.len() - len..)
                        .map(|value| self.insert(value)).collect();
                    results.push(RuntimeValue::Object(values));
                }
                Work::Map(keys) => {
                    let values = results.drain(results.len() - keys.len()..);
                    let values = keys.into_iter().zip(values).map(|(key, value)| (key, self.insert(value)))
                        .collect();
                    results.push(RuntimeValue::Map(Box::new(values)));
                }
            }
        }

        results.pop().expect("portable input has one root")
    }

    /// Copy an acyclic runtime graph into portable owned data.
    #[inline]
    pub fn snapshot(&self, root: &RuntimeValue) -> Result<OwnedValue, ValueError> {
        match root {
            RuntimeValue::Primitive(value) => return Ok(OwnedValue::Primitive(value.clone())),
            RuntimeValue::Bytes(value) => return Ok(OwnedValue::Bytes(value.clone())),
            _ => {}
        }

        enum Work<'a> {
            Visit(&'a RuntimeValue),
            Array(usize),
            Map(Vec<OwnedValue>),
        }

        let mut work = vec![Work::Visit(root)];
        let mut output = Vec::new();
        while let Some(item) = work.pop() {
            match item {
                Work::Visit(value) => match value {
                    RuntimeValue::Primitive(value) => output.push(OwnedValue::Primitive(value.clone())),
                    RuntimeValue::Opaque(value) => output.push(OwnedValue::Opaque(value.to_portable()?)),
                    RuntimeValue::Bytes(value) => output.push(OwnedValue::Bytes(value.clone())),
                    RuntimeValue::Object(children) => {
                        work.push(Work::Array(children.len()));
                        for child in children.iter().rev() {
                            work.push(Work::Visit(self.get(child)));
                        }
                    }
                    RuntimeValue::Map(children) => {
                        work.push(Work::Map(children.keys().cloned().collect()));
                        for child in children.values().rev() {
                            work.push(Work::Visit(self.get(child)));
                        }
                    }
                }
                Work::Array(len) => {
                    let children = output.split_off(output.len() - len);
                    output.push(OwnedValue::Object(children));
                }
                Work::Map(keys) => {
                    let values = output.split_off(output.len() - keys.len());
                    output.push(OwnedValue::Map(keys.into_iter().zip(values).collect()));
                }
            }
        }

        output.pop().ok_or_else(|| ValueError::ExpectedValue)
    }

    #[inline]
    pub fn snapshot_stack(&self, root: &StackValue) -> Result<OwnedValue, ValueError> {
        self.snapshot(root.as_ref(self))
    }

    /// Export a consumed stack value, moving root buffers and detaching opaque
    /// payloads without rebuilding ordinary containers in intermediate slots.
    pub fn export(&mut self, value: StackValue) -> Result<OwnedValue, ValueError> {
        enum Work {
            Visit(RuntimeValue),
            Array(usize),
            Map(Vec<OwnedValue>),
        }

        let root = value.into_value(self);
        let root = match root {
            RuntimeValue::Primitive(value) => return Ok(OwnedValue::Primitive(value)),
            RuntimeValue::Bytes(value) => return Ok(OwnedValue::Bytes(value)),
            value => value,
        };

        let mut work = vec![Work::Visit(root)];
        let mut output = Vec::new();
        while let Some(item) = work.pop() {
            match item {
                Work::Visit(value) => match value {
                    RuntimeValue::Primitive(value) => output.push(OwnedValue::Primitive(value)),
                    RuntimeValue::Bytes(value) => output.push(OwnedValue::Bytes(value)),
                    RuntimeValue::Opaque(value) => {
                        output.push(OwnedValue::Opaque(value.into_portable()?.deep_clone()));
                    },
                    RuntimeValue::Object(children) => {
                        work.push(Work::Array(children.len()));
                        for id in children.into_iter().rev() {
                            work.push(Work::Visit(self.take(id)));
                        }
                    }
                    RuntimeValue::Map(children) => {
                        let (keys, children): (Vec<_>, Vec<_>) = children.into_iter().unzip();
                        work.push(Work::Map(keys));
                        for id in children.into_iter().rev() {
                            work.push(Work::Visit(self.take(id)));
                        }
                    }
                }
                Work::Array(len) => {
                    let children = output.split_off(output.len() - len);
                    output.push(OwnedValue::Object(children));
                }
                Work::Map(keys) => {
                    let children = output.split_off(output.len() - keys.len());
                    output.push(OwnedValue::Map(keys.into_iter().zip(children).collect()));
                }
            }
        }
    
        output.pop().ok_or_else(|| ValueError::ExpectedValue)
    }

    /// Detach shared descendants, reusing slots with no other owning handles.
    pub fn detach(&mut self, root: RuntimeValue) -> Result<RuntimeValue, HeapError> {
        if matches!(root, RuntimeValue::Primitive(_) | RuntimeValue::Bytes(_)) {
            return Ok(root);
        }

        enum Work {
            Visit(RuntimeValue),
            Array(Vec<ValuePointer>),
            Map(Box<IndexMap<OwnedValue, ValuePointer>>),
            Opaque(RuntimeOpaqueWrapper, usize),
        }

        fn take_child(id: &mut ValuePointer, heap: &mut Heap) -> Result<RuntimeValue, HeapError> {
            if id.is_empty() {
                return Err(HeapError::InvalidChildren);
            }

            if id.slot().strong_count() == 2 {
                Ok(mem::take(heap.get_mut(id)))
            } else {
                let owner = id.take()?;
                Ok(heap.get(&owner).clone())
            }
        }

        fn restore_child(id: &mut ValuePointer, value: RuntimeValue, heap: &mut Heap) {
            if id.is_empty() {
                *id = heap.insert(value);
            } else {
                *heap.get_mut(id) = value;
            }
        }

        let mut work = vec![Work::Visit(root)];
        let mut output = Vec::new();
        while let Some(item) = work.pop() {
            match item {
                Work::Visit(value) => match value {
                    RuntimeValue::Primitive(_) | RuntimeValue::Bytes(_) => output.push(value),
                    RuntimeValue::Opaque(mut value) => {
                        let mut children = Vec::new();
                        let iterative = value.visit_children_mut(&mut |child| {
                            let value = match child {
                                OpaqueChildMut::Value(value) => mem::take(value),
                                OpaqueChildMut::Pointer(id) => take_child(id, self)?,
                                OpaqueChildMut::Stack(value) => value.take().into_value(self),
                            };
                            children.push(value);
                            Ok(())
                        })?;
                        if !iterative && !children.is_empty() {
                            return Err(HeapError::InvalidChildren);
                        }
                        if iterative {
                            work.push(Work::Opaque(value, children.len()));
                            work.extend(children.into_iter().rev().map(Work::Visit));
                        } else {
                            output.push(RuntimeValue::Opaque(value.deep_clone_in(self)?));
                        }
                    },
                    RuntimeValue::Object(children) => {
                        let len = children.len();
                        let parent = work.len();
                        work.push(Work::Array(children));
                        for index in (0..len).rev() {
                            let Work::Array(children) = &mut work[parent] else {
                                unreachable!("array detachment frame");
                            };
                            let value = take_child(&mut children[index], self)?;
                            work.push(Work::Visit(value));
                        }
                    }
                    RuntimeValue::Map(children) => {
                        let len = children.len();
                        let parent = work.len();
                        work.push(Work::Map(children));
                        for index in (0..len).rev() {
                            let Work::Map(children) = &mut work[parent] else {
                                unreachable!("map detachment frame");
                            };
                            let (_, id) = children.get_index_mut(index).expect("map child index");
                            let value = take_child(id, self)?;
                            work.push(Work::Visit(value));
                        }
                    }
                }
                Work::Opaque(mut value, len) => {
                    {
                        let mut children = output.drain(output.len() - len..);
                        let iterative = value.visit_children_mut(&mut |child| {
                            let value = children.next().ok_or(HeapError::InvalidChildren)?;
                            match child {
                                OpaqueChildMut::Value(target) => *target = value,
                                OpaqueChildMut::Pointer(target) => restore_child(target, value, self),
                                OpaqueChildMut::Stack(target) => *target = value.into(),
                            }
                            Ok(())
                        })?;
                        if !iterative || children.next().is_some() {
                            return Err(HeapError::InvalidChildren);
                        }
                    }
                    output.push(RuntimeValue::Opaque(value));
                }
                Work::Array(mut children) => {
                    let start = output.len() - children.len();
                    for (id, value) in children.iter_mut().zip(output.drain(start..)) {
                        restore_child(id, value, self);
                    }
                    output.push(RuntimeValue::Object(children));
                }
                Work::Map(mut children) => {
                    let start = output.len() - children.len();
                    for (id, value) in children.values_mut().zip(output.drain(start..)) {
                        restore_child(id, value, self);
                    }
                    output.push(RuntimeValue::Map(children));
                }
            }
        }
        Ok(output.pop().expect("detached root"))
    }
}

