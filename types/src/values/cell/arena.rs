use std::{collections::HashSet, fmt};
use crate::{Opaque, OpaqueChild, OwnedValue, StackValue, ValueError};
use super::{RuntimeValue, ValuePointer, Heap, HeapError};

enum Node<'a> {
    Runtime(&'a RuntimeValue),
    Owned(&'a OwnedValue),
}

impl Heap {
    /// Import portable data into fresh slots in this heap.
    pub fn import(&mut self, value: &OwnedValue) -> Result<RuntimeValue, HeapError> {
        value.to_runtime_value(self)
    }

    /// Validate all direct and opaque-contained edges without recursion.
    pub fn validate_graph(&self, root: &RuntimeValue) -> Result<(), ValueError> {
        #[derive(Clone, Copy, PartialEq, Eq, Hash)]
        enum Identity {
            Runtime(*const RuntimeValue),
            Owned(*const OwnedValue),
            Opaque(*const ()),
        }
        enum Work<'a> {
            Enter(OpaqueChild<'a>),
            Leave(Identity),
        }
        let mut work = vec![Work::Enter(OpaqueChild::Value(root))];
        let mut active = HashSet::new();
        let mut complete = HashSet::new();
        while let Some(item) = work.pop() {
            let child = match item {
                Work::Leave(id) => {
                    active.remove(&id);
                    complete.insert(id);
                    continue;
                }
                Work::Enter(OpaqueChild::Pointer(id)) => OpaqueChild::Value(self.get(id)?),
                Work::Enter(child) => child,
            };
            let id = match child {
                OpaqueChild::Value(value) => Identity::Runtime(value),
                OpaqueChild::Owned(value) => Identity::Owned(value),
                OpaqueChild::Opaque(value) => Identity::Opaque(value as *const dyn Opaque as *const ()),
                OpaqueChild::Pointer(_) => unreachable!(),
            };
            if complete.contains(&id) { continue; }
            if !active.insert(id) { return Err(ValueError::CyclicError); }
            work.push(Work::Leave(id));
            match child {
                OpaqueChild::Value(RuntimeValue::Object(children)) => {
                    work.extend(children.iter().map(|id| Work::Enter(OpaqueChild::Pointer(*id))));
                }
                OpaqueChild::Value(RuntimeValue::Map(children)) => {
                    work.extend(children.iter().flat_map(|(key, id)| [
                        Work::Enter(OpaqueChild::Owned(key)),
                        Work::Enter(OpaqueChild::Pointer(*id)),
                    ]));
                }
                OpaqueChild::Value(RuntimeValue::Opaque(value)) | OpaqueChild::Owned(OwnedValue::Opaque(value)) => {
                    work.push(Work::Enter(OpaqueChild::Opaque(value.inner())));
                }
                OpaqueChild::Owned(OwnedValue::Object(children)) => {
                    work.extend(children.iter().map(|value| Work::Enter(OpaqueChild::Owned(value))));
                }
                OpaqueChild::Owned(OwnedValue::Map(children)) => {
                    work.extend(children.iter().flat_map(|(key, value)| [
                        Work::Enter(OpaqueChild::Owned(key)),
                        Work::Enter(OpaqueChild::Owned(value)),
                    ]));
                }
                OpaqueChild::Opaque(value) => value.visit_children(&mut |child| work.push(Work::Enter(child))),
                _ => {}
            }
        }
        Ok(())
    }

    /// Export a runtime graph, rejecting cycles and out-of-bounds heap handles.
    pub fn snapshot(&self, root: &RuntimeValue) -> Result<OwnedValue, ValueError> {
        self.validate_graph(root)?;
        enum Work<'a> {
            Visit(&'a RuntimeValue),
            Enter(ValuePointer),
            Leave(ValuePointer),
            Array(usize),
            Map(Vec<OwnedValue>),
        }
        let mut work = vec![Work::Visit(root)];
        let mut output = Vec::new();
        let mut active = HashSet::new();
        while let Some(item) = work.pop() {
            match item {
                Work::Visit(value) => match value {
                    RuntimeValue::Primitive(value) => output.push(OwnedValue::Primitive(value.clone())),
                    RuntimeValue::Opaque(value) => output.push(OwnedValue::Opaque(value.clone())),
                    RuntimeValue::Bytes(value) => output.push(OwnedValue::Bytes(value.clone())),
                    RuntimeValue::Object(children) => {
                        work.push(Work::Array(children.len()));
                        for child in children.iter().rev() {
                            work.push(Work::Enter(*child));
                        }
                    }
                    RuntimeValue::Map(children) => {
                        work.push(Work::Map(children.keys().cloned().collect()));
                        for child in children.values().rev() {
                            work.push(Work::Enter(*child));
                        }
                    }
                }
                Work::Enter(id) => {
                    if !active.insert(id) { return Err(ValueError::CyclicError); }
                    work.push(Work::Leave(id));
                    work.push(Work::Visit(self.get(id)?));
                }
                Work::Leave(id) => { active.remove(&id); }
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
        output.pop().ok_or(ValueError::ExpectedValue)
    }

    /// Export a consumed stack value, moving root buffers and detaching opaque
    /// payloads without rebuilding ordinary containers in intermediate slots.
    pub fn export(&mut self, value: StackValue) -> Result<OwnedValue, ValueError> {
        self.validate_graph(value.try_as_ref(self)?)?;
        enum Work {
            Visit(RuntimeValue),
            Array(usize),
            Map(Vec<OwnedValue>),
        }
        let root = value.raw_owned(self)?;
        let mut work = vec![Work::Visit(root)];
        let mut output = Vec::new();
        while let Some(item) = work.pop() {
            match item {
                Work::Visit(value) => match value {
                    RuntimeValue::Primitive(value) => output.push(OwnedValue::Primitive(value)),
                    RuntimeValue::Bytes(value) => output.push(OwnedValue::Bytes(value)),
                    RuntimeValue::Opaque(value) => output.push(OwnedValue::Opaque(value.deep_clone_in(self)?)),
                    RuntimeValue::Object(children) => {
                        work.push(Work::Array(children.len()));
                        for id in children.into_iter().rev() { work.push(Work::Visit(id.to_owned(self)?)); }
                    }
                    RuntimeValue::Map(children) => {
                        let (keys, children): (Vec<_>, Vec<_>) = children.into_iter().unzip();
                        work.push(Work::Map(keys));
                        for id in children.into_iter().rev() { work.push(Work::Visit(id.to_owned(self)?)); }
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
        output.pop().ok_or(ValueError::ExpectedValue)
    }

    /// Consume the root and copy its descendants into fresh heap slots.
    pub fn detach(&mut self, root: RuntimeValue) -> Result<RuntimeValue, HeapError> {
        enum Work {
            Visit(RuntimeValue),
            Array(usize),
            Map(Vec<OwnedValue>),
        }
        let mut work = vec![Work::Visit(root)];
        let mut output = Vec::new();
        while let Some(item) = work.pop() {
            match item {
                Work::Visit(value) => match value {
                    RuntimeValue::Primitive(_) | RuntimeValue::Bytes(_) => output.push(value),
                    RuntimeValue::Opaque(value) => output.push(RuntimeValue::Opaque(value.deep_clone_in(self)?)),
                    RuntimeValue::Object(children) => {
                        work.push(Work::Array(children.len()));
                        for id in children.into_iter().rev() { work.push(Work::Visit(id.to_owned(self)?)); }
                    }
                    RuntimeValue::Map(children) => {
                        let (keys, children): (Vec<_>, Vec<_>) = children.into_iter().unzip();
                        work.push(Work::Map(keys));
                        for id in children.into_iter().rev() { work.push(Work::Visit(id.to_owned(self)?)); }
                    }
                }
                Work::Array(len) => {
                    let children = output.split_off(output.len() - len)
                        .into_iter().map(|child| self.insert(child)).collect::<Result<_, _>>()?;
                    output.push(RuntimeValue::Object(children));
                }
                Work::Map(keys) => {
                    let children = output.split_off(output.len() - keys.len());
                    let map = keys.into_iter().zip(children).map(|(key, child)| Ok((key, self.insert(child)?)))
                        .collect::<Result<_, HeapError>>()?;
                    output.push(RuntimeValue::Map(Box::new(map)));
                }
            }
        }
        Ok(output.pop().expect("detached root"))
    }
}

impl RuntimeValue {
    pub fn equals(&self, other: &Self, heap: &Heap) -> bool {
        let mut work = vec![(self, other)];
        while let Some((left, right)) = work.pop() {
            match (left, right) {
                (Self::Primitive(a), Self::Primitive(b)) if a == b => {}
                (Self::Opaque(a), Self::Opaque(b)) if a == b => {}
                (Self::Bytes(a), Self::Bytes(b)) if a == b => {}
                (Self::Object(a), Self::Object(b)) if a.len() == b.len() => {
                    work.extend(a.iter().zip(b).map(|(a, b)| (a.as_ref(heap), b.as_ref(heap))));
                }
                (Self::Map(a), Self::Map(b)) if a.len() == b.len() => {
                    for ((ak, av), (bk, bv)) in a.iter().zip(b.iter()) {
                        if ak != bk { return false; }
                        work.push((av.as_ref(heap), bv.as_ref(heap)));
                    }
                }
                _ => return false,
            }
        }
        true
    }

    pub fn calculate_memory_usage(&self, heap: &Heap, max: usize) -> Result<usize, ValueError> {
        if let Self::Primitive(value) = self {
            return Ok(value.get_memory_usage());
        }
        if matches!(self, Self::Opaque(_)) {
            return Ok(8);
        }
        let mut memory = 0;
        let mut work = vec![Node::Runtime(self)];
        while let Some(node) = work.pop() {
            if memory > max { return Err(ValueError::MaxMemoryReached(memory, max)); }
            match node {
                Node::Runtime(Self::Primitive(value)) | Node::Owned(OwnedValue::Primitive(value)) => memory += 1 + value.get_memory_usage(),
                Node::Runtime(Self::Opaque(_)) | Node::Owned(OwnedValue::Opaque(_)) => memory += 9,
                Node::Runtime(Self::Bytes(value)) | Node::Owned(OwnedValue::Bytes(value)) => memory += 32 + value.len(),
                Node::Runtime(Self::Object(children)) => {
                    memory += 32;
                    work.extend(children.iter().map(|id| Node::Runtime(id.as_ref(heap))));
                }
                Node::Runtime(Self::Map(children)) => {
                    memory += 64;
                    work.extend(children.iter().flat_map(|(key, id)| [Node::Owned(key), Node::Runtime(id.as_ref(heap))]));
                }
                Node::Owned(OwnedValue::Object(children)) => {
                    memory += 32;
                    work.extend(children.iter().map(Node::Owned));
                }
                Node::Owned(OwnedValue::Map(children)) => {
                    memory += 64;
                    work.extend(children.iter().flat_map(|(key, value)| [Node::Owned(key), Node::Owned(value)]));
                }
            }
        }
        Ok(memory)
    }

    pub fn calculate_depth(&self, heap: &Heap, max: usize) -> Result<usize, ValueError> {
        let mut work = vec![(OpaqueChild::Value(self), 0)];
        let mut biggest = 0;
        while let Some((child, depth)) = work.pop() {
            if depth > max { return Err(ValueError::MaxDepthReached); }
            biggest = biggest.max(depth);
            let value = match child {
                OpaqueChild::Value(value) => value,
                OpaqueChild::Owned(value) => {
                    biggest = biggest.max(depth + value.calculate_depth(max.saturating_sub(depth))?);
                    continue;
                }
                OpaqueChild::Pointer(id) => heap.get(id)?,
                OpaqueChild::Opaque(value) => {
                    value.visit_children(&mut |child| work.push((child, depth + 1)));
                    continue;
                }
            };
            match value {
                Self::Object(children) => work.extend(children.iter().map(|id| (OpaqueChild::Pointer(*id), depth + 1))),
                Self::Map(children) => {
                    for (key, id) in children.iter() {
                        let key_depth = key.calculate_depth(max.saturating_sub(depth + 1))?;
                        biggest = biggest.max(depth + 1 + key_depth);
                        work.push((OpaqueChild::Pointer(*id), depth + 1));
                    }
                }
                Self::Opaque(value) => work.push((OpaqueChild::Opaque(value.inner()), depth)),
                _ => {}
            }
        }
        Ok(biggest)
    }

    pub fn display<'a>(&'a self, heap: &'a Heap) -> impl fmt::Display + 'a {
        struct Display<'a>(&'a RuntimeValue, &'a Heap);
        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.1.snapshot(self.0).map_err(|_| fmt::Error)?.fmt(f)
            }
        }
        Display(self, heap)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{OpaqueWrapper, PrimitiveInteger};

    #[test]
    fn handles_preserve_identity_and_removed_buffers() {
        let mut heap = Heap::new();
        let bytes = vec![1, 2, 3];
        let buffer = bytes.as_ptr();
        let id = heap.insert(RuntimeValue::Bytes(bytes)).unwrap();
        let mut array = RuntimeValue::Object(vec![id]);
        let removed = array.as_mut_vec().unwrap().swap_remove(0);
        assert_eq!(id, removed);
        assert_eq!(removed.as_ref(&heap).as_bytes().unwrap().as_ptr(), buffer);
        assert_eq!(std::mem::size_of::<ValuePointer>(), 4);
        let value: StackValue = removed.into();
        assert_eq!(value.estimate_memory_usage(&heap, 0).unwrap(), 8);
    }

    #[test]
    fn exporting_an_owned_byte_buffer_reuses_its_allocation() {
        let mut heap = Heap::new();
        let bytes = vec![1, 2, 3];
        let original = bytes.as_ptr();
        let result = heap.export(StackValue::Owned(RuntimeValue::Bytes(bytes))).unwrap();
        assert_eq!(result.as_bytes().unwrap().as_ptr(), original);
    }

    #[test]
    fn heap_rejects_out_of_bounds_handles() {
        let mut heap = Heap::new();
        heap.insert(PrimitiveInteger::U64(7).into()).unwrap();
        let id = ValuePointer { index: 1 };
        assert!(matches!(heap.get(id), Err(HeapError::InvalidPointer)));
        assert!(matches!(heap.get_mut(id), Err(HeapError::InvalidPointer)));
        assert!(matches!(heap.snapshot(&RuntimeValue::Object(vec![id])), Err(ValueError::HeapError(HeapError::InvalidPointer))));
        assert!(matches!(id.to_owned(&heap), Err(HeapError::InvalidPointer)));
        let mut value: StackValue = id.into();
        assert!(matches!(value.to_owned(&mut heap), Err(HeapError::InvalidPointer)));
        assert!(matches!(value.make_owned(&mut heap), Err(HeapError::InvalidPointer)));
        assert_eq!(value.id(), Some(id));
    }

    #[test]
    fn snapshots_allow_shared_children_and_reject_cycles() {
        let mut heap = Heap::new();
        let id = heap.insert(PrimitiveInteger::U64(42).into()).unwrap();
        let root = RuntimeValue::Object(vec![id, id]);
        let snapshot = heap.snapshot(&root).unwrap();
        assert_eq!(snapshot.as_vec().unwrap()[0].as_u64().unwrap(), 42);
        assert_eq!(snapshot.as_vec().unwrap()[1].as_u64().unwrap(), 42);
        *heap.get_mut(id).unwrap() = RuntimeValue::Object(vec![id]);
        assert!(matches!(heap.snapshot(&root), Err(ValueError::CyclicError)));
        assert!(matches!(root.calculate_depth(&heap, 16), Err(ValueError::MaxDepthReached)));
    }

    #[test]
    fn explicit_deep_clone_detaches_nested_slots() {
        let mut heap = Heap::new();
        let shared = heap.insert(RuntimeValue::Bytes(vec![1, 2, 3])).unwrap();
        let map = RuntimeValue::Map(Box::new([(OwnedValue::from(7u8), shared)].into_iter().collect()));
        let map = heap.insert(map).unwrap();
        let original = RuntimeValue::Object(vec![shared, map]);
        let detached = heap.detach(original.clone_ref()).unwrap();
        let children = detached.as_vec().unwrap();
        let detached_first = children[0];
        let detached_map = children[1];
        assert_ne!(detached_first, shared);
        let nested = *detached_map.as_ref(&heap).as_map().unwrap().values().next().unwrap();
        assert_ne!(nested, shared);
        assert_ne!(nested, detached_first);
        *heap.get_mut(detached_first).unwrap() = RuntimeValue::Bytes(vec![9]);
        assert_eq!(shared.as_ref(&heap).as_bytes().unwrap(), &[1, 2, 3]);
        assert_eq!(nested.as_ref(&heap).as_bytes().unwrap(), &[1, 2, 3]);
    }

    #[test]
    fn opaque_edges_participate_in_cycle_and_bounds_validation() {
        #[derive(Debug, Clone, Opaque)]
        #[opaque(children = std::iter::once(self.0))]
        struct Retained(ValuePointer);
        let mut heap = Heap::new();
        let id = heap.insert(RuntimeValue::default()).unwrap();
        *heap.get_mut(id).unwrap() = RuntimeValue::Opaque(OpaqueWrapper::new(Retained(id)));
        let root = RuntimeValue::Object(vec![id]);
        assert!(matches!(heap.snapshot(&root), Err(ValueError::CyclicError)));
        let missing = ValuePointer { index: heap.len() as u32 };
        let opaque = RuntimeValue::Opaque(OpaqueWrapper::new(Retained(missing)));
        assert!(matches!(heap.snapshot(&opaque), Err(ValueError::HeapError(HeapError::InvalidPointer))));
    }

    #[test]
    fn clearing_a_heap_resets_its_storage() {
        let mut heap = Heap::new();
        {
            let id = heap.insert(PrimitiveInteger::U64(1).into()).unwrap();
            assert_eq!(heap.get(id).unwrap().as_u64().unwrap(), 1);
        }
        heap.clear();
        assert!(heap.is_empty());
        assert_eq!(heap.len(), 0);
        let id = heap.insert(PrimitiveInteger::U64(2).into()).unwrap();
        assert_eq!(heap.get(id).unwrap().as_u64().unwrap(), 2);
    }

    #[test]
    fn heap_growth_keeps_handles_valid() {
        let mut heap = Heap::new();
        let first = heap.insert(PrimitiveInteger::U64(42).into()).unwrap();
        for i in 0..10_000 {
            heap.insert(PrimitiveInteger::U64(i).into()).unwrap();
        }
        assert_eq!(heap.get(first).unwrap().as_u64().unwrap(), 42);
        *heap.get_mut(first).unwrap() = PrimitiveInteger::U64(99).into();
        assert_eq!(heap.get(first).unwrap().as_u64().unwrap(), 99);
    }
}
