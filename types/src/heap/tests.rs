use std::{collections::HashSet, mem, sync::{Arc, Mutex, atomic::{AtomicUsize, Ordering}}};
use indexmap::IndexMap;
use crate::{RuntimeOpaqueWrapper,Opaque, OpaqueWrapper, OwnedValue, PrimitiveInteger, StackValue};
use super::*;

#[derive(Debug, Clone, Opaque)]
struct Probe(Arc<AtomicUsize>);
impl Drop for Probe {
    fn drop(&mut self) {
        self.0.fetch_add(1, Ordering::Relaxed);
    }
}

fn probe(heap: &mut Heap, drops: &Arc<AtomicUsize>) -> ValuePointer {
    heap.insert(RuntimeValue::Opaque(OpaqueWrapper::new(Probe(drops.clone())).into()))
}

#[test]
fn ordinary_drop_reclaims_the_last_owner_and_removes_its_slot() {
    let mut heap = unsafe { Heap::new() };
    let drops = Arc::new(AtomicUsize::new(0));
    let id = probe(&mut heap, &drops);
    let alias = id.clone();
    assert_eq!(id.slot().strong_count() - 1, 2);
    drop(id);
    assert_eq!(drops.load(Ordering::Relaxed), 0);
    drop(alias);
    assert_eq!(drops.load(Ordering::Relaxed), 1);
    assert!(heap.is_empty());
    let next = probe(&mut heap, &drops);
    assert!(heap.get(&next).as_opaque_type::<Probe>().is_ok());
    assert_eq!(heap.len(), 1);
    assert_eq!(mem::size_of::<ValuePointer>(), 8);
}

#[test]
fn pointer_identity_is_preserved_when_values_are_equal_or_mutated() {
    let mut heap = unsafe { Heap::new() };
    let first = heap.insert(PrimitiveInteger::U64(42).into());
    let second = heap.insert(PrimitiveInteger::U64(42).into());
    let handles = HashSet::from([first.clone(), first.clone(), second.clone()]);
    assert_eq!(handles.len(), 2);
    *heap.get_mut(&first) = PrimitiveInteger::U64(7).into();
    assert!(handles.contains(&first));
    assert!(handles.contains(&second));
    drop(first);
    drop(second);
    assert_eq!(heap.len(), 2);
    drop(handles);
    assert!(heap.is_empty());
}

#[test]
fn shared_children_outlive_their_parents() {
    let mut heap = unsafe { Heap::new() };
    let child = heap.insert(PrimitiveInteger::U64(42).into());
    let left = heap.insert(RuntimeValue::Object(vec![child.clone()]));
    let right = heap.insert(RuntimeValue::Object(vec![child.clone()]));
    drop(left);
    drop(right);
    assert_eq!(child.slot().strong_count() - 1, 1);
    assert_eq!(heap.get(&child).as_u64().unwrap(), 42);
    drop(child);
    assert!(heap.is_empty());
}

#[test]
fn trait_clone_automatically_retains_runtime_children() {
    let mut heap = unsafe { Heap::new() };
    let id = heap.insert(PrimitiveInteger::U64(42).into());
    let value = RuntimeValue::Object(vec![id]);
    let copy = Clone::clone(&value);
    drop(value);
    assert_eq!(copy.as_vec().unwrap()[0].as_ref(&heap).as_u64().unwrap(), 42);
    drop(copy);
    assert!(heap.is_empty());
}

#[test]
fn take_moves_unique_payload_and_clones_shared_payload() {
    let mut heap = unsafe { Heap::new() };
    let child = heap.insert(PrimitiveInteger::U64(42).into());
    let parent = heap.insert(RuntimeValue::Object(vec![child]));
    let alias = parent.clone();
    let copy = heap.take(parent);
    let child = &copy.as_vec().unwrap()[0];
    assert_eq!(child.slot().strong_count() - 1, 2);
    drop(alias);
    assert_eq!(child.slot().strong_count() - 1, 1);
    drop(copy);
    assert!(heap.is_empty());
    let bytes = vec![1, 2, 3];
    let buffer = bytes.as_ptr();
    let id = heap.insert(RuntimeValue::Bytes(bytes));
    let moved = heap.take(id);
    assert_eq!(moved.as_bytes().unwrap().as_ptr(), buffer);
    assert!(heap.is_empty());
}

#[test]
fn repeated_allocate_drop_workloads_leave_no_slots() {
    let mut heap = unsafe { Heap::new() };
    let drops = Arc::new(AtomicUsize::new(0));
    for _ in 0..10_000 {
        let id = probe(&mut heap, &drops);
        drop(id);
        assert!(heap.is_empty());
    }
    assert_eq!(heap.storage.get().slots.len(), 0);
    assert_eq!(drops.load(Ordering::Relaxed), 10_000);
}

#[test]
fn deep_chains_are_released_iteratively() {
    let mut heap = unsafe { Heap::new() };
    let mut root = heap.insert(RuntimeValue::default());
    for _ in 0..20_000 {
        root = heap.insert(RuntimeValue::Object(vec![root]));
    }
    assert_eq!(heap.len(), 20_001);
    drop(root);
    assert!(heap.is_empty());
}

#[test]
fn maps_release_shared_payloads_once() {
    let mut heap = unsafe { Heap::new() };
    let drops = Arc::new(AtomicUsize::new(0));
    let child = probe(&mut heap, &drops);
    drop(RuntimeValue::Map(Box::new(IndexMap::from_iter([(OwnedValue::from(0u8), child.clone()), (OwnedValue::from(1u8), child)]))));
    assert_eq!(drops.load(Ordering::Relaxed), 1);
    assert!(heap.is_empty());
}

#[test]
fn clearing_frees_payloads_before_escaped_handles_are_dropped() {
    let mut heap = unsafe { Heap::new() };
    let drops = Arc::new(AtomicUsize::new(0));
    let id = probe(&mut heap, &drops);
    heap.clear();
    assert_eq!(drops.load(Ordering::Relaxed), 1);
    let fresh = heap.insert(PrimitiveInteger::U64(7).into());
    assert_ne!(fresh.slot().as_ptr(), id.slot().as_ptr());
    assert!(id.slot().get().is_null());
    assert_eq!(heap.get(&fresh).as_u64().unwrap(), 7);
    let expired_copy = id.clone();
    drop(id);
    drop(expired_copy);
    assert_eq!(drops.load(Ordering::Relaxed), 1);
}

#[test]
fn taking_the_last_owner_removes_its_slot() {
    let mut heap = unsafe { Heap::new() };
    let id = heap.insert(PrimitiveInteger::U64(1).into());
    let value = heap.take(id);
    assert_eq!(value.as_u64().unwrap(), 1);
    assert!(heap.is_empty());
    let next = heap.insert(PrimitiveInteger::U64(2).into());
    assert_eq!(heap.len(), 1);
    assert_eq!(heap.get(&next).as_u64().unwrap(), 2);
}

#[derive(Debug, Clone, Opaque)]
#[opaque(runtime, children = std::iter::once(&self.0))]
struct Retained(ValuePointer);

#[test]
fn runtime_opaque_clones_retain_handles_but_cannot_be_exported() {
    let mut heap = unsafe { Heap::new() };
    let id = heap.insert(PrimitiveInteger::U64(42).into());
    let value = RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(Retained(id)));
    let copy = value.clone();
    assert!(matches!(heap.snapshot(&value), Err(ValueError::HeapError(HeapError::NonPortableValue))));
    drop(value);
    assert_eq!(copy.as_opaque_type::<Retained>().unwrap().0.as_ref(&heap).as_u64().unwrap(), 42);
    drop(copy);
    assert!(heap.is_empty());
}

#[test]
fn replacing_an_object_releases_its_previous_children() {
    let mut heap = unsafe { Heap::new() };
    let drops = Arc::new(AtomicUsize::new(0));
    let child = probe(&mut heap, &drops);
    let id = heap.insert(RuntimeValue::Object(vec![child]));
    let mut owner: StackValue = id.into();
    heap.replace_stack(&mut owner, PrimitiveInteger::U64(2).into());
    assert_eq!(drops.load(Ordering::Relaxed), 1);
    assert_eq!(owner.as_u64(&heap).unwrap(), 2);
    drop(owner);
    assert!(heap.is_empty());
}


#[test]
fn sequential_clones_and_final_drops_reclaim_every_payload() {
    let mut heap = unsafe { Heap::new() };
    let drops = Arc::new(AtomicUsize::new(0));
    let handles: Vec<_> = (0..256).map(|_| probe(&mut heap, &drops)).collect();
    for _ in 0..8 {
        let copies = handles.clone();
        for _ in 0..100 {
            let temporary = copies.clone();
            drop(temporary);
        }
        drop(copies);
    }
    drop(handles);
    assert!(heap.is_empty());
    assert_eq!(drops.load(Ordering::Relaxed), 256);
}

#[test]
fn heap_drop_before_sequential_handle_drops_releases_every_payload() {
    let mut heap = unsafe { Heap::new() };
    let drops = Arc::new(AtomicUsize::new(0));
    let handles: Vec<_> = (0..32).map(|_| probe(&mut heap, &drops)).collect();
    drop(heap);
    assert_eq!(drops.load(Ordering::Relaxed), 32);
    drop(handles);
    assert_eq!(drops.load(Ordering::Relaxed), 32);
}


#[test]
fn cycles_are_freed_at_heap_drop_even_with_escaped_handles() {
    let mut heap = unsafe { Heap::new() };
    let drops = Arc::new(AtomicUsize::new(0));
    let root = heap.insert(RuntimeValue::default());
    let payload = probe(&mut heap, &drops);
    *heap.get_mut(&root) = RuntimeValue::Object(vec![root.clone(), payload]);
    assert_eq!(heap.len(), 2);
    drop(heap);
    assert_eq!(drops.load(Ordering::Relaxed), 1);
    drop(root);
}

#[test]
fn release_matches_reachability_for_generated_acyclic_graphs() {
    const NODES: usize = 32;
    let mut seed = 1u64;
    let mut random = || {
        seed = seed.wrapping_mul(6364136223846793005).wrapping_add(1);
        (seed >> 32) as usize
    };
    for _ in 0..128 {
        let mut heap = unsafe { Heap::new() };
        let mut roots: Vec<_> = (0..NODES).map(|_| Some(heap.insert(RuntimeValue::default()))).collect();
        let mut edges = vec![Vec::new(); NODES];
        for index in 0..NODES {
            if index + 1 < NODES {
                for _ in 0..random() % 4 {
                    edges[index].push(index + 1 + random() % (NODES - index - 1));
                }
            }
            let children = edges[index].iter().map(|child| roots[*child].as_ref().unwrap().clone()).collect();
            *heap.get_mut(roots[index].as_ref().unwrap()) = RuntimeValue::Object(children);
        }
        let mut work = Vec::new();
        for (index, root) in roots.iter_mut().enumerate() {
            if random() % 8 == 0 {
                work.push(index);
            } else {
                drop(root.take());
            }
        }
        let mut reachable = [false; NODES];
        while let Some(index) = work.pop() {
            if reachable[index] {
                continue;
            }
            reachable[index] = true;
            work.extend(&edges[index]);
        }
        assert_eq!(heap.len(), reachable.iter().filter(|live| **live).count());
        drop(roots);
        assert!(heap.is_empty());
    }
}

#[derive(Debug, Clone, Opaque)]
#[opaque(runtime)]
struct AllocateDuringDrop {
    heap: Arc<Mutex<Heap>>,
    drops: Arc<AtomicUsize>,
}

impl Drop for AllocateDuringDrop {
    fn drop(&mut self) {
        let mut heap = self.heap.lock().unwrap();
        assert!(heap.is_empty());
        let value = probe(&mut heap, &self.drops);
        drop(value);
    }
}

#[test]
fn nested_destructors_can_allocate_and_drop_values() {
    // The mutex only provides this test fixture with shared access to its Heap;
    // every operation and destructor still runs sequentially on this thread.
    let heap = Arc::new(Mutex::new(unsafe { Heap::new() }));
    let drops = Arc::new(AtomicUsize::new(0));
    let allocator = heap.lock().unwrap().insert(RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(
        AllocateDuringDrop {
            heap: heap.clone(),
            drops: drops.clone(),
        },
    )));
    let root = heap.lock().unwrap().insert(RuntimeValue::Object(vec![allocator]));
    drop(root);
    assert_eq!(drops.load(Ordering::Relaxed), 1);
    let heap = heap.lock().unwrap();
    assert!(heap.is_empty());
    assert_eq!(heap.storage.get().slots.len(), 0);
}

#[test]
fn portable_values_remain_send_and_sync() {
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<OwnedValue>();
}

#[test]
fn consuming_portable_inputs_moves_nested_buffers() {
    let mut heap = unsafe { Heap::new() };
    let bytes = vec![7; 1024];
    let address = bytes.as_ptr();
    let value = heap.import_owned(OwnedValue::Object(vec![OwnedValue::Bytes(bytes)]));
    let id = &value.as_vec().unwrap()[0];
    assert_eq!(heap.get(id).as_bytes().unwrap().as_ptr(), address);
    let result = heap.export(value.into()).unwrap();
    assert_eq!(result.as_vec().unwrap()[0].as_bytes().unwrap().as_ptr(), address);
}

#[derive(Debug, Clone, Opaque)]
#[opaque(runtime, children_mut = self.visit(visitor))]
struct MutableOpaqueChildren {
    value: RuntimeValue,
    pointer: ValuePointer,
    stack: StackValue,
}

impl MutableOpaqueChildren {
    fn visit(&mut self, visitor: &mut dyn FnMut(OpaqueChildMut<'_>) -> Result<(), HeapError>) -> Result<(), HeapError> {
        visitor(OpaqueChildMut::Value(&mut self.value))?;
        visitor(OpaqueChildMut::Pointer(&mut self.pointer))?;
        visitor(OpaqueChildMut::Stack(&mut self.stack))
    }
}

#[test]
fn mutable_opaque_children_detach_all_supported_edge_types() {
    let mut heap = unsafe { Heap::new() };
    {
        let original = heap.insert(PrimitiveInteger::U64(42).into());
        let value = MutableOpaqueChildren {
            value: RuntimeValue::Object(vec![original.clone()]),
            pointer: original.clone(),
            stack: original.clone().into(),
        };
        let detached = heap.detach(RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(value))).unwrap();
        *heap.get_mut(&original) = PrimitiveInteger::U64(99).into();
        let value = detached.as_opaque_type::<MutableOpaqueChildren>().unwrap();
        assert_eq!(value.value.as_vec().unwrap()[0].as_ref(&heap).as_u64().unwrap(), 42);
        assert_eq!(value.pointer.as_ref(&heap).as_u64().unwrap(), 42);
        assert_eq!(value.stack.as_u64(&heap).unwrap(), 42);
    }
    assert!(heap.is_empty());
}

#[derive(Debug, Clone, Opaque)]
#[opaque(runtime, children_mut = self.visit(visitor))]
struct UnstableOpaqueChildren {
    pointer: ValuePointer,
    visited: bool,
}

impl UnstableOpaqueChildren {
    fn visit(&mut self, visitor: &mut dyn FnMut(OpaqueChildMut<'_>) -> Result<(), HeapError>) -> Result<(), HeapError> {
        if !self.visited {
            self.visited = true;
            visitor(OpaqueChildMut::Pointer(&mut self.pointer))?;
        }
        Ok(())
    }
}

#[test]
fn mutable_opaque_child_mismatches_return_an_error_and_release_values() {
    let mut heap = unsafe { Heap::new() };
    let value = UnstableOpaqueChildren {
        pointer: heap.insert(PrimitiveInteger::U64(42).into()),
        visited: false,
    };
    assert!(matches!(heap.detach(RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(value))), Err(HeapError::InvalidChildren)));
    assert!(heap.is_empty());
}

#[test]
fn detachment_reuses_unique_slots_but_still_detaches_shared_grandchildren() {
    let mut heap = unsafe { Heap::new() };
    {
        let shared = heap.insert(PrimitiveInteger::U64(42).into());
        let parent = heap.insert(RuntimeValue::Object(vec![shared.clone()]));
        let address = parent.slot().as_ptr();
        let detached = heap.detach(RuntimeValue::Object(vec![parent])).unwrap();
        let parent = &detached.as_vec().unwrap()[0];
        assert_eq!(parent.slot().as_ptr(), address);
        let child = &heap.get(parent).as_vec().unwrap()[0];
        assert!(!child.ptr_eq(&shared));
        *heap.get_mut(&shared) = PrimitiveInteger::U64(99).into();
        let child = &heap.get(parent).as_vec().unwrap()[0];
        assert_eq!(heap.get(child).as_u64().unwrap(), 42);
    }
    assert!(heap.is_empty());
}

#[test]
fn detachment_splits_aliases_within_a_consumed_graph() {
    let mut heap = unsafe { Heap::new() };
    {
        let child = heap.insert(PrimitiveInteger::U64(42).into());
        let detached = heap.detach(RuntimeValue::Object(vec![child.clone(), child])).unwrap();
        let values = detached.as_vec().unwrap();
        assert!(!values[0].ptr_eq(&values[1]));
        *heap.get_mut(&values[0]) = PrimitiveInteger::U64(99).into();
        assert_eq!(heap.get(&values[1]).as_u64().unwrap(), 42);
    }
    assert!(heap.is_empty());
}
