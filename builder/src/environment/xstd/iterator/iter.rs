use silex_types::RuntimeOpaqueWrapper;
#[cfg(test)]
use silex_types::OpaqueWrapper;
use std::{collections::VecDeque, fmt::{Debug, Display, Formatter, Result as FmtResult}, mem};

use silex_environment::{EnvironmentError};
use silex_types::{
    PrimitiveInteger,
    Opaque,
    OpaqueChild,
    OpaqueChildMut,
    Primitive,
    StackValue,
    RuntimeValue,
    ValuePointer,
    Heap,
    HeapError,
};

/// Leaf generator: produces items without consuming another `XIterator`.
#[derive(Debug, Clone, Default)]
pub enum BaseSource {
    /// Iterates over a pre-collected dequeue of values.
    Dequeue(VecDeque<ValuePointer>),
    /// Yields exactly one value, then is exhausted.
    Once(Option<ValuePointer>),
    /// Always empty.
    #[default]
    Empty,
    /// Generates items via `closure(state) -> optional<(item, next_state)>`.
    Unfold { closure: StackValue, state: Option<StackValue> },
}

/// One adapter step in the flat pipeline stored inside `XIterator::pipe`.
///
/// Steps are applied in order from front (first applied) to back (last applied)
/// relative to the items produced by the `source`.
/// All adapter logic that used to be recursive `Box<XIterator>` nesting is now
/// a flat `VecDeque<PipeStep>`, making `Clone` and `Drop` trivial.
#[derive(Debug, Clone)]
pub enum PipeStep {
    /// Apply `closure` to every item (requires VM callback).
    Map(StackValue),
    /// Keep only items where `closure` returns `true` (requires VM callback).
    Filter(StackValue),
    /// Drop the first `n` items.
    Skip(usize),
    /// Yield at most `n` items.
    Take(usize),
    /// Pair each item with its zero-based index starting from `counter`.
    Enumerate(u32),
    /// Yield all items of `self`'s current source, then all items of `next`.
    Chain(Box<XIterator>),
    /// Zip items with `right`; stops when the shorter side is exhausted.
    Zip(Box<XIterator>),
    /// Flatten one level: each item is expanded as an array or Iterator.
    ///
    /// `pending` holds the remaining inner elements from the current expansion.
    /// When `pending` is empty the next outer item is pulled and expanded;
    /// when non-empty the front element is emitted directly.
    Flatten(VecDeque<ValuePointer>),
    /// Collect all upstream items then reverse them.
    Rev,
}

/// Maximum number of pipe steps an iterator can have.
/// Prevents abuse from scripts building extremely long adapter chains
pub const MAX_PIPE_STEPS: usize = 128;

/// Flat lazy iterator.
///
/// `source` is the leaf generator; `pipe` is the ordered list of adapter steps.
/// All adapter functions (map, filter, skip, take, …) simply push a `PipeStep`
/// onto `pipe` — O(1), no allocation of `Box<XIterator>`.
///
/// Ordinary `Clone` preserves shared payload pointers. Opaque ownership
/// conversion uses the heap walker to detach those payloads independently.
/// Clone and Drop drain nested iterators iteratively.
#[derive(Default, Opaque)]
#[opaque(runtime, name = "Iterator", display, children = self.children(), children_mut = self.visit_runtime_children(visitor), deep_clone_in = {
    let value = heap.detach(RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(self.clone())))?;
    value.into_opaque_type::<Self>().map_err(|_| HeapError::InvalidChildren)?
})]
pub struct XIterator {
    pub(crate) source: BaseSource,
    pub(crate) pipe: VecDeque<PipeStep>,
}

impl Debug for XIterator {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let source = match &self.source {
            BaseSource::Dequeue(_) => "Dequeue",
            BaseSource::Once(_) => "Once",
            BaseSource::Empty => "Empty",
            BaseSource::Unfold { .. } => "Unfold",
        };
        f.debug_struct("XIterator").field("source", &source).field("steps", &self.pipe.len()).finish()
    }
}

impl Display for XIterator {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "Iterator")
    }
}

impl XIterator {
    fn children(&self) -> impl Iterator<Item = OpaqueChild<'_>> {
        let items = match &self.source {
            BaseSource::Dequeue(items) => Some(items),
            _ => None,
        };
        let source = match &self.source {
            BaseSource::Once(item) => [item.as_ref().map(OpaqueChild::Pointer), None],
            BaseSource::Unfold { closure, state } => [Some(closure.into()), state.as_ref().map(OpaqueChild::from)],
            _ => [None, None],
        };
        let pipe = self.pipe.iter().flat_map(|step| {
            let (value, items) = match step {
                PipeStep::Map(value) | PipeStep::Filter(value) => (Some(value.into()), None),
                PipeStep::Chain(value) | PipeStep::Zip(value) => (Some(OpaqueChild::Opaque(&**value)), None),
                PipeStep::Flatten(items) => (None, Some(items)),
                _ => (None, None),
            };
            value.into_iter().chain(items.into_iter().flat_map(|items| items.iter().map(OpaqueChild::Pointer)))
        });
        items.into_iter().flat_map(|items| items.iter().map(OpaqueChild::Pointer))
            .chain(source.into_iter().flatten()).chain(pipe)
    }

    fn visit_runtime_children(&mut self, visitor: &mut dyn FnMut(OpaqueChildMut<'_>) -> Result<(), HeapError>) -> Result<(), HeapError> {
        enum Child<'a> {
            Iterator(&'a mut XIterator),
            Value(OpaqueChildMut<'a>),
        }
        fn children(iter: &mut XIterator) -> impl Iterator<Item = Child<'_>> {
            let (items, source) = match &mut iter.source {
                BaseSource::Dequeue(items) => (Some(items), [None, None]),
                BaseSource::Once(item) => (None, [item.as_mut().map(|id| Child::Value(OpaqueChildMut::Pointer(id))), None]),
                BaseSource::Unfold { closure, state } => (None, [
                    Some(Child::Value(OpaqueChildMut::Stack(closure))),
                    state.as_mut().map(|value| Child::Value(OpaqueChildMut::Stack(value))),
                ]),
                BaseSource::Empty => (None, [None, None]),
            };
            let pipe = iter.pipe.iter_mut().flat_map(|step| {
                let (child, items) = match step {
                    PipeStep::Map(value) | PipeStep::Filter(value) => (Some(Child::Value(OpaqueChildMut::Stack(value))), None),
                    PipeStep::Chain(child) | PipeStep::Zip(child) => (Some(Child::Iterator(&mut **child)), None),
                    PipeStep::Flatten(items) => (None, Some(items)),
                    _ => (None, None),
                };
                child.into_iter().chain(items.into_iter().flat_map(|items| items.iter_mut().map(|id| Child::Value(OpaqueChildMut::Pointer(id)))))
            });
            items.into_iter().flat_map(|items| items.iter_mut().map(|id| Child::Value(OpaqueChildMut::Pointer(id))))
                .chain(source.into_iter().flatten()).chain(pipe)
        }
        let mut current = children(self);
        let mut parents = Vec::new();
        loop {
            match current.next() {
                Some(Child::Value(child)) => visitor(child)?,
                Some(Child::Iterator(iter)) => parents.push(mem::replace(&mut current, children(iter))),
                None => match parents.pop() {
                    Some(parent) => current = parent,
                    None => return Ok(()),
                },
            }
        }
    }

    #[inline(always)]
    pub fn from_dequeue(items: VecDeque<ValuePointer>) -> Self {
        Self { source: BaseSource::Dequeue(items), pipe: VecDeque::new() }
    }

    #[inline(always)]
    pub fn once(value: RuntimeValue, heap: &mut Heap) -> Result<Self, HeapError> {
        Ok(Self { source: BaseSource::Once(Some(heap.insert(value))), pipe: VecDeque::new() })
    }

    #[inline(always)]
    pub fn empty() -> Self {
        Self { source: BaseSource::Empty, pipe: VecDeque::new() }
    }

    /// Push a pipe step, enforcing the maximum pipeline depth.
    #[inline]
    pub(crate) fn push_step(&mut self, step: PipeStep) -> Result<(), EnvironmentError> {
        if self.pipe.len() >= MAX_PIPE_STEPS {
            return Err(EnvironmentError::Static("iterator pipeline too deep"));
        }
        self.pipe.push_back(step);
        Ok(())
    }

    /// Returns true if advancing this iterator requires executing a VM closure.
    /// Iterative: walks the entire tree of nested Chain/Zip iterators without recursion.
    pub fn needs_callback(&self) -> bool {
        let mut to_check = Vec::new();
        let mut iter = self;
        loop {
            if matches!(iter.source, BaseSource::Unfold { .. }) {
                return true;
            }
            for step in &iter.pipe {
                match step {
                    PipeStep::Map(_) | PipeStep::Filter(_) | PipeStep::Rev => return true,
                    PipeStep::Chain(c) => to_check.push(c),
                    PipeStep::Zip(z) => to_check.push(z),
                    PipeStep::Flatten(_) => {}
                    _ => {}
                }
            }
            match to_check.pop() {
                Some(next) => iter = next,
                None => return false,
            }
        }
    }

    /// Advance a fully-sync iterator (no closures anywhere in the pipe) without VM callbacks.
    ///
    /// Fully iterative: uses an explicit stack instead of recursive calls for
    /// `Chain` and `Zip` steps. `Flatten(opaque)` drains inner items into the
    /// pending buffer inline (consistent with the array path).
    ///
    /// Mutates `self` in-place. Returns `Err` if any step requires a callback.
    pub fn next_sync(&mut self, heap: &mut Heap) -> Result<Option<ValuePointer>, EnvironmentError> {
        enum ResumeKind {
            Chain,
            ZipRight(ValuePointer),
            Flatten,
        }
        struct Frame {
            iter: XIterator,
            pipe_idx: usize,
            kind: ResumeKind,
        }
        enum Step {
            Descend { child: XIterator, pipe_idx: usize, kind: ResumeKind },
            Yield(Option<ValuePointer>),
        }

        fn advance(iter: &mut XIterator, mut resume: Option<(usize, Option<ValuePointer>)>, heap: &mut Heap) -> Result<Step, EnvironmentError> {
            // Process this iterator level until it produces a result.
            'outer: loop {
                let (mut pipe_idx, mut current) = if let Some(r) = resume.take() {
                    r
                } else {
                    // Pull one item from the source.
                    let item = match &mut iter.source {
                        BaseSource::Dequeue(d) => d.pop_front(),
                        BaseSource::Once(slot) => slot.take(),
                        BaseSource::Empty => None,
                        BaseSource::Unfold { .. } =>
                            return Err(EnvironmentError::Static("next_sync called on closure-based iterator")),
                    };
                    (0, item)
                };

                // Walk the pipe, transforming / filtering items.
                while pipe_idx < iter.pipe.len() {
                    // Handle Flatten with pending inner elements *before* the main match.
                    let pending_front = if let PipeStep::Flatten(p) = &mut iter.pipe[pipe_idx] {
                        p.pop_front()
                    } else {
                        None
                    };

                    if let Some(inner_item) = pending_front {
                        if let Some(outer_item) = current.take() {
                            match &mut iter.source {
                                BaseSource::Dequeue(d) => d.push_front(outer_item),
                                BaseSource::Once(slot) if slot.is_none() => *slot = Some(outer_item),
                                _ => {
                                    let old = std::mem::replace(&mut iter.source, BaseSource::Empty);
                                    let mut d = VecDeque::new();
                                    d.push_front(outer_item);
                                    match old {
                                        BaseSource::Dequeue(rest) => d.extend(rest),
                                        _ => {},
                                    }
                                    iter.source = BaseSource::Dequeue(d);
                                }
                            }
                        }
                        current = Some(inner_item);
                        pipe_idx += 1;
                        continue;
                    }

                    match &mut iter.pipe[pipe_idx] {
                        PipeStep::Skip(n) => {
                            if *n > 0 {
                                if current.is_some() {
                                    *n -= 1;
                                    continue 'outer;
                                }
                            }
                        }
                        PipeStep::Take(remaining) => {
                            if current.is_none() {
                            } else if *remaining == 0 {
                                current = None;
                            } else {
                                *remaining -= 1;
                            }
                        }
                        PipeStep::Enumerate(counter) => {
                            if let Some(item) = current.take() {
                                let idx = *counter;
                                *counter += 1;
                                let pair = RuntimeValue::Object(vec![
                                    heap.insert(Primitive::Integer(PrimitiveInteger::U32(idx)).into()),
                                    item,
                                ]);
                                current = Some(heap.insert(pair));
                            }
                        }
                        PipeStep::Chain(next_iter) => {
                            if current.is_none() {
                                let child = mem::take(&mut **next_iter);
                                return Ok(Step::Descend { child, pipe_idx, kind: ResumeKind::Chain });
                            }
                        }
                        PipeStep::Zip(right) => {
                            match current.take() {
                                None => {
                                    // Left exhausted → zip produces None.
                                }
                                Some(l) => {
                                    let child = mem::take(&mut **right);
                                    return Ok(Step::Descend { child, pipe_idx, kind: ResumeKind::ZipRight(l) });
                                }
                            }
                        }
                        PipeStep::Flatten(pending) => {
                            // pending is empty here (non-empty case handled above).
                            if let Some(ptr) = current.take() {
                                match heap.take(ptr) {
                                    RuntimeValue::Object(elements) => {
                                        *pending = elements.into();
                                        current = pending.pop_front();
                                    }
                                    RuntimeValue::Opaque(w) => {
                                        // Drain all items from the inner iterator into
                                        // `pending`, consistent with the array path.
                                        // The inner iterator is a separate owned value
                                        // (extracted via into_inner), so calling next_sync
                                        // on it doesn't alias the outer iterator.
                                        let inner = w.into_inner::<XIterator>()
                                            .map_err(|_| EnvironmentError::Static("flatten: expected array or Iterator"))?;
                                        pending.clear();
                                        return Ok(Step::Descend { child: inner, pipe_idx, kind: ResumeKind::Flatten });
                                    }
                                    _ => return Err(EnvironmentError::Static("flatten: expected array or Iterator")),
                                }

                                // If the inner collection was empty, restart from the outer source.
                                if current.is_none() {
                                    continue 'outer;
                                }
                            }
                        }
                        PipeStep::Map(_) | PipeStep::Filter(_) | PipeStep::Rev => {
                            return Err(EnvironmentError::Static("next_sync called on closure-based pipe step"));
                        }
                    }
                    pipe_idx += 1;
                }

                return Ok(Step::Yield(current));
            }
        }

        fn restore_child(parent: &mut XIterator, pipe_idx: usize, child: &mut XIterator) {
            match &mut parent.pipe[pipe_idx] {
                PipeStep::Chain(target) | PipeStep::Zip(target) => **target = mem::take(child),
                _ => unreachable!("nested iterator continuation"),
            }
        }

        let mut current = mem::take(self);
        let mut stack: Vec<Frame> = Vec::new();
        let mut resume = None;
        let result = (|| {
            loop {
                match advance(&mut current, resume.take(), heap)? {
                    Step::Descend { child, pipe_idx, kind } => {
                        let parent = mem::replace(&mut current, child);
                        stack.push(Frame { iter: parent, pipe_idx, kind });
                    }
                    Step::Yield(result) => {
                        let Some(mut frame) = stack.pop() else {
                            return Ok(result);
                        };
                        match frame.kind {
                            ResumeKind::Chain => {
                                restore_child(&mut frame.iter, frame.pipe_idx, &mut current);
                                current = frame.iter;
                                resume = Some((frame.pipe_idx + 1, result));
                            }
                            ResumeKind::ZipRight(left) => {
                                restore_child(&mut frame.iter, frame.pipe_idx, &mut current);
                                current = frame.iter;
                                let result = match result {
                                    Some(right) => Some(heap.insert(RuntimeValue::Object(vec![left, right]))),
                                    None => None,
                                };
                                resume = Some((frame.pipe_idx + 1, result));
                            }
                            ResumeKind::Flatten => {
                                let PipeStep::Flatten(pending) = &mut frame.iter.pipe[frame.pipe_idx] else {
                                    unreachable!("flatten continuation");
                                };
                                if let Some(item) = result {
                                    pending.push_back(item);
                                    stack.push(frame);
                                } else {
                                    let item = pending.pop_front();
                                    current = frame.iter;
                                    resume = item.map(|item| (frame.pipe_idx + 1, Some(item)));
                                }
                            }
                        }
                    }
                }
            }
        })();

        // Restore ordinary error state as well as successful traversal. A
        // consumed flatten source is dropped; borrowed chain/zip children go
        // back into their original parent boxes.
        while let Some(mut frame) = stack.pop() {
            if matches!(frame.kind, ResumeKind::Chain | ResumeKind::ZipRight(_)) {
                restore_child(&mut frame.iter, frame.pipe_idx, &mut current);
            }
            current = frame.iter;
        }
        *self = current;
        result
    }

}

/// Wraps an `XIterator` into an opaque `StackValue` suitable for VM storage.
#[inline(always)]
pub(crate) fn wrap_iter(iter: XIterator) -> StackValue {
    RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(iter)).into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use silex_environment::VMContext;
    fn retains_id(root: &RuntimeValue, expected: &ValuePointer, heap: &Heap) -> bool {
        let mut work = vec![OpaqueChild::Value(root)];
        while let Some(child) = work.pop() {
            let cell = match child {
                OpaqueChild::Pointer(id) => {
                    if id == expected {
                        return true;
                    }
                    id.as_ref(heap)
                }
                OpaqueChild::Value(value) => value,
                OpaqueChild::Opaque(value) => {
                    value.visit_children(&mut |child| work.push(child));
                    continue;
                }
                OpaqueChild::Owned(_) => continue,
            };
            match cell {
                RuntimeValue::Object(children) => work.extend(children.iter().map(|id| OpaqueChild::Pointer(id))),
                RuntimeValue::Map(children) => work.extend(children.values().map(|id| OpaqueChild::Pointer(id))),
                RuntimeValue::Opaque(value) => work.push(OpaqueChild::Opaque(value.inner())),
                _ => {}
            }
        }
        false
    }


    #[test]
    fn owned_iterators_detach_their_source_slots() {
        let mut runtime_heap = unsafe { Heap::new() };
        let heap = &mut runtime_heap;
        let values = &mut *heap;
        let slot = values.insert(Primitive::Integer(PrimitiveInteger::U64(1)).into());
        let iter = XIterator::from_dequeue(VecDeque::from([slot.clone()]));
        let value = RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(iter));
        let copy = value.clone();
        let detached = values.detach(copy).unwrap();
        // No payload references are live while replacing the original slot.
        *slot.as_mut(values) = detached;
        assert!(!retains_id(slot.as_ref(values), &slot, values));
        drop(value);
    }

    #[test]
    fn deep_clone_detaches_every_retained_payload_and_nested_iterator() {
        let mut runtime_heap = unsafe { Heap::new() };
        let heap = &mut runtime_heap;
        let values = &mut *heap;
        for position in 0..10 {
            let original = values.insert(Primitive::Integer(PrimitiveInteger::U64(1)).into());
            let payload = RuntimeValue::Object(vec![original.clone()]);
            let mut iter = XIterator::empty();
            match position {
                0 => iter.source = BaseSource::Dequeue(VecDeque::from([values.insert(payload)])),
                1 => iter.source = BaseSource::Once(Some(values.insert(payload))),
                2 => {
                    iter.source = BaseSource::Unfold {
                        closure: payload.into(),
                        state: None,
                    }
                }
                3 => {
                    iter.source = BaseSource::Unfold {
                        closure: Primitive::Null.into(),
                        state: Some(payload.into()),
                    }
                }
                4 => iter.pipe.push_back(PipeStep::Map(payload.into())),
                5 => iter.pipe.push_back(PipeStep::Filter(payload.into())),
                6 => iter
                    .pipe
                    .push_back(PipeStep::Flatten(VecDeque::from([values.insert(payload)]))),
                7 | 8 => {
                    let child = Box::new(XIterator::once(payload, values).unwrap());
                    iter.pipe.push_back(if position == 7 {
                        PipeStep::Chain(child)
                    } else {
                        PipeStep::Zip(child)
                    });
                }
                9 => {
                    let inner = XIterator::once(payload, values).unwrap();
                    iter = XIterator::once(RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(inner)), values).unwrap()
                }
                _ => unreachable!(),
            }
            let source = RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(iter));
            let copy = source.clone();
            let snapshot = values.detach(copy).unwrap();
            drop(source);
            assert!(!retains_id(&snapshot, &original, values), "retained source slot at position {position}");
        }
    }

    #[test]
    fn flatten_retains_deep_transformed_payloads() {
        let mut heap = unsafe { Heap::new() };
        let mut iter = XIterator::once(PrimitiveInteger::U64(1).into(), &mut heap).unwrap();
        for _ in 0..64 {
            iter.push_step(PipeStep::Enumerate(0)).unwrap();
        }
        iter.push_step(PipeStep::Flatten(VecDeque::new())).unwrap();
        assert_eq!(iter.next_sync(&mut heap).unwrap().unwrap().as_ref(&heap).as_u32().unwrap(), 0);
    }

    #[test]
    fn next_does_not_scan_unconsumed_payloads() {
        let mut context = VMContext::default();
        let mut runtime_heap = unsafe { Heap::new() };
        let heap = &mut runtime_heap;
        let values = &mut *heap;
        use std::sync::{
            atomic::{AtomicUsize, Ordering},
            Arc,
        };
        #[derive(Debug, Clone, Opaque)]
        #[opaque(children = self.children())]
        struct Probe(Arc<AtomicUsize>);
        impl Probe {
            fn children(&self) -> impl Iterator<Item = &RuntimeValue> {
                self.0.fetch_add(1, Ordering::Relaxed);
                std::iter::empty()
            }
        }
        let calls = Arc::new(AtomicUsize::new(0));
        let iter = XIterator::from_dequeue(VecDeque::from([
            values.insert(Primitive::Integer(PrimitiveInteger::U64(1)).into()),
            values.insert(RuntimeValue::Opaque(OpaqueWrapper::new(Probe(calls.clone())).into())),
        ]));
        let mut instance: StackValue =
            RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(iter)).into();
        assert_eq!(calls.load(Ordering::Relaxed), 0);
        calls.store(0, Ordering::Relaxed);
        let module = silex_bytecode::Module::new();
        let environment = crate::EnvironmentBuilder::<()>::default().build();
        let metadata = silex_environment::ModuleMetadata {
            module: (&module).into(),
            environment: (&environment).into(),
            metadata: (&()).into(),
        };
        let result = super::super::fns::iter_next(
            Ok(&mut instance),
            &mut vec![],
            &metadata,
            &mut context,
            heap,
        )
        .unwrap();
        let silex_environment::SysCallResult::Return(value) = result else {
            panic!("expected an item");
        };
        assert_eq!(value.as_u64(&*heap).unwrap(), 1);
        assert_eq!(calls.load(Ordering::Relaxed), 0);
    }

    #[test]
    fn flatten_reinserts_deep_transformed_items_without_a_depth_limit() {
        let mut heap = unsafe { Heap::new() };
        let mut iter = XIterator::from_dequeue(VecDeque::from([
            heap.insert(PrimitiveInteger::U64(1).into()),
            heap.insert(PrimitiveInteger::U64(2).into()),
        ]));
        for _ in 0..64 {
            iter.push_step(PipeStep::Enumerate(0)).unwrap();
        }
        iter.push_step(PipeStep::Flatten(VecDeque::new())).unwrap();
        assert!(iter.next_sync(&mut heap).unwrap().is_some());
        assert!(iter.next_sync(&mut heap).unwrap().is_some());
    }

}

impl From<XIterator> for StackValue {
    fn from(value: XIterator) -> Self {
        RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(value)).into()
    }
}
