use std::collections::VecDeque;

use silex_environment::{EnvironmentError};
use silex_types::{
    PrimitiveInteger,
    Opaque,
    OpaqueChild,
    OpaqueWrapper,
    Primitive,
    StackValue,
    RuntimeValue,
    ValueError,
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
pub const MAX_PIPE_DEPTH: usize = 128;

/// Flat lazy iterator.
///
/// `source` is the leaf generator; `pipe` is the ordered list of adapter steps.
/// All adapter functions (map, filter, skip, take, …) simply push a `PipeStep`
/// onto `pipe` — O(1), no allocation of `Box<XIterator>`.
///
/// Ordinary `Clone` preserves shared payload pointers. Opaque ownership
/// conversion uses `detached_clone` to copy those payloads independently.
/// Chain/Zip nesting is bounded by the VM, and Drop drains it iteratively.
#[derive(Debug, Clone, Opaque)]
#[opaque(name = "Iterator", display, children = self.children(), deep_clone_in = self.detached_clone(heap)?)]
pub struct XIterator {
    pub(crate) source: BaseSource,
    pub(crate) pipe: VecDeque<PipeStep>,
    /// Maximum nesting depth of Chain/Zip children reachable from this iterator.
    /// Tracked at construction time for O(1) depth checks.
    pub(crate) depth: usize,
}

impl std::fmt::Display for XIterator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Iterator")
    }
}

impl XIterator {
    pub(crate) fn detached_clone(&self, heap: &mut Heap) -> Result<Self, HeapError> {
        fn clone_values(items: &VecDeque<ValuePointer>, heap: &mut Heap) -> Result<VecDeque<ValuePointer>, HeapError> {
            items.iter().map(|id| {
                let value = id.to_owned(heap)?;
                let value = heap.detach(value)?;
                heap.insert(value)
            }).collect()
        }
        let source = match &self.source {
            BaseSource::Dequeue(items) => BaseSource::Dequeue(clone_values(items, heap)?),
            BaseSource::Once(value) => BaseSource::Once(value.map(|id| {
                let value = id.to_owned(heap)?;
                let value = heap.detach(value)?;
                heap.insert(value)
            }).transpose()?),
            BaseSource::Empty => BaseSource::Empty,
            BaseSource::Unfold { closure, state } => BaseSource::Unfold {
                closure: closure.to_owned(heap)?,
                state: state.as_ref().map(|value| value.to_owned(heap)).transpose()?,
            },
        };
        let pipe = self.pipe.iter().map(|step| Ok(match step {
            PipeStep::Map(closure) => PipeStep::Map(closure.to_owned(heap)?),
            PipeStep::Filter(closure) => PipeStep::Filter(closure.to_owned(heap)?),
            PipeStep::Chain(child) => PipeStep::Chain(Box::new(child.detached_clone(heap)?)),
            PipeStep::Zip(child) => PipeStep::Zip(Box::new(child.detached_clone(heap)?)),
            PipeStep::Flatten(items) => PipeStep::Flatten(clone_values(items, heap)?),
            step => step.clone(),
        })).collect::<Result<_, HeapError>>()?;
        Ok(Self { source, pipe, depth: self.depth })
    }

    fn children(&self) -> std::vec::IntoIter<OpaqueChild<'_>> {
        let mut children = Vec::new();
        match &self.source {
            BaseSource::Dequeue(items) => children.extend(items.iter().map(|id| OpaqueChild::Pointer(*id))),
            BaseSource::Once(item) => children.extend(item.iter().map(|id| OpaqueChild::Pointer(*id))),
            BaseSource::Unfold { closure, state } => {
                children.push(closure.into());
                children.extend(state.iter().map(OpaqueChild::from));
            }
            BaseSource::Empty => {}
        }
        for step in &self.pipe {
            match step {
                PipeStep::Map(value) | PipeStep::Filter(value) => children.push(value.into()),
                PipeStep::Chain(value) | PipeStep::Zip(value) => children.push(OpaqueChild::Opaque(&**value)),
                PipeStep::Flatten(items) => children.extend(items.iter().map(|id| OpaqueChild::Pointer(*id))),
                _ => {}
            }
        }
        children.into_iter()
    }

    #[inline(always)]
    pub fn from_dequeue(items: VecDeque<ValuePointer>) -> Self {
        Self { source: BaseSource::Dequeue(items), pipe: VecDeque::new(), depth: 0 }
    }

    #[inline(always)]
    pub fn once(value: RuntimeValue, heap: &mut Heap) -> Result<Self, HeapError> {
        Ok(Self { source: BaseSource::Once(Some(heap.insert(value)?)), pipe: VecDeque::new(), depth: 0 })
    }

    #[inline(always)]
    pub fn empty() -> Self {
        Self { source: BaseSource::Empty, pipe: VecDeque::new(), depth: 0 }
    }

    /// Push a pipe step, enforcing the maximum pipeline depth.
    #[inline]
    pub(crate) fn push_step(&mut self, step: PipeStep) -> Result<(), EnvironmentError> {
        if self.pipe.len() >= MAX_PIPE_DEPTH {
            return Err(EnvironmentError::Static("iterator pipeline too deep"));
        }
        self.pipe.push_back(step);
        Ok(())
    }

    /// Push a Chain or Zip step, enforcing both pipeline depth and nesting depth.
    pub(crate) fn push_nested_step(&mut self, step: PipeStep, child_depth: usize, max_depth: usize) -> Result<(), EnvironmentError> {
        if child_depth >= max_depth {
            return Err(EnvironmentError::Static("iterator nesting too deep"));
        }
        self.push_step(step)?;
        self.depth = self.depth.max(child_depth + 1);
        Ok(())
    }

    /// Returns true if advancing this iterator requires executing a VM closure.
    /// Iterative: walks the entire tree of nested Chain/Zip iterators without recursion.
    pub fn needs_callback(&self) -> bool {
        let mut to_check: Vec<&XIterator> = vec![self];
        while let Some(iter) = to_check.pop() {
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
        }
        false
    }

    fn check_retained_depth(
        value: &RuntimeValue,
        max_depth: usize,
        nesting: usize,
        heap: &Heap,
    ) -> Result<(), EnvironmentError> {
        let budget = max_depth
            .checked_sub(nesting)
            .and_then(|depth| depth.checked_sub(1))
            .ok_or(ValueError::MaxDepthReached)?;

        value.calculate_depth(heap, budget)?;

        Ok(())
    }

    fn check_pending_depth(
        values: &VecDeque<ValuePointer>,
        max_depth: usize,
        nesting: usize,
        heap: &Heap,
    ) -> Result<(), EnvironmentError> {
        for value in values {
            Self::check_retained_depth(value.as_ref(heap), max_depth, nesting, heap)?;
        }

        Ok(())
    }

    /// Advance a fully-sync iterator (no closures anywhere in the pipe) without VM callbacks.
    ///
    /// Fully iterative: uses an explicit stack instead of recursive calls for
    /// `Chain` and `Zip` steps. `Flatten(opaque)` drains inner items into the
    /// pending buffer inline (consistent with the array path).
    ///
    /// Mutates `self` in-place. Returns `Err` if any step requires a callback.
    /// New payloads are checked against the caller's remaining depth budget.
    /// Existing validated data is not rescanned.
    pub fn next_sync(&mut self, max_depth: usize, heap: &mut Heap) -> Result<Option<ValuePointer>, EnvironmentError> {
        /// How to resume the parent iterator after completing a nested descent.
        enum ResumeKind {
            /// Chain: use the nested result as `current`, continue from `pipe_idx + 1`.
            Chain,
            /// Zip: pair the saved left value with the nested result.
            ZipRight(ValuePointer),
        }

        /// One level on the explicit call stack.
        struct Frame {
            /// Raw pointer to the parent `XIterator`.
            iter_ptr: *mut XIterator,
            /// The pipe index where we paused (the Chain/Zip step).
            pipe_idx: usize,
            /// How to incorporate the nested result.
            kind: ResumeKind,
        }

        let mut stack: Vec<Frame> = Vec::new();
        let mut cur: *mut XIterator = self;
        // When `Some`, we are resuming a parent frame: skip pulling from source
        // and continue its pipe from the given `(pipe_idx, current)`.
        let mut resume: Option<(usize, Option<ValuePointer>)> = None;

        'descend: loop {
            // SAFETY: `cur` points to one of:
            //   - `self` (the root, valid for the entire call),
            //   - a `Box<XIterator>` inside a `PipeStep::Chain`/`Zip` of an ancestor
            //     on `stack` (valid because the ancestor is not mutated while we descend).
            // Only ONE `&mut XIterator` reference is live at any time.
            let iter: &mut XIterator = unsafe { &mut *cur };

            // Process this iterator level until it produces a result.
            let result: Option<ValuePointer> = 'outer: loop {
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
                            Self::check_retained_depth(outer_item.as_ref(heap), max_depth, stack.len(), heap)?;
                            match &mut iter.source {
                                BaseSource::Dequeue(d) => d.push_front(outer_item),
                                BaseSource::Once(slot) if slot.is_none() => *slot = Some(outer_item),
                                _ => {
                                    let old = std::mem::replace(&mut iter.source, BaseSource::Empty);
                                    let mut d = VecDeque::new();
                                    d.push_front(outer_item);
                                    if let BaseSource::Dequeue(rest) = old {
                                        d.extend(rest);
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
                                    heap.insert(Primitive::Integer(PrimitiveInteger::U32(idx)).into())?,
                                    item,
                                ]);
                                current = Some(heap.insert(pair)?);
                            }
                        }
                        PipeStep::Chain(next_iter) => {
                            if current.is_none() {
                                // Descend into the chained iterator instead of recursing.
                                let nested_ptr: *mut XIterator = &mut **next_iter;
                                stack.push(Frame {
                                    iter_ptr: cur,
                                    pipe_idx,
                                    kind: ResumeKind::Chain,
                                });
                                cur = nested_ptr;
                                resume = None;
                                continue 'descend;
                            }
                        }
                        PipeStep::Zip(right) => {
                            match current.take() {
                                None => {
                                    // Left exhausted → zip produces None.
                                }
                                Some(l) => {
                                    // Descend into the right iterator instead of recursing.
                                    let nested_ptr: *mut XIterator = &mut **right;
                                    stack.push(Frame {
                                        iter_ptr: cur,
                                        pipe_idx,
                                        kind: ResumeKind::ZipRight(l),
                                    });
                                    cur = nested_ptr;
                                    resume = None;
                                    continue 'descend;
                                }
                            }
                        }
                        PipeStep::Flatten(pending) => {
                            // pending is empty here (non-empty case handled above).
                            if let Some(ptr) = current.take() {
                                match ptr.to_owned(heap)? {
                                    RuntimeValue::Object(elements) => {
                                        *pending = elements.into();
                                        current = pending.pop_front();
                                        Self::check_pending_depth(&pending, max_depth, stack.len(), heap)?;
                                    }
                                    RuntimeValue::Opaque(w) => {
                                        // Drain all items from the inner iterator into
                                        // `pending`, consistent with the array path.
                                        // The inner iterator is a separate owned value
                                        // (extracted via into_inner), so calling next_sync
                                        // on it doesn't alias the outer iterator.
                                        let mut inner = w.into_inner::<XIterator>()
                                            .map_err(|_| EnvironmentError::Static("flatten: expected array or Iterator"))?;
                                        let budget = max_depth.checked_sub(stack.len())
                                            .ok_or(ValueError::MaxDepthReached)?;
                                        pending.clear();
                                        while let Some(item) = inner.next_sync(budget, heap)? {
                                            pending.push_back(item);
                                        }
                                        current = pending.pop_front();
                                        Self::check_pending_depth(&pending, max_depth, stack.len(), heap)?;
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

                break current;
            };

            // `result` is the value produced by this iterator level.
            // Unwind one frame or return to the caller.
            match stack.pop() {
                None => return Ok(result),
                Some(frame) => {
                    cur = frame.iter_ptr;
                    match frame.kind {
                        ResumeKind::Chain => {
                            resume = Some((frame.pipe_idx + 1, result));
                        }
                        ResumeKind::ZipRight(left) => {
                            let zipped = match result {
                                None => None,
                                Some(r) => Some(heap.insert(RuntimeValue::Object(vec![left, r]))?),
                            };
                            resume = Some((frame.pipe_idx + 1, zipped));
                        }
                    }
                    continue 'descend;
                }
            }
        }
    }
}

/// Wraps an `XIterator` into an opaque `StackValue` suitable for VM storage.
#[inline(always)]
pub(crate) fn wrap_iter(iter: XIterator) -> StackValue {
    RuntimeValue::Opaque(OpaqueWrapper::new(iter)).into()
}

/// Custom `Drop` to iteratively drain nested `Chain`/`Zip` iterators.
/// Without this, deeply nested iterators (e.g. from repeated `[1].iter().chain(it)`)
/// would cause recursive drops that overflow the stack.
impl Drop for XIterator {
    fn drop(&mut self) {
        // Fast path: nothing nested.
        if self.depth == 0 {
            return;
        }

        let mut to_drain: Vec<XIterator> = Vec::new();

        // Collect nested iterators from our own pipe.
        for step in self.pipe.drain(..) {
            match step {
                PipeStep::Chain(inner) | PipeStep::Zip(inner) => {
                    to_drain.push(*inner);
                }
                // Other variants are dropped normally (no deep recursion).
                _ => {}
            }
        }

        // Iteratively drain all nested iterators.
        while let Some(mut iter) = to_drain.pop() {
            for step in iter.pipe.drain(..) {
                match step {
                    PipeStep::Chain(inner) | PipeStep::Zip(inner) => {
                        to_drain.push(*inner);
                    }
                    _ => {}
                }
            }
            // `iter.source` and remaining fields drop here (shallow).
        }
    }
}

#[cfg(test)]
mod depth_tests {
    use super::*;
    use silex_environment::VMContext;
    fn retains_id(root: &RuntimeValue, expected: ValuePointer, heap: &Heap) -> bool {
        let mut work = vec![OpaqueChild::Value(root)];
        while let Some(child) = work.pop() {
            let cell = match child {
                OpaqueChild::Pointer(id) => {
                    if id == expected { return true; }
                    id.as_ref(heap)
                }
                OpaqueChild::Value(value) => value,
                OpaqueChild::Opaque(value) => { value.visit_children(&mut |child| work.push(child)); continue; }
                OpaqueChild::Owned(_) => continue,
            };
            match cell {
                RuntimeValue::Object(children) => work.extend(children.iter().map(|id| OpaqueChild::Pointer(*id))),
                RuntimeValue::Map(children) => work.extend(children.values().map(|id| OpaqueChild::Pointer(*id))),
                RuntimeValue::Opaque(value) => work.push(OpaqueChild::Opaque(value.inner())),
                _ => {}
            }
        }
        false
    }


    #[test]
    fn owned_iterators_detach_their_source_slots() {
        let mut context = VMContext::default();
        let values = context.heap_mut();
        let slot = values.insert(Primitive::Integer(PrimitiveInteger::U64(1)).into()).unwrap();
        let iter = XIterator::from_dequeue(VecDeque::from([slot.clone()]));
        let value = RuntimeValue::Opaque(OpaqueWrapper::new(iter));
        let detached = values.detach(value.clone_ref()).unwrap();
        // No payload references are live while replacing the original slot.
        *slot.as_mut(values) = detached;
        assert_eq!(slot.as_ref(values).calculate_depth(values, 16).unwrap(), 1);
        assert_eq!(value.calculate_depth(values, 16).unwrap(), 2);
        drop(value);
        assert_eq!(slot.as_ref(values).calculate_depth(values, 16).unwrap(), 1);
    }

    #[test]
    fn deep_clone_detaches_every_retained_payload_and_nested_iterator() {
        let mut context = VMContext::default();
        let values = context.heap_mut();
        for position in 0..10 {
            let original = values.insert(Primitive::Integer(PrimitiveInteger::U64(1)).into()).unwrap();
            let payload = RuntimeValue::Object(vec![original.clone()]);
            let mut iter = XIterator::empty();
            match position {
                0 => iter.source = BaseSource::Dequeue(VecDeque::from([values.insert(payload).unwrap()])),
                1 => iter.source = BaseSource::Once(Some(values.insert(payload).unwrap())),
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
                    .push_back(PipeStep::Flatten(VecDeque::from([values.insert(payload).unwrap()]))),
                7 | 8 => {
                    let child = Box::new(XIterator::once(payload, values).unwrap());
                    iter.depth = 1;
                    iter.pipe.push_back(if position == 7 {
                        PipeStep::Chain(child)
                    } else {
                        PipeStep::Zip(child)
                    });
                }
                9 => {
                    let inner = XIterator::once(payload, values).unwrap();
                    iter = XIterator::once(RuntimeValue::Opaque(OpaqueWrapper::new(inner)), values).unwrap()
                }
                _ => unreachable!(),
            }
            let source = RuntimeValue::Opaque(OpaqueWrapper::new(iter));
            let snapshot = values.detach(source.clone_ref()).unwrap();
            drop(source);
            assert!(!retains_id(&snapshot, original, values), "retained source slot at position {position}");
            snapshot.calculate_depth(values, 16).unwrap();
        }
    }

    fn payload(depth: usize, heap: &mut Heap) -> RuntimeValue {
        let mut value = RuntimeValue::Primitive(Primitive::Integer(PrimitiveInteger::U64(42)));
        for _ in 0..depth {
            value = RuntimeValue::Object(vec![heap.insert(value).unwrap()]);
        }
        value
    }
    fn depth(iter: XIterator, heap: &Heap) -> Result<usize, ValueError> {
        RuntimeValue::Opaque(OpaqueWrapper::new(iter)).calculate_depth(heap, 16)
    }
    #[test]
    fn stored_payloads_obey_the_depth_boundary() {
        let mut context = VMContext::default();
        let values = context.heap_mut();
        for position in 0..7 {
            for nesting in [15, 16] {
                let value = payload(nesting, values);
                let mut iter = XIterator::empty();
                match position {
                    0 => iter.source = BaseSource::Once(Some(values.insert(value).unwrap())),
                    1 => iter.source = BaseSource::Dequeue(VecDeque::from([values.insert(value).unwrap()])),
                    2 => {
                        iter.source = BaseSource::Unfold {
                            closure: value.into(),
                            state: None,
                        }
                    }
                    3 => {
                        iter.source = BaseSource::Unfold {
                            closure: Primitive::Integer(PrimitiveInteger::U64(1)).into(),
                            state: Some(value.into()),
                        }
                    }
                    4 => iter.pipe.push_back(PipeStep::Map(value.into())),
                    5 => iter.pipe.push_back(PipeStep::Filter(value.into())),
                    6 => iter
                        .pipe
                        .push_back(PipeStep::Flatten(VecDeque::from([values.insert(value).unwrap()]))),
                    _ => unreachable!(),
                }
                if nesting == 15 {
                    assert_eq!(depth(iter, values).unwrap(), 16);
                } else {
                    assert!(matches!(depth(iter, values), Err(ValueError::MaxDepthReached)));
                }
            }
        }
    }
    #[test]
    fn chain_and_zip_include_nested_payload_depth() {
        let mut context = VMContext::default();
        let values = context.heap_mut();
        for zip in [false, true] {
            for nesting in [14, 15] {
                let mut iter = XIterator::empty();
                let value = payload(nesting, values);
                let child = Box::new(XIterator::once(value, values).unwrap());
                iter.depth = 1;
                iter.pipe.push_back(if zip {
                    PipeStep::Zip(child)
                } else {
                    PipeStep::Chain(child)
                });
                if nesting == 14 {
                    assert_eq!(depth(iter, values).unwrap(), 16);
                } else {
                    assert!(matches!(depth(iter, values), Err(ValueError::MaxDepthReached)));
                }
            }
        }
    }
    #[test]
    fn deeply_nested_empty_iterators_are_bounded_without_recursion() {
        let mut context = VMContext::default();
        let values = context.heap_mut();
        let mut iter = XIterator::empty();
        for nesting in 1..=1000 {
            let mut parent = XIterator::empty();
            parent.depth = nesting;
            parent.pipe.push_back(PipeStep::Chain(Box::new(iter)));
            iter = parent;
        }
        assert!(matches!(depth(iter, values), Err(ValueError::MaxDepthReached)));
    }
    #[test]
    fn cyclic_payloads_are_bounded_without_consuming_the_iterator() {
        let mut context = VMContext::default();
        let values = context.heap_mut();
        let pointer = values.insert(Primitive::Integer(PrimitiveInteger::U64(1)).into()).unwrap();
        let mut iter = XIterator::empty();
        iter.source = BaseSource::Once(Some(pointer.clone()));
        let value = RuntimeValue::Opaque(OpaqueWrapper::new(iter));
        // SAFETY: no payload references are live during these mutations.
        *pointer.as_mut(values) = RuntimeValue::Object(vec![pointer.clone()]);
        let result = value.calculate_depth(values, 16);
        *pointer.as_mut(values) = Primitive::Integer(PrimitiveInteger::U64(1)).into();
        assert!(matches!(result, Err(ValueError::MaxDepthReached)));
        assert_eq!(value.calculate_depth(values, 16).unwrap(), 1);
    }

    #[test]
    fn flatten_checks_only_newly_retained_payloads() {
        let mut context = VMContext::default();
        let values = context.heap_mut();
        for nesting in [16, 17] {
            let mut iter = XIterator::once(Primitive::Integer(PrimitiveInteger::U64(1)).into(), values).unwrap();
            for _ in 0..nesting {
                iter.push_step(PipeStep::Enumerate(0)).unwrap();
            }
            iter.push_step(PipeStep::Flatten(VecDeque::new())).unwrap();
            let result = iter.next_sync(16, values);
            if nesting == 16 {
                assert_eq!(result.unwrap().unwrap().as_ref(values).as_u32().unwrap(), 0);
                assert_eq!(depth(iter, values).unwrap(), 16);
            } else {
                assert!(matches!(
                    result,
                    Err(EnvironmentError::ValueError(ValueError::MaxDepthReached))
                ));
            }
        }
    }

    #[test]
    fn flatten_pending_budget_includes_nested_iterator_position() {
        let mut context = VMContext::default();
        let values = context.heap_mut();
        let mut child = XIterator::once(Primitive::Integer(PrimitiveInteger::U64(1)).into(), values).unwrap();
        for _ in 0..16 {
            child.push_step(PipeStep::Enumerate(0)).unwrap();
        }
        child.push_step(PipeStep::Flatten(VecDeque::new())).unwrap();
        let mut parent = XIterator::empty();
        parent
            .push_nested_step(PipeStep::Chain(Box::new(child)), 0, 16)
            .unwrap();
        assert!(matches!(
            parent.next_sync(16, values),
            Err(EnvironmentError::ValueError(ValueError::MaxDepthReached))
        ));
    }

    #[test]
    fn next_does_not_scan_unconsumed_payloads() {
        let mut context = VMContext::default();
        let values = context.heap_mut();
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
            values.insert(Primitive::Integer(PrimitiveInteger::U64(1)).into()).unwrap(),
            values.insert(RuntimeValue::Opaque(OpaqueWrapper::new(Probe(calls.clone())))).unwrap(),
        ]));
        let instance: StackValue =
            RuntimeValue::Opaque(OpaqueWrapper::new(iter)).into();
        instance.as_ref(values).calculate_depth(values, 16).unwrap();
        assert_eq!(calls.load(Ordering::Relaxed), 1);
        calls.store(0, Ordering::Relaxed);
        let module = silex_bytecode::Module::new();
        let environment = crate::EnvironmentBuilder::<()>::default().build();
        let metadata = silex_environment::ModuleMetadata {
            module: (&module).into(),
            environment: (&environment).into(),
            metadata: (&()).into(),
        };
        let result = super::super::fns::iter_next(
            Ok(instance),
            vec![],
            &metadata,
            &mut context,
        )
        .unwrap();
        let silex_environment::SysCallResult::Return(value) = result else {
            panic!("expected an item");
        };
        assert_eq!(value.as_u64(context.heap()).unwrap(), 1);
        assert_eq!(calls.load(Ordering::Relaxed), 0);
    }

    #[test]
    fn flatten_checks_transformed_items_reinserted_into_the_source() {
        let mut context = VMContext::default();
        let values = context.heap_mut();
        let mut iter = XIterator::from_dequeue(VecDeque::from([
            values.insert(Primitive::Integer(PrimitiveInteger::U64(1)).into()).unwrap(),
            values.insert(Primitive::Integer(PrimitiveInteger::U64(2)).into()).unwrap(),
        ]));
        for _ in 0..16 {
            iter.push_step(PipeStep::Enumerate(0)).unwrap();
        }
        iter.push_step(PipeStep::Flatten(VecDeque::new())).unwrap();
        assert!(iter.next_sync(16, values).unwrap().is_some());
        assert!(matches!(
            iter.next_sync(16, values),
            Err(EnvironmentError::ValueError(ValueError::MaxDepthReached))
        ));
    }
}
