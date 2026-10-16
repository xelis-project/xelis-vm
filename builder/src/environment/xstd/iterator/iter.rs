use std::collections::VecDeque;

use silex_environment::{EnvironmentError, VMContext};
use silex_types::{
    PrimitiveInteger,
    OpaqueChild,
    OpaqueWrapper,
    Primitive,
    StackValue,
    ValueCell,
    ValueError,
    ValuePointer,
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
#[derive(Debug, Clone, silex_types::Opaque)]
#[opaque(name = "Iterator", display, children = self.children(), deep_clone = self.detached_clone())]
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
    /// Detach every VM payload while preserving the source and pipeline state.
    fn detached_clone(&self) -> Self {
        fn clone_values(values: &VecDeque<ValuePointer>) -> VecDeque<ValuePointer> {
            values
                .iter()
                .map(|value| ValuePointer::new(value.as_ref().deep_clone()))
                .collect()
        }
        let source = match &self.source {
            BaseSource::Dequeue(values) => BaseSource::Dequeue(clone_values(values)),
            BaseSource::Once(value) => BaseSource::Once(
                value
                    .as_ref()
                    .map(|value| ValuePointer::new(value.as_ref().deep_clone())),
            ),
            BaseSource::Empty => BaseSource::Empty,
            BaseSource::Unfold { closure, state } => BaseSource::Unfold {
                closure: closure.to_owned(),
                state: state.as_ref().map(StackValue::to_owned),
            },
        };
        let pipe = self
            .pipe
            .iter()
            .map(|step| match step {
                PipeStep::Map(closure) => PipeStep::Map(closure.to_owned()),
                PipeStep::Filter(closure) => PipeStep::Filter(closure.to_owned()),
                PipeStep::Chain(child) => PipeStep::Chain(Box::new(child.detached_clone())),
                PipeStep::Zip(child) => PipeStep::Zip(Box::new(child.detached_clone())),
                PipeStep::Flatten(values) => PipeStep::Flatten(clone_values(values)),
                step => step.clone(),
            })
            .collect();
        Self {
            source,
            pipe,
            depth: self.depth,
        }
    }

    // Expose direct payloads without advancing the iterator or running closures.
    fn children(&self) -> impl Iterator<Item = OpaqueChild<'_>> {
        let dequeue = match &self.source {
            BaseSource::Dequeue(values) => Some(values),
            _ => None,
        };
        let once = match &self.source {
            BaseSource::Once(value) => value.as_ref().map(ValuePointer::as_ref),
            _ => None,
        };
        let unfold = match &self.source {
            BaseSource::Unfold { closure, state } => Some((closure, state)),
            _ => None,
        };
        dequeue
            .into_iter()
            .flat_map(|values| values.iter().map(ValuePointer::as_ref))
            .chain(once)
            .chain(unfold.into_iter().flat_map(|(closure, state)| {
                std::iter::once(closure.as_ref()).chain(state.as_ref().map(StackValue::as_ref))
            }))
            .chain(self.pipe.iter().filter_map(|step| match step {
                PipeStep::Map(closure) | PipeStep::Filter(closure) => Some(closure.as_ref()),
                _ => None,
            }))
            .chain(
                self.pipe
                    .iter()
                    .filter_map(|step| match step {
                        PipeStep::Flatten(values) => Some(values),
                        _ => None,
                    })
                    .flat_map(|values| values.iter().map(ValuePointer::as_ref)),
            )
            .map(OpaqueChild::Value)
            .chain(self.pipe.iter().filter_map(|step| match step {
                PipeStep::Chain(child) | PipeStep::Zip(child) => {
                    Some(OpaqueChild::Opaque(child.as_ref()))
                }
                _ => None,
            }))
    }

    #[inline(always)]
    pub fn from_dequeue(items: VecDeque<ValuePointer>) -> Self {
        Self { source: BaseSource::Dequeue(items), pipe: VecDeque::new(), depth: 0 }
    }

    #[inline(always)]
    pub fn once(value: ValueCell) -> Self {
        Self { source: BaseSource::Once(Some(ValuePointer::new(value))), pipe: VecDeque::new(), depth: 0 }
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
    pub(crate) fn push_nested_step(&mut self, step: PipeStep, child_depth: usize, context: &VMContext<'_, '_>) -> Result<(), EnvironmentError> {
        if child_depth >= context.max_value_depth() {
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
        value: &ValueCell,
        max_depth: usize,
        nesting: usize,
    ) -> Result<(), EnvironmentError> {
        let budget = max_depth
            .checked_sub(nesting)
            .and_then(|depth| depth.checked_sub(1))
            .ok_or(ValueError::MaxDepthReached)?;

        value.calculate_depth(budget)?;

        Ok(())
    }

    fn check_pending_depth(
        values: &VecDeque<ValuePointer>,
        max_depth: usize,
        nesting: usize,
    ) -> Result<(), EnvironmentError> {
        for value in values {
            Self::check_retained_depth(value.as_ref(), max_depth, nesting)?;
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
    pub fn next_sync(&mut self, max_depth: usize) -> Result<Option<ValuePointer>, EnvironmentError> {
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
                            Self::check_retained_depth(outer_item.as_ref(), max_depth, stack.len())?;
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
                                let pair = ValueCell::Object(vec![
                                    Primitive::Integer(PrimitiveInteger::U32(idx)).into(),
                                    item,
                                ]);
                                current = Some(ValuePointer::new(pair));
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
                                match ptr.to_owned() {
                                    ValueCell::Object(elements) => {
                                        *pending = elements.into();
                                        current = pending.pop_front();
                                        Self::check_pending_depth(&pending, max_depth, stack.len())?;
                                    }
                                    ValueCell::Opaque(w) => {
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
                                        while let Some(item) = inner.next_sync(budget)? {
                                            pending.push_back(item);
                                        }
                                        current = pending.pop_front();
                                        Self::check_pending_depth(&pending, max_depth, stack.len())?;
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
                                Some(r) => Some(ValuePointer::new(ValueCell::Object(vec![left, r]))),
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
    ValueCell::Opaque(OpaqueWrapper::new(iter)).into()
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

    #[test]
    fn owned_iterators_detach_their_source_slots() {
        let mut slot = ValuePointer::new(Primitive::Integer(PrimitiveInteger::U64(1)).into());
        let iter = XIterator::from_dequeue(VecDeque::from([slot.clone()]));
        let value = ValueCell::Opaque(OpaqueWrapper::new(iter));
        let detached = value.deep_clone();
        // No payload references are live while replacing the original slot.
        unsafe {
            *slot.as_mut() = detached;
        }
        assert_eq!(slot.as_ref().calculate_depth(16).unwrap(), 1);
        assert_eq!(value.calculate_depth(16).unwrap(), 2);
        drop(value);
        assert_eq!(slot.strong_count(), 1);
    }

    #[test]
    fn deep_clone_detaches_every_retained_payload_and_nested_iterator() {
        for position in 0..10 {
            let original = ValuePointer::new(Primitive::Integer(PrimitiveInteger::U64(1)).into());
            let payload = ValueCell::Object(vec![original.clone()]);
            let mut iter = XIterator::empty();
            match position {
                0 => iter.source = BaseSource::Dequeue(VecDeque::from([payload.into()])),
                1 => iter.source = BaseSource::Once(Some(payload.into())),
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
                    .push_back(PipeStep::Flatten(VecDeque::from([payload.into()]))),
                7 | 8 => {
                    let child = Box::new(XIterator::once(payload));
                    iter.depth = 1;
                    iter.pipe.push_back(if position == 7 {
                        PipeStep::Chain(child)
                    } else {
                        PipeStep::Zip(child)
                    });
                }
                9 => {
                    iter = XIterator::once(ValueCell::Opaque(OpaqueWrapper::new(
                        XIterator::once(payload),
                    )))
                }
                _ => unreachable!(),
            }
            let source = ValueCell::Opaque(OpaqueWrapper::new(iter));
            let snapshot = source.deep_clone();
            drop(source);
            assert_eq!(
                original.strong_count(),
                1,
                "retained source slot at position {position}"
            );
            snapshot.calculate_depth(16).unwrap();
        }
    }

    fn payload(depth: usize) -> ValueCell {
        let mut value = ValueCell::Primitive(Primitive::Integer(PrimitiveInteger::U64(42)));
        for _ in 0..depth {
            value = ValueCell::Object(vec![value.into()]);
        }
        value
    }
    fn depth(iter: XIterator) -> Result<usize, ValueError> {
        ValueCell::Opaque(OpaqueWrapper::new(iter)).calculate_depth(16)
    }
    #[test]
    fn stored_payloads_obey_the_depth_boundary() {
        for position in 0..7 {
            for nesting in [15, 16] {
                let value = payload(nesting);
                let mut iter = XIterator::empty();
                match position {
                    0 => iter.source = BaseSource::Once(Some(value.into())),
                    1 => iter.source = BaseSource::Dequeue(VecDeque::from([value.into()])),
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
                        .push_back(PipeStep::Flatten(VecDeque::from([value.into()]))),
                    _ => unreachable!(),
                }
                if nesting == 15 {
                    assert_eq!(depth(iter).unwrap(), 16);
                } else {
                    assert!(matches!(depth(iter), Err(ValueError::MaxDepthReached)));
                }
            }
        }
    }
    #[test]
    fn chain_and_zip_include_nested_payload_depth() {
        for zip in [false, true] {
            for nesting in [14, 15] {
                let mut iter = XIterator::empty();
                let child = Box::new(XIterator::once(payload(nesting)));
                iter.depth = 1;
                iter.pipe.push_back(if zip {
                    PipeStep::Zip(child)
                } else {
                    PipeStep::Chain(child)
                });
                if nesting == 14 {
                    assert_eq!(depth(iter).unwrap(), 16);
                } else {
                    assert!(matches!(depth(iter), Err(ValueError::MaxDepthReached)));
                }
            }
        }
    }
    #[test]
    fn deeply_nested_empty_iterators_are_bounded_without_recursion() {
        let mut iter = XIterator::empty();
        for nesting in 1..=1000 {
            let mut parent = XIterator::empty();
            parent.depth = nesting;
            parent.pipe.push_back(PipeStep::Chain(Box::new(iter)));
            iter = parent;
        }
        assert!(matches!(depth(iter), Err(ValueError::MaxDepthReached)));
    }
    #[test]
    fn cyclic_payloads_are_bounded_without_consuming_the_iterator() {
        let mut pointer = ValuePointer::new(Primitive::Integer(PrimitiveInteger::U64(1)).into());
        let mut iter = XIterator::empty();
        iter.source = BaseSource::Once(Some(pointer.clone()));
        let value = ValueCell::Opaque(OpaqueWrapper::new(iter));
        // SAFETY: no payload references are live during these mutations.
        unsafe {
            *pointer.as_mut() = ValueCell::Object(vec![pointer.clone()]);
        }
        let result = value.calculate_depth(16);
        unsafe {
            *pointer.as_mut() = Primitive::Integer(PrimitiveInteger::U64(1)).into();
        }
        assert!(matches!(result, Err(ValueError::MaxDepthReached)));
        assert_eq!(value.calculate_depth(16).unwrap(), 1);
    }

    #[test]
    fn flatten_checks_only_newly_retained_payloads() {
        for nesting in [16, 17] {
            let mut iter = XIterator::once(Primitive::Integer(PrimitiveInteger::U64(1)).into());
            for _ in 0..nesting {
                iter.push_step(PipeStep::Enumerate(0)).unwrap();
            }
            iter.push_step(PipeStep::Flatten(VecDeque::new())).unwrap();
            let result = iter.next_sync(16);
            if nesting == 16 {
                assert_eq!(result.unwrap().unwrap().as_ref(), &Primitive::Integer(PrimitiveInteger::U32(0)).into());
                assert_eq!(depth(iter).unwrap(), 16);
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
        let mut child = XIterator::once(Primitive::Integer(PrimitiveInteger::U64(1)).into());
        for _ in 0..16 {
            child.push_step(PipeStep::Enumerate(0)).unwrap();
        }
        child.push_step(PipeStep::Flatten(VecDeque::new())).unwrap();
        let mut parent = XIterator::empty();
        parent
            .push_nested_step(PipeStep::Chain(Box::new(child)), 0, &VMContext::default())
            .unwrap();
        assert!(matches!(
            parent.next_sync(16),
            Err(EnvironmentError::ValueError(ValueError::MaxDepthReached))
        ));
    }

    #[test]
    fn next_does_not_scan_unconsumed_payloads() {
        use std::sync::{
            atomic::{AtomicUsize, Ordering},
            Arc,
        };
        #[derive(Debug, Clone, silex_types::Opaque)]
        #[opaque(children = self.children())]
        struct Probe(Arc<AtomicUsize>);
        impl Probe {
            fn children(&self) -> impl Iterator<Item = &ValueCell> {
                self.0.fetch_add(1, Ordering::Relaxed);
                std::iter::empty()
            }
        }
        let calls = Arc::new(AtomicUsize::new(0));
        let iter = XIterator::from_dequeue(VecDeque::from([
            Primitive::Integer(PrimitiveInteger::U64(1)).into(),
            ValueCell::Opaque(OpaqueWrapper::new(Probe(calls.clone())))
                .into(),
        ]));
        let instance: StackValue =
            ValueCell::Opaque(OpaqueWrapper::new(iter)).into();
        instance.as_ref().calculate_depth(16).unwrap();
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
            &mut VMContext::default(),
        )
        .unwrap();
        let silex_environment::SysCallResult::Return(value) = result else {
            panic!("expected an item");
        };
        assert_eq!(value.as_u64().unwrap(), 1);
        assert_eq!(calls.load(Ordering::Relaxed), 0);
    }

    #[test]
    fn flatten_checks_transformed_items_reinserted_into_the_source() {
        let mut iter = XIterator::from_dequeue(VecDeque::from([
            Primitive::Integer(PrimitiveInteger::U64(1)).into(),
            Primitive::Integer(PrimitiveInteger::U64(2)).into(),
        ]));
        for _ in 0..16 {
            iter.push_step(PipeStep::Enumerate(0)).unwrap();
        }
        iter.push_step(PipeStep::Flatten(VecDeque::new())).unwrap();
        assert!(iter.next_sync(16).unwrap().is_some());
        assert!(matches!(
            iter.next_sync(16),
            Err(EnvironmentError::ValueError(ValueError::MaxDepthReached))
        ));
    }
}
