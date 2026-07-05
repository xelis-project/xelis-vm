use std::{
    any::TypeId,
    collections::VecDeque,
    hash::{Hash, Hasher}
};

use xelis_environment::{EnvironmentError, VMContext};
use xelis_types::{
    OpaqueWrapper,
    Primitive,
    StackValue,
    ValueCell,
    ValuePointer,
    opaque::{
        Opaque,
        traits::{DynType, JSONHelper, Serializable},
    },
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
/// Because no `XIterator` owns another `XIterator` (only `PipeStep::Chain` and
/// `PipeStep::Zip` box a secondary iterator, and those are typically shallow),
/// the default `Clone` and `Drop` are sufficient — no custom recursive unwinding.
#[derive(Debug, Clone)]
pub struct XIterator {
    pub(crate) source: BaseSource,
    pub(crate) pipe: VecDeque<PipeStep>,
    /// Maximum nesting depth of Chain/Zip children reachable from this iterator.
    /// Tracked at construction time for O(1) depth checks.
    pub(crate) depth: usize,
}

impl PartialEq for XIterator {
    fn eq(&self, _: &Self) -> bool { false }
}
impl Eq for XIterator {}
impl Hash for XIterator {
    fn hash<H: Hasher>(&self, _: &mut H) {}
}

impl DynType for XIterator {
    fn get_type_name(&self) -> &'static str { "Iterator" }
    fn get_type(&self) -> TypeId { TypeId::of::<XIterator>() }
}
impl JSONHelper for XIterator {}
impl Serializable for XIterator {}

impl Opaque for XIterator {
    fn clone_box(&self) -> Box<dyn Opaque> {
        Box::new(self.clone())
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Iterator")
    }
}

impl XIterator {
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
        if child_depth + 1 > context.max_value_depth() {
            return Err(EnvironmentError::Static("iterator nesting too deep"));
        }
        self.depth = self.depth.max(child_depth + 1);
        self.push_step(step)
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

    /// Advance a fully-sync iterator (no closures anywhere in the pipe) without VM callbacks.
    ///
    /// Fully iterative: uses an explicit stack instead of recursive calls for
    /// `Chain` and `Zip` steps. `Flatten(opaque)` drains inner items into the
    /// pending buffer inline (consistent with the array path).
    ///
    /// Mutates `self` in-place. Returns `Err` if any step requires a callback.
    pub fn next_sync(&mut self) -> Result<Option<ValuePointer>, EnvironmentError> {
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
                                    Primitive::U32(idx).into(),
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
                                    }
                                    ValueCell::Primitive(Primitive::Opaque(w)) => {
                                        // Drain all items from the inner iterator into
                                        // `pending`, consistent with the array path.
                                        // The inner iterator is a separate owned value
                                        // (extracted via into_inner), so calling next_sync
                                        // on it doesn't alias the outer iterator.
                                        let mut inner = w.into_inner::<XIterator>()
                                            .map_err(|_| EnvironmentError::Static("flatten: expected array or Iterator"))?;
                                        while let Some(item) = inner.next_sync()? {
                                            pending.push_back(item);
                                        }
                                        current = pending.pop_front();
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
    ValueCell::Primitive(Primitive::Opaque(OpaqueWrapper::new(iter))).into()
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