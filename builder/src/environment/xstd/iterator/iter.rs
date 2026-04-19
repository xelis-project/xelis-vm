use std::{
    any::TypeId,
    collections::VecDeque,
    hash::{Hash, Hasher}
};

use xelis_environment::EnvironmentError;
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
#[derive(Debug, Clone)]
pub enum BaseSource {
    /// Iterates over a pre-collected dequeue of values.
    Dequeue(VecDeque<ValuePointer>),
    /// Yields exactly one value, then is exhausted.
    Once(Option<ValuePointer>),
    /// Always empty.
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
    fn clone_box(&self) -> Box<dyn Opaque> { Box::new(self.clone()) }
    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Iterator")
    }
}

impl XIterator {
    #[inline(always)]
    pub fn from_dequeue(items: VecDeque<ValuePointer>) -> Self {
        Self { source: BaseSource::Dequeue(items), pipe: VecDeque::new() }
    }

    #[inline(always)]
    pub fn once(value: ValueCell) -> Self {
        Self { source: BaseSource::Once(Some(ValuePointer::new(value))), pipe: VecDeque::new() }
    }

    #[inline(always)]
    pub fn empty() -> Self {
        Self { source: BaseSource::Empty, pipe: VecDeque::new() }
    }

    /// Returns true if advancing this iterator requires executing a VM closure.
    pub fn needs_callback(&self) -> bool {
        if matches!(self.source, BaseSource::Unfold { .. }) {
            return true;
        }
        self.pipe.iter().any(|step| match step {
            PipeStep::Map(_) | PipeStep::Filter(_) | PipeStep::Rev => true,
            PipeStep::Chain(c) => c.needs_callback(),
            PipeStep::Zip(z) => z.needs_callback(),
            // Flatten is lazy: it expands each item inline.
            // It only needs a callback if the sub-iterators it will encounter need one,
            // which cannot be known statically from the outer pipe alone — treat as safe
            // (no callback needed at the pipe-step level; inner sources are checked at runtime).
            PipeStep::Flatten(_) => false,
            _ => false,
        })
    }

    /// Advance a fully-sync iterator (no closures anywhere in the pipe) without VM callbacks.
    ///
    /// Mutates `self` in-place. Returns `Err` if any step requires a callback.
    pub fn next_sync(&mut self) -> Result<Option<ValuePointer>, EnvironmentError> {
        'outer: loop {
            // Pull one item from the source.
            let item = match &mut self.source {
                BaseSource::Dequeue(d) => d.pop_front(),
                BaseSource::Once(slot) => slot.take(),
                BaseSource::Empty => None,
                BaseSource::Unfold { .. } =>
                    return Err(EnvironmentError::Static("next_sync called on closure-based iterator")),
            };

            // Walk the pipe, transforming / filtering items.
            let mut current = item;
            let mut pipe_idx = 0;
            let mut needs_restart = false;

            while pipe_idx < self.pipe.len() {
                // Handle Flatten with pending inner elements *before* the borrow in the main match.
                // If pending is non-empty we emit the next inner element, pushing the just-pulled
                // outer item back to the source so it is seen again on the next outer iteration.
                let pending_front = if let PipeStep::Flatten(p) = &mut self.pipe[pipe_idx] {
                    p.pop_front()
                } else {
                    None
                };

                if let Some(inner_item) = pending_front {
                    if let Some(outer_item) = current.take() {
                        match &mut self.source {
                            BaseSource::Dequeue(d) => d.push_front(outer_item),
                            BaseSource::Once(slot) if slot.is_none() => *slot = Some(outer_item),
                            _ => {
                                let old = std::mem::replace(&mut self.source, BaseSource::Empty);
                                let mut d = VecDeque::new();
                                d.push_front(outer_item);
                                if let BaseSource::Dequeue(rest) = old {
                                    d.extend(rest);
                                }
                                self.source = BaseSource::Dequeue(d);
                            }
                        }
                    }
                    current = Some(inner_item);
                    pipe_idx += 1;
                    continue;
                }

                match &mut self.pipe[pipe_idx] {
                    PipeStep::Skip(n) => {
                        if *n > 0 {
                            // item is consumed; loop to pull the next one from source
                            if current.is_some() {
                                *n -= 1;
                                // restart from source
                                continue 'outer;
                            }
                            // exhausted before skip finished → propagate None
                        }
                        // skip satisfied; transparent for this and all future items
                    }
                    PipeStep::Take(remaining) => {
                        if current.is_none() {
                            // nothing more
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
                            // self's source exhausted; pull directly from the Chain's iterator
                            // and short-circuit the rest of the pipe (chain is the last logical adapter
                            // that hands control off to a new source).
                            current = next_iter.next_sync()?; 
                        }
                    }
                    PipeStep::Zip(right) => {
                        match current.take() {
                            None => {
                                current = None;
                            }
                            Some(l) => match right.next_sync()? {
                                None => {
                                    current = None;
                                }
                                Some(r) => {
                                    current = Some(ValuePointer::new(ValueCell::Object(vec![l, r])));
                                }
                            }
                        }
                    }
                    PipeStep::Flatten(pending) => {
                        // pending is empty here (non-empty case handled above with push-back).
                        // Expand the current outer item into `pending`, yield the first element.
                        if let Some(ptr) = current.take() {
                            match ptr.to_owned() {
                                ValueCell::Object(elements) => {
                                    // faster to take the elements and transform it than remove(0)
                                    *pending = elements.into();
                                    current = pending.pop_front();
                                }
                                ValueCell::Primitive(Primitive::Opaque(mut w)) => {
                                    let inner = w.as_mut::<XIterator>()
                                        .map_err(|_| EnvironmentError::Static("flatten: expected array or Iterator"))?;

                                    current = inner.next_sync()?;
                                }
                                _ => return Err(EnvironmentError::Static("flatten: expected array or Iterator")),
                            }

                            // If the inner collection was empty, restart from the outer source.
                            if current.is_none() {
                                needs_restart = true;
                                break;
                            }
                        }
                        // current is None: nothing to flatten, propagate None through remaining steps.
                    }
                    PipeStep::Map(_) | PipeStep::Filter(_) | PipeStep::Rev => {
                        return Err(EnvironmentError::Static("next_sync called on closure-based pipe step"));
                    }
                }
                pipe_idx += 1;
            }

            if needs_restart {
                continue 'outer;
            }

            return Ok(current);
        }
    }
}

/// Wraps an `XIterator` into an opaque `StackValue` suitable for VM storage.
#[inline(always)]
pub(crate) fn wrap_iter(iter: XIterator) -> StackValue {
    ValueCell::Primitive(Primitive::Opaque(OpaqueWrapper::new(iter))).into()
}