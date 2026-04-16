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

/// Lazy iterator source tree.
///
/// Variants without closures (Array, Once, Empty, Skip, Take, Chain, Rev, Enumerate, Zip,
/// Flatten) advance synchronously via `next_sync` without involving the VM.
///
/// Closure-based variants (Map, Filter, Unfold) must be driven by a terminal operation
/// through `SysCallResult::ExecuteAndCallback`, one item at a time.
#[derive(Debug, Clone)]
pub enum IterSource {
    /// Iterates over a pre-collected slice of values.
    Dequeue { items: VecDeque<ValuePointer> },
    /// Yields exactly one value, then is exhausted.
    Once(Option<ValuePointer>),
    /// Always empty.
    Empty,
    /// Skips the first `n` items of the inner iterator.
    Skip { inner: Box<XIterator>, n: usize },
    /// Yields at most `remaining` items from the inner iterator.
    Take { inner: Box<XIterator>, remaining: usize },
    /// Yields all items of `first`, then all items of `second`.
    Chain { first: Box<XIterator>, second: Box<XIterator> },
    /// Items stored in reverse order; advances from front.
    Rev { items: Vec<ValuePointer>, index: usize },
    /// Pairs each item with its zero-based index.
    Enumerate { inner: Box<XIterator>, counter: u32 },
    /// Pairs items from two iterators; stops when the shorter one is exhausted.
    Zip { left: Box<XIterator>, right: Box<XIterator> },
    /// Flattens one level of nesting (each item must be an array or Iterator).
    Flatten { outer: Box<XIterator>, current: Option<Box<XIterator>> },
    /// Applies `closure` to each item; requires VM callbacks.
    Map { inner: Box<XIterator>, closure: StackValue },
    /// Keeps only items for which `closure` returns true; requires VM callbacks.
    Filter { inner: Box<XIterator>, closure: StackValue },
    /// Generates items by repeatedly calling `closure(state) -> optional<(item, next_state)>`.
    Unfold { closure: StackValue, state: Option<StackValue> },
}

#[derive(Debug, Clone)]
pub struct XIterator {
    pub(crate) source: IterSource,
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
        Self { source: IterSource::Dequeue { items } }
    }

    #[inline(always)]
    pub fn once(value: ValueCell) -> Self {
        Self { source: IterSource::Once(Some(ValuePointer::new(value))) }
    }

    #[inline(always)]
    pub fn empty() -> Self {
        Self { source: IterSource::Empty }
    }

    /// Returns true if advancing this iterator requires executing a VM closure.
    ///
    /// Closure-based sources (Map, Filter, Unfold) need callbacks.
    /// Wrapper adapters (Skip, Take, Chain, …) propagate the flag from their
    /// inner iterators so callers never accidentally call `next_sync` on a
    /// chain that contains a closure-based source.
    pub fn needs_callback(&self) -> bool {
        match &self.source {
            IterSource::Map { .. } | IterSource::Filter { .. } | IterSource::Unfold { .. } => true,
            // Passthrough adapters: callback-needed if any inner source needs one.
            IterSource::Skip { inner, .. } | IterSource::Take { inner, .. }
            | IterSource::Enumerate { inner, .. } => inner.needs_callback(),
            IterSource::Chain { first, second } => first.needs_callback() || second.needs_callback(),
            IterSource::Zip { left, right } => left.needs_callback() || right.needs_callback(),
            IterSource::Flatten { outer, current } => {
                outer.needs_callback()
                    || current.as_deref().map_or(false, |c| c.needs_callback())
            }
            // Rev already materialized all items; Array / Once / Empty never need a callback.
            _ => false,
        }
    }

    /// Advance the iterator without invoking any VM closure.
    ///
    /// Calling this on a Map, Filter, or Unfold source (or any adapter that
    /// wraps one) is a logic error and returns `Err`.
    pub fn next_sync(&mut self) -> Result<Option<ValuePointer>, EnvironmentError> {
        match &mut self.source {
            IterSource::Dequeue { items} => Ok(items.pop_front()),
            IterSource::Once(slot) => Ok(slot.take()),
            IterSource::Empty => Ok(None),
            IterSource::Skip { inner, n } => {
                while *n > 0 {
                    match inner.next_sync()? {
                        Some(_) => *n -= 1,
                        None => {
                            *n = 0;
                            break;
                        }
                    }
                }
                inner.next_sync()
            }
            IterSource::Take { inner, remaining } => {
                if *remaining == 0 {
                    return Ok(None);
                }
                let item = inner.next_sync()?;
                if item.is_some() {
                    *remaining -= 1;
                }
                Ok(item)
            }
            IterSource::Chain { first, second } => {
                match first.next_sync()? {
                    Some(x) => Ok(Some(x)),
                    None => second.next_sync(),
                }
            }
            IterSource::Rev { items, index } => {
                if *index < items.len() {
                    let item = items[*index].clone();
                    *index += 1;
                    Ok(Some(item))
                } else {
                    Ok(None)
                }
            }
            IterSource::Enumerate { inner, counter } => {
                match inner.next_sync()? {
                    Some(item) => {
                        let idx = *counter;
                        *counter += 1;
                        let pair = ValueCell::Object(vec![
                            ValuePointer::new(Primitive::U32(idx).into()),
                            item,
                        ]);
                        Ok(Some(ValuePointer::new(pair)))
                    }
                    None => Ok(None),
                }
            }
            IterSource::Zip { left, right } => {
                match (left.next_sync()?, right.next_sync()?) {
                    (Some(l), Some(r)) => Ok(Some(ValuePointer::new(ValueCell::Object(vec![l, r])))),
                    _ => Ok(None),
                }
            }
            IterSource::Flatten { outer, current } => {
                loop {
                    if let Some(inner) = current {
                        match inner.next_sync()? {
                            Some(x) => return Ok(Some(x)),
                            None => {
                                *current = None;
                            }
                        }
                    }
                    match outer.next_sync()? {
                        None => return Ok(None),
                        Some(ptr) => {
                            let cell = ptr.to_owned();
                            let new_inner = match cell {
                                ValueCell::Object(v) => XIterator::from_dequeue(v.into()),
                                ValueCell::Primitive(Primitive::Opaque(w)) => {
                                    w.into_inner::<XIterator>()
                                        .map_err(|_| EnvironmentError::Static("flatten: expected Iterator or array"))?
                                }
                                _ => return Err(EnvironmentError::Static("flatten: expected array or Iterator")),
                            };
                            *current = Some(Box::new(new_inner));
                        }
                    }
                }
            }
            IterSource::Map { .. } | IterSource::Filter { .. } | IterSource::Unfold { .. } => {
                Err(EnvironmentError::Static("next_sync called on closure-based iterator"))
            }
        }
    }
}

/// Wraps an `XIterator` into an opaque `StackValue` suitable for VM storage.
#[inline(always)]
pub(crate) fn wrap_iter(iter: XIterator) -> StackValue {
    ValueCell::Primitive(Primitive::Opaque(OpaqueWrapper::new(iter))).into()
}

/// Unwraps an opaque `StackValue` back into an owned `XIterator`.
#[inline(always)]
pub(crate) fn take_iter(sv: StackValue) -> Result<XIterator, EnvironmentError> {
    sv.into_owned()
        .into_opaque_type::<XIterator>()
        .map_err(|_| EnvironmentError::InvalidType)
}
