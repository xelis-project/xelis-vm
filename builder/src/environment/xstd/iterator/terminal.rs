use std::collections::VecDeque;
use xelis_environment::{
    CallbackState,
    CallbackType,
    EnvironmentError,
    FnParams,
    FnReturnType,
    SysCallResult,
};
use xelis_types::{
    Primitive,
    StackValue,
    ValueCell,
    ValuePointer,
};

use super::iter::{IterSource, XIterator, wrap_iter};

/// What to do with the fully-collected item list once all elements have been gathered.
///
/// Simple terminals (Collect, Count, Sum) produce a value immediately.
/// Closure terminals (ForEach, Find, …) invoke a VM closure per item iteratively.
/// Composition ops (ThenMap, ThenFilter) are used when multiple closure-based adapters
/// are chained: the inner one is collected first, then the outer one is applied.
/// Builder ops (BuildMap, BuildFilter, BuildTake, BuildSkip) reconstruct a new lazy
/// iterator from the collected items so that further adapters can be chained.
#[derive(Debug, Clone)]
pub(crate) enum TerminalOp {
    Collect,
    Count,
    Sum,
    ForEach(StackValue),
    Find(StackValue),
    Any(StackValue),
    All(StackValue),
    Position(StackValue),
    Fold { init: StackValue, closure: StackValue },
    /// Collected inner items; apply `closure` over them then forward to `then`.
    ThenMap { closure: StackValue, then: Box<TerminalOp> },
    /// Collected inner items; keep those where `closure` is true then forward to `then`.
    ThenFilter { closure: StackValue, then: Box<TerminalOp> },
    /// Return a lazy Map iterator wrapping the collected items.
    BuildMap(StackValue),
    /// Return a lazy Filter iterator wrapping the collected items.
    BuildFilter(StackValue),
    /// Return a lazy iterator that yields at most `n` of the collected items.
    BuildTake(usize),
    /// Return a lazy iterator that skips the first `n` collected items.
    BuildSkip(usize),
    /// Advance the iterator by one step: return the first item (or null) and, if
    /// `write_back` is `Some`, replace the caller's storage location with an
    /// Array-backed iterator over the remaining items so subsequent calls work correctly.
    Next(Option<ValuePointer>),
}

impl TerminalOp {
    /// If this op chain ends with `BuildTake(n)` or `Next`, returns `Some(n)` — used
    /// by the Unfold callback to stop collecting early instead of looping forever.
    fn take_limit(&self) -> Option<usize> {
        match self {
            TerminalOp::BuildTake(n) => Some(*n),
            TerminalOp::Next(_) => Some(1),
            TerminalOp::ThenMap { then, .. } => then.take_limit(),
            TerminalOp::ThenFilter { then, .. } => then.take_limit(),
            _ => None,
        }
    }

    /// Consume the fully-collected `items` and produce the syscall result.
    pub(crate) fn finish<M: 'static>(self, mut items: VecDeque<ValuePointer>) -> FnReturnType<M> {
        match self {
            TerminalOp::Collect => Ok(SysCallResult::Return(ValueCell::Object(items.into()).into())),
            TerminalOp::Count => Ok(SysCallResult::Return(Primitive::U32(items.len() as u32).into())),
            TerminalOp::Sum => Ok(SysCallResult::Return(sum_items(items)?)),
            TerminalOp::ForEach(f) => apply_items_none(items, f, 0),
            TerminalOp::Find(pred) => apply_items_find(items, pred, 0),
            TerminalOp::Any(pred) => apply_items_any(items, pred, 0),
            TerminalOp::All(pred) => apply_items_all(items, pred, 0),
            TerminalOp::Position(pred) => apply_items_position(items, pred, 0),
            TerminalOp::Fold { init, closure } => apply_items_fold(items, closure, init, 0),
            TerminalOp::ThenMap { closure, then } => apply_items_map(items, closure, 0, VecDeque::new(), *then),
            TerminalOp::ThenFilter { closure, then } => apply_items_filter(items, closure, 0, VecDeque::new(), *then),
            TerminalOp::BuildMap(closure) => Ok(SysCallResult::Return(wrap_iter(XIterator {
                source: IterSource::Map { inner: Box::new(XIterator::from_dequeue(items)), closure },
            }))),
            TerminalOp::BuildFilter(closure) => Ok(SysCallResult::Return(wrap_iter(XIterator {
                source: IterSource::Filter { inner: Box::new(XIterator::from_dequeue(items)), closure },
            }))),
            TerminalOp::BuildTake(n) => {
                // Truncate to the first `n` collected items and wrap as an Array iterator.
                items.truncate(n);
                Ok(SysCallResult::Return(wrap_iter(XIterator::from_dequeue(items))))
            }
            TerminalOp::BuildSkip(n) => {
                // Drop the first `n` collected items and wrap the rest as an Array iterator.
                items.drain(..n.min(items.len()));
                Ok(SysCallResult::Return(wrap_iter(XIterator::from_dequeue(items))))
            }
            TerminalOp::Next(write_back) => {
                // O(1) pop from the front (the key reason this accumulator is a VecDeque)
                let head = items.pop_front();
                // If the iterator was stored in a variable (write_back is Some), replace
                // its storage location with an Array iterator over the remaining items so
                // that the next call to .next() works correctly on the same binding.
                if let Some(mut ptr) = write_back {
                    let remaining = wrap_iter(XIterator::from_dequeue(items)).into_owned();
                    // SAFETY: the VM guarantees no other live mutable references to this
                    // location at syscall time (same guarantee upheld for all as_mut calls).
                    unsafe { *ptr.as_mut() = remaining; }
                }

                Ok(SysCallResult::Return(match head {
                    Some(ptr) => ptr.to_owned().into(),
                    None => Primitive::Null.into(),
                }))
            }
        }
    }
}

// Drain the iterator into acc, then run op.  Iterative: no Rust-stack recursion
// for nested closure-based adapters (Map/Filter with closure inner) — instead
// wraps the op with ThenMap/ThenFilter and loops.
/// Collect all items from `iter` into `acc`, then run `op` on the result.
///
/// **Iterative** — no Rust-stack recursion for nested closure-based adapters:
/// instead of calling itself recursively, it wraps `op` inside a `ThenMap` /
/// `ThenFilter` and reassigns `iter` to continue the outer loop.
///
/// Execution model per source type:
/// - Sync sources (Array, Once, …): drained in a tight inner loop, O(1) stack.
/// - Map / Filter:  one `ExecuteAndCallback` per item; O(1) Rust stack per item.
/// - Unfold: one `ExecuteAndCallback` per generated item; O(1) Rust stack per item.
pub(crate) fn lazy_collect<M: 'static>(
    mut iter: XIterator,
    mut acc: VecDeque<ValuePointer>,
    mut op: TerminalOp,
) -> FnReturnType<M> {
    loop {
        loop {
            if iter.needs_callback() {
                break;
            }

            match iter.next_sync()? {
                Some(item) => acc.push_back(item),
                None => return op.finish(acc),
            }
        }

        match iter.source {
            IterSource::Map { inner, closure } => {
                if inner.needs_callback() {
                    op = TerminalOp::ThenMap { closure, then: Box::new(op) };
                    iter = *inner;
                } else {
                    return map_next_sync(*inner, closure, acc, op);
                }
            }
            IterSource::Filter { inner, closure } => {
                if inner.needs_callback() {
                    op = TerminalOp::ThenFilter { closure, then: Box::new(op) };
                    iter = *inner;
                } else {
                    return filter_next_sync(*inner, closure, acc, op);
                }
            }
            IterSource::Unfold { closure, state } => {
                let seed = match state {
                    Some(s) => s,
                    None => return op.finish(acc),
                };

                // If the final op is BuildTake(n), stop collecting after n items so
                // that an infinite unfold doesn't loop forever.
                let take_limit = op.take_limit();

                struct S { closure: StackValue, acc: VecDeque<ValuePointer>, op: TerminalOp, take_limit: Option<usize> }

                fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
                    let mut s = state.downcast::<S>()
                        .map_err(|_| EnvironmentError::InvalidCallbackState)?;

                    let value = params.into_iter()
                        .next()
                        .ok_or(EnvironmentError::InvalidCallbackParameters)?.into_owned();

                    match value {
                        ValueCell::Primitive(Primitive::Null) => s.op.finish(s.acc),
                        ValueCell::Object(tuple) => {
                            if tuple.len() != 2 {
                                return Err(EnvironmentError::Static("unfold: expected (item, next_state)"));
                            }
                            s.acc.push_back(tuple[0].clone());
                            // Early-stop: reached the take limit, no need to call the closure again.
                            if s.take_limit.map_or(false, |lim| s.acc.len() >= lim) {
                                return s.op.finish(s.acc);
                            }
                            let next_seed: StackValue = tuple[1].to_owned().into();
                            Ok(SysCallResult::ExecuteAndCallback {
                                ptr: s.closure.clone(), params: vec![next_seed].into(),
                                state: s, callback_params_len: 1,
                                callback: CallbackType::Sync(cb::<M>),
                            })
                        }
                        _ => Err(EnvironmentError::Static("unfold: expected optional tuple")),
                    }
                }
                return Ok(SysCallResult::ExecuteAndCallback {
                    ptr: closure.clone(), params: vec![seed].into(),
                    state: Box::new(S { closure, acc, op, take_limit }),
                    callback_params_len: 1, callback: CallbackType::Sync(cb::<M>),
                });
            }
            _ => return Err(EnvironmentError::Static("lazy_collect: unsupported iterator source for callback-based iteration")),
        }
    }
}

/// Drive a `Map { inner (sync), closure }` one item at a time via VM callbacks.
/// The callback adds the mapped item to `acc` and recurses into the next item.
pub(crate) fn map_next_sync<M: 'static>(
    mut inner: XIterator,
    closure: StackValue,
    acc: VecDeque<ValuePointer>,
    op: TerminalOp,
) -> FnReturnType<M> {
    match inner.next_sync()? {
        None => op.finish(acc),
        Some(raw) => {
            struct S { remaining: XIterator, acc: VecDeque<ValuePointer>, closure: StackValue, op: TerminalOp }
            fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
                let mut s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
                let mapped = params.into_iter().next()
                    .ok_or(EnvironmentError::InvalidCallbackParameters)?.into_owned();
                s.acc.push_back(ValuePointer::new(mapped));
                map_next_sync(s.remaining, s.closure, s.acc, s.op)
            }
            Ok(SysCallResult::ExecuteAndCallback {
                ptr: closure.clone(),
                params: vec![raw.to_owned().into()].into(),
                state: Box::new(S { remaining: inner, acc, closure, op }),
                callback_params_len: 1,
                callback: CallbackType::Sync(cb::<M>),
            })
        }
    }
}

/// Drive a `Filter { inner (sync), closure }` one item at a time via VM callbacks.
/// The callback conditionally pushes the item to `acc` and recurses.
pub(crate) fn filter_next_sync<M: 'static>(
    mut inner: XIterator,
    closure: StackValue,
    acc: VecDeque<ValuePointer>,
    op: TerminalOp,
) -> FnReturnType<M> {
    match inner.next_sync()? {
        None => op.finish(acc),
        Some(raw) => {
            struct S { raw: ValuePointer, remaining: XIterator, acc: VecDeque<ValuePointer>, closure: StackValue, op: TerminalOp }
            fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
                let mut s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
                let keep = params.first().ok_or(EnvironmentError::InvalidCallbackParameters)?.as_bool()?;
                if keep { s.acc.push_back(s.raw); }
                filter_next_sync(s.remaining, s.closure, s.acc, s.op)
            }
            let raw_clone = raw.clone();
            Ok(SysCallResult::ExecuteAndCallback {
                ptr: closure.clone(),
                params: vec![raw.to_owned().into()].into(),
                state: Box::new(S { raw: raw_clone, remaining: inner, acc, closure, op }),
                callback_params_len: 1,
                callback: CallbackType::Sync(cb::<M>),
            })
        }
    }
}

/// Apply a map closure over a collected `items` vec, building a new `mapped` vec,
/// then forward to `then` once all items are processed.
pub(crate) fn apply_items_map<M: 'static>(
    items: VecDeque<ValuePointer>,
    closure: StackValue,
    index: usize,
    mapped: VecDeque<ValuePointer>,
    then: TerminalOp,
) -> FnReturnType<M> {
    let Some(item) = items.get(index) else { return then.finish(mapped); };
    struct S { items: VecDeque<ValuePointer>, index: usize, closure: StackValue, mapped: VecDeque<ValuePointer>, then: TerminalOp }
    fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let result = params.into_iter().next().ok_or(EnvironmentError::InvalidCallbackParameters)?.into_owned();
        s.mapped.push_back(ValuePointer::new(result));
        apply_items_map(s.items, s.closure, s.index + 1, s.mapped, s.then)
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure.clone(), params: vec![item.to_owned().into()].into(),
        state: Box::new(S { items, index, closure, mapped, then }),
        callback_params_len: 1, callback: CallbackType::Sync(cb::<M>),
    })
}

/// Apply a filter closure over a collected `items` vec, building a `kept` vec,
/// then forward to `then` once all items are tested.
pub(crate) fn apply_items_filter<M: 'static>(
    items: VecDeque<ValuePointer>,
    closure: StackValue,
    index: usize,
    kept: VecDeque<ValuePointer>,
    then: TerminalOp,
) -> FnReturnType<M> {
    let Some(item) = items.get(index) else { return then.finish(kept); };
    struct S { items: VecDeque<ValuePointer>, index: usize, closure: StackValue, kept: VecDeque<ValuePointer>, then: TerminalOp }
    fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let keep = params.first().ok_or(EnvironmentError::InvalidCallbackParameters)?.as_bool()?;
        if keep { s.kept.push_back(s.items[s.index].clone()); }
        apply_items_filter(s.items, s.closure, s.index + 1, s.kept, s.then)
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure.clone(), params: vec![item.to_owned().into()].into(),
        state: Box::new(S { items, index, closure, kept, then }),
        callback_params_len: 1, callback: CallbackType::Sync(cb::<M>),
    })
}

/// Call `f` for every item in `items` (for_each semantics); return `None` when done.
pub(crate) fn apply_items_none<M: 'static>(items: VecDeque<ValuePointer>, f: StackValue, index: usize) -> FnReturnType<M> {
    let Some(item) = items.get(index) else { return Ok(SysCallResult::None); };
    struct S { items: VecDeque<ValuePointer>, index: usize, f: StackValue }
    fn cb<M: 'static>(state: CallbackState, _: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
        let s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        apply_items_none(s.items, s.f, s.index + 1)
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: f.clone(), params: vec![item.to_owned().into()].into(),
        state: Box::new(S { items, index, f }),
        callback_params_len: 0, callback: CallbackType::Sync(cb::<M>),
    })
}

/// Return the first item for which `pred` returns true, or null.
pub(crate) fn apply_items_find<M: 'static>(items: VecDeque<ValuePointer>, pred: StackValue, index: usize) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(Primitive::Null.into()));
    };
    struct S { items: VecDeque<ValuePointer>, index: usize, pred: StackValue }
    fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
        let s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let matched = params.first().ok_or(EnvironmentError::InvalidCallbackParameters)?.as_bool()?;
        if matched { Ok(SysCallResult::Return(s.items[s.index].to_owned().into())) }
        else { apply_items_find(s.items, s.pred, s.index + 1) }
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: pred.clone(), params: vec![item.to_owned().into()].into(),
        state: Box::new(S { items, index, pred }),
        callback_params_len: 1, callback: CallbackType::Sync(cb::<M>),
    })
}

/// Return `true` as soon as `pred` matches any item, or `false` if none match.
pub(crate) fn apply_items_any<M: 'static>(items: VecDeque<ValuePointer>, pred: StackValue, index: usize) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(Primitive::Boolean(false).into()));
    };
    struct S { items: VecDeque<ValuePointer>, index: usize, pred: StackValue }
    fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
        let s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let matched = params.first().ok_or(EnvironmentError::InvalidCallbackParameters)?.as_bool()?;
        if matched { Ok(SysCallResult::Return(Primitive::Boolean(true).into())) }
        else { apply_items_any(s.items, s.pred, s.index + 1) }
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: pred.clone(), params: vec![item.to_owned().into()].into(),
        state: Box::new(S { items, index, pred }),
        callback_params_len: 1, callback: CallbackType::Sync(cb::<M>),
    })
}

/// Return `false` as soon as `pred` fails for any item, or `true` if all match.
pub(crate) fn apply_items_all<M: 'static>(items: VecDeque<ValuePointer>, pred: StackValue, index: usize) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(Primitive::Boolean(true).into()));
    };
    struct S { items: VecDeque<ValuePointer>, index: usize, pred: StackValue }
    fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
        let s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let matched = params.first().ok_or(EnvironmentError::InvalidCallbackParameters)?.as_bool()?;
        if !matched { Ok(SysCallResult::Return(Primitive::Boolean(false).into())) }
        else { apply_items_all(s.items, s.pred, s.index + 1) }
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: pred.clone(), params: vec![item.to_owned().into()].into(),
        state: Box::new(S { items, index, pred }),
        callback_params_len: 1, callback: CallbackType::Sync(cb::<M>),
    })
}

/// Return the index of the first item for which `pred` returns true, or null.
pub(crate) fn apply_items_position<M: 'static>(items: VecDeque<ValuePointer>, pred: StackValue, index: usize) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(Primitive::Null.into()));
    };
    struct S { items: VecDeque<ValuePointer>, index: usize, pred: StackValue }
    fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
        let s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let matched = params.first().ok_or(EnvironmentError::InvalidCallbackParameters)?.as_bool()?;
        if matched { Ok(SysCallResult::Return(Primitive::U32(s.index as u32).into())) }
        else { apply_items_position(s.items, s.pred, s.index + 1) }
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: pred.clone(), params: vec![item.to_owned().into()].into(),
        state: Box::new(S { items, index, pred }),
        callback_params_len: 1, callback: CallbackType::Sync(cb::<M>),
    })
}

/// Left-fold over `items` using `closure(acc, item) -> acc`.
pub(crate) fn apply_items_fold<M: 'static>(
    items: VecDeque<ValuePointer>,
    closure: StackValue,
    acc: StackValue,
    index: usize,
) -> FnReturnType<M> {
    let Some(item) = items.get(index) else { return Ok(SysCallResult::Return(acc)); };
    struct S { items: VecDeque<ValuePointer>, index: usize, closure: StackValue }
    fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
        let s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let new_acc = params.into_iter().next().ok_or(EnvironmentError::InvalidCallbackParameters)?;
        apply_items_fold(s.items, s.closure, new_acc, s.index + 1)
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure.clone(), params: vec![acc, item.to_owned().into()].into(),
        state: Box::new(S { items, index, closure }),
        callback_params_len: 1, callback: CallbackType::Sync(cb::<M>),
    })
}

/// Sum a homogeneous list of numeric values, preserving the concrete number type.
///
/// Uses `Primitive::add` which keeps the exact type (u8, u16, …, u256) and
/// wrapping semantics.  Returns `U64(0)` for an empty list as a
/// conventional fallback (the type cannot be inferred without items).
pub(crate) fn sum_items(items: VecDeque<ValuePointer>) -> Result<StackValue, EnvironmentError> {
    let mut acc: Option<Primitive> = None;
    for ptr in &items {
        match ptr.as_ref() {
            ValueCell::Primitive(p) => {
                match acc.take() {
                    Some(a) => {
                        acc = Some(a.add(p)?);
                    },
                    None => {
                        if p.is_number() {
                            acc = Some(p.clone());
                        } else {
                            return Err(EnvironmentError::Static("sum: only numeric types are supported"));
                        }
                    },
                }
            },
            _ => return Err(EnvironmentError::Static("sum: only numeric types are supported")),
        }
    }

    Ok(acc.unwrap_or(Primitive::U64(0)).into())
}
