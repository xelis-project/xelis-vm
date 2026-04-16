use xelis_environment::{
    EnvironmentError,
    FnInstance,
    FnParams,
    FnReturnType,
    ModuleMetadata,
    SysCallResult,
    VMContext,
};
use xelis_types::{
    Primitive,
    StackValue,
};

use std::collections::VecDeque;
use super::iter::{IterSource, XIterator, wrap_iter, take_iter};
use super::terminal::{TerminalOp, lazy_collect};

// Static constructors

pub(super) fn iter_once<M>(
    _: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let value = p.remove(0).into_owned();
    Ok(SysCallResult::Return(wrap_iter(XIterator::once(value))))
}

pub(super) fn iter_empty<M>(
    _: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    Ok(SysCallResult::Return(wrap_iter(XIterator::empty())))
}

// Returns a lazy Unfold iterator — the closure is NOT called until a terminal
// operation (collect, fold, take, …) drives the iterator.
pub(super) fn iter_unfold<M: 'static>(
    _: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let seed = p.remove(0);
    let closure = p.remove(0);
    Ok(SysCallResult::Return(wrap_iter(XIterator { source: IterSource::Unfold { closure, state: Some(seed) } })))
}

// Borrows the underlying array items and wraps them in an Array iterator.
pub(super) fn array_iter<M>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext,
) -> FnReturnType<M> {
    let sv = zelf?;
    let items = sv.as_ref().as_vec()?.clone();
    context.increase_gas_usage(items.len() as u64)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator::from_dequeue(items.into()))))
}

// O(1) lazy adapter constructors (sync inners).
// When the inner iterator is closure-based, we must collect it eagerly first
// so that sync adapters never try to call next_sync on a closure-based source.

pub(super) fn iter_skip<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let n = p.remove(0).as_u32()? as usize;
    let inner = take_iter(zelf?)?;
    if inner.needs_callback() {
        // Collect all items from the closure-based inner, then skip the first n.
        lazy_collect::<M>(inner, VecDeque::new(), TerminalOp::BuildSkip(n))
    } else {
        Ok(SysCallResult::Return(wrap_iter(XIterator { source: IterSource::Skip { inner: Box::new(inner), n } })))
    }
}

pub(super) fn iter_take<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let n = p.remove(0).as_u32()? as usize;
    let inner = take_iter(zelf?)?;
    if inner.needs_callback() {
        // Collect all items from the closure-based inner, then truncate to n.
        lazy_collect::<M>(inner, VecDeque::new(), TerminalOp::BuildTake(n))
    } else {
        Ok(SysCallResult::Return(wrap_iter(XIterator { source: IterSource::Take { inner: Box::new(inner), remaining: n } })))
    }
}

pub(super) fn iter_chain<M>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let second = take_iter(p.remove(0))?;
    let first = take_iter(zelf?)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator {
        source: IterSource::Chain { first: Box::new(first), second: Box::new(second) },
    })))
}

pub(super) fn iter_enumerate<M>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let inner = take_iter(zelf?)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator {
        source: IterSource::Enumerate { inner: Box::new(inner), counter: 0 },
    })))
}

// rev materializes all sync items so it can reverse them.
pub(super) fn iter_rev<M>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext,
) -> FnReturnType<M> {
    let mut inner = take_iter(zelf?)?;
    if inner.needs_callback() {
        return Err(EnvironmentError::Static(
            "rev() is not supported on map/filter/unfold iterators; call collect() first",
        ));
    }
    let mut items = Vec::new();
    while let Some(item) = inner.next_sync()? {
        context.increase_gas_usage(1)?;
        items.push(item);
    }
    items.reverse();
    Ok(SysCallResult::Return(wrap_iter(XIterator { source: IterSource::Rev { items, index: 0 } })))
}

pub(super) fn iter_zip<M>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let right = take_iter(p.remove(0))?;
    let left = take_iter(zelf?)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator {
        source: IterSource::Zip { left: Box::new(left), right: Box::new(right) },
    })))
}

pub(super) fn iter_flatten<M>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let outer = take_iter(zelf?)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator {
        source: IterSource::Flatten { outer: Box::new(outer), current: None },
    })))
}

// Closure-based adapters (map / filter).
// When the inner is also closure-based, we collect it first to keep the
// callback chain from two different closures cleanly separated.

pub(super) fn iter_map<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let closure = p.remove(0);
    let inner = take_iter(zelf?)?;
    if inner.needs_callback() {
        lazy_collect::<M>(inner, VecDeque::new(), TerminalOp::BuildMap(closure))
    } else {
        Ok(SysCallResult::Return(wrap_iter(XIterator {
            source: IterSource::Map { inner: Box::new(inner), closure },
        })))
    }
}

pub(super) fn iter_filter<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let closure = p.remove(0);
    let inner = take_iter(zelf?)?;
    if inner.needs_callback() {
        lazy_collect::<M>(inner, VecDeque::new(), TerminalOp::BuildFilter(closure))
    } else {
        Ok(SysCallResult::Return(wrap_iter(XIterator {
            source: IterSource::Filter { inner: Box::new(inner), closure },
        })))
    }
}

// Terminal operations.

pub(super) fn iter_next<M: 'static>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    if sv.as_ref().as_opaque_type::<XIterator>().map_or(false, |it| it.needs_callback()) {
        // Callback-based iterator (Map, Filter, Unfold, or any adapter wrapping one):
        // collect all resulting items, then return the first.
        // If zelf lives in a named variable (Pointer case), write the remaining items
        // back as an Array iterator so the same binding can be iterated further.
        let write_back = match &sv {
            StackValue::Pointer { ptr, .. } => Some(ptr.clone()),
            StackValue::Owned(_) => None,
        };
        let iter = take_iter(sv)?;
        lazy_collect(iter, VecDeque::new(), TerminalOp::Next(write_back))
    } else {
        // Sync path: advance the iterator in-place without any VM callbacks.
        let result = sv.as_mut().as_opaque_mut()?.as_mut::<XIterator>()?.next_sync()?;
        Ok(SysCallResult::Return(match result {
            Some(ptr) => ptr.to_owned().into(),
            None => Primitive::Null.into(),
        }))
    }
}

pub(super) fn iter_collect<M: 'static>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    lazy_collect(take_iter(zelf?)?, VecDeque::new(), TerminalOp::Collect)
}

pub(super) fn iter_count<M: 'static>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    lazy_collect(take_iter(zelf?)?, VecDeque::new(), TerminalOp::Count)
}

pub(super) fn iter_sum<M: 'static>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    lazy_collect(take_iter(zelf?)?, VecDeque::new(), TerminalOp::Sum)
}

pub(super) fn iter_for_each<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let f = p.remove(0);
    lazy_collect(take_iter(zelf?)?, VecDeque::new(), TerminalOp::ForEach(f))
}

pub(super) fn iter_find<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let pred = p.remove(0);
    lazy_collect(take_iter(zelf?)?, VecDeque::new(), TerminalOp::Find(pred))
}

pub(super) fn iter_any<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let pred = p.remove(0);
    lazy_collect(take_iter(zelf?)?, VecDeque::new(), TerminalOp::Any(pred))
}

pub(super) fn iter_all<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let pred = p.remove(0);
    lazy_collect(take_iter(zelf?)?, VecDeque::new(), TerminalOp::All(pred))
}

pub(super) fn iter_position<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let pred = p.remove(0);
    lazy_collect(take_iter(zelf?)?, VecDeque::new(), TerminalOp::Position(pred))
}

pub(super) fn iter_fold<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let init = p.remove(0);
    let closure = p.remove(0);
    lazy_collect(take_iter(zelf?)?, VecDeque::new(), TerminalOp::Fold { init, closure })
}
