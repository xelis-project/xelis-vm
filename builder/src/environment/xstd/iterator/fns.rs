use silex_environment::{
    EnvironmentError,
    FnInstance,
    FnParams,
    FnReturnType,
    ModuleMetadata,
    SysCallResult,
    VMContext,
};
use silex_types::Primitive;

use std::collections::VecDeque;
use super::{
    iter::{BaseSource, PipeStep, XIterator, wrap_iter},
    terminal::{TerminalOp, lazy_collect, lazy_next}
};

// Static constructors

pub(super) fn iter_once<M>(
    _: FnInstance,
    mut p: FnParams,
    _: &ModuleMetadata<'_, M>,
    _: &mut VMContext,
) -> FnReturnType<M> {
    let value = p.remove(0).into_owned();
    Ok(SysCallResult::Return(wrap_iter(XIterator::once(value))))
}

pub(super) fn iter_empty<M>(
    _: FnInstance,
    _: FnParams,
    _: &ModuleMetadata<'_, M>,
    _: &mut VMContext,
) -> FnReturnType<M> {
    Ok(SysCallResult::Return(wrap_iter(XIterator::empty())))
}

pub(super) fn iter_unfold<M: 'static>(
    _: FnInstance,
    mut p: FnParams,
    _: &ModuleMetadata<'_, M>,
    _: &mut VMContext,
) -> FnReturnType<M> {
    let closure = p.remove(1);
    let seed = p.remove(0);
    Ok(SysCallResult::Return(wrap_iter(XIterator {
        source: BaseSource::Unfold { closure, state: Some(seed) },
        pipe: VecDeque::new(),
        depth: 0,
    })))
}

pub(super) fn array_iter<M>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext,
) -> FnReturnType<M> {
    let sv = zelf?.raw_owned();
    let items = sv.into_vec()?;
    context.increase_gas_usage(items.len() as u64)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator::from_dequeue(items.into()))))
}

// --- Lazy O(1) adapter constructors ---
// All adapters simply push a PipeStep onto the existing iterator.
// No cloning, no Box<XIterator>, no callback check needed here.

pub(super) fn iter_skip<M>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let n = p.remove(0).as_u32()? as usize;
    let mut sv = zelf?;
    let iter = sv.as_opaque_type_mut::<XIterator>()?;
    iter.push_step(PipeStep::Skip(n))?;
    Ok(SysCallResult::Return(sv))
}

pub(super) fn iter_take<M>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let n = p.remove(0).as_u32()? as usize;
    let mut sv = zelf?;
    let iter = sv.as_opaque_type_mut::<XIterator>()?;
    iter.push_step(PipeStep::Take(n))?;
    Ok(SysCallResult::Return(sv))
}

pub(super) fn iter_chain<M>(
    zelf: FnInstance,
    mut p: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let param = p.remove(0)
        .into_owned();

    let iter = sv.as_opaque_type_mut::<XIterator>()?;

    let second = param.into_opaque_type::<XIterator>()?;
    let child_depth = second.depth;

    iter.push_nested_step(PipeStep::Chain(Box::new(second)), child_depth, context)?;
    Ok(SysCallResult::Return(sv))
}

pub(super) fn iter_enumerate<M>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let iter = sv.as_opaque_type_mut::<XIterator>()?;
    iter.push_step(PipeStep::Enumerate(0))?;

    Ok(SysCallResult::Return(sv))
}

pub(super) fn iter_rev<M>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let iter = sv.as_opaque_type_mut::<XIterator>()?;

    // Rev is only valid on a direct array source with no prior pipe steps.
    if matches!(iter.source, BaseSource::Unfold { .. }) || iter.pipe.iter().any(|p| !matches!(p, PipeStep::Map(_))) {
        return Err(EnvironmentError::Static(
            "rev: can only be called on simple iterators with an array source and no prior non-map adapters".into(),
        ));
    }

    // Reverse the backing dequeue in-place immediately (no deferred PipeStep needed).
    if let BaseSource::Dequeue(d) = &mut iter.source {
        d.make_contiguous().reverse();
    } else {
        iter.push_step(PipeStep::Rev)?;
    }

    Ok(SysCallResult::Return(sv))
}

pub(super) fn iter_zip<M>(
    zelf: FnInstance,
    mut p: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let param = p.remove(0)
        .into_owned();

    let iter = sv.as_opaque_type_mut::<XIterator>()?;
    let second = param.into_opaque_type::<XIterator>()?;
    let child_depth = second.depth;

    iter.push_nested_step(PipeStep::Zip(Box::new(second)), child_depth, context)?;
    Ok(SysCallResult::Return(sv))
}

pub(super) fn iter_flatten<M>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let iter = sv.as_opaque_type_mut::<XIterator>()?;
    iter.push_step(PipeStep::Flatten(VecDeque::new()))?;
    Ok(SysCallResult::Return(sv))
}

pub(super) fn iter_map<M>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let closure = p.remove(0)
        .into_owned();
    let iter = sv.as_opaque_type_mut::<XIterator>()?;
    iter.push_step(PipeStep::Map(closure.into()))?;
    Ok(SysCallResult::Return(sv))
}

pub(super) fn iter_filter<M>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let closure = p.remove(0)
        .into_owned();
    let iter = sv.as_opaque_type_mut::<XIterator>()?;
    iter.push_step(PipeStep::Filter(closure.into()))?;
    Ok(SysCallResult::Return(sv))
}

// Terminal operations.
pub(super) fn iter_next<M: 'static>(
    zelf: FnInstance,
    _: FnParams,
    _: &ModuleMetadata<'_, M>,
    _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;

    // Fast path: pure-sync pipeline — mutate the iterator in place through
    // the pointer (if `sv` is a Pointer the change is immediately visible
    // to the caller) and return the item.
    if !sv.as_opaque_type::<XIterator>()?.needs_callback() {
        let it = sv.as_opaque_type_mut::<XIterator>()?;
        return Ok(SysCallResult::Return(match it.next_sync()? {
            Some(ptr) => ptr.to_owned().into(),
            None => Primitive::Null.into(),
        }));
    }

    // Callback path: we cannot hold `&mut XIterator` across async boundaries,
    // so we steal ownership and set up a write-back reference if `sv` was a
    // pointer (i.e. a named variable in the XELIS script).
    let write_back = sv.reference();
    // Steal the XIterator, leaving an empty one in the slot.
    // We will write back the updated iterator after the callback completes, so the caller sees the mutation.
    let it = sv.as_opaque_type_mut::<XIterator>()?;
    let iter = std::mem::replace(it, XIterator::empty());

    lazy_next(iter, write_back)
}

pub(super) fn iter_collect<M: 'static>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let it = sv.as_opaque_type_mut::<XIterator>()?;

    lazy_collect(it, VecDeque::new(), TerminalOp::Collect)
}

pub(super) fn iter_count<M: 'static>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let it = sv.as_opaque_type_mut::<XIterator>()?;

    lazy_collect(it, VecDeque::new(), TerminalOp::Count)
}

pub(super) fn iter_sum<M: 'static>(
    zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let it = sv.as_opaque_type_mut::<XIterator>()?;

    lazy_collect(it, VecDeque::new(), TerminalOp::Sum)
}

pub(super) fn iter_for_each<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let f = p.remove(0)
        .into_owned();

    let it = sv.as_opaque_type_mut::<XIterator>()?;

    lazy_collect(it, VecDeque::new(), TerminalOp::ForEach(f.into()))
}

pub(super) fn iter_find<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let pred = p.remove(0)
        .into_owned();
    let it = sv.as_opaque_type_mut::<XIterator>()?;

    lazy_collect(it, VecDeque::new(), TerminalOp::Find(pred.into()))
}

pub(super) fn iter_any<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let pred = p.remove(0)
        .into_owned();
    let it = sv.as_opaque_type_mut::<XIterator>()?;

    lazy_collect(it, VecDeque::new(), TerminalOp::Any(pred.into()))
}

pub(super) fn iter_all<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let pred = p.remove(0)
        .into_owned();
    let it = sv.as_opaque_type_mut::<XIterator>()?;

    lazy_collect(it, VecDeque::new(), TerminalOp::All(pred.into()))
}

pub(super) fn iter_position<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let pred = p.remove(0)
        .into_owned();

    let it = sv.as_opaque_type_mut::<XIterator>()?;

    lazy_collect(it, VecDeque::new(), TerminalOp::Position(pred.into()))
}

pub(super) fn iter_fold<M: 'static>(
    zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let init = p.remove(0)
        .into_owned()
        .into();
    let closure = p.remove(0)
        .into_owned()
        .into();
    let it = sv.as_opaque_type_mut::<XIterator>()?;

    lazy_collect(it, VecDeque::new(), TerminalOp::Fold { init, closure })
}
