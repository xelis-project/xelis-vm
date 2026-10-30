use std::collections::VecDeque;
use silex_environment::{EnvironmentError, FnInstance, FnParams, FnReturnType, ModuleMetadata, SysCallResult, VMContext};
use silex_types::{Primitive, ValueError};
use super::{
    iter::{BaseSource, PipeStep, XIterator, wrap_iter},
    terminal::{TerminalOp, lazy_collect, lazy_next},
};

pub(super) fn iter_once<M>(_: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let value = p.remove(0).into_owned(context.heap_mut())?;
    Ok(SysCallResult::Return(wrap_iter(XIterator::once(value, context.heap_mut())?)))
}

pub(super) fn iter_empty<M>(_: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    Ok(SysCallResult::Return(wrap_iter(XIterator::empty())))
}

pub(super) fn iter_unfold<M: 'static>(_: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let closure = p.remove(1);
    let seed = p.remove(0).into_owned(context.heap_mut())?.into();
    Ok(SysCallResult::Return(wrap_iter(XIterator {
        source: BaseSource::Unfold { closure, state: Some(seed) },
        pipe: VecDeque::new(),
        depth: 0,
    })))
}

pub(super) fn array_iter<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let items = zelf?.raw_owned(context.heap())?.into_vec()?;
    context.resources_mut().increase_gas_usage(items.len() as u64)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator::from_dequeue(items.into()))))
}

fn adapter<M>(zelf: FnInstance, step: PipeStep, context: &mut VMContext) -> FnReturnType<M> {
    let mut value = zelf?;
    value.as_opaque_type_mut::<XIterator>(context.heap_mut())?.push_step(step)?;
    Ok(SysCallResult::Return(value))
}

pub(super) fn iter_skip<M>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let n = p.remove(0).as_u32(context.heap())? as usize;
    adapter(zelf, PipeStep::Skip(n), context)
}

pub(super) fn iter_take<M>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let n = p.remove(0).as_u32(context.heap())? as usize;
    adapter(zelf, PipeStep::Take(n), context)
}

pub(super) fn iter_chain<M>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let second = p.remove(0).into_owned(context.heap_mut())?.into_opaque_type::<XIterator>()?;
    let mut value = zelf?;
    let max_depth = context.resources().max_value_depth();
    let depth = second.depth;
    value.as_opaque_type_mut::<XIterator>(context.heap_mut())?.push_nested_step(PipeStep::Chain(Box::new(second)), depth, max_depth)?;
    Ok(SysCallResult::Return(value))
}

pub(super) fn iter_zip<M>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let second = p.remove(0).into_owned(context.heap_mut())?.into_opaque_type::<XIterator>()?;
    let mut value = zelf?;
    let max_depth = context.resources().max_value_depth();
    let depth = second.depth;
    value.as_opaque_type_mut::<XIterator>(context.heap_mut())?.push_nested_step(PipeStep::Zip(Box::new(second)), depth, max_depth)?;
    Ok(SysCallResult::Return(value))
}

pub(super) fn iter_enumerate<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    adapter(zelf, PipeStep::Enumerate(0), context)
}

pub(super) fn iter_rev<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let mut value = zelf?;
    let iter = value.as_opaque_type_mut::<XIterator>(context.heap_mut())?;
    if matches!(iter.source, BaseSource::Unfold { .. }) || iter.pipe.iter().any(|p| !matches!(p, PipeStep::Map(_))) {
        return Err(EnvironmentError::Static("rev: can only be called on simple iterators with an array source and no prior non-map adapters"));
    }
    if let BaseSource::Dequeue(items) = &mut iter.source {
        items.make_contiguous().reverse();
    } else {
        iter.push_step(PipeStep::Rev)?;
    }
    Ok(SysCallResult::Return(value))
}

pub(super) fn iter_flatten<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    adapter(zelf, PipeStep::Flatten(VecDeque::new()), context)
}

pub(super) fn iter_map<M>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(context.heap_mut())?.into();
    adapter(zelf, PipeStep::Map(closure), context)
}

pub(super) fn iter_filter<M>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(context.heap_mut())?.into();
    adapter(zelf, PipeStep::Filter(closure), context)
}

pub(super) fn iter_next<M: 'static>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let mut value = zelf?;
    let budget = context.resources().max_value_depth().checked_sub(value.depth()).ok_or(ValueError::MaxDepthReached)?;
    if !value.as_opaque_type::<XIterator>(context.heap())?.needs_callback() {
        return context.with_value(&mut value, |cell, context| {
            let iter = cell.as_opaque_type_mut::<XIterator>()?;
            Ok(SysCallResult::Return(match iter.next_sync(budget, context.heap_mut())? {
                Some(id) => id.to_owned(context.heap())?.into(),
                None => Primitive::Null.into(),
            }))
        });
    }
    let write_back = value.reference(context.heap_mut())?;
    let iter = std::mem::replace(value.as_opaque_type_mut::<XIterator>(context.heap_mut())?, XIterator::empty());
    lazy_next(iter, write_back, budget, context.heap_mut())
}

fn terminal<M: 'static>(zelf: FnInstance, op: TerminalOp, context: &mut VMContext) -> FnReturnType<M> {
    let mut value = zelf?;
    let max_depth = context.resources().max_value_depth();
    context.with_value(&mut value, |cell, context| {
        lazy_collect(cell.as_opaque_type_mut::<XIterator>()?, VecDeque::new(), op, max_depth, context.heap_mut())
    })
}

pub(super) fn iter_collect<M: 'static>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    terminal(zelf, TerminalOp::Collect, context)
}
pub(super) fn iter_count<M: 'static>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    terminal(zelf, TerminalOp::Count, context)
}
pub(super) fn iter_sum<M: 'static>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    terminal(zelf, TerminalOp::Sum, context)
}

pub(super) fn iter_for_each<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(context.heap_mut())?.into();
    terminal(zelf, TerminalOp::ForEach(closure), context)
}

pub(super) fn iter_find<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(context.heap_mut())?.into();
    terminal(zelf, TerminalOp::Find(closure), context)
}

pub(super) fn iter_any<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(context.heap_mut())?.into();
    terminal(zelf, TerminalOp::Any(closure), context)
}

pub(super) fn iter_all<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(context.heap_mut())?.into();
    terminal(zelf, TerminalOp::All(closure), context)
}

pub(super) fn iter_position<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(context.heap_mut())?.into();
    terminal(zelf, TerminalOp::Position(closure), context)
}

pub(super) fn iter_fold<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let init = p.remove(0).into_owned(context.heap_mut())?.into();
    let closure = p.remove(0).into_owned(context.heap_mut())?.into();
    terminal(zelf, TerminalOp::Fold { init, closure }, context)
}
