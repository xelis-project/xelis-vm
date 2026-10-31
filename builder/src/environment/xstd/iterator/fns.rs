use silex_types::Heap;
use std::collections::VecDeque;
use silex_environment::{EnvironmentError, FnInstance, FnParams, FnReturnType, ModuleMetadata, SysCallResult, VMContext};
use silex_types::Primitive;
use super::{
    iter::{BaseSource, PipeStep, XIterator, wrap_iter},
    terminal::{TerminalOp, lazy_collect, lazy_next},
};

pub(super) fn iter_once<M>(_: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let value = p.remove(0).into_owned(heap)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator::once(value, heap)?)))
}

pub(super) fn iter_empty<M>(_: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext, _heap: &mut Heap) -> FnReturnType<M> {
    Ok(SysCallResult::Return(wrap_iter(XIterator::empty())))
}

pub(super) fn iter_unfold<M: 'static>(_: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let closure = p.remove(1);
    let seed = p.remove(0).into_owned(heap)?.into();
    Ok(SysCallResult::Return(wrap_iter(XIterator {
        source: BaseSource::Unfold { closure, state: Some(seed) },
        pipe: VecDeque::new(),
    })))
}

pub(super) fn array_iter<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let items = zelf?.take().into_value(heap).into_vec()?;
    context.increase_gas_usage(items.len() as u64)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator::from_dequeue(items.into()))))
}

fn adapter<M>(zelf: FnInstance, step: PipeStep, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let value = zelf?;
    value.as_opaque_type_mut::<XIterator>(heap)?.push_step(step)?;
    Ok(SysCallResult::Return(value.take()))
}

pub(super) fn iter_skip<M>(zelf: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let n = p.remove(0).into_owned(heap)?.as_u32()? as usize;
    adapter(zelf, PipeStep::Skip(n), context, heap)
}

pub(super) fn iter_take<M>(zelf: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let n = p.remove(0).into_owned(heap)?.as_u32()? as usize;
    adapter(zelf, PipeStep::Take(n), context, heap)
}

pub(super) fn iter_chain<M>(zelf: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let second = p.remove(0).into_owned(heap)?.into_opaque_type::<XIterator>()?;
    let value = zelf?;
    value.as_opaque_type_mut::<XIterator>(heap)?.push_step(PipeStep::Chain(Box::new(second)))?;
    Ok(SysCallResult::Return(value.take()))
}

pub(super) fn iter_zip<M>(zelf: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let second = p.remove(0).into_owned(heap)?.into_opaque_type::<XIterator>()?;
    let value = zelf?;
    value.as_opaque_type_mut::<XIterator>(heap)?.push_step(PipeStep::Zip(Box::new(second)))?;
    Ok(SysCallResult::Return(value.take()))
}

pub(super) fn iter_enumerate<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    adapter(zelf, PipeStep::Enumerate(0), context, heap)
}

pub(super) fn iter_rev<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let value = zelf?;
    let iter = value.as_opaque_type_mut::<XIterator>(heap)?;
    if matches!(iter.source, BaseSource::Unfold { .. }) || iter.pipe.iter().any(|p| !matches!(p, PipeStep::Map(_))) {
        return Err(EnvironmentError::Static("rev: can only be called on simple iterators with an array source and no prior non-map adapters"));
    }
    if let BaseSource::Dequeue(items) = &mut iter.source {
        items.make_contiguous().reverse();
    } else {
        iter.push_step(PipeStep::Rev)?;
    }
    Ok(SysCallResult::Return(value.take()))
}

pub(super) fn iter_flatten<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    adapter(zelf, PipeStep::Flatten(VecDeque::new()), context, heap)
}

pub(super) fn iter_map<M>(zelf: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(heap)?.into();
    adapter(zelf, PipeStep::Map(closure), context, heap)
}

pub(super) fn iter_filter<M>(zelf: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(heap)?.into();
    adapter(zelf, PipeStep::Filter(closure), context, heap)
}

pub(super) fn iter_next<M: 'static>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let value = zelf?;
    if !value.as_opaque_type::<XIterator>(heap)?.needs_callback() {
        return heap.with_value(value, |cell, heap| {
            let iter = cell.as_opaque_type_mut::<XIterator>()?;
            Ok(SysCallResult::Return(match iter.next_sync(heap)? {
                Some(id) => heap.take(id).into(),
                None => Primitive::Null.into(),
            }))
        });
    }
    let write_back = value.reference(heap);
    let iter = std::mem::replace(value.as_opaque_type_mut::<XIterator>(heap)?, XIterator::empty());
    lazy_next(iter, write_back, heap)
}

fn terminal<M: 'static>(zelf: FnInstance, op: TerminalOp, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let value = zelf?;
    heap.with_value(value, |cell, heap| {
        lazy_collect(cell.as_opaque_type_mut::<XIterator>()?, VecDeque::new(), op, heap)
    })
}

pub(super) fn iter_collect<M: 'static>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    terminal(zelf, TerminalOp::Collect, context, heap)
}
pub(super) fn iter_count<M: 'static>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    terminal(zelf, TerminalOp::Count, context, heap)
}
pub(super) fn iter_sum<M: 'static>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    terminal(zelf, TerminalOp::Sum, context, heap)
}

pub(super) fn iter_for_each<M: 'static>(zelf: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(heap)?.into();
    terminal(zelf, TerminalOp::ForEach(closure), context, heap)
}

pub(super) fn iter_find<M: 'static>(zelf: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(heap)?.into();
    terminal(zelf, TerminalOp::Find(closure), context, heap)
}

pub(super) fn iter_any<M: 'static>(zelf: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(heap)?.into();
    terminal(zelf, TerminalOp::Any(closure), context, heap)
}

pub(super) fn iter_all<M: 'static>(zelf: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(heap)?.into();
    terminal(zelf, TerminalOp::All(closure), context, heap)
}

pub(super) fn iter_position<M: 'static>(zelf: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let closure = p.remove(0).into_owned(heap)?.into();
    terminal(zelf, TerminalOp::Position(closure), context, heap)
}

pub(super) fn iter_fold<M: 'static>(zelf: FnInstance, p: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let init = p.remove(0).into_owned(heap)?.into();
    let closure = p.remove(0).into_owned(heap)?.into();
    terminal(zelf, TerminalOp::Fold { init, closure }, context, heap)
}
