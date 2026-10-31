use silex_types::RuntimeOpaqueWrapper;
use std::{collections::VecDeque, mem};

use silex_environment::{
    CallbackState,
    CallbackType,
    EnvironmentError,
    FnParams,
    FnReturnType,
    SysCallResult,
};
use silex_types::{
    PrimitiveInteger,
    Primitive,
    StackValue,
    RuntimeValue,
    ValuePointer,
    Heap,
};

use super::iter::{BaseSource, PipeStep, XIterator};

/// A single transformation step inside an [`Aggregated`] pipeline.
///
/// Steps are stored flat in a `VecDeque` so the pipeline can be processed
/// iteratively without creating a recursive `Box<TerminalOp>` chain.
/// This means **drop is O(1) stack depth** regardless of pipeline length.
#[derive(Debug, Clone)]
pub(crate) enum AggStep {
    /// Apply `closure` to every item (map semantics).
    Map(StackValue),
    /// Keep only items where `closure` returns true (filter semantics).
    Filter(StackValue),
    /// Drop the first `n` items.
    Skip(usize),
    /// Keep only the first `n` items.
    Take(usize),
    /// Pair each item with its zero-based index starting from `counter`.
    Enumerate(u32),
    /// Collect `remaining` into the same accumulator (chain semantics).
    Chain(XIterator),
    /// Prepend `prefix` items to the front of the current accumulator.
    /// Used by `Chain` handling so that pipe steps in the chained iterator
    /// only apply to its own items, not to the already-collected prefix.
    Prepend(VecDeque<ValuePointer>),
    /// Collect the right side then zip with the already-collected left items.
    Zip(XIterator),
    /// Zip with the already-collected `left` items (`items` passed to finish
    /// is the right side).
    ZipMerge(VecDeque<ValuePointer>),
    /// Expand each item as a sub-collection (flatten semantics).
    Flatten,
    /// Resume flatten: expand the remaining `containers` and append to the
    /// items produced by the just-collected sub-iterator.
    FlattenContinue(VecDeque<ValuePointer>),
    /// Reverse the accumulated items in-place.
    Rev,
}

/// What to do with the fully-collected item list once all elements have been gathered.
///
/// Simple terminals (Collect, Count, Sum, ForEach, …) produce a value immediately or
/// invoke a VM closure per item.
///
/// `Aggregated` replaces all the old `Then*` variants with a **flat** pipeline of
/// [`AggStep`]s that can be processed iteratively — no recursive Box nesting, so
/// dropping even a very long chain is stack-safe.
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
    /// Advance the iterator by one step: return the first item (or null).
    /// After popping the head, reconstructs a Dequeue
    /// iterator from the remaining items and writes it back to `write_back`.
    /// Used as fallback when Rev / Flatten / Zip force full collection.
    Next(Box<IteratorWriteBack>),
    /// A flat pipeline of transformation steps followed by a simple terminal.
    /// All `Then*` logic from the old design lives here; processing is iterative.
    Aggregated {
        steps: VecDeque<AggStep>,
        then: Box<TerminalOp>,
    },
}

impl TerminalOp {
    /// Push a new [`AggStep`] as the **next** step to process (i.e. it will be
    /// processed after the current accumulator is ready).
    ///
    /// If `self` is already `Aggregated`, the step is pushed to the **back** of
    /// the deque (appended last). If `self` is a simple terminal, it is wrapped
    /// in a new `Aggregated` with this single step.
    ///
    /// Callers in `lazy_collect` use this to build the pipeline while unwrapping
    /// the iterator layer by layer.  Because we peel the iterator from outside-in
    /// (e.g., outer Skip before inner Map), each new step should run *before* the
    /// previously registered steps.  We therefore `push_front` so that the first
    /// step peeled ends up at the back (closest to the terminal) and the last one
    /// peeled ends up at the front (first to execute after collection).
    #[inline]
    pub(crate) fn push_step(self, step: AggStep) -> Self {
        match self {
            TerminalOp::Aggregated { mut steps, then } => {
                steps.push_front(step);
                TerminalOp::Aggregated { steps, then }
            }
            other => {
                let mut steps = VecDeque::with_capacity(4);
                steps.push_front(step);
                TerminalOp::Aggregated { steps, then: Box::new(other) }
            }
        }
    }

    /// Return an upper bound on the number of items needed from the source.
    /// Used by the `Unfold` path to stop generating items early.
    /// Returns `None` when the limit cannot be determined statically (e.g. after a `Filter`).
    fn take_limit(&self) -> Option<usize> {
        let mut current = self;
        let mut layers = Vec::new();
        while let TerminalOp::Aggregated { steps, then } = current {
            layers.push(steps);
            current = then;
        }
        let mut limit = if matches!(current, TerminalOp::Next(_)) { Some(1) } else { None };
        for steps in layers.into_iter().rev() {
            for step in steps.iter().rev() {
                match step {
                    AggStep::Take(n) => limit = Some(*n),
                    AggStep::Skip(n) => limit = limit.and_then(|limit| limit.checked_add(*n)),
                    _ => {}
                }
            }
        }
        limit
    }

    /// Consume the fully-collected `items` and produce the syscall result.
    pub(crate) fn finish<M: 'static>(self, items: VecDeque<ValuePointer>, heap: &mut Heap) -> FnReturnType<M> {
        drive(Collection::Finish { items, op: self }, heap)
    }

}

enum Collection<M> {
    Finish {
        items: VecDeque<ValuePointer>,
        op: TerminalOp,
    },
    Collect {
        iter: XIterator,
        items: VecDeque<ValuePointer>,
        op: TerminalOp,
    },
    Steps {
        items: VecDeque<ValuePointer>,
        steps: VecDeque<AggStep>,
        then: TerminalOp,
    },
    Flatten {
        containers: VecDeque<ValuePointer>,
        flat: VecDeque<ValuePointer>,
        then: TerminalOp,
    },
    Result(SysCallResult<M>),
}

fn drive<M: 'static>(mut next: Collection<M>, heap: &mut Heap) -> FnReturnType<M> {
    loop {
        next = match next {
            Collection::Result(result) => return Ok(result),
            Collection::Collect { mut iter, items, op } => collect_work(&mut iter, items, op, heap)?,
            Collection::Steps { items, steps, then } => finish_aggregated(items, steps, then, heap)?,
            Collection::Flatten { containers, flat, then } => flatten_finish(containers, flat, then, heap)?,
            Collection::Finish { mut items, op } => match op {
                TerminalOp::Collect => return Ok(SysCallResult::Return(RuntimeValue::Object(items.into()).into())),
                TerminalOp::Count => {
                    let len = items.len() as u32;
                    return Ok(SysCallResult::Return(PrimitiveInteger::U32(len).into()));
                }
                TerminalOp::Sum => return Ok(SysCallResult::Return(sum_items(items, heap)?)),
                TerminalOp::ForEach(f) => return apply_items_none(items, f, 0, heap),
                TerminalOp::Find(pred) => return apply_items_find(items, pred, 0, heap),
                TerminalOp::Any(pred) => return apply_items_any(items, pred, 0, heap),
                TerminalOp::All(pred) => return apply_items_all(items, pred, 0, heap),
                TerminalOp::Position(pred) => return apply_items_position(items, pred, 0, heap),
                TerminalOp::Fold { init, closure } => return apply_items_fold(items, closure, init, 0, heap),
                TerminalOp::Next(write_back) => {
                    let head = items.pop_front();
                    write_back_iter(*write_back, XIterator::from_dequeue(items), heap)?;
                    return Ok(SysCallResult::Return(match head {
                        Some(ptr) => heap.take(ptr).into(),
                        None => Primitive::Null.into(),
                    }));
                }
                TerminalOp::Aggregated { steps, then } => Collection::Steps { items, steps, then: *then },
            }

        };
    }
}

/// Execute the flat `steps` pipeline iteratively, then run `then`.
///
/// Sync steps (Skip, Take, Enumerate, ZipMerge) are handled in a loop with no
/// Rust-stack growth.  Callback-requiring steps (Map, Filter, Chain, Zip,
/// Flatten, FlattenContinue) suspend via `ExecuteAndCallback` / `lazy_collect`
/// and resume with the remaining steps still flat in the deque.
fn finish_aggregated<M: 'static>(
    mut items: VecDeque<ValuePointer>,
    mut steps: VecDeque<AggStep>,
    then: TerminalOp,
    heap: &mut Heap,
) -> Result<Collection<M>, EnvironmentError> {
    while let Some(step) = steps.pop_front() {
        match step {
            AggStep::Skip(n) => {
                items.drain(..n.min(items.len()));
            }
            AggStep::Take(n) => {
                items.truncate(n);
            }
            AggStep::Enumerate(counter) => {
                let enumerated: VecDeque<ValuePointer> = items.into_iter().enumerate().map(|(i, item)| {
                    let pair = RuntimeValue::Object(vec![
                        heap.insert(Primitive::Integer(PrimitiveInteger::U32(counter + i as u32)).into()),
                        item,
                    ]);
                    heap.insert(pair)
                }).collect();
                items = enumerated;
            }
            AggStep::Prepend(prefix) => {
                let mut combined = prefix;
                combined.extend(items.drain(..));
                items = combined;
            }
            AggStep::ZipMerge(left) => {
                let mut left = left;
                let len = left.len().min(items.len());
                left.truncate(len);
                items.truncate(len);
                let zipped: VecDeque<ValuePointer> = left.into_iter().zip(items).map(|(l, r)| {
                    heap.insert(RuntimeValue::Object(vec![l, r]))
                }).collect();
                items = zipped;
            }
            AggStep::Map(closure) => {
                // Need per-item VM callbacks; package remaining steps + then as continuation.
                let cont = TerminalOp::Aggregated { steps, then: Box::new(then) };
                if items.is_empty() {
                    return Ok(Collection::Finish { items, op: cont });
                }
                return Ok(Collection::Result(apply_items_map(items, closure, VecDeque::new(), cont, heap)?));
            }
            AggStep::Filter(closure) => {
                let cont = TerminalOp::Aggregated { steps, then: Box::new(then) };
                if items.is_empty() {
                    return Ok(Collection::Finish { items, op: cont });
                }
                return Ok(Collection::Result(apply_items_filter(items, closure, VecDeque::new(), cont, heap)?));
            }
            AggStep::Chain(remaining) => {
                // Collect `remaining` into a *fresh* accumulator so that any pipe
                // steps inside `remaining` (Map, Filter, …) only apply to its own
                // items, not to the already-collected first-part `items`.
                // `AggStep::Prepend` splices the first-part items back at the front.
                let cont = TerminalOp::Aggregated { steps, then: Box::new(then) }
                    .push_step(AggStep::Prepend(items));
                return Ok(Collection::Collect { iter: remaining, items: VecDeque::new(), op: cont });
            }
            AggStep::Zip(right) => {
                // `items` is the left side; collect the right side then merge.
                let cont = TerminalOp::Aggregated { steps, then: Box::new(then) }
                    .push_step(AggStep::ZipMerge(items));
                return Ok(Collection::Collect { iter: right, items: VecDeque::new(), op: cont });
            }
            AggStep::Flatten => {
                let cont = TerminalOp::Aggregated { steps, then: Box::new(then) };
                return Ok(Collection::Flatten { containers: items, flat: VecDeque::new(), then: cont });
            }
            AggStep::FlattenContinue(containers) => {
                let cont = TerminalOp::Aggregated { steps, then: Box::new(then) };
                return Ok(Collection::Flatten { containers, flat: items, then: cont });
            }
            AggStep::Rev => {
                // Reverse the accumulated items in-place.
                let n = items.len();
                for i in 0..n / 2 {
                    items.swap(i, n - 1 - i);
                }
            }
        }
    }
    Ok(Collection::Finish { items, op: then })
}

/// Expand `containers` (each an array or Iterator) into `flat`, then run `then`.
/// If a container is a callback-based Iterator, suspends via `lazy_collect` and
/// resumes via `AggStep::FlattenContinue`.
fn flatten_finish<M: 'static>(
    mut containers: VecDeque<ValuePointer>,
    mut flat: VecDeque<ValuePointer>,
    then: TerminalOp,
    heap: &mut Heap,
) -> Result<Collection<M>, EnvironmentError> {
    while let Some(ptr) = containers.pop_front() {
        match ptr.into_owned(heap)? {
            RuntimeValue::Object(v) => flat.extend(v),
            RuntimeValue::Opaque(w) => {
                let inner = w.into_inner::<XIterator>()
                    .map_err(|_| EnvironmentError::Static("flatten: expected Iterator or array"))?;
                if inner.needs_callback() {
                    // Collect this sub-iterator via callbacks; resume the remaining containers
                    // afterwards through AggStep::FlattenContinue.
                    return Ok(Collection::Collect { iter: inner, items: flat, op: then.push_step(AggStep::FlattenContinue(containers)) });
                }
                let mut it = inner;
                while let Some(item) = it.next_sync(heap)? {
                    flat.push_back(item);
                }
            }
            _ => return Err(EnvironmentError::Static("flatten: expected array or Iterator")),
        }
    }
    Ok(Collection::Finish { items: flat, op: then })
}

/// Collect all items from `iter` into `acc`, then run `op` on the result.
///
/// The flat pipeline (`iter.pipe`) is translated step-by-step into `AggStep`s
/// pushed onto `op` before collecting from the leaf source.  No recursion is
/// needed because `XIterator` is already flat.
///
/// Execution model:
/// - Sync leaf (Dequeue, Once, Empty): drained in a tight inner loop, O(1) stack.
/// - Unfold: one `ExecuteAndCallback` per generated item; O(1) Rust stack per item.
/// - Map / Filter pipe steps invoke `apply_items_*` via `AggStep`.
pub(crate) fn lazy_collect<M: 'static>(iter: &mut XIterator, acc: VecDeque<ValuePointer>, op: TerminalOp, heap: &mut Heap) -> FnReturnType<M> {
    let work = collect_work(iter, acc, op, heap)?;
    drive(work, heap)
}

fn collect_work<M: 'static>(
    iter: &mut XIterator,
    mut acc: VecDeque<ValuePointer>,
    mut op: TerminalOp,
    heap: &mut Heap,
) -> Result<Collection<M>, EnvironmentError> {
    // Convert each PipeStep into an AggStep. `push_step` prepends, so we
    // iterate the pipe back-to-front: the first pipe step ends up first
    // in the resulting AggStep queue.
    while let Some(step) = iter.pipe.pop_back() {
        let agg = match step {
            PipeStep::Map(cl) => AggStep::Map(cl),
            PipeStep::Filter(cl) => AggStep::Filter(cl),
            PipeStep::Skip(n) => AggStep::Skip(n),
            PipeStep::Take(n) => AggStep::Take(n),
            PipeStep::Enumerate(c) => AggStep::Enumerate(c),
            PipeStep::Rev => AggStep::Rev,
            PipeStep::Chain(next) => AggStep::Chain(*next),
            PipeStep::Zip(right) => AggStep::Zip(*right),
            PipeStep::Flatten(_) => AggStep::Flatten,
        };
        op = op.push_step(agg);
    }

    // Now drain the leaf source into acc, then apply op.
    match &mut iter.source {
        BaseSource::Dequeue(d) => {
            acc.extend(d.drain(..));
            Ok(Collection::Finish { items: acc, op })
        }
        BaseSource::Once(slot) => {
            if let Some(ptr) = slot.take() {
                acc.push_back(ptr);
            }
            Ok(Collection::Finish { items: acc, op })
        }
        BaseSource::Empty => Ok(Collection::Finish { items: acc, op }),
        BaseSource::Unfold { closure, state } => {
            let seed = match state {
                Some(s) => s,
                None => return Ok(Collection::Finish { items: acc, op }),
            };

            let take_limit = op.take_limit();

            struct LocalState {
                closure: StackValue,
                acc: VecDeque<ValuePointer>,
                op: TerminalOp,
                take_limit: Option<usize>,
            }

            fn callback<M: 'static>(
                state: CallbackState,
                params: FnParams,
                heap: &mut Heap,
            ) -> Result<SysCallResult<M>, EnvironmentError> {
                let mut s = state.downcast::<LocalState>()
                    .map_err(|_| EnvironmentError::InvalidCallbackState)?;

                let value = params.first_mut().map(StackValue::take)
                    .ok_or_else(|| EnvironmentError::InvalidCallbackParameters)?.into_owned(heap)?;

                match value {
                    RuntimeValue::Primitive(Primitive::Null) => {
                        s.op.finish(s.acc, heap)
                    },
                    RuntimeValue::Object(tuple) => {
                        if tuple.len() != 2 {
                            return Err(EnvironmentError::Static("unfold: expected (item, next_state)"));
                        }
                        let mut fields = tuple.into_iter();
                        s.acc.push_back(fields.next().expect("unfold item"));
                        let seed = fields.next().expect("unfold seed");
                        if s.take_limit.map_or(false, |lim| s.acc.len() >= lim) {
                            return s.op.finish(s.acc, heap);
                        }
                        let next_seed: StackValue = heap.take(seed).into();
                        Ok(SysCallResult::ExecuteAndCallback {
                            ptr: s.closure.clone(),
                            params: vec![next_seed],
                            state: s,
                            callback_params_len: 1,
                            callback: CallbackType::Sync(callback::<M>),
                        })
                    }
                    _ => Err(EnvironmentError::Static("unfold: expected optional tuple")),
                }
            }
            Ok(Collection::Result(SysCallResult::ExecuteAndCallback {
                ptr: closure.clone(),
                params: vec![seed.reference(heap)],
                state: Box::new(LocalState { closure: closure.reference(heap), acc, op, take_limit }),
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            }))
        }
    }
}

/// Apply a map closure over a collected `items` vec, building a new `mapped` vec,
/// then forward to `then` once all items are processed.
pub(crate) fn apply_items_map<M: 'static>(
    mut items: VecDeque<ValuePointer>,
    closure: StackValue,
    mapped: VecDeque<ValuePointer>,
    then: TerminalOp,
    heap: &mut Heap,
) -> FnReturnType<M> {
    let Some(item) = items.pop_front() else {
        return then.finish(mapped, heap);
    };

    struct LocalState {
        items: VecDeque<ValuePointer>,
        closure: StackValue,
        mapped: VecDeque<ValuePointer>,
        then: TerminalOp,
    }

    fn callback<M: 'static>(
        state: CallbackState,
        params: FnParams,
        heap: &mut Heap,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let result = params.first_mut().map(StackValue::take).ok_or_else(|| EnvironmentError::InvalidCallbackParameters)?.into_owned(heap)?;
        s.mapped.push_back(heap.insert(result));

        if let Some(next) = s.items.pop_front() {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.closure.clone(),
                params: vec![heap.take(next).into()],
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }
        s.then.finish(s.mapped, heap)
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure.clone(),
        params: vec![heap.take(item).into()],
        state: Box::new(LocalState { items, closure, mapped, then }),
        callback_params_len: 1,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Apply a filter closure over a collected `items` vec, building a `kept` vec,
/// then forward to `then` once all items are tested.
pub(crate) fn apply_items_filter<M: 'static>(
    mut items: VecDeque<ValuePointer>,
    closure: StackValue,
    kept: VecDeque<ValuePointer>,
    then: TerminalOp,
    heap: &mut Heap,
) -> FnReturnType<M> {
    let Some(current) = items.pop_front() else {
        return then.finish(kept, heap);
    };

    struct LocalState {
        items: VecDeque<ValuePointer>,
        current: ValuePointer,
        closure: StackValue,
        kept: VecDeque<ValuePointer>,
        then: TerminalOp,
    }

    fn callback<M: 'static>(
        state: CallbackState,
        params: FnParams,
        heap: &mut Heap,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let keep = params.first()
            .ok_or_else(|| EnvironmentError::InvalidCallbackParameters)?
            .as_bool(heap)?;

        if keep {
            s.kept.push_back(s.current);
        }

        if let Some(next) = s.items.pop_front() {
            s.current = next;
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.closure.clone(),
                params: vec![s.current.clone_value(heap).into()],
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }
        s.then.finish(s.kept, heap)
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure.clone(),
        params: vec![current.clone_value(heap).into()],
        state: Box::new(LocalState { items, current, closure, kept, then }),
        callback_params_len: 1,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Call `f` for every item in `items` (for_each semantics); return `None` when done.
pub(crate) fn apply_items_none<M: 'static>(items: VecDeque<ValuePointer>, f: StackValue, index: usize, heap: &mut Heap) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::None);
    };
    struct LocalState {
        items: VecDeque<ValuePointer>,
        index: usize,
        f: StackValue,
    }
    fn callback<M: 'static>(
        state: CallbackState,
        _: FnParams,
        heap: &mut Heap,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;

        s.index += 1;
        if let Some(next) = s.items.get(s.index) {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.f.clone(),
                params: vec![next.clone_value(heap).into()],
                state: s,
                callback_params_len: 0,
                callback: CallbackType::Sync(callback::<M>),
            });
        }
        Ok(SysCallResult::None)
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: f.clone(),
        params: vec![item.clone_value(heap).into()],
        state: Box::new(LocalState { items, index, f }),
        callback_params_len: 0,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Return the first item for which `pred` returns true, or null.
pub(crate) fn apply_items_find<M: 'static>(items: VecDeque<ValuePointer>, pred: StackValue, index: usize, heap: &mut Heap) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(Primitive::Null.into()));
    };
    struct LocalState {
        items: VecDeque<ValuePointer>,
        index: usize,
        pred: StackValue,
    }
    fn callback<M: 'static>(
        state: CallbackState,
        params: FnParams,
        heap: &mut Heap,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let matched = params.first().ok_or_else(|| EnvironmentError::InvalidCallbackParameters)?.as_bool(heap)?;
        if matched {
            let result = s.items[s.index].clone_value(heap).into();
            return Ok(SysCallResult::Return(result));
        }

        s.index += 1;
        if let Some(next) = s.items.get(s.index) {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.pred.clone(),
                params: vec![next.clone_value(heap).into()],
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: pred.clone(),
        params: vec![item.clone_value(heap).into()],
        state: Box::new(LocalState { items, index, pred }),
        callback_params_len: 1,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Return `true` as soon as `pred` matches any item, or `false` if none match.
pub(crate) fn apply_items_any<M: 'static>(items: VecDeque<ValuePointer>, pred: StackValue, index: usize, heap: &mut Heap) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(Primitive::Boolean(false).into()));
    };
    struct LocalState {
        items: VecDeque<ValuePointer>,
        index: usize,
        pred: StackValue,
    }
    fn callback<M: 'static>(
        state: CallbackState,
        params: FnParams,
        heap: &mut Heap,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let matched = params.first().ok_or_else(|| EnvironmentError::InvalidCallbackParameters)?.as_bool(heap)?;
        if matched {
            let result = Primitive::Boolean(true).into();
            return Ok(SysCallResult::Return(result));
        }

        s.index += 1;
        if let Some(next) = s.items.get(s.index) {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.pred.clone(),
                params: vec![next.clone_value(heap).into()],
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }
        Ok(SysCallResult::Return(Primitive::Boolean(false).into()))
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: pred.clone(),
        params: vec![item.clone_value(heap).into()],
        state: Box::new(LocalState { items, index, pred }),
        callback_params_len: 1,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Return `false` as soon as `pred` fails for any item, or `true` if all match.
pub(crate) fn apply_items_all<M: 'static>(items: VecDeque<ValuePointer>, pred: StackValue, index: usize, heap: &mut Heap) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(Primitive::Boolean(true).into()));
    };
    struct LocalState {
        items: VecDeque<ValuePointer>,
        index: usize,
        pred: StackValue,
    }
    fn callback<M: 'static>(
        state: CallbackState,
        params: FnParams,
        heap: &mut Heap,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let matched = params.first().ok_or_else(|| EnvironmentError::InvalidCallbackParameters)?.as_bool(heap)?;
        if !matched {
            let result = Primitive::Boolean(false).into();
            return Ok(SysCallResult::Return(result));
        }

        s.index += 1;
        if let Some(next) = s.items.get(s.index) {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.pred.clone(),
                params: vec![next.clone_value(heap).into()],
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }
        Ok(SysCallResult::Return(Primitive::Boolean(true).into()))
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: pred.clone(),
        params: vec![item.clone_value(heap).into()],
        state: Box::new(LocalState { items, index, pred }),
        callback_params_len: 1,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Return the index of the first item for which `pred` returns true, or null.
pub(crate) fn apply_items_position<M: 'static>(items: VecDeque<ValuePointer>, pred: StackValue, index: usize, heap: &mut Heap) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(Primitive::Null.into()));
    };
    struct LocalState {
        items: VecDeque<ValuePointer>,
        index: usize,
        pred: StackValue,
    }
    fn callback<M: 'static>(
        state: CallbackState,
        params: FnParams,
        heap: &mut Heap,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let matched = params.first().ok_or_else(|| EnvironmentError::InvalidCallbackParameters)?.as_bool(heap)?;
        if matched {
            let result = Primitive::Integer(PrimitiveInteger::U32(s.index as u32)).into();
            return Ok(SysCallResult::Return(result));
        }

        s.index += 1;
        if let Some(next) = s.items.get(s.index) {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.pred.clone(),
                params: vec![next.clone_value(heap).into()],
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: pred.clone(),
        params: vec![item.clone_value(heap).into()],
        state: Box::new(LocalState { items, index, pred }),
        callback_params_len: 1,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Left-fold over `items` using `closure(acc, item) -> acc`.
pub(crate) fn apply_items_fold<M: 'static>(
    items: VecDeque<ValuePointer>,
    closure: StackValue,
    mut acc: StackValue,
    index: usize,
    heap: &mut Heap,
) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(acc));
    };
    let state_acc = acc.reference(heap);
    struct LocalState {
        items: VecDeque<ValuePointer>,
        index: usize,
        closure: StackValue,
        acc: StackValue,
    }
    fn callback<M: 'static>(
        state: CallbackState,
        params: FnParams,
        heap: &mut Heap,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let new_acc = params.first_mut().map(StackValue::take).ok_or_else(|| EnvironmentError::InvalidCallbackParameters)?;
        s.acc = new_acc;
        s.index += 1;

        if let Some(next) = s.items.get(s.index) {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.closure.clone(),
                params: vec![s.acc.reference(heap), next.clone_value(heap).into()],
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }
        Ok(SysCallResult::Return(s.acc))
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure.clone(),
        params: vec![acc, item.clone_value(heap).into()],
        state: Box::new(LocalState { items, index, closure, acc: state_acc }),
        callback_params_len: 1,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Sum a homogeneous list of numeric values, preserving the concrete number type.
///
/// Uses `Primitive::add` which keeps the exact type (u8, u16, …, u256) and
/// wrapping semantics.  Returns `U64(0)` for an empty list as a
/// conventional fallback (the type cannot be inferred without items).
pub(crate) fn sum_items(items: VecDeque<ValuePointer>, heap: &mut Heap) -> Result<StackValue, EnvironmentError> {
    let mut acc: Option<Primitive> = None;
    for ptr in &items {
        match ptr.as_ref(heap) {
            RuntimeValue::Primitive(p) => {
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
    Ok(acc.unwrap_or_else(|| Primitive::Integer(PrimitiveInteger::U64(0))).into())
}

/// Retain the destination slot across callback continuations.
#[derive(Debug, Clone)]
pub(crate) struct IteratorWriteBack {
    slot: StackValue,
}

/// Restore detached callback-generated state to its VM slot.
fn write_back_iter(
    mut write_back: IteratorWriteBack,
    new_iter: XIterator,
    heap: &mut Heap,
) -> Result<(), EnvironmentError> {
    let value = RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(new_iter));
    heap.replace_stack(&mut write_back.slot, value);
    Ok(())
}

/// When the current source is exhausted, try to promote the first `Chain`
/// step to become the new source.  The chain's own pipe steps are spliced in
/// so they run before any outer steps that follow the `Chain` adapter:
///
/// ```text
/// pipe: [A, Chain(src2 / [B, C]), D]
///   ->  source = src2, pipe = [B, C, D]
/// ```
///
/// Returns `true` if a chain was promoted, `false` if none remain.
fn promote_chain_to_source(iter: &mut XIterator) -> bool {
    let Some(pos) = iter.pipe.iter().position(|s| matches!(s, PipeStep::Chain(_))) else {
        return false;
    };

    if let Some(PipeStep::Chain(chain)) = iter.pipe.remove(pos) {
        let mut chain = *chain;
        let after: VecDeque<PipeStep> = iter.pipe.drain(pos..).collect();
        // New pipe = chain's own steps + outer steps that followed Chain.
        let mut new_pipe = mem::take(&mut chain.pipe);
        new_pipe.extend(after);
        let replacement = XIterator { source: mem::take(&mut chain.source), pipe: new_pipe };
        *iter = replacement;
    }

    true
}

/// Push `item` back to the front of `iter`'s source so it is seen again on
/// the next pull.  Used to "undo" a pull before falling back to full
/// collection.
fn prepend_item_to_source(
    iter: &mut XIterator,
    item: ValuePointer,
) -> Result<(), EnvironmentError> {
    match &mut iter.source {
        BaseSource::Dequeue(d) => d.push_front(item),
        BaseSource::Once(slot) if slot.is_none() => *slot = Some(item),
        _ => {
            // General case: wrap everything in a new Dequeue.
            let old = mem::replace(&mut iter.source, BaseSource::Empty);
            let mut d = VecDeque::new();
            d.push_back(item);
            match old {
                BaseSource::Dequeue(rest) => d.extend(rest),
                BaseSource::Once(Some(v)) => d.push_back(v),
                _ => {},
            }
            iter.source = BaseSource::Dequeue(d);
        }
    }
    Ok(())
}

/// Pull one raw item from the source, handling Chain promotion on exhaustion,
/// then hand off to [`lazy_next_pipe`].
fn lazy_next_pull<M: 'static>(
    mut iter: XIterator,
    write_back: IteratorWriteBack,
    heap: &mut Heap,
) -> FnReturnType<M> {
    loop {
        let raw = match &mut iter.source {
            BaseSource::Dequeue(d) => d.pop_front(),
            BaseSource::Once(slot) => slot.take(),
            BaseSource::Empty => None,
            BaseSource::Unfold { .. } => return lazy_next_unfold(iter, write_back, heap),
        };

        match raw {
            Some(item) => return lazy_next_pipe(iter, item, 0, write_back, heap),
            None => {
                if !promote_chain_to_source(&mut iter) {
                    write_back_iter(write_back, XIterator::empty(), heap)?;
                    return Ok(SysCallResult::Return(Primitive::Null.into()));
                }
                // Loop: pull from the newly-promoted source.
            }
        }
    }
}

/// Pull the next item from a sync source (Dequeue / Once / Empty), handling
/// chain promotion on exhaustion.  Returns `None` if everything is exhausted,
/// `NeedsCallback` if the source is `Unfold`.
enum SourcePull { Item(Option<ValuePointer>), NeedsCallback }

fn pull_sync_source(iter: &mut XIterator) -> SourcePull {
    loop {
        let raw = match &mut iter.source {
            BaseSource::Dequeue(d) => d.pop_front(),
            BaseSource::Once(slot) => slot.take(),
            BaseSource::Empty => None,
            BaseSource::Unfold { .. } => return SourcePull::NeedsCallback,
        };
        match raw {
            Some(item) => return SourcePull::Item(Some(item)),
            None => {
                if !promote_chain_to_source(iter) {
                    return SourcePull::Item(None);
                }
                // Loop: pull from the newly-promoted source.
            }
        }
    }
}

/// Walk `iter.pipe` from `start_idx`, applying each step to `current`.
///
/// * Sync steps (Skip, Take, Enumerate, Chain pass-through): mutate state
///   inline and loop.
/// * Map / Filter: suspend via `ExecuteAndCallback`; resume via the nested
///   callback functions.
/// * Rev / Zip: fall back to full collection + `NextWriteBack`.
fn lazy_next_pipe<M: 'static>(
    mut iter: XIterator,
    mut current: ValuePointer,
    start_idx: usize,
    write_back: IteratorWriteBack,
    heap: &mut Heap,
) -> FnReturnType<M> {
    let mut idx = start_idx;

    loop {
        if idx >= iter.pipe.len() {
            // Item passed every step — yield it and write back the iterator state.
            write_back_iter(write_back, iter, heap)?;
            return Ok(SysCallResult::Return(heap.take(current).into()));
        }

        // Handle Flatten with pending inner elements before the borrow in the main match.
        // If pending is non-empty: push the just-pulled outer item back to the source and emit
        // the next inner element.
        let pending_front: Option<ValuePointer> =
            if let PipeStep::Flatten(p) = &mut iter.pipe[idx] {
                if !p.is_empty() { p.pop_front() } else { None }
            } else {
                None
            };
        if let Some(inner_item) = pending_front {
            prepend_item_to_source(&mut iter, current)?;
            current = inner_item;
            idx += 1;
            continue;
        }

        // Lazy Zip: if the right side requires no callbacks, advance one item from each
        // side without collecting the full sequence on either side.
        // (The borrow of right_box ends after `next_sync(heap)`: NLL lets us reborrow `iter`
        //  in the main match below when this early-return path is not taken.)
        if let PipeStep::Zip(right_box) = &mut iter.pipe[idx] {
            if !right_box.needs_callback() {
                let r = right_box.next_sync(heap)?;
                // right_box borrow ends here (NLL: last use above)
                match r {
                    None => {
                        // Right side exhausted, zip has terminated
                        write_back_iter(write_back, XIterator::empty(), heap)?;
                        return Ok(SysCallResult::Return(Primitive::Null.into()));
                    }
                    Some(r_val) => {
                        current = heap.insert(RuntimeValue::Object(vec![current, r_val]));
                        idx += 1;
                        continue;
                    }
                }
            }
        }

        match &mut iter.pipe[idx] {
            PipeStep::Skip(n) => {
                if *n > 0 {
                    *n -= 1;
                    // Item consumed by skip; fetch the next one iteratively
                    // for sync sources to avoid O(n) stack growth.
                    match pull_sync_source(&mut iter) {
                        SourcePull::Item(Some(item)) => {
                            current = item;
                            idx = 0;
                            continue;
                        }
                        SourcePull::Item(None) => {
                            write_back_iter(write_back, XIterator::empty(), heap)?;
                            return Ok(SysCallResult::Return(Primitive::Null.into()));
                        }
                        SourcePull::NeedsCallback => {
                            // Unfold source: fall back to callback path (bounded
                            // by the callback roundtrip, so no stack overflow).
                            return lazy_next_pull(iter, write_back, heap);
                        }
                    }
                }
                idx += 1;
            }
            PipeStep::Take(remaining) => {
                if *remaining == 0 {
                    write_back_iter(write_back, XIterator::empty(), heap)?;
                    return Ok(SysCallResult::Return(Primitive::Null.into()));
                }
                *remaining -= 1;
                idx += 1;
            }
            PipeStep::Enumerate(counter) => {
                let c = *counter;
                *counter += 1;
                let counter = heap.insert(Primitive::Integer(PrimitiveInteger::U32(c)).into());
                current = heap.insert(RuntimeValue::Object(vec![
                    counter,
                    current,
                ]));
                idx += 1;
            }
            // Chain is a secondary source activated only when the primary
            // source is empty. Items from the primary source pass straight
            // through this step.
            PipeStep::Chain(_) => {
                idx += 1;
            }
            PipeStep::Map(closure) => {
                let closure = closure.clone();
                struct LocalState {
                    iter: XIterator,
                    pipe_idx: usize,
                    write_back: IteratorWriteBack,
                }
                fn callback<M: 'static>(
                    state: CallbackState,
                    params: FnParams,
                    heap: &mut Heap,
                ) -> Result<SysCallResult<M>, EnvironmentError> {
                    let s = state
                        .downcast::<LocalState>()
                        .map_err(|_| EnvironmentError::InvalidCallbackState)?;
                    let mapped = params
                        .drain(..)
                        .next()
                        .ok_or_else(|| EnvironmentError::InvalidCallbackParameters)?;
                    let mapped = mapped.into_owned(heap)?;
                    let ptr = heap.insert(mapped);
                    lazy_next_pipe(s.iter, ptr, s.pipe_idx + 1, s.write_back, heap)
                }
                return Ok(SysCallResult::ExecuteAndCallback {
                    ptr: closure,
                    params: vec![heap.take(current).into()],
                    state: Box::new(LocalState { iter, pipe_idx: idx, write_back }),
                    callback_params_len: 1,
                    callback: CallbackType::Sync(callback::<M>),
                });
            }
            PipeStep::Filter(closure) => {
                let closure = closure.clone();
                struct LocalState {
                    iter: XIterator,
                    current: ValuePointer,
                    pipe_idx: usize,
                    write_back: IteratorWriteBack,
                }
                fn callback<M: 'static>(
                    state: CallbackState,
                    params: FnParams,
                    heap: &mut Heap,
                ) -> Result<SysCallResult<M>, EnvironmentError> {
                    let s = state
                        .downcast::<LocalState>()
                        .map_err(|_| EnvironmentError::InvalidCallbackState)?;
                    let keep = params
                        .first()
                        .ok_or_else(|| EnvironmentError::InvalidCallbackParameters)?
                        .as_bool(heap)?;
                    if keep {
                        lazy_next_pipe(s.iter, s.current, s.pipe_idx + 1, s.write_back, heap)
                    } else {
                        // Item rejected — go back and pull the next one.
                        lazy_next_pull(s.iter, s.write_back, heap)
                    }
                }
                return Ok(SysCallResult::ExecuteAndCallback {
                    ptr: closure,
                    params: vec![current.clone_value(heap).into()],
                    state: Box::new(LocalState { iter, current, pipe_idx: idx, write_back }),
                    callback_params_len: 1,
                    callback: CallbackType::Sync(callback::<M>),
                });
            }
            // Rev requires full collection (unreachable in practice: iter_rev now reverses
            // in-place and never pushes PipeStep::Rev).
            // Zip with an async right side (needs_callback() == true) also falls back here.
            PipeStep::Rev | PipeStep::Zip(_) => {
                prepend_item_to_source(&mut iter, current)?;
                return lazy_collect(
                    &mut iter,
                    VecDeque::new(),
                    TerminalOp::Next(Box::new(write_back)),
                    heap,
                );
            }
            // Flatten is fully lazy: `pending` holds remaining inner elements from the
            // current expansion.  When pending is non-empty, the item was already emitted
            // above (before this match).  Here pending is always empty: expand `current`
            // (the next outer item) by moving its elements into pending and yielding the
            // first element immediately.
            PipeStep::Flatten(pending) => {
                let item_cell = current.into_owned(heap)?;
                match item_cell {
                    RuntimeValue::Object(elements) => {
                        let mut d: VecDeque<ValuePointer> = elements.into();
                        let first = d.pop_front();
                        *pending = d;
                        match first {
                            Some(f) => {
                                current = f;
                                idx += 1;
                            }
                            // Empty inner array: skip to next outer item iteratively.
                            None => {
                                match pull_sync_source(&mut iter) {
                                    SourcePull::Item(Some(item)) => {
                                        current = item;
                                        idx = 0;
                                        continue;
                                    }
                                    SourcePull::Item(None) => {
                                        write_back_iter(write_back, XIterator::empty(), heap)?;
                                        return Ok(SysCallResult::Return(Primitive::Null.into()));
                                    }
                                    SourcePull::NeedsCallback => return lazy_next_pull(iter, write_back, heap),
                                }
                            }
                        }
                    }
                    RuntimeValue::Opaque(w) => {
                        let inner = w.into_inner::<XIterator>()
                            .map_err(|_| EnvironmentError::Static("flatten: expected array or Iterator"))?;
                        if inner.needs_callback() {
                            return Err(EnvironmentError::Static(
                                "flatten: callback-based inner iterators are not supported in lazy next()",
                            ));
                        }
                        let mut it = inner;
                        let mut d = VecDeque::new();
                        while let Some(item) = it.next_sync(heap)? {
                            d.push_back(item);
                        }
                        let first = d.pop_front();
                        *pending = d;
                        match first {
                            Some(f) => {
                                current = f;
                                idx += 1;
                            }
                            None => {
                                match pull_sync_source(&mut iter) {
                                    SourcePull::Item(Some(item)) => {
                                        current = item;
                                        idx = 0;
                                        continue;
                                    }
                                    SourcePull::Item(None) => {
                                        write_back_iter(write_back, XIterator::empty(), heap)?;
                                        return Ok(SysCallResult::Return(Primitive::Null.into()));
                                    }
                                    SourcePull::NeedsCallback => return lazy_next_pull(iter, write_back, heap),
                                }
                            }
                        }
                    }
                    _ => return Err(EnvironmentError::Static("flatten: expected array or Iterator")),
                }
            }
        }
    }
}

/// Handle the `Unfold` source variant in the lazy-next path.
/// Fires the unfold closure once and, on a `(item, next_state)` result,
/// feeds the item into [`lazy_next_pipe`].
fn lazy_next_unfold<M: 'static>(
    mut iter: XIterator,
    write_back: IteratorWriteBack,
    heap: &mut Heap,
) -> FnReturnType<M> {
    let (closure, seed) = match &mut iter.source {
        BaseSource::Unfold { closure, state: Some(s) } => {
            (closure.clone(), s.reference(heap))
        }
        BaseSource::Unfold { state: None, .. } => {
            iter.source = BaseSource::Empty;
            if promote_chain_to_source(&mut iter) {
                return lazy_next_pull(iter, write_back, heap);
            }
            write_back_iter(write_back, XIterator::empty(), heap)?;
            return Ok(SysCallResult::Return(Primitive::Null.into()));
        }
        _ => return Err(EnvironmentError::Static("lazy_next_unfold called on non-Unfold source")),
    };

    struct LocalState {
        iter: XIterator,
        write_back: IteratorWriteBack,
    }
    fn callback<M: 'static>(
        state: CallbackState,
        params: FnParams,
        heap: &mut Heap,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state
            .downcast::<LocalState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let result = params
            .drain(..)
            .next()
            .ok_or_else(|| EnvironmentError::InvalidCallbackParameters)?
            .into_owned(heap)?;

        match result {
            RuntimeValue::Primitive(Primitive::Null) => {
                s.iter.source = BaseSource::Empty;
                if promote_chain_to_source(&mut s.iter) {
                    return lazy_next_pull(s.iter, s.write_back, heap);
                }
                write_back_iter(s.write_back, XIterator::empty(), heap)?;
                Ok(SysCallResult::Return(Primitive::Null.into()))
            }
            RuntimeValue::Object(tuple) if tuple.len() == 2 => {
                let mut fields = tuple.into_iter();
                let item_ptr = fields.next().expect("unfold item");
                let next_state: StackValue = heap.take(fields.next().expect("unfold state")).into();
                if let BaseSource::Unfold { state, .. } = &mut s.iter.source {
                    *state = Some(next_state);
                }
                lazy_next_pipe(s.iter, item_ptr, 0, s.write_back, heap)
            }
            _ => Err(EnvironmentError::Static("unfold: expected optional (item, next_state)")),
        }
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure,
        params: vec![seed],
        state: Box::new(LocalState { iter, write_back }),
        callback_params_len: 1,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Lazily advance `iter` by exactly one item, applying every pipe step
/// item-by-item without collecting the full sequence.
///
/// When `write_back` is `Some`, the modified iterator is written back to
/// that stack slot after the item is found (or when exhausted), so that
/// subsequent `next()` calls see the updated state.
pub(crate) fn lazy_next<M: 'static>(
    iter: XIterator,
    write_back: StackValue,
    heap: &mut Heap,
) -> FnReturnType<M> {
    lazy_next_pull(iter, IteratorWriteBack { slot: write_back }, heap)
}

#[cfg(test)]
mod tests {
    use super::*;

    // Stored data is shallow, but enumeration creates progressively deeper
    // tuples before flattening retains the remaining nested tuple.
    fn inner_iter(nesting: usize, heap: &mut Heap) -> XIterator {
        let mut iter = XIterator::once(Primitive::Integer(PrimitiveInteger::U64(1)).into(), heap).unwrap();
        for _ in 0..nesting {
            iter.push_step(PipeStep::Enumerate(0)).unwrap();
        }
        iter.push_step(PipeStep::Flatten(VecDeque::new())).unwrap();
        iter
    }

    fn inner_value(nesting: usize, heap: &mut Heap) -> RuntimeValue {
        RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(inner_iter(nesting, heap))).into()
    }

    fn inner_id(nesting: usize, heap: &mut Heap) -> ValuePointer {
        let value = inner_value(nesting, heap);
        heap.insert(value)
    }

    fn resume(result: SysCallResult<()>, value: RuntimeValue, heap: &mut Heap) -> FnReturnType<()> {
        let SysCallResult::ExecuteAndCallback {
            state,
            callback: CallbackType::Sync(callback),
            ..
        } = result
        else {
            panic!("expected a synchronous callback continuation");
        };
        let mut params = vec![value.into()];
        callback(state, &mut params, heap)
    }

    fn assert_count(result: SysCallResult<()>, heap: &Heap) {
        let SysCallResult::Return(count) = result else {
            panic!("expected count");
        };
        assert_eq!(count.as_u32(heap).unwrap(), 2);
    }

    #[test]
    fn eager_flatten_accepts_deep_transformed_items() {
        let mut heap = unsafe { Heap::new() };
        for nesting in [4, 17, 64] {
            let input = inner_id(nesting, &mut heap);
            let work = flatten_finish::<()>(VecDeque::from([input]), VecDeque::new(), TerminalOp::Count, &mut heap).unwrap();
            assert_count(drive(work, &mut heap).unwrap(), &heap);
        }
    }

    #[test]
    fn map_callback_accepts_deep_transformed_items() {
        let mut heap = unsafe { Heap::new() };
        let item = heap.insert(PrimitiveInteger::U64(1).into());
        let result = apply_items_map::<()>(VecDeque::from([item]), Primitive::Null.into(), VecDeque::new(), TerminalOp::Count.push_step(AggStep::Flatten), &mut heap).unwrap();
        let value = inner_value(64, &mut heap);
        assert_count(resume(result, value, &mut heap).unwrap(), &heap);
    }

    #[test]
    fn filter_callback_accepts_deep_transformed_items() {
        let mut heap = unsafe { Heap::new() };
        let item = inner_id(64, &mut heap);
        let result = apply_items_filter::<()>(VecDeque::from([item]), Primitive::Null.into(), VecDeque::new(), TerminalOp::Count.push_step(AggStep::Flatten), &mut heap).unwrap();
        assert_count(resume(result, Primitive::Boolean(true).into(), &mut heap).unwrap(), &heap);
    }

    #[test]
    fn unfold_callback_accepts_deep_transformed_items() {
        let mut heap = unsafe { Heap::new() };
        let mut iter = XIterator::empty();
        iter.source = BaseSource::Unfold {
            closure: Primitive::Null.into(),
            state: Some(PrimitiveInteger::U64(1).into()),
        };
        iter.push_step(PipeStep::Take(1)).unwrap();
        iter.push_step(PipeStep::Flatten(VecDeque::new())).unwrap();
        let result = lazy_collect::<()>(&mut iter, VecDeque::new(), TerminalOp::Count, &mut heap).unwrap();
        let inner = inner_id(64, &mut heap);
        let state = heap.insert(PrimitiveInteger::U64(2).into());
        assert_count(resume(result, RuntimeValue::Object(vec![inner, state]), &mut heap).unwrap(), &heap);
    }
}
