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
    Next(Box<StackValue>),
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
        match self {
            TerminalOp::Next(_) => Some(1),
            TerminalOp::Aggregated { steps, then } => {
                // Walk the steps looking for an early-termination hint.
                let base = then.take_limit();
                let mut limit = base;
                // Accumulate extra items needed for Skip steps (in reverse).
                for step in steps.iter().rev() {
                    match step {
                        AggStep::Take(n) => limit = Some(*n),
                        AggStep::Skip(n) => {
                            limit = limit.and_then(|l| l.checked_add(*n));
                        }
                        _ => {}
                    }
                }
                limit
            }
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
            TerminalOp::Next(write_back) => {
                let head = items.pop_front();
                // Write remaining items back as a fresh Dequeue iterator.
                write_back_iter(*write_back, XIterator::from_dequeue(items))?;
                Ok(SysCallResult::Return(match head {
                    Some(ptr) => ptr.to_owned().into(),
                    None => Primitive::Null.into(),
                }))
            }
            TerminalOp::Aggregated { steps, then } => {
                finish_aggregated(items, steps, *then)
            }
        }
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
) -> FnReturnType<M> {
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
                    let pair = ValueCell::Object(vec![
                        ValuePointer::new(Primitive::U32(counter + i as u32).into()),
                        item,
                    ]);
                    ValuePointer::new(pair)
                }).collect();
                items = enumerated;
            }
            AggStep::Prepend(prefix) => {
                let mut combined = prefix;
                combined.extend(items.drain(..));
                items = combined;
            }
            AggStep::ZipMerge(left) => {
                let zipped: VecDeque<ValuePointer> = left.into_iter().zip(items).map(|(l, r)| {
                    ValuePointer::new(ValueCell::Object(vec![l, r]))
                }).collect();
                items = zipped;
            }
            AggStep::Map(closure) => {
                // Need per-item VM callbacks; package remaining steps + then as continuation.
                let cont = TerminalOp::Aggregated { steps, then: Box::new(then) };
                return apply_items_map(items, closure, VecDeque::new(), cont);
            }
            AggStep::Filter(closure) => {
                let cont = TerminalOp::Aggregated { steps, then: Box::new(then) };
                return apply_items_filter(items, closure, VecDeque::new(), cont);
            }
            AggStep::Chain(mut remaining) => {
                // Collect `remaining` into a *fresh* accumulator so that any pipe
                // steps inside `remaining` (Map, Filter, …) only apply to its own
                // items, not to the already-collected first-part `items`.
                // `AggStep::Prepend` splices the first-part items back at the front.
                let cont = TerminalOp::Aggregated { steps, then: Box::new(then) }
                    .push_step(AggStep::Prepend(items));
                return lazy_collect(&mut remaining, VecDeque::new(), cont);
            }
            AggStep::Zip(mut right) => {
                // `items` is the left side; collect the right side then merge.
                let cont = TerminalOp::Aggregated { steps, then: Box::new(then) }
                    .push_step(AggStep::ZipMerge(items));
                return lazy_collect(&mut right, VecDeque::new(), cont);
            }
            AggStep::Flatten => {
                let cont = TerminalOp::Aggregated { steps, then: Box::new(then) };
                return flatten_finish(items, VecDeque::new(), cont);
            }
            AggStep::FlattenContinue(containers) => {
                let cont = TerminalOp::Aggregated { steps, then: Box::new(then) };
                return flatten_finish(containers, items, cont);
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
    then.finish(items)
}

/// Expand `containers` (each an array or Iterator) into `flat`, then run `then`.
/// If a container is a callback-based Iterator, suspends via `lazy_collect` and
/// resumes via `AggStep::FlattenContinue`.
fn flatten_finish<M: 'static>(
    mut containers: VecDeque<ValuePointer>,
    mut flat: VecDeque<ValuePointer>,
    then: TerminalOp,
) -> FnReturnType<M> {
    while let Some(ptr) = containers.pop_front() {
        match ptr.into_owned() {
            ValueCell::Object(v) => flat.extend(v),
            ValueCell::Primitive(Primitive::Opaque(w)) => {
                let mut inner = w.into_inner::<XIterator>()
                    .map_err(|_| EnvironmentError::Static("flatten: expected Iterator or array"))?;
                if inner.needs_callback() {
                    // Collect this sub-iterator via callbacks; resume the remaining containers
                    // afterwards through AggStep::FlattenContinue.
                    return lazy_collect(&mut inner, flat, then.push_step(AggStep::FlattenContinue(containers)));
                }
                let mut it = inner;
                while let Some(item) = it.next_sync()? {
                    flat.push_back(item);
                }
            }
            _ => return Err(EnvironmentError::Static("flatten: expected array or Iterator")),
        }
    }
    then.finish(flat)
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
pub(crate) fn lazy_collect<M: 'static>(
    iter: &mut XIterator,
    mut acc: VecDeque<ValuePointer>,
    mut op: TerminalOp,
) -> FnReturnType<M> {
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
            op.finish(acc)
        }
        BaseSource::Once(slot) => {
            if let Some(ptr) = slot.take() {
                acc.push_back(ptr);
            }
            op.finish(acc)
        }
        BaseSource::Empty => op.finish(acc),
        BaseSource::Unfold { closure, state } => {
            let seed = match state {
                Some(s) => s,
                None => return op.finish(acc),
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
            ) -> Result<SysCallResult<M>, EnvironmentError> {
                let mut s = state.downcast::<LocalState>()
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
                        if s.take_limit.map_or(false, |lim| s.acc.len() >= lim) {
                            return s.op.finish(s.acc);
                        }
                        let next_seed: StackValue = tuple[1].to_owned().into();
                        Ok(SysCallResult::ExecuteAndCallback {
                            ptr: s.closure.clone(),
                            params: vec![next_seed].into(),
                            state: s,
                            callback_params_len: 1,
                            callback: CallbackType::Sync(callback::<M>),
                        })
                    }
                    _ => Err(EnvironmentError::Static("unfold: expected optional tuple")),
                }
            }
            Ok(SysCallResult::ExecuteAndCallback {
                ptr: closure.clone(),
                params: vec![seed.reference()].into(),
                state: Box::new(LocalState { closure: closure.reference(), acc, op, take_limit }),
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            })
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
) -> FnReturnType<M> {
    let Some(item) = items.pop_front() else {
        return then.finish(mapped);
    };

    struct LocalState {
        items: VecDeque<ValuePointer>,
        closure: StackValue,
        mapped: VecDeque<ValuePointer>,
        then: TerminalOp
    }

    fn callback<M: 'static>(
        state: CallbackState,
        params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let result = params.into_iter().next().ok_or(EnvironmentError::InvalidCallbackParameters)?.into_owned();
        s.mapped.push_back(ValuePointer::new(result));

        if let Some(next) = s.items.pop_front() {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.closure.clone(),
                params: vec![next.to_owned().into()].into(),
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }

        s.then.finish(s.mapped)
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure.clone(),
        params: vec![item.to_owned().into()].into(),
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
) -> FnReturnType<M> {
    let Some(current) = items.pop_front() else {
        return then.finish(kept);
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
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let keep = params.first()
            .ok_or(EnvironmentError::InvalidCallbackParameters)?
            .as_bool()?;

        if keep {
            s.kept.push_back(s.current.clone());
        }

        if let Some(next) = s.items.pop_front() {
            s.current = next.clone();
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.closure.clone(),
                params: vec![next.to_owned().into()].into(),
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }

        s.then.finish(s.kept)
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure.clone(),
        params: vec![current.to_owned().into()].into(),
        state: Box::new(LocalState { items, current, closure, kept, then }),
        callback_params_len: 1,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Call `f` for every item in `items` (for_each semantics); return `None` when done.
pub(crate) fn apply_items_none<M: 'static>(items: VecDeque<ValuePointer>, f: StackValue, index: usize) -> FnReturnType<M> {
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
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;

        s.index += 1;
        if let Some(next) = s.items.get(s.index) {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.f.clone(),
                params: vec![next.to_owned().into()].into(),
                state: s,
                callback_params_len: 0,
                callback: CallbackType::Sync(callback::<M>),
            });
        }

        Ok(SysCallResult::None)
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: f.clone(),
        params: vec![item.to_owned().into()].into(),
        state: Box::new(LocalState { items, index, f }),
        callback_params_len: 0,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Return the first item for which `pred` returns true, or null.
pub(crate) fn apply_items_find<M: 'static>(items: VecDeque<ValuePointer>, pred: StackValue, index: usize) -> FnReturnType<M> {
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
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let matched = params.first().ok_or(EnvironmentError::InvalidCallbackParameters)?.as_bool()?;
        if matched {
            return Ok(SysCallResult::Return(s.items[s.index].to_owned().into()));
        }

        s.index += 1;
        if let Some(next) = s.items.get(s.index) {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.pred.clone(),
                params: vec![next.to_owned().into()].into(),
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }

        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: pred.clone(),
        params: vec![item.to_owned().into()].into(),
        state: Box::new(LocalState { items, index, pred }),
        callback_params_len: 1,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Return `true` as soon as `pred` matches any item, or `false` if none match.
pub(crate) fn apply_items_any<M: 'static>(items: VecDeque<ValuePointer>, pred: StackValue, index: usize) -> FnReturnType<M> {
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
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let matched = params.first().ok_or(EnvironmentError::InvalidCallbackParameters)?.as_bool()?;
        if matched {
            return Ok(SysCallResult::Return(Primitive::Boolean(true).into()));
        }

        s.index += 1;
        if let Some(next) = s.items.get(s.index) {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.pred.clone(),
                params: vec![next.to_owned().into()].into(),
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }

        Ok(SysCallResult::Return(Primitive::Boolean(false).into()))
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: pred.clone(),
        params: vec![item.to_owned().into()].into(),
        state: Box::new(LocalState { items, index, pred }),
        callback_params_len: 1,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Return `false` as soon as `pred` fails for any item, or `true` if all match.
pub(crate) fn apply_items_all<M: 'static>(items: VecDeque<ValuePointer>, pred: StackValue, index: usize) -> FnReturnType<M> {
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
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let matched = params.first().ok_or(EnvironmentError::InvalidCallbackParameters)?.as_bool()?;
        if !matched {
            return Ok(SysCallResult::Return(Primitive::Boolean(false).into()));
        }

        s.index += 1;
        if let Some(next) = s.items.get(s.index) {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.pred.clone(),
                params: vec![next.to_owned().into()].into(),
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }

        Ok(SysCallResult::Return(Primitive::Boolean(true).into()))
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: pred.clone(),
        params: vec![item.to_owned().into()].into(),
        state: Box::new(LocalState { items, index, pred }),
        callback_params_len: 1,
        callback: CallbackType::Sync(callback::<M>),
    })
}

/// Return the index of the first item for which `pred` returns true, or null.
pub(crate) fn apply_items_position<M: 'static>(items: VecDeque<ValuePointer>, pred: StackValue, index: usize) -> FnReturnType<M> {
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
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let matched = params.first().ok_or(EnvironmentError::InvalidCallbackParameters)?.as_bool()?;
        if matched {
            return Ok(SysCallResult::Return(Primitive::U32(s.index as u32).into()));
        }

        s.index += 1;
        if let Some(next) = s.items.get(s.index) {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.pred.clone(),
                params: vec![next.to_owned().into()].into(),
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }

        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: pred.clone(),
        params: vec![item.to_owned().into()].into(),
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
) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(acc));
    };
    let state_acc = acc.reference();
    struct LocalState {
        items: VecDeque<ValuePointer>,
        index: usize,
        closure: StackValue,
        acc: StackValue,
    }
    fn callback<M: 'static>(
        state: CallbackState,
        params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<LocalState>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let new_acc = params.into_iter().next().ok_or(EnvironmentError::InvalidCallbackParameters)?;
        s.acc = new_acc;
        s.index += 1;

        if let Some(next) = s.items.get(s.index) {
            return Ok(SysCallResult::ExecuteAndCallback {
                ptr: s.closure.clone(),
                params: vec![s.acc.reference(), next.to_owned().into()].into(),
                state: s,
                callback_params_len: 1,
                callback: CallbackType::Sync(callback::<M>),
            });
        }

        Ok(SysCallResult::Return(s.acc))
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure.clone(),
        params: vec![acc, item.to_owned().into()].into(),
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

// =============================================================================
// Truly lazy `next()` — per-item pipeline execution
// =============================================================================

/// Write `new_iter` back to the stack slot referenced by `write_back`.
fn write_back_iter(mut write_back: StackValue, new_iter: XIterator) -> Result<(), EnvironmentError> {
    *write_back.as_opaque_type_mut::<XIterator>()? = new_iter;
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

    if let Some(PipeStep::Chain(chain_box)) = iter.pipe.remove(pos) {
        // Use ManuallyDrop to move fields out of the boxed iterator
        // without triggering XIterator's custom Drop.
        let mut chain = std::mem::ManuallyDrop::new(*chain_box);
        // Drain the outer steps that come after the removed Chain position.
        let after: VecDeque<PipeStep> = iter.pipe.drain(pos..).collect();
        // New pipe = chain's own steps + outer steps that followed Chain.
        let mut new_pipe = std::mem::take(&mut chain.pipe);
        new_pipe.extend(after);
        iter.source = std::mem::replace(&mut chain.source, BaseSource::Empty);
        iter.depth = chain.depth;
        iter.pipe = new_pipe;
    }

    true
}

/// Push `item` back to the front of `iter`'s source so it is seen again on
/// the next pull.  Used to "undo" a pull before falling back to full
/// collection.
fn prepend_item_to_source(iter: &mut XIterator, item: ValuePointer) {
    match &mut iter.source {
        BaseSource::Dequeue(d) => d.push_front(item),
        BaseSource::Once(slot) if slot.is_none() => *slot = Some(item),
        _ => {
            // General case: wrap everything in a new Dequeue.
            let old = std::mem::replace(&mut iter.source, BaseSource::Empty);
            let mut d = VecDeque::new();
            d.push_back(item);
            match old {
                BaseSource::Dequeue(rest) => d.extend(rest),
                BaseSource::Once(Some(v)) => d.push_back(v),
                _ => {}
            }
            iter.source = BaseSource::Dequeue(d);
        }
    }
}

/// Pull one raw item from the source, handling Chain promotion on exhaustion,
/// then hand off to [`lazy_next_pipe`].
fn lazy_next_pull<M: 'static>(
    mut iter: XIterator,
    write_back: StackValue,
) -> FnReturnType<M> {
    loop {
        let raw = match &mut iter.source {
            BaseSource::Dequeue(d) => d.pop_front(),
            BaseSource::Once(slot) => slot.take(),
            BaseSource::Empty => None,
            BaseSource::Unfold { .. } => return lazy_next_unfold(iter, write_back),
        };

        match raw {
            Some(item) => return lazy_next_pipe(iter, item, 0, write_back),
            None => {
                if !promote_chain_to_source(&mut iter) {
                    write_back_iter(write_back, XIterator::empty())?;
                    return Ok(SysCallResult::Return(Primitive::Null.into()));
                }
                // Loop: pull from the newly-promoted source.
            }
        }
    }
}

/// Pull the next item from a sync source (Dequeue / Once / Empty), handling
/// chain promotion on exhaustion.  Returns `None` if everything is exhausted,
/// `Err(true)` if the source is `Unfold` (needs callbacks).
fn pull_sync_source(iter: &mut XIterator) -> Result<Option<ValuePointer>, bool> {
    loop {
        let raw = match &mut iter.source {
            BaseSource::Dequeue(d) => d.pop_front(),
            BaseSource::Once(slot) => slot.take(),
            BaseSource::Empty => None,
            BaseSource::Unfold { .. } => return Err(true),
        };
        match raw {
            Some(item) => return Ok(Some(item)),
            None => {
                if !promote_chain_to_source(iter) {
                    return Ok(None);
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
    write_back: StackValue,
) -> FnReturnType<M> {
    let mut idx = start_idx;

    loop {
        if idx >= iter.pipe.len() {
            // Item passed every step — yield it and write back the iterator state.
            write_back_iter(write_back, iter)?;
            return Ok(SysCallResult::Return(current.to_owned().into()));
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
            prepend_item_to_source(&mut iter, current);
            current = inner_item;
            idx += 1;
            continue;
        }

        // Lazy Zip: if the right side requires no callbacks, advance one item from each
        // side without collecting the full sequence on either side.
        // (The borrow of right_box ends after `next_sync()`: NLL lets us reborrow `iter`
        //  in the main match below when this early-return path is not taken.)
        if let PipeStep::Zip(right_box) = &mut iter.pipe[idx] {
            if !right_box.needs_callback() {
                let r = right_box.next_sync()?;
                // right_box borrow ends here (NLL: last use above)
                match r {
                    None => {
                        // Right side exhausted, zip has terminated
                        write_back_iter(write_back, XIterator::empty())?;
                        return Ok(SysCallResult::Return(Primitive::Null.into()));
                    }
                    Some(r_val) => {
                        current = ValuePointer::new(ValueCell::Object(vec![current, r_val]));
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
                        Ok(Some(item)) => {
                            current = item;
                            idx = 0;
                            continue;
                        }
                        Ok(None) => {
                            write_back_iter(write_back, XIterator::empty())?;
                            return Ok(SysCallResult::Return(Primitive::Null.into()));
                        }
                        Err(_) => {
                            // Unfold source: fall back to callback path (bounded
                            // by the callback roundtrip, so no stack overflow).
                            return lazy_next_pull(iter, write_back);
                        }
                    }
                }
                idx += 1;
            }
            PipeStep::Take(remaining) => {
                if *remaining == 0 {
                    write_back_iter(write_back, XIterator::empty())?;
                    return Ok(SysCallResult::Return(Primitive::Null.into()));
                }
                *remaining -= 1;
                idx += 1;
            }
            PipeStep::Enumerate(counter) => {
                let c = *counter;
                *counter += 1;
                current = ValuePointer::new(ValueCell::Object(vec![
                    ValuePointer::new(Primitive::U32(c).into()),
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
                    write_back: StackValue,
                }
                fn callback<M: 'static>(
                    state: CallbackState,
                    params: FnParams,
                ) -> Result<SysCallResult<M>, EnvironmentError> {
                    let s = state
                        .downcast::<LocalState>()
                        .map_err(|_| EnvironmentError::InvalidCallbackState)?;
                    let mapped = params
                        .into_iter()
                        .next()
                        .ok_or(EnvironmentError::InvalidCallbackParameters)?;
                    let ptr = ValuePointer::new(mapped.into_owned());
                    lazy_next_pipe(s.iter, ptr, s.pipe_idx + 1, s.write_back)
                }
                return Ok(SysCallResult::ExecuteAndCallback {
                    ptr: closure,
                    params: vec![current.to_owned().into()].into(),
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
                    write_back: StackValue,
                }
                fn callback<M: 'static>(
                    state: CallbackState,
                    params: FnParams,
                ) -> Result<SysCallResult<M>, EnvironmentError> {
                    let s = state
                        .downcast::<LocalState>()
                        .map_err(|_| EnvironmentError::InvalidCallbackState)?;
                    let keep = params
                        .first()
                        .ok_or(EnvironmentError::InvalidCallbackParameters)?
                        .as_bool()?;
                    if keep {
                        lazy_next_pipe(s.iter, s.current, s.pipe_idx + 1, s.write_back)
                    } else {
                        // Item rejected — go back and pull the next one.
                        lazy_next_pull(s.iter, s.write_back)
                    }
                }
                return Ok(SysCallResult::ExecuteAndCallback {
                    ptr: closure,
                    params: vec![current.to_owned().into()].into(),
                    state: Box::new(LocalState { iter, current, pipe_idx: idx, write_back }),
                    callback_params_len: 1,
                    callback: CallbackType::Sync(callback::<M>),
                });
            }
            // Rev requires full collection (unreachable in practice: iter_rev now reverses
            // in-place and never pushes PipeStep::Rev).
            // Zip with an async right side (needs_callback() == true) also falls back here.
            PipeStep::Rev | PipeStep::Zip(_) => {
                prepend_item_to_source(&mut iter, current);
                return lazy_collect(
                    &mut iter,
                    VecDeque::new(),
                    TerminalOp::Next(Box::new(write_back)),
                );
            }
            // Flatten is fully lazy: `pending` holds remaining inner elements from the
            // current expansion.  When pending is non-empty, the item was already emitted
            // above (before this match).  Here pending is always empty: expand `current`
            // (the next outer item) by moving its elements into pending and yielding the
            // first element immediately.
            PipeStep::Flatten(pending) => {
                let item_cell = current.into_owned();
                match item_cell {
                    ValueCell::Object(elements) => {
                        let mut d: VecDeque<ValuePointer> = elements.into();
                        let first = d.pop_front();
                        *pending = d;
                        match first {
                            Some(f) => { current = f; idx += 1; }
                            // Empty inner array: skip to next outer item iteratively.
                            None => {
                                match pull_sync_source(&mut iter) {
                                    Ok(Some(item)) => {
                                        current = item;
                                        idx = 0;
                                        continue;
                                    }
                                    Ok(None) => {
                                        write_back_iter(write_back, XIterator::empty())?;
                                        return Ok(SysCallResult::Return(Primitive::Null.into()));
                                    }
                                    Err(_) => return lazy_next_pull(iter, write_back),
                                }
                            }
                        }
                    }
                    ValueCell::Primitive(Primitive::Opaque(w)) => {
                        let inner = w.into_inner::<XIterator>()
                            .map_err(|_| EnvironmentError::Static("flatten: expected array or Iterator"))?;
                        if inner.needs_callback() {
                            return Err(EnvironmentError::Static(
                                "flatten: callback-based inner iterators are not supported in lazy next()",
                            ));
                        }
                        let mut it = inner;
                        let mut d = VecDeque::new();
                        while let Some(item) = it.next_sync()? {
                            d.push_back(item);
                        }
                        let first = d.pop_front();
                        *pending = d;
                        match first {
                            Some(f) => { current = f; idx += 1; }
                            None => {
                                match pull_sync_source(&mut iter) {
                                    Ok(Some(item)) => {
                                        current = item;
                                        idx = 0;
                                        continue;
                                    }
                                    Ok(None) => {
                                        write_back_iter(write_back, XIterator::empty())?;
                                        return Ok(SysCallResult::Return(Primitive::Null.into()));
                                    }
                                    Err(_) => return lazy_next_pull(iter, write_back),
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
    write_back: StackValue,
) -> FnReturnType<M> {
    let (closure, seed) = match &mut iter.source {
        BaseSource::Unfold { closure, state: Some(s) } => {
            (closure.clone(), s.reference())
        }
        BaseSource::Unfold { state: None, .. } => {
            iter.source = BaseSource::Empty;
            if promote_chain_to_source(&mut iter) {
                return lazy_next_pull(iter, write_back);
            }
            write_back_iter(write_back, XIterator::empty())?;
            return Ok(SysCallResult::Return(Primitive::Null.into()));
        }
        _ => unreachable!("lazy_next_unfold called on non-Unfold source"),
    };

    struct LocalState {
        iter: XIterator,
        write_back: StackValue,
    }
    fn callback<M: 'static>(
        state: CallbackState,
        params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state
            .downcast::<LocalState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let result = params
            .into_iter()
            .next()
            .ok_or(EnvironmentError::InvalidCallbackParameters)?
            .into_owned();

        match result {
            ValueCell::Primitive(Primitive::Null) => {
                s.iter.source = BaseSource::Empty;
                if promote_chain_to_source(&mut s.iter) {
                    return lazy_next_pull(s.iter, s.write_back);
                }
                write_back_iter(s.write_back, XIterator::empty())?;
                Ok(SysCallResult::Return(Primitive::Null.into()))
            }
            ValueCell::Object(tuple) if tuple.len() == 2 => {
                let item_ptr = tuple[0].clone();
                let next_state: StackValue = tuple[1].to_owned().into();
                if let BaseSource::Unfold { state, .. } = &mut s.iter.source {
                    *state = Some(next_state);
                }
                lazy_next_pipe(s.iter, item_ptr, 0, s.write_back)
            }
            _ => Err(EnvironmentError::Static("unfold: expected optional (item, next_state)")),
        }
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure,
        params: vec![seed].into(),
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
) -> FnReturnType<M> {
    lazy_next_pull(iter, write_back)
}
