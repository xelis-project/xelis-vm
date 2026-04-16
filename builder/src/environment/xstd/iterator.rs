use std::{
    any::TypeId,
    hash::{Hash, Hasher},
};

use xelis_environment::{
    CallbackState,
    CallbackType,
    EnvironmentError,
    FnInstance,
    FnParams,
    FnReturnType,
    FunctionHandler,
    ModuleMetadata,
    SysCallResult,
    VMContext,
};
use xelis_types::{
    ClosureType,
    OpaqueWrapper,
    Primitive,
    StackValue,
    Type,
    ValueCell,
    ValuePointer,
    opaque::{
        Opaque,
        OpaqueType,
        traits::{DynType, JSONHelper, Serializable},
    },
};

use crate::EnvironmentBuilder;

/// What a terminal operation should do once all items are collected.
#[derive(Debug, Clone)]
enum TerminalOp {
    Collect,
    Count,
    Sum,
    ForEach(StackValue),
    Find(StackValue),
    Any(StackValue),
    All(StackValue),
    Position(StackValue),
    Fold { init: StackValue, closure: StackValue },
    /// Apply a map closure to the collected items, then run the next op.
    ThenMap { closure: StackValue, then: Box<TerminalOp> },
    /// Apply a filter closure to the collected items, then run the next op.
    ThenFilter { closure: StackValue, then: Box<TerminalOp> },
    /// Return a lazy Map iterator from the collected (inner) items.
    /// Used when iter_map is called on a closure-based inner iterator so that
    /// the map closure runs in a fresh syscall when the next terminal op fires.
    BuildMap(StackValue),
    /// Return a lazy Filter iterator from the collected (inner) items.
    BuildFilter(StackValue),
}

impl TerminalOp {
    fn finish<M: 'static>(self, items: Vec<ValuePointer>) -> FnReturnType<M> {
        match self {
            TerminalOp::Collect => Ok(SysCallResult::Return(ValueCell::Object(items).into())),
            TerminalOp::Count => Ok(SysCallResult::Return(Primitive::U32(items.len() as u32).into())),
            TerminalOp::Sum => Ok(SysCallResult::Return(sum_items(items)?)),
            TerminalOp::ForEach(f) => apply_items_none(items, f, 0),
            TerminalOp::Find(pred) => apply_items_find(items, pred, 0),
            TerminalOp::Any(pred) => apply_items_any(items, pred, 0),
            TerminalOp::All(pred) => apply_items_all(items, pred, 0),
            TerminalOp::Position(pred) => apply_items_position(items, pred, 0),
            TerminalOp::Fold { init, closure } => apply_items_fold(items, closure, init, 0),
            TerminalOp::ThenMap { closure, then } => apply_items_map(items, closure, 0, Vec::new(), *then),
            TerminalOp::ThenFilter { closure, then } => apply_items_filter(items, closure, 0, Vec::new(), *then),
            TerminalOp::BuildMap(closure) => Ok(SysCallResult::Return(wrap_iter(XIterator {
                source: IterSource::Map { inner: Box::new(XIterator::from_array(items)), closure },
            }))),
            TerminalOp::BuildFilter(closure) => Ok(SysCallResult::Return(wrap_iter(XIterator {
                source: IterSource::Filter { inner: Box::new(XIterator::from_array(items)), closure },
            }))),
        }
    }
}

/// Lazy iterator source/adapter tree.
///
/// Variants without closures (Array, Once, Empty, Skip, Take, Chain, Rev,
/// Enumerate, Zip, Flatten) implement `next_sync` and advance without the VM.
///
/// Closure-based variants (Map, Filter, Unfold) are driven by terminal
/// operations through `SysCallResult::ExecuteAndCallback` one item at a time,
/// so no upfront collection or copying occurs when building adapter chains.
#[derive(Debug, Clone)]
pub enum IterSource {
    Array { items: Vec<ValuePointer>, index: usize },
    Once(Option<ValuePointer>),
    Empty,
    Skip    { inner: Box<XIterator>, n: usize },
    Take    { inner: Box<XIterator>, remaining: usize },
    Chain   { first: Box<XIterator>, second: Box<XIterator> },
    Rev     { items: Vec<ValuePointer>, index: usize },
    Enumerate { inner: Box<XIterator>, counter: u32 },
    Zip     { left: Box<XIterator>, right: Box<XIterator> },
    Flatten { outer: Box<XIterator>, current: Option<Box<XIterator>> },
    Map     { inner: Box<XIterator>, closure: StackValue },
    Filter  { inner: Box<XIterator>, closure: StackValue },
    Unfold  { closure: StackValue, state: Option<StackValue> },
}

#[derive(Debug, Clone)]
pub struct XIterator {
    source: IterSource,
}

impl PartialEq for XIterator { fn eq(&self, _: &Self) -> bool { false } }
impl Eq for XIterator {}
impl Hash for XIterator { fn hash<H: Hasher>(&self, _: &mut H) {} }

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
    pub fn from_array(items: Vec<ValuePointer>) -> Self {
        Self { source: IterSource::Array { items, index: 0 } }
    }
    pub fn once(value: ValueCell) -> Self {
        Self { source: IterSource::Once(Some(ValuePointer::new(value))) }
    }
    pub fn empty() -> Self { Self { source: IterSource::Empty } }

    /// Whether this iterator requires VM callbacks to produce the next item.
    pub fn needs_callback(&self) -> bool {
        matches!(self.source,
            IterSource::Map { .. } | IterSource::Filter { .. } | IterSource::Unfold { .. })
    }

    /// Advance without invoking any VM closure.  Returns `Err` for Map/Filter/Unfold.
    pub fn next_sync(&mut self) -> Result<Option<ValuePointer>, EnvironmentError> {
        match &mut self.source {
            IterSource::Array { items, index } => {
                if *index < items.len() {
                    let item = items[*index].clone();
                    *index += 1;
                    Ok(Some(item))
                } else { Ok(None) }
            }
            IterSource::Once(slot) => Ok(slot.take()),
            IterSource::Empty => Ok(None),
            IterSource::Skip { inner, n } => {
                while *n > 0 {
                    match inner.next_sync()? {
                        Some(_) => *n -= 1,
                        None => { *n = 0; break; }
                    }
                }
                inner.next_sync()
            }
            IterSource::Take { inner, remaining } => {
                if *remaining == 0 { return Ok(None); }
                let item = inner.next_sync()?;
                if item.is_some() { *remaining -= 1; }
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
                } else { Ok(None) }
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
                            None => { *current = None; }
                        }
                    }
                    match outer.next_sync()? {
                        None => return Ok(None),
                        Some(ptr) => {
                            let cell = ptr.to_owned();
                            let new_inner = match cell {
                                ValueCell::Object(v) => XIterator::from_array(v),
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
                Err(EnvironmentError::Static(
                    "next_sync called on closure-based iterator; use a terminal operation",
                ))
            }
        }
    }
}

fn wrap_iter(iter: XIterator) -> StackValue {
    ValueCell::Primitive(Primitive::Opaque(OpaqueWrapper::new(iter))).into()
}

fn take_iter(sv: StackValue) -> Result<XIterator, EnvironmentError> {
    sv.into_owned()
        .into_opaque_type::<XIterator>()
        .map_err(|_| EnvironmentError::InvalidType)
}

// ─── registration ─────────────────────────────────────────────────────────────

pub fn register<M: 'static>(env: &mut EnvironmentBuilder<M>) -> OpaqueType {
    let iter_template = env.register_generic_opaque::<XIterator>("Iterator", false, 1);
    let iter_t0 = || Type::Opaque(iter_template.with_generics(vec![Type::T(Some(0))]));

    env.register_static_function("once", Type::Opaque(iter_template.clone()),
        vec![("value", Type::T(Some(0)))], FunctionHandler::Sync(iter_once), 2, Some(iter_t0()));

    env.register_static_function("empty", Type::Opaque(iter_template.clone()),
        vec![], FunctionHandler::Sync(iter_empty), 1, Some(iter_t0()));

    let unfold_closure = Type::Closure(ClosureType::new(
        vec![Type::T(Some(0))],
        Some(Type::Optional(Box::new(Type::Tuples(vec![Type::T(Some(1)), Type::T(Some(0))])))),
    ));
    env.register_static_function("unfold", Type::Opaque(iter_template.clone()),
        vec![("seed", Type::T(Some(0))), ("f", unfold_closure)],
        FunctionHandler::Sync(iter_unfold), 10,
        Some(Type::Opaque(iter_template.with_generics(vec![Type::T(Some(1))]))));

    env.register_native_function("iter", Some(Type::Array(Box::new(Type::T(Some(0))))),
        vec![], FunctionHandler::Sync(array_iter), 5, Some(iter_t0()));

    env.register_native_function("next", Some(iter_t0()), vec![],
        FunctionHandler::Sync(iter_next), 2,
        Some(Type::Optional(Box::new(Type::T(Some(0))))));

    env.register_native_function("count", Some(iter_t0()), vec![],
        FunctionHandler::Sync(iter_count), 1, Some(Type::U32));

    env.register_native_function("skip", Some(iter_t0()), vec![("n", Type::U32)],
        FunctionHandler::Sync(iter_skip), 2, Some(iter_t0()));

    env.register_native_function("take", Some(iter_t0()), vec![("n", Type::U32)],
        FunctionHandler::Sync(iter_take), 2, Some(iter_t0()));

    env.register_native_function("chain", Some(iter_t0()), vec![("other", iter_t0())],
        FunctionHandler::Sync(iter_chain), 5, Some(iter_t0()));

    env.register_native_function("enumerate", Some(iter_t0()), vec![],
        FunctionHandler::Sync(iter_enumerate), 5,
        Some(Type::Opaque(
            iter_template.with_generics(vec![Type::Tuples(vec![Type::U32, Type::T(Some(0))])])
        )));

    env.register_native_function("rev", Some(iter_t0()), vec![],
        FunctionHandler::Sync(iter_rev), 5, Some(iter_t0()));

    env.register_native_function("collect", Some(iter_t0()), vec![],
        FunctionHandler::Sync(iter_collect), 5,
        Some(Type::Array(Box::new(Type::T(Some(0))))));

    env.register_native_function("zip", Some(iter_t0()),
        vec![("other", Type::Opaque(iter_template.with_generics(vec![Type::T(Some(1))])))],
        FunctionHandler::Sync(iter_zip), 5,
        Some(Type::Opaque(
            iter_template.with_generics(vec![Type::Tuples(vec![Type::T(Some(0)), Type::T(Some(1))])])
        )));

    env.register_native_function("flatten",
        Some(Type::Opaque(
            iter_template.with_generics(vec![Type::Array(Box::new(Type::T(Some(0))))])
        )),
        vec![], FunctionHandler::Sync(iter_flatten), 10, Some(iter_t0()));

    env.register_native_function("flatten",
        Some(Type::Opaque(iter_template.with_generics(vec![iter_t0()]))),
        vec![], FunctionHandler::Sync(iter_flatten), 10, Some(iter_t0()));

    env.register_native_function("sum", Some(iter_t0()), vec![],
        FunctionHandler::Sync(iter_sum), 10, Some(Type::T(Some(0))));

    let mapper_closure = Type::Closure(ClosureType::new(vec![Type::T(Some(0))], Some(Type::T(Some(1)))));
    env.register_native_function("map", Some(iter_t0()), vec![("mapper", mapper_closure)],
        FunctionHandler::Sync(iter_map), 10,
        Some(Type::Opaque(iter_template.with_generics(vec![Type::T(Some(1))]))));

    let predicate_closure = Type::Closure(ClosureType::new(vec![Type::T(Some(0))], Some(Type::Bool)));
    env.register_native_function("filter", Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_filter), 10, Some(iter_t0()));

    let consumer_closure = Type::Closure(ClosureType::new(vec![Type::T(Some(0))], None));
    env.register_native_function("for_each", Some(iter_t0()), vec![("f", consumer_closure)],
        FunctionHandler::Sync(iter_for_each), 10, None);

    env.register_native_function("find", Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_find), 10,
        Some(Type::Optional(Box::new(Type::T(Some(0))))));

    env.register_native_function("any", Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_any), 10, Some(Type::Bool));

    env.register_native_function("all", Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_all), 10, Some(Type::Bool));

    let fold_closure = Type::Closure(ClosureType::new(
        vec![Type::T(Some(0)), Type::T(Some(0))], Some(Type::T(Some(0)))));
    env.register_native_function("fold", Some(iter_t0()),
        vec![("init", Type::T(Some(0))), ("f", fold_closure)],
        FunctionHandler::Sync(iter_fold), 10, Some(Type::T(Some(0))));

    env.register_native_function("position", Some(iter_t0()),
        vec![("predicate", predicate_closure)],
        FunctionHandler::Sync(iter_position), 10,
        Some(Type::Optional(Box::new(Type::U32))));

    iter_template
}

// ─── constructors ─────────────────────────────────────────────────────────────

fn iter_once<M>(_: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let value = p.remove(0).into_owned();
    Ok(SysCallResult::Return(wrap_iter(XIterator::once(value))))
}

fn iter_empty<M>(_: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    Ok(SysCallResult::Return(wrap_iter(XIterator::empty())))
}

/// `unfold` drives the closure eagerly to collect all items (seed -> next_seed chain).
fn iter_unfold<M: 'static>(_: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let seed = p.remove(0);
    let closure = p.remove(0);
    struct S { closure: StackValue, acc: Vec<ValuePointer> }
    fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let value = params.into_iter().next()
            .ok_or(EnvironmentError::InvalidCallbackParameters)?.into_owned();
        match value {
            ValueCell::Primitive(Primitive::Null) => {
                Ok(SysCallResult::Return(wrap_iter(XIterator::from_array(s.acc))))
            }
            ValueCell::Object(tuple) => {
                if tuple.len() != 2 {
                    return Err(EnvironmentError::Static("unfold: expected (item, next_state)"));
                }
                s.acc.push(tuple[0].clone());
                let next_seed: StackValue = tuple[1].to_owned().into();
                Ok(SysCallResult::ExecuteAndCallback {
                    ptr: s.closure.clone(), params: vec![next_seed].into(),
                    state: s, callback_params_len: 1, callback: CallbackType::Sync(cb::<M>),
                })
            }
            _ => Err(EnvironmentError::Static("unfold: expected optional tuple")),
        }
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure.clone(), params: vec![seed].into(),
        state: Box::new(S { closure, acc: Vec::new() }),
        callback_params_len: 1, callback: CallbackType::Sync(cb::<M>),
    })
}

fn array_iter<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let sv = zelf?;
    let items = sv.as_ref().as_vec()?.clone();
    context.increase_gas_usage(items.len() as u64)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator::from_array(items))))
}

// --- O(1) lazy adapter constructors ------------------------------------------

fn iter_skip<M>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let n = p.remove(0).as_u32()? as usize;
    let inner = take_iter(zelf?)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator { source: IterSource::Skip { inner: Box::new(inner), n } })))
}

fn iter_take<M>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let n = p.remove(0).as_u32()? as usize;
    let inner = take_iter(zelf?)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator { source: IterSource::Take { inner: Box::new(inner), remaining: n } })))
}

fn iter_chain<M>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let second = take_iter(p.remove(0))?;
    let first = take_iter(zelf?)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator {
        source: IterSource::Chain { first: Box::new(first), second: Box::new(second) },
    })))
}

fn iter_enumerate<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let inner = take_iter(zelf?)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator { source: IterSource::Enumerate { inner: Box::new(inner), counter: 0 } })))
}

fn iter_rev<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
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

fn iter_zip<M>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let right = take_iter(p.remove(0))?;
    let left = take_iter(zelf?)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator {
        source: IterSource::Zip { left: Box::new(left), right: Box::new(right) },
    })))
}

fn iter_flatten<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let outer = take_iter(zelf?)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator {
        source: IterSource::Flatten { outer: Box::new(outer), current: None },
    })))
}

fn iter_map<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let closure = p.remove(0);
    let inner = take_iter(zelf?)?;
    if inner.needs_callback() {
        // Inner has closure-based adapters; collect it now so the map closure
        // runs in a fresh (separate) syscall callback chain with clean VM
        // register state.  This avoids the RegisterOverflow that occurs when
        // two different closures from the same parent chunk are chained inside
        // one continuous callback sequence.
        lazy_collect::<M>(inner, Vec::new(), TerminalOp::BuildMap(closure))
    } else {
        Ok(SysCallResult::Return(wrap_iter(XIterator { source: IterSource::Map { inner: Box::new(inner), closure } })))
    }
}

fn iter_filter<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let closure = p.remove(0);
    let inner = take_iter(zelf?)?;
    if inner.needs_callback() {
        // Same reasoning as iter_map: collect the closure-based inner eagerly
        // so the filter closure runs in a separate callback chain with clean
        // VM register state.
        lazy_collect::<M>(inner, Vec::new(), TerminalOp::BuildFilter(closure))
    } else {
        Ok(SysCallResult::Return(wrap_iter(XIterator { source: IterSource::Filter { inner: Box::new(inner), closure } })))
    }
}

fn iter_next<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let mut sv = zelf?;
    let iter = sv.as_mut().as_opaque_mut()?.as_mut::<XIterator>()?;
    if iter.needs_callback() {
        return Err(EnvironmentError::Static(
            "next() is not supported on map/filter/unfold iterators; use a terminal (collect, for_each, ...)",
        ));
    }
    let result = iter.next_sync()?;
    Ok(SysCallResult::Return(match result {
        Some(ptr) => ptr.to_owned().into(),
        None => Primitive::Null.into(),
    }))
}

// --- Lazy collection engine --------------------------------------------------
//
// `lazy_collect` drains the entire iterator lazily into a Vec<ValuePointer>.
// Sync adapters (Array, Skip, Take, Chain, Rev, Enumerate, Zip, Flatten) are
// drained in a tight loop with zero allocations per item beyond the Vec push.
// Map/Filter each trigger one ExecuteAndCallback per item.
// Unfold triggers one ExecuteAndCallback per generated item.
// TerminalOp is an enum so state can be Send + Sync.

fn lazy_collect<M: 'static>(mut iter: XIterator, mut acc: Vec<ValuePointer>, op: TerminalOp) -> FnReturnType<M> {
    // Drain all sync items first.
    loop {
        if iter.needs_callback() { break; }
        match iter.next_sync()? {
            Some(item) => acc.push(item),
            None => return op.finish(acc),
        }
    }
    // Closure-based source: handle one item then recurse.
    match iter.source {
        IterSource::Map { inner, closure } => {
            if inner.needs_callback() {
                // Inner is also closure-based: collect inner first, then map over collected items.
                lazy_collect(*inner, acc, TerminalOp::ThenMap { closure, then: Box::new(op) })
            } else {
                map_next_sync(*inner, closure, acc, op)
            }
        }
        IterSource::Filter { inner, closure } => {
            if inner.needs_callback() {
                // Inner is also closure-based: collect inner first, then filter over collected items.
                lazy_collect(*inner, acc, TerminalOp::ThenFilter { closure, then: Box::new(op) })
            } else {
                filter_next_sync(*inner, closure, acc, op)
            }
        }
        IterSource::Unfold { closure, state } => {
            let seed = match state { Some(s) => s, None => return op.finish(acc) };
            struct S { closure: StackValue, acc: Vec<ValuePointer>, op: TerminalOp }
            fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
                let mut s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
                let value = params.into_iter().next()
                    .ok_or(EnvironmentError::InvalidCallbackParameters)?.into_owned();
                match value {
                    ValueCell::Primitive(Primitive::Null) => s.op.finish(s.acc),
                    ValueCell::Object(tuple) => {
                        if tuple.len() != 2 {
                            return Err(EnvironmentError::Static("unfold: expected (item, next_state)"));
                        }
                        s.acc.push(tuple[0].clone());
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
            Ok(SysCallResult::ExecuteAndCallback {
                ptr: closure.clone(), params: vec![seed].into(),
                state: Box::new(S { closure, acc, op }),
                callback_params_len: 1, callback: CallbackType::Sync(cb::<M>),
            })
        }
        _ => unreachable!("all non-closure sources handled in the drain loop above"),
    }
}

/// Drive `Map { inner (sync), closure }` one item at a time.
fn map_next_sync<M: 'static>(mut inner: XIterator, closure: StackValue, acc: Vec<ValuePointer>, op: TerminalOp) -> FnReturnType<M> {
    match inner.next_sync()? {
        None => op.finish(acc),
        Some(raw) => {
            struct S { remaining: XIterator, acc: Vec<ValuePointer>, closure: StackValue, op: TerminalOp }
            fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
                let mut s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
                let mapped = params.into_iter().next()
                    .ok_or(EnvironmentError::InvalidCallbackParameters)?.into_owned();
                s.acc.push(ValuePointer::new(mapped));
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

/// Drive `Filter { inner (sync), closure }` one item at a time.
fn filter_next_sync<M: 'static>(mut inner: XIterator, closure: StackValue, acc: Vec<ValuePointer>, op: TerminalOp) -> FnReturnType<M> {
    match inner.next_sync()? {
        None => op.finish(acc),
        Some(raw) => {
            struct S { raw: ValuePointer, remaining: XIterator, acc: Vec<ValuePointer>, closure: StackValue, op: TerminalOp }
            fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
                let mut s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
                let keep = params.first().ok_or(EnvironmentError::InvalidCallbackParameters)?.as_bool()?;
                if keep { s.acc.push(s.raw); }
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

/// Apply a map closure over a collected Vec of items, then run `then` op.
fn apply_items_map<M: 'static>(items: Vec<ValuePointer>, closure: StackValue, index: usize, mapped: Vec<ValuePointer>, then: TerminalOp) -> FnReturnType<M> {
    let Some(item) = items.get(index) else { return then.finish(mapped); };
    struct S { items: Vec<ValuePointer>, index: usize, closure: StackValue, mapped: Vec<ValuePointer>, then: TerminalOp }
    fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let result = params.into_iter().next().ok_or(EnvironmentError::InvalidCallbackParameters)?.into_owned();
        s.mapped.push(ValuePointer::new(result));
        apply_items_map(s.items, s.closure, s.index + 1, s.mapped, s.then)
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure.clone(), params: vec![item.to_owned().into()].into(),
        state: Box::new(S { items, index, closure, mapped, then }),
        callback_params_len: 1, callback: CallbackType::Sync(cb::<M>),
    })
}

/// Apply a filter closure over a collected Vec of items, then run `then` op.
fn apply_items_filter<M: 'static>(items: Vec<ValuePointer>, closure: StackValue, index: usize, kept: Vec<ValuePointer>, then: TerminalOp) -> FnReturnType<M> {
    let Some(item) = items.get(index) else { return then.finish(kept); };
    struct S { items: Vec<ValuePointer>, index: usize, closure: StackValue, kept: Vec<ValuePointer>, then: TerminalOp }
    fn cb<M: 'static>(state: CallbackState, params: FnParams) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut s = state.downcast::<S>().map_err(|_| EnvironmentError::InvalidCallbackState)?;
        let keep = params.first().ok_or(EnvironmentError::InvalidCallbackParameters)?.as_bool()?;
        if keep { s.kept.push(s.items[s.index].clone()); }
        apply_items_filter(s.items, s.closure, s.index + 1, s.kept, s.then)
    }
    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure.clone(), params: vec![item.to_owned().into()].into(),
        state: Box::new(S { items, index, closure, kept, then }),
        callback_params_len: 1, callback: CallbackType::Sync(cb::<M>),
    })
}



fn iter_collect<M: 'static>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    lazy_collect(take_iter(zelf?)?, Vec::new(), TerminalOp::Collect)
}

fn iter_count<M: 'static>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    lazy_collect(take_iter(zelf?)?, Vec::new(), TerminalOp::Count)
}

fn iter_sum<M: 'static>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    lazy_collect(take_iter(zelf?)?, Vec::new(), TerminalOp::Sum)
}

fn iter_for_each<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let f = p.remove(0);
    lazy_collect(take_iter(zelf?)?, Vec::new(), TerminalOp::ForEach(f))
}

fn iter_find<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let pred = p.remove(0);
    lazy_collect(take_iter(zelf?)?, Vec::new(), TerminalOp::Find(pred))
}

fn iter_any<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let pred = p.remove(0);
    lazy_collect(take_iter(zelf?)?, Vec::new(), TerminalOp::Any(pred))
}

fn iter_all<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let pred = p.remove(0);
    lazy_collect(take_iter(zelf?)?, Vec::new(), TerminalOp::All(pred))
}

fn iter_position<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let pred = p.remove(0);
    lazy_collect(take_iter(zelf?)?, Vec::new(), TerminalOp::Position(pred))
}

fn iter_fold<M: 'static>(zelf: FnInstance, mut p: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let init = p.remove(0);
    let closure = p.remove(0);
    lazy_collect(take_iter(zelf?)?, Vec::new(), TerminalOp::Fold { init, closure })
}

// --- Per-item closure application --------------------------------------------

fn apply_items_none<M: 'static>(items: Vec<ValuePointer>, f: StackValue, index: usize) -> FnReturnType<M> {
    let Some(item) = items.get(index) else { return Ok(SysCallResult::None); };
    struct S { items: Vec<ValuePointer>, index: usize, f: StackValue }
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

fn apply_items_find<M: 'static>(items: Vec<ValuePointer>, pred: StackValue, index: usize) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(Primitive::Null.into()));
    };
    struct S { items: Vec<ValuePointer>, index: usize, pred: StackValue }
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

fn apply_items_any<M: 'static>(items: Vec<ValuePointer>, pred: StackValue, index: usize) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(Primitive::Boolean(false).into()));
    };
    struct S { items: Vec<ValuePointer>, index: usize, pred: StackValue }
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

fn apply_items_all<M: 'static>(items: Vec<ValuePointer>, pred: StackValue, index: usize) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(Primitive::Boolean(true).into()));
    };
    struct S { items: Vec<ValuePointer>, index: usize, pred: StackValue }
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

fn apply_items_position<M: 'static>(items: Vec<ValuePointer>, pred: StackValue, index: usize) -> FnReturnType<M> {
    let Some(item) = items.get(index) else {
        return Ok(SysCallResult::Return(Primitive::Null.into()));
    };
    struct S { items: Vec<ValuePointer>, index: usize, pred: StackValue }
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

fn apply_items_fold<M: 'static>(items: Vec<ValuePointer>, closure: StackValue, acc: StackValue, index: usize) -> FnReturnType<M> {
    let Some(item) = items.get(index) else { return Ok(SysCallResult::Return(acc)); };
    struct S { items: Vec<ValuePointer>, index: usize, closure: StackValue }
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

// --- sum helper --------------------------------------------------------------

fn sum_items(items: Vec<ValuePointer>) -> Result<StackValue, EnvironmentError> {
    if items.is_empty() { return Ok(Primitive::U64(0).into()); }
    let mut acc: u128 = 0;
    let mut kind: Option<u8> = None;
    for ptr in &items {
        match ptr.as_ref() {
            ValueCell::Primitive(p) => match p {
                Primitive::U8(v)   => { let k = 0u8; if *kind.get_or_insert(k) != k { return Err(EnvironmentError::InvalidType); } acc += *v as u128; }
                Primitive::U16(v)  => { let k = 1u8; if *kind.get_or_insert(k) != k { return Err(EnvironmentError::InvalidType); } acc += *v as u128; }
                Primitive::U32(v)  => { let k = 2u8; if *kind.get_or_insert(k) != k { return Err(EnvironmentError::InvalidType); } acc += *v as u128; }
                Primitive::U64(v)  => { let k = 3u8; if *kind.get_or_insert(k) != k { return Err(EnvironmentError::InvalidType); } acc += *v as u128; }
                Primitive::U128(v) => { let k = 4u8; if *kind.get_or_insert(k) != k { return Err(EnvironmentError::InvalidType); } acc = acc.wrapping_add(*v); }
                _ => return Err(EnvironmentError::Static("sum: only numeric types are supported")),
            },
            _ => return Err(EnvironmentError::Static("sum: only numeric types are supported")),
        }
    }
    Ok(match kind {
        Some(0) => Primitive::U8(acc as u8),
        Some(1) => Primitive::U16(acc as u16),
        Some(2) => Primitive::U32(acc as u32),
        Some(3) => Primitive::U64(acc as u64),
        Some(4) => Primitive::U128(acc),
        _ => Primitive::U64(0),
    }.into())
}
