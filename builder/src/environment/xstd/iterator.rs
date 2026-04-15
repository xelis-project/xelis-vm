use std::{
    any::TypeId,
    collections::VecDeque,
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

/// Iterator over arbitrary ValueCell items.
/// Items are stored as `ValuePointer` to match how arrays store elements.
#[derive(Debug, Clone)]
pub struct XIterator {
    items: Vec<ValuePointer>,
    index: usize,
}

impl PartialEq for XIterator {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index && self.items.len() == other.items.len()
    }
}

impl Eq for XIterator {}

impl Hash for XIterator {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.index.hash(state);
        self.items.len().hash(state);
    }
}

impl DynType for XIterator {
    fn get_type_name(&self) -> &'static str {
        "Iterator"
    }

    fn get_type(&self) -> TypeId {
        TypeId::of::<XIterator>()
    }
}

impl JSONHelper for XIterator {}
impl Serializable for XIterator {}

impl Opaque for XIterator {
    fn clone_box(&self) -> Box<dyn Opaque> {
        Box::new(self.clone())
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Iterator(len={}, index={})", self.items.len(), self.index)
    }
}

impl XIterator {
    pub fn new(items: Vec<ValuePointer>) -> Self {
        Self { items, index: 0 }
    }

    pub fn from_cell_array(items: Vec<ValuePointer>) -> Self {
        Self::new(items)
    }

    pub fn once(value: ValueCell) -> Self {
        Self::new(vec![ValuePointer::new(value)])
    }

    pub fn empty() -> Self {
        Self::new(vec![])
    }

    /// Remaining number of elements.
    pub fn remaining(&self) -> usize {
        self.items.len().saturating_sub(self.index)
    }

    /// Advance and return the next item (as a cloned ValueCell).
    pub fn next_item(&mut self) -> Option<ValuePointer> {
        if self.index < self.items.len() {
            let item = self.items[self.index].clone();
            self.index += 1;
            Some(item)
        } else {
            None
        }
    }
}

/// Helper: wrap an `XIterator` in an `OpaqueWrapper` and return it as a `StackValue`.
fn wrap_iter(iter: XIterator) -> StackValue {
    ValueCell::Primitive(Primitive::Opaque(OpaqueWrapper::new(iter))).into()
}

pub fn register<M>(env: &mut EnvironmentBuilder<M>) -> OpaqueType {
    // Iterator is a generic opaque type with one type parameter: the element type.
    // e.g.  u64[].iter() : Iterator<u64>
    let iter_template = env.register_generic_opaque::<XIterator>("Iterator", false, 1);

    // Shorthand: Iterator<T(0)> (used as on_type for all instance methods)
    let iter_t0 = || Type::Opaque(iter_template.with_generics(vec![Type::T(Some(0))]));

    // Iterator::once(value: T(0)) -> Iterator<T(0)>
    // (When called as a static on the bare template, T(0) resolves to Any)
    env.register_static_function(
        "once",
        Type::Opaque(iter_template.clone()),
        vec![("value", Type::T(Some(0)))],
        FunctionHandler::Sync(iter_once),
        2,
        Some(iter_t0()),
    );

    // Iterator::empty() -> Iterator<T(0)>
    env.register_static_function(
        "empty",
        Type::Opaque(iter_template.clone()),
        vec![],
        FunctionHandler::Sync(iter_empty),
        1,
        Some(iter_t0()),
    );

    // Iterator::unfold(seed: T(0), f: fn(T(0)) -> optional<(T(1), T(0))>) -> Iterator<T(1)>
    // The closure returns (item, next_state) to continue, or null to stop.
    let unfold_closure = Type::Closure(ClosureType::new(
        vec![Type::T(Some(0))],
        Some(Type::Optional(Box::new(Type::Tuples(vec![Type::T(Some(1)), Type::T(Some(0))])))),
    ));
    env.register_static_function(
        "unfold",
        Type::Opaque(iter_template.clone()),
        vec![("seed", Type::T(Some(0))), ("f", unfold_closure)],
        FunctionHandler::Sync(iter_unfold),
        10,
        Some(Type::Opaque(iter_template.with_generics(vec![Type::T(Some(1))]))),
    );

    // array.iter() -> Iterator<T(0)>   (T(0) = the array's element type)
    env.register_native_function(
        "iter",
        Some(Type::Array(Box::new(Type::T(Some(0))))),
        vec![],
        FunctionHandler::Sync(array_iter),
        5,
        Some(iter_t0()),
    );

    // next() -> optional<T(0)>
    env.register_native_function(
        "next",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_next),
        2,
        Some(Type::Optional(Box::new(Type::T(Some(0))))),
    );

    // count() -> u32
    env.register_native_function(
        "count",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_count),
        1,
        Some(Type::U32),
    );

    // skip(n: u32) -> Iterator<T(0)>
    env.register_native_function(
        "skip",
        Some(iter_t0()),
        vec![("n", Type::U32)],
        FunctionHandler::Sync(iter_skip),
        2,
        Some(iter_t0()),
    );

    // take(n: u32) -> Iterator<T(0)>
    env.register_native_function(
        "take",
        Some(iter_t0()),
        vec![("n", Type::U32)],
        FunctionHandler::Sync(iter_take),
        2,
        Some(iter_t0()),
    );

    // chain(other: Iterator<T(0)>) -> Iterator<T(0)>
    env.register_native_function(
        "chain",
        Some(iter_t0()),
        vec![("other", iter_t0())],
        FunctionHandler::Sync(iter_chain),
        5,
        Some(iter_t0()),
    );

    // enumerate() -> Iterator<(u32, T(0))>
    env.register_native_function(
        "enumerate",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_enumerate),
        5,
        Some(Type::Opaque(
            iter_template.with_generics(vec![Type::Tuples(vec![Type::U32, Type::T(Some(0))])])
        )),
    );

    // rev() -> Iterator<T(0)>
    env.register_native_function(
        "rev",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_rev),
        5,
        Some(iter_t0()),
    );

    // collect() -> T(0)[]
    env.register_native_function(
        "collect",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_collect),
        5,
        Some(Type::Array(Box::new(Type::T(Some(0))))),
    );

    // zip(other: Iterator<T(1)>) -> Iterator<(T(0), T(1))>
    // T(1) is inferred from the element type of `other` at the call site.
    env.register_native_function(
        "zip",
        Some(iter_t0()),
        vec![("other", Type::Opaque(iter_template.with_generics(vec![Type::T(Some(1))])))],
        FunctionHandler::Sync(iter_zip),
        5,
        Some(Type::Opaque(iter_template.with_generics(vec![Type::Tuples(vec![Type::T(Some(0)), Type::T(Some(1))])]))),
    );

    // flatten() on Iterator<T(0)[]> -> Iterator<T(0)>
    // Only valid when each element is an array.
    env.register_native_function(
        "flatten",
        Some(Type::Opaque(
            iter_template.with_generics(vec![Type::Array(Box::new(Type::T(Some(0))))])
        )),
        vec![],
        FunctionHandler::Sync(iter_flatten),
        10,
        Some(iter_t0()),
    );

    // flatten() on Iterator<Iterator<T(0)>> -> Iterator<T(0)>
    // Only valid when each element is itself an Iterator.
    env.register_native_function(
        "flatten",
        Some(Type::Opaque(iter_template.with_generics(vec![iter_t0()]))),
        vec![],
        FunctionHandler::Sync(iter_flatten),
        10,
        Some(iter_t0()),
    );

    // sum() -> T(0)    (numeric iterators only; runtime checks the element type)
    env.register_native_function(
        "sum",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_sum),
        10,
        Some(Type::T(Some(0))),
    );

    // map(mapper: fn(T(0)) -> T(1)) -> Iterator<T(1)>
    // T(1) is inferred from the actual closure's return type at the call site.
    let mapper_closure = Type::Closure(ClosureType::new(vec![Type::T(Some(0))], Some(Type::T(Some(1)))));
    env.register_native_function(
        "map",
        Some(iter_t0()),
        vec![("mapper", mapper_closure)],
        FunctionHandler::Sync(iter_map),
        10,
        Some(Type::Opaque(iter_template.with_generics(vec![Type::T(Some(1))]))),
    );

    // filter(predicate: fn(T(0)) -> bool) -> Iterator<T(0)>
    let predicate_closure = Type::Closure(ClosureType::new(vec![Type::T(Some(0))], Some(Type::Bool)));
    env.register_native_function(
        "filter",
        Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_filter),
        10,
        Some(iter_t0()),
    );

    // for_each(f: fn(T(0)))
    let consumer_closure = Type::Closure(ClosureType::new(vec![Type::T(Some(0))], None));
    env.register_native_function(
        "for_each",
        Some(iter_t0()),
        vec![("f", consumer_closure)],
        FunctionHandler::Sync(iter_for_each),
        10,
        None,
    );

    // find(predicate: fn(T(0)) -> bool) -> optional<T(0)>
    env.register_native_function(
        "find",
        Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_find),
        10,
        Some(Type::Optional(Box::new(Type::T(Some(0))))),
    );

    // any(predicate: fn(T(0)) -> bool) -> bool
    env.register_native_function(
        "any",
        Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_any),
        10,
        Some(Type::Bool),
    );

    // all(predicate: fn(T(0)) -> bool) -> bool
    env.register_native_function(
        "all",
        Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_all),
        10,
        Some(Type::Bool),
    );

    // fold(init: T(0), f: fn(T(0), T(0)) -> T(0)) -> T(0)
    let fold_closure = Type::Closure(ClosureType::new(
        vec![Type::T(Some(0)), Type::T(Some(0))],
        Some(Type::T(Some(0))),
    ));
    env.register_native_function(
        "fold",
        Some(iter_t0()),
        vec![("init", Type::T(Some(0))), ("f", fold_closure)],
        FunctionHandler::Sync(iter_fold),
        10,
        Some(Type::T(Some(0))),
    );

    // position(predicate: fn(T(0)) -> bool) -> optional<u32>
    env.register_native_function(
        "position",
        Some(iter_t0()),
        vec![("predicate", predicate_closure)],
        FunctionHandler::Sync(iter_position),
        10,
        Some(Type::Optional(Box::new(Type::U32))),
    );

    iter_template
}

fn iter_once<M>(
    _: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    _: &mut VMContext,
) -> FnReturnType<M> {
    let value = parameters.remove(0).into_owned();
    Ok(SysCallResult::Return(wrap_iter(XIterator::once(value))))
}

fn iter_empty<M>(
    _: FnInstance,
    _: FnParams,
    _: &ModuleMetadata<'_, M>,
    _: &mut VMContext,
) -> FnReturnType<M> {
    Ok(SysCallResult::Return(wrap_iter(XIterator::empty())))
}

struct UnfoldState {
    closure_ptr: StackValue,
    result: Vec<ValuePointer>,
}

fn iter_unfold<M>(
    _: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    _: &mut VMContext,
) -> FnReturnType<M> {
    let seed = parameters.remove(0);
    let closure_ptr = parameters.remove(0);

    fn unfold_callback<M>(
        state: CallbackState,
        params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut state = state
            .downcast::<UnfoldState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;

        let value = params
            .into_iter()
            .next()
            .ok_or(EnvironmentError::InvalidCallbackParameters)?
            .into_owned();

        match value {
            ValueCell::Primitive(Primitive::Null) => {
                Ok(SysCallResult::Return(wrap_iter(XIterator::new(state.result))))
            }
            ValueCell::Object(tuple) => {
                if tuple.len() != 2 {
                    return Err(EnvironmentError::Static("unfold: expected tuple (item, next_state)"));
                }

                state.result.push(tuple[0].clone());
                let next_state: StackValue = tuple[1].to_owned().into();

                Ok(SysCallResult::ExecuteAndCallback {
                    ptr: state.closure_ptr.clone(),
                    params: vec![next_state].into(),
                    state,
                    callback_params_len: 1,
                    callback: CallbackType::Sync(unfold_callback),
                })
            }
            _ => Err(EnvironmentError::Static("unfold: expected optional tuple (item, next_state)")),
        }
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure_ptr.clone(),
        params: vec![seed].into(),
        state: Box::new(UnfoldState {
            closure_ptr,
            result: Vec::new(),
        }),
        callback_params_len: 1,
        callback: CallbackType::Sync(unfold_callback),
    })
}

fn array_iter<M>(
    zelf: FnInstance,
    _: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let sv = zelf?;
    let items = sv.as_ref().as_vec()?.clone();
    context.increase_gas_usage(items.len() as u64)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator::from_cell_array(items))))
}

fn iter_next<M>(
    zelf: FnInstance,
    _: FnParams,
    _: &ModuleMetadata<'_, M>,
    _: &mut VMContext,
) -> FnReturnType<M> {
    let mut sv = zelf?;
    let result = sv.as_mut().as_opaque_mut()?.as_mut::<XIterator>()?.next_item();
    Ok(SysCallResult::Return(match result {
        Some(ptr) => ptr.to_owned().into(),
        None => Primitive::Null.into(),
    }))
}

fn iter_count<M>(
    zelf: FnInstance,
    _: FnParams,
    _: &ModuleMetadata<'_, M>,
    _: &mut VMContext,
) -> FnReturnType<M> {
    let sv = zelf?;
    let remaining = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?.remaining();
    Ok(SysCallResult::Return(Primitive::U32(remaining as u32).into()))
}

fn iter_skip<M>(
    zelf: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let n = parameters.remove(0).as_u32()? as usize;
    let mut sv = zelf?;
    {
        let iter = sv.as_mut().as_opaque_mut()?.as_mut::<XIterator>()?;
        let skip = n.min(iter.remaining());
        context.increase_gas_usage(skip as u64)?;
        iter.index += skip;
    }
    Ok(SysCallResult::Return(sv))
}

fn iter_take<M>(
    zelf: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let n = parameters.remove(0).as_u32()? as usize;
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let end = (iter.index + n).min(iter.items.len());
    let items: Vec<ValuePointer> = iter.items[iter.index..end].to_vec();
    context.increase_gas_usage(items.len() as u64)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator::new(items))))
}

fn iter_chain<M>(
    zelf: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let other_sv = parameters.remove(0);
    let other_ref = other_sv.as_ref();
    let other = other_ref.as_opaque()?.as_ref::<XIterator>()?;
    let other_remaining: Vec<ValuePointer> = other.items[other.index..].to_vec();

    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let mut items: Vec<ValuePointer> = iter.items[iter.index..].to_vec();
    items.extend(other_remaining);

    context.increase_gas_usage(items.len() as u64)?;
    Ok(SysCallResult::Return(wrap_iter(XIterator::new(items))))
}

fn iter_enumerate<M>(
    zelf: FnInstance,
    _: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let remaining = &iter.items[iter.index..];
    context.increase_gas_usage(remaining.len() as u64)?;

    let mut enumerated = Vec::with_capacity(remaining.len());
    for (offset, item) in remaining.iter().enumerate() {
        let idx = (iter.index + offset) as u32;
        let tuple = ValueCell::Object(vec![
            ValuePointer::new(Primitive::U32(idx).into()),
            item.clone(),
        ]);
        enumerated.push(ValuePointer::new(tuple));
    }
    Ok(SysCallResult::Return(wrap_iter(XIterator::new(enumerated))))
}

fn iter_rev<M>(
    zelf: FnInstance,
    _: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let mut items: Vec<ValuePointer> = iter.items[iter.index..].to_vec();
    context.increase_gas_usage(items.len() as u64)?;
    items.reverse();
    Ok(SysCallResult::Return(wrap_iter(XIterator::new(items))))
}

fn iter_collect<M>(
    zelf: FnInstance,
    _: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let items: Vec<ValuePointer> = iter.items[iter.index..].to_vec();
    context.increase_gas_usage(items.len() as u64)?;
    Ok(SysCallResult::Return(ValueCell::Object(items).into()))
}

fn iter_zip<M>(
    zelf: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let other_sv = parameters.remove(0);
    let other_ref = other_sv.as_ref();
    let other = other_ref.as_opaque()?.as_ref::<XIterator>()?;
    let other_items = &other.items[other.index..];

    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let self_items = &iter.items[iter.index..];

    let len = self_items.len().min(other_items.len());
    context.increase_gas_usage(len as u64)?;

    let mut zipped = Vec::with_capacity(len);
    for (a, b) in self_items.iter().zip(other_items.iter()).take(len) {
        let pair = ValueCell::Object(vec![a.clone(), b.clone()]);
        zipped.push(ValuePointer::new(pair));
    }

    Ok(SysCallResult::Return(wrap_iter(XIterator::new(zipped))))
}

fn iter_flatten<M>(
    zelf: FnInstance,
    _: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let remaining = &iter.items[iter.index..];
    context.increase_gas_usage(remaining.len() as u64)?;

    let mut flat: Vec<ValuePointer> = Vec::new();
    for item in remaining {
        let cell = item.as_ref();
        match cell {
            ValueCell::Object(inner) => {
                context.increase_gas_usage(inner.len() as u64)?;
                flat.extend(inner.iter().cloned());
            }
            // Also support nested iterators stored as opaque
            ValueCell::Primitive(Primitive::Opaque(wrapper)) => {
                let inner_iter = wrapper.as_ref::<XIterator>()
                    .map_err(|_| EnvironmentError::Static("flatten: expected Iterator or array elements"))?;
                let inner_remaining = &inner_iter.items[inner_iter.index..];
                context.increase_gas_usage(inner_remaining.len() as u64)?;
                flat.extend(inner_remaining.iter().cloned());
            }
            _ => return Err(EnvironmentError::Static("flatten: expected Iterator of arrays or Iterators")),
        }
    }

    Ok(SysCallResult::Return(wrap_iter(XIterator::new(flat))))
}

fn iter_sum<M>(
    zelf: FnInstance,
    _: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let remaining = &iter.items[iter.index..];

    if remaining.is_empty() {
        return Ok(SysCallResult::Return(Primitive::U64(0).into()));
    }

    context.increase_gas_usage(remaining.len() as u64)?;

    // Determine type from first element and accumulate
    let mut acc: u128 = 0;
    let mut kind: Option<u8> = None; // 0=u8,1=u16,2=u32,3=u64,4=u128

    for ptr in remaining {
        let cell = ptr.as_ref();
        match cell {
            ValueCell::Primitive(p) => match p {
                Primitive::U8(v) => {
                    if kind.is_none() { kind = Some(0); }
                    if kind != Some(0) { return Err(EnvironmentError::InvalidType); }
                    acc += *v as u128;
                }
                Primitive::U16(v) => {
                    if kind.is_none() { kind = Some(1); }
                    if kind != Some(1) { return Err(EnvironmentError::InvalidType); }
                    acc += *v as u128;
                }
                Primitive::U32(v) => {
                    if kind.is_none() { kind = Some(2); }
                    if kind != Some(2) { return Err(EnvironmentError::InvalidType); }
                    acc += *v as u128;
                }
                Primitive::U64(v) => {
                    if kind.is_none() { kind = Some(3); }
                    if kind != Some(3) { return Err(EnvironmentError::InvalidType); }
                    acc += *v as u128;
                }
                Primitive::U128(v) => {
                    if kind.is_none() { kind = Some(4); }
                    if kind != Some(4) { return Err(EnvironmentError::InvalidType); }
                    acc = acc.wrapping_add(*v);
                }
                _ => return Err(EnvironmentError::Static("sum: only numeric types are supported")),
            },
            _ => return Err(EnvironmentError::Static("sum: only numeric types are supported")),
        }
    }

    let result = match kind {
        Some(0) => Primitive::U8(acc as u8),
        Some(1) => Primitive::U16(acc as u16),
        Some(2) => Primitive::U32(acc as u32),
        Some(3) => Primitive::U64(acc as u64),
        Some(4) => Primitive::U128(acc),
        _ => Primitive::U64(0),
    };

    Ok(SysCallResult::Return(result.into()))
}

struct MapState {
    closure_ptr: StackValue,
    // remaining items yet to process
    remaining: VecDeque<ValuePointer>,
    // accumulated results
    result: Vec<ValuePointer>,
}

fn iter_map<M>(
    zelf: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let closure_ptr = parameters.remove(0);
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let remaining: VecDeque<ValuePointer> = iter.items[iter.index..].iter().cloned().collect();

    context.increase_gas_usage(remaining.len() as u64 * 10)?;

    let Some(first) = remaining.front().cloned() else {
        return Ok(SysCallResult::Return(wrap_iter(XIterator::empty())));
    };

    fn map_callback<M>(
        state: CallbackState,
        params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut state = state
            .downcast::<MapState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;

        let mapped = params
            .into_iter()
            .next()
            .ok_or(EnvironmentError::InvalidCallbackParameters)?;

        state.result.push(ValuePointer::new(mapped.into_owned()));

        // pop the front (already processed)
        state.remaining.pop_front();

        match state.remaining.front().cloned() {
            Some(next) => Ok(SysCallResult::ExecuteAndCallback {
                ptr: state.closure_ptr.clone(),
                params: vec![next.to_owned().into()].into(),
                state,
                callback_params_len: 1,
                callback: CallbackType::Sync(map_callback),
            }),
            None => Ok(SysCallResult::Return(wrap_iter(XIterator::new(state.result)))),
        }
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure_ptr.clone(),
        params: vec![first.to_owned().into()].into(),
        state: Box::new(MapState {
            closure_ptr,
            remaining,
            result: Vec::new(),
        }),
        callback_params_len: 1,
        callback: CallbackType::Sync(map_callback),
    })
}

struct FilterState {
    closure_ptr: StackValue,
    remaining: VecDeque<ValuePointer>,
    result: Vec<ValuePointer>,
}

fn iter_filter<M>(
    zelf: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let closure_ptr = parameters.remove(0);
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let remaining: VecDeque<ValuePointer> = iter.items[iter.index..].iter().cloned().collect();

    context.increase_gas_usage(remaining.len() as u64 * 10)?;

    let Some(first) = remaining.front().cloned() else {
        return Ok(SysCallResult::Return(wrap_iter(XIterator::empty())));
    };

    fn filter_callback<M>(
        state: CallbackState,
        params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut state = state
            .downcast::<FilterState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;

        let keep = params
            .first()
            .ok_or(EnvironmentError::InvalidCallbackParameters)?
            .as_bool()?;

        let item = state.remaining.pop_front()
            .ok_or(EnvironmentError::InvalidCallbackParameters)?;

        if keep {
            state.result.push(item);
        }

        match state.remaining.front().cloned() {
            Some(next) => Ok(SysCallResult::ExecuteAndCallback {
                ptr: state.closure_ptr.clone(),
                params: vec![next.to_owned().into()].into(),
                state,
                callback_params_len: 1,
                callback: CallbackType::Sync(filter_callback),
            }),
            None => Ok(SysCallResult::Return(wrap_iter(XIterator::new(state.result)))),
        }
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure_ptr.clone(),
        params: vec![first.to_owned().into()].into(),
        state: Box::new(FilterState {
            closure_ptr,
            remaining,
            result: Vec::new(),
        }),
        callback_params_len: 1,
        callback: CallbackType::Sync(filter_callback),
    })
}

struct ForEachState {
    closure_ptr: StackValue,
    remaining: VecDeque<ValuePointer>,
}

fn iter_for_each<M>(
    zelf: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let closure_ptr = parameters.remove(0);
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let remaining: VecDeque<ValuePointer> = iter.items[iter.index..].iter().cloned().collect();

    context.increase_gas_usage(remaining.len() as u64 * 5)?;

    let Some(first) = remaining.front().cloned() else {
        return Ok(SysCallResult::None);
    };

    fn for_each_callback<M>(
        state: CallbackState,
        _params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut state = state
            .downcast::<ForEachState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;

        state.remaining.pop_front();

        match state.remaining.front().cloned() {
            Some(next) => Ok(SysCallResult::ExecuteAndCallback {
                ptr: state.closure_ptr.clone(),
                params: vec![next.to_owned().into()].into(),
                state,
                callback_params_len: 0,
                callback: CallbackType::Sync(for_each_callback),
            }),
            None => Ok(SysCallResult::None),
        }
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure_ptr.clone(),
        params: vec![first.to_owned().into()].into(),
        state: Box::new(ForEachState {
            closure_ptr,
            remaining,
        }),
        callback_params_len: 0,
        callback: CallbackType::Sync(for_each_callback),
    })
}

struct FindState {
    closure_ptr: StackValue,
    remaining: VecDeque<ValuePointer>,
}

fn iter_find<M>(
    zelf: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let closure_ptr = parameters.remove(0);
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let remaining: VecDeque<ValuePointer> = iter.items[iter.index..].iter().cloned().collect();

    context.increase_gas_usage(remaining.len() as u64 * 5)?;

    let Some(first) = remaining.front().cloned() else {
        return Ok(SysCallResult::Return(Primitive::Null.into()));
    };

    fn find_callback<M>(
        state: CallbackState,
        params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut state = state
            .downcast::<FindState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;

        let matched = params
            .first()
            .ok_or(EnvironmentError::InvalidCallbackParameters)?
            .as_bool()?;

        let item = state.remaining.pop_front()
            .ok_or(EnvironmentError::InvalidCallbackParameters)?;

        if matched {
            return Ok(SysCallResult::Return(item.to_owned().into()));
        }

        match state.remaining.front().cloned() {
            Some(next) => Ok(SysCallResult::ExecuteAndCallback {
                ptr: state.closure_ptr.clone(),
                params: vec![next.to_owned().into()].into(),
                state,
                callback_params_len: 1,
                callback: CallbackType::Sync(find_callback),
            }),
            None => Ok(SysCallResult::Return(Primitive::Null.into())),
        }
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure_ptr.clone(),
        params: vec![first.to_owned().into()].into(),
        state: Box::new(FindState {
            closure_ptr,
            remaining,
        }),
        callback_params_len: 1,
        callback: CallbackType::Sync(find_callback),
    })
}

struct AnyState {
    closure_ptr: StackValue,
    remaining: VecDeque<ValuePointer>,
}

fn iter_any<M>(
    zelf: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let closure_ptr = parameters.remove(0);
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let remaining: VecDeque<ValuePointer> = iter.items[iter.index..].iter().cloned().collect();

    context.increase_gas_usage(remaining.len() as u64 * 5)?;

    let Some(first) = remaining.front().cloned() else {
        return Ok(SysCallResult::Return(Primitive::Boolean(false).into()));
    };

    fn any_callback<M>(
        state: CallbackState,
        params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut state = state
            .downcast::<AnyState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;

        let matched = params
            .first()
            .ok_or(EnvironmentError::InvalidCallbackParameters)?
            .as_bool()?;

        state.remaining.pop_front();

        if matched {
            return Ok(SysCallResult::Return(Primitive::Boolean(true).into()));
        }

        match state.remaining.front().cloned() {
            Some(next) => Ok(SysCallResult::ExecuteAndCallback {
                ptr: state.closure_ptr.clone(),
                params: vec![next.to_owned().into()].into(),
                state,
                callback_params_len: 1,
                callback: CallbackType::Sync(any_callback),
            }),
            None => Ok(SysCallResult::Return(Primitive::Boolean(false).into())),
        }
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure_ptr.clone(),
        params: vec![first.to_owned().into()].into(),
        state: Box::new(AnyState {
            closure_ptr,
            remaining,
        }),
        callback_params_len: 1,
        callback: CallbackType::Sync(any_callback),
    })
}

struct AllState {
    closure_ptr: StackValue,
    remaining: VecDeque<ValuePointer>,
}

fn iter_all<M>(
    zelf: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let closure_ptr = parameters.remove(0);
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let remaining: VecDeque<ValuePointer> = iter.items[iter.index..].iter().cloned().collect();

    context.increase_gas_usage(remaining.len() as u64 * 5)?;

    let Some(first) = remaining.front().cloned() else {
        // all() on empty is vacuously true
        return Ok(SysCallResult::Return(Primitive::Boolean(true).into()));
    };

    fn all_callback<M>(
        state: CallbackState,
        params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut state = state
            .downcast::<AllState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;

        let matched = params
            .first()
            .ok_or(EnvironmentError::InvalidCallbackParameters)?
            .as_bool()?;

        state.remaining.pop_front();

        if !matched {
            return Ok(SysCallResult::Return(Primitive::Boolean(false).into()));
        }

        match state.remaining.front().cloned() {
            Some(next) => Ok(SysCallResult::ExecuteAndCallback {
                ptr: state.closure_ptr.clone(),
                params: vec![next.to_owned().into()].into(),
                state,
                callback_params_len: 1,
                callback: CallbackType::Sync(all_callback),
            }),
            None => Ok(SysCallResult::Return(Primitive::Boolean(true).into())),
        }
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure_ptr.clone(),
        params: vec![first.to_owned().into()].into(),
        state: Box::new(AllState {
            closure_ptr,
            remaining,
        }),
        callback_params_len: 1,
        callback: CallbackType::Sync(all_callback),
    })
}

struct FoldState {
    closure_ptr: StackValue,
    remaining: VecDeque<ValuePointer>,
}

fn iter_fold<M>(
    zelf: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let init = parameters.remove(0);
    let closure_ptr = parameters.remove(0);
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let mut remaining: VecDeque<ValuePointer> = iter.items[iter.index..].iter().cloned().collect();

    context.increase_gas_usage(remaining.len() as u64 * 5)?;

    let Some(first) = remaining.pop_front() else {
        return Ok(SysCallResult::Return(init));
    };

    fn fold_callback<M>(
        state: CallbackState,
        params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut state = state
            .downcast::<FoldState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;

        let acc = params
            .into_iter()
            .next()
            .ok_or(EnvironmentError::InvalidCallbackParameters)?;

        match state.remaining.pop_front() {
            Some(next) => Ok(SysCallResult::ExecuteAndCallback {
                ptr: state.closure_ptr.clone(),
                params: vec![acc, next.to_owned().into()].into(),
                state,
                callback_params_len: 1,
                callback: CallbackType::Sync(fold_callback),
            }),
            None => Ok(SysCallResult::Return(acc)),
        }
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure_ptr.clone(),
        params: vec![init, first.to_owned().into()].into(),
        state: Box::new(FoldState {
            closure_ptr,
            remaining,
        }),
        callback_params_len: 1,
        callback: CallbackType::Sync(fold_callback),
    })
}

struct PositionState {
    closure_ptr: StackValue,
    remaining: VecDeque<ValuePointer>,
    current_index: u32,
}

fn iter_position<M>(
    zelf: FnInstance,
    mut parameters: FnParams,
    _: &ModuleMetadata<'_, M>,
    context: &mut VMContext,
) -> FnReturnType<M> {
    let closure_ptr = parameters.remove(0);
    let sv = zelf?;
    let iter = sv.as_ref().as_opaque()?.as_ref::<XIterator>()?;
    let remaining: VecDeque<ValuePointer> = iter.items[iter.index..].iter().cloned().collect();

    context.increase_gas_usage(remaining.len() as u64 * 5)?;

    let Some(first) = remaining.front().cloned() else {
        return Ok(SysCallResult::Return(Primitive::Null.into()));
    };

    fn position_callback<M>(
        state: CallbackState,
        params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut state = state
            .downcast::<PositionState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;

        let matched = params
            .first()
            .ok_or(EnvironmentError::InvalidCallbackParameters)?
            .as_bool()?;

        let idx = state.current_index;
        state.remaining.pop_front();
        state.current_index += 1;

        if matched {
            return Ok(SysCallResult::Return(Primitive::U32(idx).into()));
        }

        match state.remaining.front().cloned() {
            Some(next) => Ok(SysCallResult::ExecuteAndCallback {
                ptr: state.closure_ptr.clone(),
                params: vec![next.to_owned().into()].into(),
                state,
                callback_params_len: 1,
                callback: CallbackType::Sync(position_callback),
            }),
            None => Ok(SysCallResult::Return(Primitive::Null.into())),
        }
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: closure_ptr.clone(),
        params: vec![first.to_owned().into()].into(),
        state: Box::new(PositionState {
            closure_ptr,
            remaining,
            current_index: 0,
        }),
        callback_params_len: 1,
        callback: CallbackType::Sync(position_callback),
    })
}
