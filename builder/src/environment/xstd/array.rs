use xelis_types::{ClosureType, Constant, Primitive, StackValue, Type, U256, ValueCell};
use xelis_environment::{CallbackState, Context, EnvironmentError, FnInstance, FnParams, FnReturnType, FunctionHandler, ModuleMetadata, SysCallResult};
use super::EnvironmentBuilder;
use paste::paste;

macro_rules! array_number_with_size {
    ($env: expr, $op: ident, $t: ident) => {
        paste! {
            fn [<with_size_ $op>](params: Vec<Constant>) -> Result<Constant, anyhow::Error> {
                let count = params[0].as_u32()? as usize;
                let values = vec![Constant::Primitive(Primitive::$t(Default::default())); count];
                Ok(Constant::Array(values))
            }

            // Registering the generated function in the environment
            $env.register_const_function(
                // Function name as a string
                "with_size",
                Type::Array(Box::new(Type::$t)),
                vec![("size", Type::U32)],
                // The function ptr
                [<with_size_ $op>]
            );
        }
    };
}

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    env.register_native_function("len", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![], FunctionHandler::Sync(len), 1, Some(Type::U32));
    env.register_native_function("push", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("value", Type::T(Some(0)))], FunctionHandler::Sync(push), 2, None);
    env.register_native_function("remove", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("index", Type::U32)], FunctionHandler::Sync(remove), 5, Some(Type::T(Some(0))));
    env.register_native_function("swap_remove", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("index", Type::U32)], FunctionHandler::Sync(swap_remove), 8, Some(Type::T(Some(0))));
    env.register_native_function("insert", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("index", Type::U32), ("value", Type::T(Some(0)))], FunctionHandler::Sync(insert), 5, None);
    env.register_native_function("index_of", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("value", Type::T(Some(0)))], FunctionHandler::Sync(index_of), 5, Some(Type::Optional(Box::new(Type::U32))));

    env.register_native_function("pop", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![], FunctionHandler::Sync(pop), 1, Some(Type::Optional(Box::new(Type::T(Some(0))))));
    env.register_native_function("slice", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("range", Type::Range(Box::new(Type::U32)))], FunctionHandler::Sync(slice), 5, Some(Type::Array(Box::new(Type::T(Some(0))))));
    env.register_native_function("contains", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("value", Type::T(Some(0)))], FunctionHandler::Sync(contains), 10, Some(Type::Bool));
    env.register_native_function("get", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("index", Type::U32)], FunctionHandler::Sync(get), 1, Some(Type::Optional(Box::new(Type::T(Some(0))))));
    env.register_native_function("first", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![], FunctionHandler::Sync(first), 1, Some(Type::Optional(Box::new(Type::T(Some(0))))));
    env.register_native_function("last", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![], FunctionHandler::Sync(last), 1, Some(Type::Optional(Box::new(Type::T(Some(0))))));

    env.register_native_function("extend", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("other", Type::Array(Box::new(Type::T(Some(0)))))], FunctionHandler::Sync(extend), 5, None);
    env.register_native_function("concat", Some(Type::Array(Box::new(Type::Array(Box::new(Type::T(Some(0))))))), vec![], FunctionHandler::Sync(concat), 5, Some(Type::Array(Box::new(Type::T(Some(0))))));
    env.register_native_function("split_off", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("index", Type::U32)], FunctionHandler::Sync(split_off), 5, Some(Type::Array(Box::new(Type::T(Some(0))))));
    env.register_native_function("truncate", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("size", Type::U32)], FunctionHandler::Sync(truncate), 5, None);

    // Transform a Type::Array(U8) into bytes easily
    env.register_native_function("to_bytes", Some(Type::Array(Box::new(Type::U8))), vec![], FunctionHandler::Sync(to_bytes), 1, Some(Type::Bytes));

    // Constant function
    env.register_const_function("with", Type::Array(Box::new(Type::T(Some(0)))), vec![("size", Type::U32), ("default", Type::T(Some(0)))], const_with);

    // Sort function
    // Only works with primitive types that implement Ord
    env.register_native_function(
        "sort",
        Some(Type::Array(Box::new(Type::T(Some(0))))),
        vec![],
        FunctionHandler::Sync(sort),
        20,
        None,
    );

    env.register_native_function(
        "reverse",
        Some(Type::Array(Box::new(Type::T(Some(0))))),
        vec![],
        FunctionHandler::Sync(reverse),
        10,
        None,
    );

    env.register_native_function(
        "map",
        Some(Type::Array(Box::new(Type::T(Some(0))))),
        vec![
            ("mapper", Type::Closure(ClosureType::new(
                vec![Type::T(Some(0))],
                Some(Type::Any)
            )))
        ],
        FunctionHandler::Sync(map),
        10,
        Some(Type::Array(Box::new(Type::Any))),
    );

    // O(n) to build key cache + sort by key implementation
    // Only support integer types
    env.register_native_function(
        "sort_by_key",
        Some(Type::Array(Box::new(Type::T(Some(0))))),
        vec![
            ("key_fn", Type::Closure(ClosureType::new(
                vec![Type::T(Some(0))],
                Some(Type::Any)
            ))),
            ("ascending", Type::Bool),
        ],
        FunctionHandler::Sync(sort_by_key),
        30,
        None,
    );

    array_number_with_size!(env, u8, U8);
    array_number_with_size!(env, u16, U16);
    array_number_with_size!(env, u32, U32);
    array_number_with_size!(env, u64, U64);
    array_number_with_size!(env, u128, U128);
    array_number_with_size!(env, u256, U256);
}

// native functions
fn len<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let len = zelf?.as_vec()?.len();
    Ok(SysCallResult::Return(Primitive::U32(len as u32).into()))
}

fn push<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let mut zelf = zelf?;
    let array = zelf.as_mut().as_mut_vec()?;
    if array.len() >= u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }

    let param = parameters.remove(0);
    let depth = param.depth();
    let value = param.into_owned();

    value.calculate_depth(
        context.max_value_depth()
            .saturating_sub(depth.saturating_add(1))
    )?;

    array.push(value.into());

    Ok(SysCallResult::None)
}

fn remove<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let index = parameters.remove(0)
        .as_u32()? as usize;

    let mut zelf = zelf?;
    let array = zelf.as_mut().as_mut_vec()?;

    if index >= array.len() {
        return Err(EnvironmentError::OutOfBounds(index, array.len()))
    }

    // moving all elements after the index to the left is costly
    let shift_len = array.len() - index;
    context.increase_gas_usage((shift_len as u64) * 5)?;

    Ok(SysCallResult::Return(array.remove(index).into()))
}

fn swap_remove<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let index = parameters.remove(0).as_u32()? as usize;

    let mut zelf = zelf?;
    let array = zelf.as_mut().as_mut_vec()?;
    if index >= array.len() {
        return Err(EnvironmentError::OutOfBounds(index, array.len()))
    }

    Ok(SysCallResult::Return(array.swap_remove(index).into()))
}

fn insert<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let index = parameters.remove(0).as_u32()? as usize;
    let param = parameters.remove(0);
    let depth = param.depth();
    let value = param.into_owned();

    let mut zelf = zelf?;
    let array = zelf.as_mut().as_mut_vec()?;

    if index >= array.len() {
        return Err(EnvironmentError::OutOfBounds(index, array.len()))
    }

    if array.len() >= u32::MAX as usize || array.len() + 1 > u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }

    value.calculate_depth(
        context.max_value_depth()
            .saturating_sub(depth.saturating_add(1))
    )?;

    // moving all elements after the index to the right is costly
    let shift_len = array.len() - index;
    context.increase_gas_usage((shift_len as u64) * 5)?;

    array.insert(index, value.into());

    Ok(SysCallResult::None)
}

fn index_of<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let value = parameters.remove(0);
    let handle = value.as_ref();
    let zelf = zelf?;
    let vec = zelf.as_vec()?;

    // we need to go through all elements in the slice, thus we increase the gas usage
    context.increase_gas_usage((vec.len() as u64) * 5)?;

    for (i, v) in vec.iter().enumerate() {
        if *v.as_ref() == *handle {
            return Ok(SysCallResult::Return(Primitive::U32(i as u32).into()))
        }
    }

    Ok(Primitive::Null.into())
}

fn pop<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let mut zelf = zelf?;
    let array = zelf.as_mut().as_mut_vec()?;
    if let Some(value) = array.pop() {
        Ok(SysCallResult::Return(value.into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn slice<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let param = parameters.remove(0);
    let range = param.as_ref();
    let (start, end) = range.as_range()?;

    let start = start.as_u32()?;
    let end = end.as_u32()?;

    let mut zelf = zelf?;
    let vec = zelf.as_mut().as_mut_vec()?;
    let len = vec.len() as u32;
    if start >= len || end >= len || start >= end {
        return Err(EnvironmentError::InvalidRange(start, end))
    }

    // we need to go through all elements in the slice, thus we increase the gas usage
    context.increase_gas_usage((vec.len() as u64) * 5)?;

    let mut slice = Vec::new();
    for i in start..end {
        // due to ValuePointer, slice are connected.
        let value = vec.get(i as usize)
            .cloned()
            .ok_or(EnvironmentError::NoValueFoundAtIndex(i))?;
        slice.push(value);
    }

    Ok(SysCallResult::Return(ValueCell::Object(slice).into()))
}

fn contains<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let value = parameters.remove(0);
    let handle = value.as_ref();
    let zelf = zelf?;
    let vec = zelf.as_vec()?;

    // we need to go through all elements in the slice, thus we increase the gas usage
    context.increase_gas_usage((vec.len() as u64) * 5)?;

    Ok(SysCallResult::Return(Primitive::Boolean(vec.iter().find(|v| *v.as_ref() == *handle).is_some()).into()))
}

fn get<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let index = parameters.remove(0).as_u32()? as usize;
    let zelf = zelf?;
    let vec = zelf.as_vec()?;
    if let Some(value) = vec.get(index) {
        Ok(SysCallResult::Return(value.to_owned().into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn first<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let vec = zelf.as_vec()?;
    if let Some(value) = vec.first() {
        Ok(SysCallResult::Return(value.to_owned().into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn last<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let vec = zelf.as_vec()?;
    if let Some(value) = vec.last() {
        Ok(SysCallResult::Return(value.to_owned().into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn extend<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let other = parameters.remove(0)
        .into_owned()
        .to_vec()?;

    context.increase_gas_usage(other.len() as _)?;

    let mut zelf = zelf?;
    // SAFETY: we have exclusive access to zelf
    let vec = zelf.as_mut().as_mut_vec()?;

    if other.len() as u64 + vec.len() as u64 > u32::MAX as u64 {
        return Err(EnvironmentError::OutOfMemory)
    }

    vec.extend(other);

    Ok(SysCallResult::None)
}

fn concat<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let vec = zelf.as_vec()?;
    context.increase_gas_usage(vec.len() as u64)?;

    let mut result = Vec::new();
    for el in vec.iter() {
        let v = el.as_ref().as_vec()?;

        context.increase_gas_usage(v.len() as u64)?;
        // Check len is <= u32::MAX
        if result.len() as u64 + v.len() as u64 > u32::MAX as u64 {
            return Err(EnvironmentError::OutOfMemory)
        }

        result.extend(v.iter().cloned());
    }

    Ok(SysCallResult::Return(ValueCell::Object(result).into()))
}

fn const_with(mut params: Vec<Constant>) -> Result<Constant, anyhow::Error> {
    let default = params.remove(1);
    let count = params[0].as_u32()? as usize;
    let values = vec![default; count];
    Ok(Constant::Array(values))
}

fn to_bytes<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let values = zelf.as_vec()?;
    let len = values.len();

    context.increase_gas_usage(len as _)?;

    let bytes = values.iter()
        .map(|v| v.as_ref().as_u8())
        .collect::<Result<Vec<_>, _>>()?;

    Ok(SysCallResult::Return(ValueCell::Bytes(bytes).into()))
}

fn split_off<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let index = parameters.remove(0).as_u32()? as usize;

    let mut zelf = zelf?;
    let vec = zelf.as_mut().as_mut_vec()?;
    if index > vec.len() {
        return Err(EnvironmentError::OutOfBounds(index, vec.len()))
    }

    // we need to allocate all elements in the new slice, thus we increase the gas usage
    context.increase_gas_usage((vec.len() - index) as u64)?;

    let second = vec.split_off(index);
    Ok(SysCallResult::Return(ValueCell::Object(second).into()))
}

fn truncate<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let size = parameters.remove(0).as_u32()? as usize;

    let mut zelf = zelf?;
    let vec = zelf.as_mut().as_mut_vec()?;
    if size > vec.len() {
        return Err(EnvironmentError::OutOfBounds(size, vec.len()))
    }

    vec.truncate(size);

    Ok(SysCallResult::None)
}

fn sort<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let mut zelf = zelf?;
    let vec = zelf.as_mut().as_mut_vec()?;

    // Simple bubble sort implementation
    let len = vec.len();
    context.increase_gas_usage((len * len) as u64)?;

    for i in 0..len {
        for j in 0..(len - i - 1) {
            let a = vec[j].as_ref();
            let b = vec[j + 1].as_ref();
            if a.as_value()? > b.as_value()? {
                vec.swap(j, j + 1);
            }
        }
    }

    Ok(SysCallResult::None)
}

fn reverse<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let mut zelf = zelf?;
    let vec = zelf.as_mut().as_mut_vec()?;

    let len = vec.len();
    context.increase_gas_usage((len / 2) as u64)?;

    vec.reverse();

    Ok(SysCallResult::None)
}

pub struct MapState {
    closure_ptr: StackValue,
    index: usize,
    ptr: StackValue,
    tmp: StackValue,
}

fn map<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let key_fn = parameters.remove(0);
    let mut zelf = zelf?;

    // First we need to prepare a state with all the mapped keys
    let vec = zelf.as_mut().as_mut_vec()?;

    let len = vec.len();
    context.increase_gas_usage(len as u64 * 10)?;

    let Some(first) = vec.first().cloned() else {
        return Ok(SysCallResult::Return(zelf))
    };

    fn map_callback<M>(
        state: CallbackState,
        params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut state = state
            .downcast::<MapState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;

        let key = params
            .get(0)
            .ok_or(EnvironmentError::InvalidCallbackParameters)?
            .clone();

        let tmp = state.tmp
            .as_mut_vec()?;
        tmp.push(key.into_owned().into());

        state.index += 1;

        let Some(next) = state.ptr
            .as_mut_vec()?
            .get(state.index)
            .cloned() else {
                return Ok(SysCallResult::Return(state.tmp))
            };

        Ok(SysCallResult::ExecuteAndCallback {
            ptr: state.closure_ptr.clone(),
            params: vec![next.into()].into(),
            state,
            callback_params_len: 1,
            callback: map_callback,
        })
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: key_fn.clone(),
        params: vec![first.into()].into(),
        state: Box::new(MapState {
            closure_ptr: key_fn,
            index: 0,
            ptr: zelf.into(),
            tmp: ValueCell::Object(Vec::new()).into(),
        }),
        callback_params_len: 1,
        callback: map_callback,
    })
}

struct SortByKeyState {
    closure_ptr: StackValue,
    index: usize,
    // actual array
    ptr: StackValue,
    // current cache being built
    // index of original element and its mapped key
    cache: Vec<(usize, OrdKey)>,
    // Force the key type
    kind: Option<OrdKeyKind>,
    // ascending or descending
    ascending: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum OrdKey {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    U256(U256),
}

impl OrdKey {
    #[inline]
    const fn kind(&self) -> OrdKeyKind {
        match self {
            OrdKey::U8(_) => OrdKeyKind::U8,
            OrdKey::U16(_) => OrdKeyKind::U16,
            OrdKey::U32(_) => OrdKeyKind::U32,
            OrdKey::U64(_) => OrdKeyKind::U64,
            OrdKey::U128(_) => OrdKeyKind::U128,
            OrdKey::U256(_) => OrdKeyKind::U256,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum OrdKeyKind {
    U8,
    U16,
    U32,
    U64,
    U128,
    U256,
}

fn sort_by_key<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let ascending = parameters.remove(1).as_bool()?;
    let key_fn = parameters.remove(0);
    let mut zelf = zelf?;

    // First we need to prepare a state with all the mapped keys
    let vec = zelf.as_mut().as_mut_vec()?;

    let len = vec.len() as u64;
    context.increase_gas_usage((len * len) * 15)?;

    let Some(first) = vec.first().cloned() else {
        return Ok(SysCallResult::None)
    };

    fn sort_by_key_callback<M>(
        state: CallbackState,
        params: FnParams,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut state = state
            .downcast::<SortByKeyState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;

        let key = params
            .get(0)
            .ok_or(EnvironmentError::InvalidCallbackParameters)?
            .clone();

        let mapped_key = match key.into_owned().into_value()? {
            Primitive::U8(v) => OrdKey::U8(v),
            Primitive::U16(v) => OrdKey::U16(v),
            Primitive::U32(v) => OrdKey::U32(v),
            Primitive::U64(v) => OrdKey::U64(v),
            Primitive::U128(v) => OrdKey::U128(v),
            Primitive::U256(v) => OrdKey::U256(v),
            _ => {
                return Err(EnvironmentError::Static("sort_by_key only supports primitive types that implement Ord"))
            }
        };

        let kind = mapped_key.kind();
        match state.kind {
            Some(existing_kind) => {
                if existing_kind != kind {
                    return Err(EnvironmentError::Static("sort_by_key require that all keys must be of the same type"))
                }
            },
            None => {
                state.kind = Some(kind);
            }
        }

        state.cache.push((state.index, mapped_key));
        state.index += 1;

        let next = state.ptr
            .as_mut_vec()?
            .get(state.index)
            .cloned();
        match next {
            Some(next) => {
                Ok(SysCallResult::ExecuteAndCallback {
                    ptr: state.closure_ptr.clone(),
                    params: vec![next.into()].into(),
                    state,
                    callback_params_len: 1,
                    callback: sort_by_key_callback,
                })
            },
            None => {
                // Cache is built, now we can sort the array based on it
                let array = state.ptr
                    .as_mut_vec()?;

                let mut keys = state.cache;

                if keys.len() != array.len() {
                    return Err(EnvironmentError::Static("SortByKeyState keys and array length mismatch"))
                }

                if state.ascending {
                    keys.sort_by(|a, b| a.1.cmp(&b.1));
                } else {
                    keys.sort_by(|a, b| b.1.cmp(&a.1));
                }

                // Now, we need to reorder the original array based on the sorted keys
                let original_array = array.clone();
                for (new_index, (original_index, _)) in keys.into_iter().enumerate() {
                    let value = original_array
                        .get(original_index)
                        .cloned()
                        .ok_or(EnvironmentError::OutOfBounds(original_index, array.len()))?;

                    array[new_index] = value;
                }

                Ok(SysCallResult::None)
            }
        }
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: key_fn.clone(),
        params: vec![first.into()].into(),
        state: Box::new(SortByKeyState {
            closure_ptr: key_fn,
            index: 0,
            ptr: zelf.into(),
            cache: Vec::new(),
            kind: None,
            ascending,
        }),
        callback_params_len: 1,
        callback: sort_by_key_callback,
    })
}