use silex_types::{PrimitiveInteger, ClosureType, Constant, Primitive, StackValue, Type, U256, RuntimeValue, Heap};
use silex_environment::{VMContext, CallbackState, CallbackType, EnvironmentError, FnInstance, FnParams, FnReturnType, FunctionHandler, ModuleMetadata, SysCallResult};
use crate::ConstFunctionError;

use super::EnvironmentBuilder;
use paste::paste;

macro_rules! array_number_with_size {
    ($env: expr, $op: ident, $t: ident) => {
        paste! {
            fn [<with_size_ $op>](params: Vec<Constant>) -> Result<Constant, ConstFunctionError> {
                let count = params[0].as_u32()? as usize;
                let values = vec![Constant::Primitive(Primitive::Integer(PrimitiveInteger::$t(Default::default()))); count];
                Ok(Constant::Array(values))
            }

            // Registering the generated function in the environment
            $env.register_const_function_with_comment(
                // Function name as a string
                "with_size",
                Type::Array(Box::new(Type::$t)),
                vec![("size", Type::U32)],
                // The function ptr
                [<with_size_ $op>],
                concat!("Creates an array of ", stringify!($t), " zero values with the requested size at compile time.")
            );
        }
    };
}

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    env.register_native_function_with_comment("len", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![], FunctionHandler::Sync(len), 1, Some(Type::U32), "Returns the number of elements in the array.");
    env.register_native_function_with_comment("push", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("element", Type::T(Some(0)))], FunctionHandler::Sync(push), 2, None, "Appends an element to the end of the array.");
    env.register_native_function_with_comment("remove", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("index", Type::U32)], FunctionHandler::Sync(remove), 5, Some(Type::T(Some(0))), "Removes and returns the element at the index, shifting later elements left.");
    env.register_native_function_with_comment("swap_remove", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("index", Type::U32)], FunctionHandler::Sync(swap_remove), 8, Some(Type::T(Some(0))), "Removes and returns the element at the index by swapping in the last element.");
    env.register_native_function_with_comment("insert", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("index", Type::U32), ("element", Type::T(Some(0)))], FunctionHandler::Sync(insert), 5, None, "Inserts an element at the index, shifting later elements right.");
    env.register_native_function_with_comment("index_of", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("element", Type::T(Some(0)))], FunctionHandler::Sync(index_of), 5, Some(Type::Optional(Box::new(Type::U32))), "Returns the first index of the element, or null when absent.");

    env.register_native_function_with_comment("pop", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![], FunctionHandler::Sync(pop), 1, Some(Type::Optional(Box::new(Type::T(Some(0))))), "Removes and returns the last element, or null when empty.");
    env.register_native_function_with_comment("slice", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("range", Type::Range(Box::new(Type::U32)))], FunctionHandler::Sync(slice), 5, Some(Type::Array(Box::new(Type::T(Some(0))))), "Returns a shallow slice of the array for the range.");
    env.register_native_function_with_comment("contains", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("element", Type::T(Some(0)))], FunctionHandler::Sync(contains), 10, Some(Type::Bool), "Returns true when the array contains the element.");
    env.register_native_function_with_comment("get", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("index", Type::U32)], FunctionHandler::Sync(get), 1, Some(Type::Optional(Box::new(Type::T(Some(0))))), "Returns the element at the index, or null if out of bounds.");
    env.register_native_function_with_comment("first", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![], FunctionHandler::Sync(first), 1, Some(Type::Optional(Box::new(Type::T(Some(0))))), "Returns the first element, or null when empty.");
    env.register_native_function_with_comment("last", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![], FunctionHandler::Sync(last), 1, Some(Type::Optional(Box::new(Type::T(Some(0))))), "Returns the last element, or null when empty.");

    env.register_native_function_with_comment("extend", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("other", Type::Array(Box::new(Type::T(Some(0)))))], FunctionHandler::Sync(extend), 5, None, "Appends all elements from another array.");
    env.register_native_function_with_comment("concat", Some(Type::Array(Box::new(Type::Array(Box::new(Type::T(Some(0))))))), vec![], FunctionHandler::Sync(concat), 5, Some(Type::Array(Box::new(Type::T(Some(0))))), "Flattens one level of nested arrays into a single array.");
    env.register_native_function_with_comment("split_off", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("index", Type::U32)], FunctionHandler::Sync(split_off), 5, Some(Type::Array(Box::new(Type::T(Some(0))))), "Splits the array at the index, keeping the first part and returning the second.");
    env.register_native_function_with_comment("truncate", Some(Type::Array(Box::new(Type::T(Some(0))))), vec![("size", Type::U32)], FunctionHandler::Sync(truncate), 5, None, "Shortens the array to the requested size.");

    // Transform a Type::Array(U8) into bytes easily
    env.register_native_function_with_comment("to_bytes", Some(Type::Array(Box::new(Type::U8))), vec![], FunctionHandler::Sync(to_bytes), 1, Some(Type::Bytes), "Converts an array of u8 values into bytes.");

    // Constant function
    env.register_const_function_with_comment("with", Type::Array(Box::new(Type::T(Some(0)))), vec![("size", Type::U32), ("default", Type::T(Some(0)))], const_with, "Creates an array filled with the default value at compile time.");

    // Sort function
    // Only works with primitive types that implement Ord
    env.register_native_function_with_comment(
        "sort",
        Some(Type::Array(Box::new(Type::T(Some(0))))),
        vec![("ascending", Type::Bool)],
        FunctionHandler::Sync(sort),
        20,
        None,
        "Sorts the array in ascending or descending order.",
    );

    env.register_native_function_with_comment(
        "reverse",
        Some(Type::Array(Box::new(Type::T(Some(0))))),
        vec![],
        FunctionHandler::Sync(reverse),
        10,
        None,
        "Reverses the array in place.",
    );

    env.register_native_function_with_comment(
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
        "Calls the mapper for each element and returns the mapped values.",
    );

    // O(n) to build key cache + sort by key implementation
    // Only support integer types
    env.register_native_function_with_comment(
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
        "Sorts the array by keys returned from the key function.",
    );

    array_number_with_size!(env, u8, U8);
    array_number_with_size!(env, u16, U16);
    array_number_with_size!(env, u32, U32);
    array_number_with_size!(env, u64, U64);
    array_number_with_size!(env, u128, U128);
    array_number_with_size!(env, u256, U256);
}

// native functions
fn len<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let len = zelf?.as_vec(context.heap())?.len();
    Ok(SysCallResult::Return(Primitive::Integer(PrimitiveInteger::U32(len as u32)).into()))
}

fn push<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let mut zelf = zelf?;
    if zelf.as_vec(heap)?.len() >= u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }
    let param = parameters.remove(0);
    let depth = param.depth();
    let value = param.into_owned(heap)?;
    value.calculate_depth(heap, resources.max_value_depth().saturating_sub(depth.saturating_add(1)))?;
    let id = heap.insert(value)?;
    zelf.as_mut_vec(heap)?.push(id);
    Ok(SysCallResult::None)
}

fn remove<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let index = parameters.remove(0).as_u32(heap)? as usize;
    let mut zelf = zelf?;
    let len = zelf.as_vec(heap)?.len();
    if index >= len {
        return Err(EnvironmentError::OutOfBounds(index, len))
    }
    let shift_len = len - index;
    resources.increase_gas_usage((shift_len as u64) * 5)?;
    Ok(SysCallResult::Return(zelf.as_mut_vec(heap)?.remove(index).into()))
}

fn swap_remove<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let index = parameters.remove(0).as_u32(heap)? as usize;
    let mut zelf = zelf?;
    let array = zelf.as_vec(heap)?;
    if index >= array.len() {
        return Err(EnvironmentError::OutOfBounds(index, array.len()))
    }
    // Preserve the legacy size-based charge; removal itself moves a handle.
    let memory = array[index].as_ref(heap).calculate_memory_usage(heap, resources.memory_left())?;
    resources.increase_gas_usage(memory as u64)?;
    Ok(SysCallResult::Return(zelf.as_mut_vec(heap)?.swap_remove(index).into()))
}

fn insert<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let index = parameters.remove(0).as_u32(heap)? as usize;
    let param = parameters.remove(0);
    let depth = param.depth();
    let value = param.into_owned(heap)?;
    let mut zelf = zelf?;
    let len = zelf.as_vec(heap)?.len();
    if index >= len {
        return Err(EnvironmentError::OutOfBounds(index, len))
    }
    if len >= u32::MAX as usize || len + 1 > u32::MAX as usize {
        return Err(EnvironmentError::OutOfMemory)
    }
    value.calculate_depth(heap, resources.max_value_depth().saturating_sub(depth.saturating_add(1)))?;
    let shift_len = len - index;
    resources.increase_gas_usage((shift_len as u64) * 5)?;
    let id = heap.insert(value)?;
    zelf.as_mut_vec(heap)?.insert(index, id);
    Ok(SysCallResult::None)
}

fn index_of<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let value = parameters.remove(0);
    let handle = value.as_ref(heap);
    let zelf = zelf?;
    let vec = zelf.as_vec(heap)?;

    // we need to go through all elements in the slice, thus we increase the gas usage
    resources.increase_gas_usage((vec.len() as u64) * 5)?;

    for (i, v) in vec.iter().enumerate() {
        if v.as_ref(heap).equals(handle, heap) {
            return Ok(SysCallResult::Return(Primitive::Integer(PrimitiveInteger::U32(i as u32)).into()))
        }
    }

    Ok(Primitive::Null.into())
}

fn pop<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let mut zelf = zelf?;
    let array = zelf.as_mut(context.heap_mut()).as_mut_vec()?;
    if let Some(value) = array.pop() {
        Ok(SysCallResult::Return(value.into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn slice<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let param = parameters.remove(0);
    let range = param.as_ref(heap).as_range()?;
    let start = Primitive::Integer(range.start()).as_u32()?;
    let end = Primitive::Integer(range.end()).as_u32()?;
    let zelf = zelf?;
    let array = zelf.as_vec(heap)?;
    let len = array.len() as u32;
    if start >= len || end >= len || start >= end {
        return Err(EnvironmentError::InvalidRange(start, end))
    }
    resources.increase_gas_usage((array.len() as u64) * 5)?;
    let slice = array[start as usize..end as usize].to_vec();
    Ok(SysCallResult::Return(RuntimeValue::Object(slice).into()))
}

fn contains<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let value = parameters.remove(0);
    let handle = value.as_ref(heap);
    let zelf = zelf?;
    let vec = zelf.as_vec(heap)?;

    // we need to go through all elements in the slice, thus we increase the gas usage
    resources.increase_gas_usage((vec.len() as u64) * 5)?;

    Ok(SysCallResult::Return(Primitive::Boolean(vec.iter().find(|v| v.as_ref(heap).equals(handle, heap)).is_some()).into()))
}

fn get<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let index = parameters.remove(0).as_u32(context.heap())? as usize;
    let zelf = zelf?;
    let vec = zelf.as_vec(context.heap())?;
    if let Some(value) = vec.get(index) {
        Ok(SysCallResult::Return(value.to_owned(context.heap())?.into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn first<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let zelf = zelf?;
    let vec = zelf.as_vec(context.heap())?;
    if let Some(value) = vec.first() {
        Ok(SysCallResult::Return(value.to_owned(context.heap())?.into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn last<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let zelf = zelf?;
    let vec = zelf.as_vec(context.heap())?;
    if let Some(value) = vec.last() {
        Ok(SysCallResult::Return(value.to_owned(context.heap())?.into()))
    } else {
        Ok(SysCallResult::Return(Primitive::Null.into()))
    }
}

fn extend<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let other = parameters.remove(0).into_owned(heap)?.to_vec()?;
    resources.increase_gas_usage(other.len() as _)?;
    let mut zelf = zelf?;
    let array = zelf.as_mut_vec(heap)?;
    if other.len() as u64 + array.len() as u64 > u32::MAX as u64 {
        return Err(EnvironmentError::OutOfMemory)
    }
    array.extend(other);
    Ok(SysCallResult::None)
}

fn concat<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let zelf = zelf?;
    let vec = zelf.as_vec(heap)?;
    resources.increase_gas_usage(vec.len() as u64)?;

    let mut result = Vec::new();
    for el in vec.iter() {
        let v = el.as_ref(heap).as_vec()?;

        resources.increase_gas_usage(v.len() as u64)?;
        // Check len is <= u32::MAX
        if result.len() as u64 + v.len() as u64 > u32::MAX as u64 {
            return Err(EnvironmentError::OutOfMemory)
        }

        result.extend(v.iter().cloned());
    }

    Ok(SysCallResult::Return(RuntimeValue::Object(result).into()))
}

fn const_with(mut params: Vec<Constant>) -> Result<Constant, ConstFunctionError> {
    let default = params.remove(1);
    let count = params[0].as_u32()? as usize;
    let values = vec![default; count];
    Ok(Constant::Array(values))
}

fn to_bytes<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let zelf = zelf?;
    let values = zelf.as_vec(heap)?;
    let len = values.len();

    resources.increase_gas_usage(len as _)?;

    let bytes = values.iter()
        .map(|v| v.as_ref(heap).as_u8())
        .collect::<Result<Vec<_>, _>>()?;

    Ok(SysCallResult::Return(RuntimeValue::Bytes(bytes).into()))
}

fn split_off<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let index = parameters.remove(0).as_u32(heap)? as usize;
    let mut zelf = zelf?;
    let len = zelf.as_vec(heap)?.len();
    if index > len {
        return Err(EnvironmentError::OutOfBounds(index, len))
    }
    resources.increase_gas_usage((len - index) as u64)?;
    let second = zelf.as_mut_vec(heap)?.split_off(index);
    Ok(SysCallResult::Return(RuntimeValue::Object(second).into()))
}

fn truncate<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let size = parameters.remove(0).as_u32(heap)? as usize;
    let mut zelf = zelf?;
    let len = zelf.as_vec(heap)?.len();
    if size > len {
        return Err(EnvironmentError::OutOfBounds(size, len))
    }
    resources.increase_gas_usage((len - size) as u64)?;
    zelf.as_mut_vec(heap)?.truncate(size);
    Ok(SysCallResult::None)
}

fn sort<M>(zelf: FnInstance, params: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let mut zelf = zelf?;
    let ascending = params[0].as_bool(heap)?;
    let array = zelf.as_vec(heap)?;
    if array.is_empty() {
        return Ok(SysCallResult::None)
    }
    let len = array.len() as u64;
    let log2n = len.ilog2() as u64;
    resources.increase_gas_usage(len * (log2n + 5))?;
    let mut keys = Vec::with_capacity(array.len());
    for (i, id) in array.iter().enumerate() {
        keys.push((i, OrdKey::from_value(id.to_owned(heap)?)?));
    }
    if ascending {
        keys.sort_by(|a, b| a.1.cmp(&b.1));
    } else {
        keys.sort_by(|a, b| b.1.cmp(&a.1));
    }
    let original = array.clone();
    let array = zelf.as_mut_vec(heap)?;
    for (new_index, (original_index, _)) in keys.into_iter().enumerate() {
        array[new_index] = original[original_index];
    }
    Ok(SysCallResult::None)
}

fn reverse<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let mut zelf = zelf?;
    let len = zelf.as_vec(heap)?.len();
    resources.increase_gas_usage((len / 2) as u64)?;
    zelf.as_mut_vec(heap)?.reverse();
    Ok(SysCallResult::None)
}

pub struct MapState {
    closure_ptr: StackValue,
    index: usize,
    ptr: StackValue,
    tmp: StackValue,
}

fn map<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let key_fn = parameters.remove(0);
    let mut zelf = zelf?;

    // First we need to prepare a state with all the mapped keys
    let vec = zelf.as_mut(heap).as_mut_vec()?;

    let len = vec.len();
    resources.increase_gas_usage(len as u64 * 10)?;

    let Some(first) = vec.first().cloned() else {
        return Ok(SysCallResult::Return(zelf))
    };

    fn map_callback<M>(
        state: CallbackState,
        params: FnParams,
        heap: &mut Heap,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut state = state
            .downcast::<MapState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;

        let key = params
            .get(0)
            .ok_or(EnvironmentError::InvalidCallbackParameters)?
            .clone();

        let key = key.into_owned(heap)?;
        let key = heap.insert(key)?;
        let tmp = state.tmp.as_mut_vec(heap)?;
        tmp.push(key);

        state.index += 1;

        let Some(next) = state.ptr
            .as_mut_vec(heap)?
            .get(state.index)
            .cloned() else {
                return Ok(SysCallResult::Return(state.tmp))
            };

        Ok(SysCallResult::ExecuteAndCallback {
            ptr: state.closure_ptr.clone(),
            params: vec![next.into()].into(),
            state,
            callback_params_len: 1,
            callback: CallbackType::Sync(map_callback),
        })
    }

    Ok(SysCallResult::ExecuteAndCallback {
        ptr: key_fn.clone(),
        params: vec![first.into()].into(),
        state: Box::new(MapState {
            closure_ptr: key_fn,
            index: 0,
            ptr: zelf.into(),
            tmp: RuntimeValue::Object(Vec::new()).into(),
        }),
        callback_params_len: 1,
        callback: CallbackType::Sync(map_callback),
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
    Bytes(Vec<u8>),
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
            OrdKey::Bytes(_) => OrdKeyKind::Bytes,
        }
    }

    fn from_value(p: RuntimeValue) -> Result<Self, EnvironmentError> {
        match p {
            RuntimeValue::Primitive(Primitive::Integer(PrimitiveInteger::U8(v))) => Ok(OrdKey::U8(v)),
            RuntimeValue::Primitive(Primitive::Integer(PrimitiveInteger::U16(v))) => Ok(OrdKey::U16(v)),
            RuntimeValue::Primitive(Primitive::Integer(PrimitiveInteger::U32(v))) => Ok(OrdKey::U32(v)),
            RuntimeValue::Primitive(Primitive::Integer(PrimitiveInteger::U64(v))) => Ok(OrdKey::U64(v)),
            RuntimeValue::Primitive(Primitive::Integer(PrimitiveInteger::U128(v))) => Ok(OrdKey::U128(v)),
            RuntimeValue::Primitive(Primitive::Integer(PrimitiveInteger::U256(v))) => Ok(OrdKey::U256(v)),
            RuntimeValue::Bytes(v) => Ok(OrdKey::Bytes(v)),
            _ => Err(EnvironmentError::Static("only supports primitive types that implement Ord")),
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
    Bytes,
}

fn sort_by_key<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let (heap, resources) = context.split_heap();
    let ascending = parameters.remove(1).as_bool(heap)?;
    let key_fn = parameters.remove(0);
    let mut zelf = zelf?;

    // First we need to prepare a state with all the mapped keys
    let vec = zelf.as_mut(heap).as_mut_vec()?;

    let Some(first) = vec.first().cloned() else {
        return Ok(SysCallResult::None)
    };

    // Len cannot be zero as we checked for first above
    let len = vec.len() as u64;
    // n × (log₂n + 5) without float
    let log2n = len.ilog2() as u64;
    resources.increase_gas_usage(len * (log2n + 5))?;

    fn sort_by_key_callback<M>(
        state: CallbackState,
        params: FnParams,
        heap: &mut Heap,
    ) -> Result<SysCallResult<M>, EnvironmentError> {
        let mut state = state
            .downcast::<SortByKeyState>()
            .map_err(|_| EnvironmentError::InvalidCallbackState)?;

        let key = params
            .get(0)
            .ok_or(EnvironmentError::InvalidCallbackParameters)?
            .clone();

        let mapped_key = OrdKey::from_value(key.into_owned(heap)?)?;

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
            .as_mut_vec(heap)?
            .get(state.index)
            .cloned();
        match next {
            Some(next) => {
                Ok(SysCallResult::ExecuteAndCallback {
                    ptr: state.closure_ptr.clone(),
                    params: vec![next.into()].into(),
                    state,
                    callback_params_len: 1,
                    callback: CallbackType::Sync(sort_by_key_callback),
                })
            },
            None => {
                // Cache is built, now we can sort the array based on it
                let array = state.ptr
                    .as_mut_vec(heap)?;

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
                    array[new_index] = original_array[original_index].clone();
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
        callback: CallbackType::Sync(sort_by_key_callback),
    })
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_ord_key_ordering() {
        use super::OrdKey;

        let key1 = OrdKey::U32(10);
        let key2 = OrdKey::U32(20);
        let key3 = OrdKey::U64(10);

        assert!(key1 < key2);
        assert!(key2 > key1);
        assert!(key1 != key3);

        let key4 = OrdKey::Bytes(vec![1, 2, 3]);
        let key5 = OrdKey::Bytes(vec![1, 2, 4]);
        assert!(key4 < key5);
    }
}
