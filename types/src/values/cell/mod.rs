mod stack_value;
mod serde_map;
mod pointer;

use std::{
    borrow::Cow,
    fmt,
    hash::{Hash, Hasher},
    mem
};
use indexmap::IndexMap;
use itertools::Itertools;
use serde::{Deserialize, Serialize};
use crate::{
    opaque::OpaqueWrapper,
    values::cell::pointer::ValuePointer,
    DefinedType,
    Opaque,
    Type,
    U256
};
use super::{Constant, Primitive, ValueError};

pub use stack_value::*;

pub type CellArray = Vec<ValuePointer>;
pub type CellMap = IndexMap<ValueCell, ValuePointer>;

// Give inner mutability for values with inner types.
// This is NOT thread-safe due to the RefCell usage.
#[derive(Debug, Eq, Serialize, Deserialize)]
#[cfg_attr(not(feature = "infinite-cell-depth"), derive(Clone))]
#[serde(rename_all = "snake_case", tag = "type", content = "value")]
pub enum ValueCell {
    Default(Primitive),
    Bytes(Vec<u8>),
    Object(CellArray),
    // Map cannot be used as a key in another map
    // Key must be immutable also!
    #[serde(
        serialize_with = "serde_map::serialize",
        deserialize_with = "serde_map::deserialize"
    )]
    Map(Box<CellMap>),
}

pub enum ValueCellRef<'a> {
    Owned(ValueCell),
    Ref(&'a ValueCell),
    Pointer(ValuePointer),
}

impl<'a> ValueCellRef<'a> {
    pub fn value(&self) -> &ValueCell {
        match self {
            Self::Owned(v) => v,
            Self::Ref(v) => v,
            Self::Pointer(v) => v.as_ref(),
        }
    }
}

#[cfg(feature = "infinite-cell-depth")]
impl Drop for ValueCell {
    fn drop(&mut self) {
        if matches!(self, Self::Default(_) | Self::Bytes(_)) {
            return;
        }

        let mut stack = Vec::new();
        match self {
            ValueCell::Object(values) => {
                stack.reserve(values.len());
                stack.append(values);
            },
            ValueCell::Map(map) => {
                stack.reserve(map.len() * 2);
                stack.extend(map.drain().flat_map(|(k, v)| [k, v]));
            },
            _ => unreachable!()
        };

        while let Some(mut value) = stack.pop() {
            match &mut value {
                ValueCell::Object(values) => {
                    stack.reserve(values.len());
                    stack.append(values);
                },
                ValueCell::Map(map) => {
                    stack.reserve(map.len() * 2);
                    stack.extend(
                        map.drain()
                            .flat_map(|(k, v)| [k, v])
                    );
                }
                _ => {}
            };
        }
    }
}

impl PartialEq for ValueCell {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Default(a), Self::Default(b)) => a == b,
            (Self::Bytes(a), Self::Bytes(b)) => a == b,
            (Self::Object(a), Self::Object(b)) => a == b,
            (Self::Map(a), Self::Map(b)) => a == b,
            _ => false
        }
    }
}

impl Hash for ValueCell {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Fast path
        if let Self::Default(v) = self {
            v.hash(state);
            return;
        }

        let mut stack = vec![ValueCellRef::Ref(self)];
        while let Some(value) = stack.pop() {
            match value.value() {
                Self::Default(v) => v.hash(state),
                Self::Object(values) => {
                    12u8.hash(state);
                    stack.extend(values.iter().cloned().map(ValueCellRef::Pointer));
                },
                Self::Bytes(values) => {
                    13u8.hash(state);
                    values.hash(state);
                }
                Self::Map(map) => {
                    14u8.hash(state);
                    stack.extend(
                        map.iter()
                            .flat_map(|(k, v)| [ValueCellRef::Owned(k.clone()), ValueCellRef::Pointer(v.clone())])
                    )
                }
            }
        }
    }
}

impl Default for ValueCell {
    fn default() -> Self {
        Self::Default(Default::default())
    }
}

impl From<Primitive> for ValueCell {
    fn from(value: Primitive) -> Self {
        Self::Default(value)
    }
}

impl From<Constant> for ValueCell {
    fn from(value: Constant) -> Self {
        match value {
            Constant::Default(v) => Self::Default(v),
            Constant::Array(values) => Self::Object(values.into_iter().map(|v| v.into()).collect()),
            Constant::Bytes(values) => ValueCell::Bytes(values),
            Constant::Map(map) => Self::Map(Box::new(map.into_iter().map(|(k, v)| (k.into(), v.into())).collect())),
            Constant::Typed(values, ty) => {
                let mut cells = Vec::with_capacity(values.len());

                // We inject the variant id
                if let DefinedType::Enum(ty) = ty {
                    cells.push(Primitive::U8(ty.variant_id()).into());
                }

                cells.extend(values.into_iter().map(|v| v.into()));

                Self::Object(cells)
            },
        }
    }
}

impl ValueCell {
    // Check if our type contains any sub values
    // that could be used to get a pointer from
    pub fn has_sub_values(&self) -> bool {
        match self {
            Self::Object(_)
            | Self::Map(_) => true,
            _ => false,
        }
    }

    // Calculate the depth of the value
    pub fn calculate_depth<'a>(&'a self, max_depth: usize) -> Result<usize, ValueError> {
        // Prevent allocation if the value is a default value
        if matches!(self, Self::Default(_)) {
            return Ok(0);
        }

        let mut stack = vec![(ValueCellRef::Ref(self), 0)];
        let mut biggest_depth = 0;

        while let Some((next, depth)) = stack.pop() {
            if depth > max_depth {
                return Err(ValueError::MaxDepthReached);
            }

            if depth > biggest_depth {
                biggest_depth = depth;
            }

            match next.value() {
                ValueCell::Default(_) => {},
                ValueCell::Bytes(_) => {},
                ValueCell::Object(values) => {
                    let depth = depth + 1;
                    stack.extend(values.iter().cloned().map(|v| (ValueCellRef::Pointer(v), depth)));
                },
                ValueCell::Map(map) => {
                    let depth = depth + 1;
                    stack.extend(
                        map.iter()
                            .flat_map(|(k, v)| [(ValueCellRef::Owned(k.clone()), depth), (ValueCellRef::Pointer(v.clone()), depth)])
                    )
                }
            };
        }

        Ok(0)
    }

    // Calculate the depth of the value
    pub fn calculate_memory_usage<'a>(&'a self, max_memory: usize) -> Result<usize, ValueError> {
        // Prevent allocation if the value is a default value
        if let Self::Default(v) = self {
            return Ok(v.get_memory_usage());
        }

        let mut stack = vec![ValueCellRef::Ref(self)];
        let mut memory = 0;

        while let Some(next) = stack.pop() {
            if memory > max_memory {
                return Err(ValueError::MaxMemoryReached(memory, max_memory));
            }

            match next.value() {
                ValueCell::Default(v) => {
                    memory += 1;
                    memory += v.get_memory_usage();
                },
                ValueCell::Bytes(bytes) => {
                    memory += 32;
                    memory += bytes.len();
                },
                ValueCell::Object(values) => {
                    memory += 32;
                    stack.extend(values.iter().cloned().map(ValueCellRef::Pointer));
                },
                ValueCell::Map(map) => {
                    memory += 64;
                    stack.extend(
                        map.iter()
                            .flat_map(|(k, v)| [ValueCellRef::Owned(k.clone()), ValueCellRef::Pointer(v.clone())])
                    )
                }
            };
        }

        Ok(memory)
    }

    #[inline]
    pub fn is_null(&self) -> bool {
        match &self {
            Self::Default(Primitive::Null) => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        match &self {
            Self::Default(Primitive::String(_)) => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_map(&self) -> bool {
        match &self {
            Self::Map(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn as_u8(&self) -> Result<u8, ValueError> {
        match self {
            Self::Default(Primitive::U8(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U8))
        }
    }

    #[inline]
    pub fn as_u16(&self) -> Result<u16, ValueError> {
        match self {
            Self::Default(Primitive::U16(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U16))
        }
    }

    #[inline]
    pub fn as_u32(&self) -> Result<u32, ValueError> {
        match self {
            Self::Default(Primitive::U32(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U32))
        }
    }

    #[inline]
    pub fn as_u64(&self) -> Result<u64, ValueError> {
        match self {
            Self::Default(Primitive::U64(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U64))
        }
    }

    #[inline]
    pub fn as_u128(&self) -> Result<u128, ValueError> {
        match self {
            Self::Default(Primitive::U128(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U128))
        }
    }

    #[inline]
    pub fn as_u256(&self) -> Result<U256, ValueError> {
        match self {
            Self::Default(Primitive::U256(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U256))
        }
    }

    #[inline]
    pub fn as_string(&self) -> Result<&str, ValueError> {
        match self {
            Self::Default(Primitive::String(n)) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::String))
        }
    }

    #[inline]
    pub fn as_bool(&self) -> Result<bool, ValueError> {
        match self {
            Self::Default(Primitive::Boolean(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Bool))
        }
    }

    #[inline]
    pub fn as_map(&self) -> Result<&CellMap, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedStruct)
        }
    }

    #[inline]
    pub fn as_mut_map(&mut self) -> Result<&mut CellMap, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedStruct),
        }
    }

    #[inline]
    pub fn as_vec<'a>(&'a self) -> Result<&'a CellArray, ValueError> {
        match self {
            Self::Object(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn as_mut_vec<'a>(&'a mut self) -> Result<&'a mut CellArray, ValueError> {
        match self {
            Self::Object(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn to_vec<'a>(&'a mut self) -> Result<CellArray, ValueError> {
        match mem::take(self) {
            Self::Object(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
        }
    }


    #[inline]
    pub fn as_bytes<'a>(&'a self) -> Result<&'a Vec<u8>, ValueError> {
        match self {
            Self::Bytes(bytes) => Ok(bytes),
            _ => Err(ValueError::ExpectedBytes)
        }
    }


    #[inline]
    pub fn as_bytes_mut<'a>(&'a mut self) -> Result<&'a mut Vec<u8>, ValueError> {
        match self {
            Self::Bytes(bytes) => Ok(bytes),
            _ => Err(ValueError::ExpectedBytes)
        }
    }

    #[inline]
    pub fn take_as_optional(&mut self) -> Result<Option<Self>, ValueError> {
        match self {
            Self::Default(Primitive::Null) => Ok(None),
            v => {
                let value = std::mem::take(v);
                Ok(Some(value))
            }
        }
    }

    #[inline]
    pub fn to_u8(&self) -> Result<u8, ValueError> {
        match self {
            Self::Default(Primitive::U8(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U8))
        }
    }

    #[inline]
    pub fn to_u16(&self) -> Result<u16, ValueError> {
        match self {
            Self::Default(Primitive::U16(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U16))
        }
    }

    #[inline]
    pub fn to_u32(&self) -> Result<u32, ValueError> {
        match self {
            Self::Default(Primitive::U32(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U32))
        }
    }

    #[inline]
    pub fn to_u64(&self) -> Result<u64, ValueError> {
        match self {
            Self::Default(Primitive::U64(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U64))
        }
    }

    #[inline]
    pub fn to_u128(&self) -> Result<u128, ValueError> {
        match self {
            Self::Default(Primitive::U128(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U128))
        }
    }

    #[inline]
    pub fn to_u256(&self) -> Result<U256, ValueError> {
        match self {
            Self::Default(Primitive::U256(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U256))
        }
    }

    #[inline]
    pub fn to_bool(&self) -> Result<bool, ValueError> {
        match self {
            Self::Default(Primitive::Boolean(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Bool))
        }
    }

    #[inline]
    pub fn into_string(&mut self) -> Result<String, ValueError> {
        match self {
            Self::Default(Primitive::String(v)) => Ok(mem::take(v)),
            _ => Err(ValueError::ExpectedValueOfType(Type::Bool))
        }
    }

    #[inline]
    pub fn as_opaque(&self) -> Result<&OpaqueWrapper, ValueError> {
        match self {
            Self::Default(Primitive::Opaque(opaque)) => Ok(opaque),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_opaque_mut(&mut self) -> Result<&mut OpaqueWrapper, ValueError> {
        match self {
            Self::Default(Primitive::Opaque(opaque)) => Ok(opaque),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_opaque_type<T: Opaque>(&self) -> Result<&T, ValueError> {
        match self {
            Self::Default(Primitive::Opaque(opaque)) => opaque.as_ref::<T>(),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_opaque_type_mut<T: Opaque>(&mut self) -> Result<&mut T, ValueError> {
        match self {
            Self::Default(Primitive::Opaque(opaque)) => opaque.as_mut::<T>(),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn into_opaque_type<T: Opaque>(&mut self) -> Result<T, ValueError> {
        match mem::take(self) {
            Self::Default(Primitive::Opaque(opaque)) => opaque.into_inner::<T>(),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn is_serializable(&self) -> bool {
        match self {
            Self::Default(Primitive::Opaque(op)) => op.is_serializable(),
            _ => true
        }
    }

    #[inline]
    pub fn as_range(&self) -> Result<(&Primitive, &Primitive), ValueError> {
        self.as_value().and_then(Primitive::as_range)
    }

    #[inline]
    pub fn to_range(&mut self) -> Result<(Primitive, Primitive), ValueError> {
        self.into_value().and_then(Primitive::to_range)
    }

    // Check if the value is a number
    #[inline]
    pub fn is_number(&self) -> bool {
        match self {
            Self::Default(v) => v.is_number(),
            _ => false
        }
    }

    // Increment the value
    pub fn increment(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Self::Default(v) => v.increment()?,
            _ => return Err(ValueError::OperationNotNumberType)
        })
    }

    // Decrement the value
    pub fn decrement(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Self::Default(v) => v.decrement()?,
            _ => return Err(ValueError::OperationNotNumberType)
        })
    }

    // Cast value to string
    #[inline]
    pub fn cast_to_string(&mut self) -> Result<String, ValueError> {
        self.into_value().and_then(Primitive::cast_to_string)
    }

    // Transform a value to a string
    #[inline]
    pub fn as_string_formatted<'a>(&'a self) -> Result<Cow<'a, str>, ValueError> {
        match self {
            Self::Default(v) => v.as_string_formatted(),
            _ => Err(ValueError::ExpectedValueOfType(Type::String))
        }
    }

    // Cast the value to the expected type
    pub fn mut_checked_cast_to_primitive_type(&mut self, expected: &Type) -> Result<(), ValueError> {
        let value = self.checked_cast_to_primitive_type(expected)?;
        *self = value;
        Ok(())
    }

    // Cast without loss in the expected type
    #[inline]
    pub fn checked_cast_to_primitive_type(&mut self, expected: &Type) -> Result<Self, ValueError> {
        match expected {
            Type::U8 => self.checked_cast_to_u8().map(Primitive::U8),
            Type::U16 => self.checked_cast_to_u16().map(Primitive::U16),
            Type::U32 => self.checked_cast_to_u32().map(Primitive::U32),
            Type::U64 => self.checked_cast_to_u64().map(Primitive::U64),
            Type::U128 => self.checked_cast_to_u128().map(Primitive::U128),
            Type::U256 => self.checked_cast_to_u256().map(Primitive::U256),
            Type::String => self.cast_to_string().map(Primitive::String),
            Type::Bool => self.cast_to_bool().map(Primitive::Boolean),
            Type::Optional(inner) => {
                if self.is_null() {
                    return Ok(Default::default())
                } else {
                    return self.checked_cast_to_primitive_type(inner)
                }
            },
            Type::Range(inner) => {
                let (start, end) = self.to_range()?;
                let start = start.checked_cast_to_primitive_type(inner)?;
                let end = end.checked_cast_to_primitive_type(inner)?;
                Ok(Primitive::Range(Box::new((start, end))))
            },
            _ => Err(ValueError::InvalidCastType(expected.clone()))
        }.map(Self::Default)
    }

    // Cast to u8, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u8(&mut self) -> Result<u8, ValueError> {
        self.into_value().and_then(Primitive::checked_cast_to_u8)
    }

    // Cast to u16, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u16(&mut self) -> Result<u16, ValueError> {
        self.into_value().and_then(Primitive::checked_cast_to_u16)
    }

    // Cast to u32, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u32(&mut self) -> Result<u32, ValueError> {
        self.into_value().and_then(Primitive::checked_cast_to_u32)
    }

    // Cast to u64, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u64(&mut self) -> Result<u64, ValueError> {
        self.into_value().and_then(Primitive::checked_cast_to_u64)
    }

    // Cast to u128, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u128(&mut self) -> Result<u128, ValueError> {
        self.into_value().and_then(Primitive::checked_cast_to_u128)
    }

    // Cast to u256, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u256(&mut self) -> Result<U256, ValueError> {
        self.into_value().and_then(Primitive::checked_cast_to_u256)
    }

    // Cast value to bool
    #[inline]
    pub fn cast_to_bool(&mut self) -> Result<bool, ValueError> {
        self.into_value().and_then(Primitive::cast_to_bool)
    }

    // Cast value to u8
    #[inline]
    pub fn cast_to_u8(&mut self) -> Result<u8, ValueError> {
        self.into_value().and_then(Primitive::cast_to_u8)
    }

    // Cast value to u16
    #[inline]
    pub fn cast_to_u16(&mut self) -> Result<u16, ValueError> {
        self.into_value().and_then(Primitive::cast_to_u16)
    }

    // Cast value to u32
    #[inline]
    pub fn cast_to_u32(&mut self) -> Result<u32, ValueError> {
        self.into_value().and_then(Primitive::cast_to_u32)
    }

    // Cast value to u64
    #[inline]
    pub fn cast_to_u64(&mut self) -> Result<u64, ValueError> {
        self.into_value().and_then(Primitive::cast_to_u64)
    }

    // Cast value to u128
    #[inline]
    pub fn cast_to_u128(&mut self) -> Result<u128, ValueError> {
        self.into_value().and_then(Primitive::cast_to_u128)
    }

    // Cast value to u256
    #[inline]
    pub fn cast_to_u256(&mut self) -> Result<U256, ValueError> {
        self.into_value().and_then(Primitive::cast_to_u256)
    }

    #[inline(always)]
    pub fn as_value(&self) -> Result<&Primitive, ValueError> {
        match self {
            Self::Default(v) => Ok(v),
            _ => Err(ValueError::ExpectedValueOfType(Type::Any))
        }
    }

    #[inline(always)]
    pub fn into_value(&mut self) -> Result<Primitive, ValueError> {
        match self {
            Self::Default(v) => Ok(mem::take(v)),
            _ => Err(ValueError::ExpectedValueOfType(Type::Any))
        }
    }

    // Create a clone in a iterative way
    pub fn deep_clone(&self) -> Self {
        match self {
            Self::Bytes(v) => return Self::Bytes(v.clone()),
            Self::Default(v) => return Self::Default(v.clone()),
            _ => {}
        };

        #[derive(Debug)]
        enum QueueItem {
            Primitive(Primitive),
            Array {
                len: usize,
            },
            Map {
                len: usize,
            },
            Bytes(Vec<u8>)
        }

        let mut stack = vec![ValueCellRef::Ref(self)];
        let mut queue = Vec::new();

        // Disassemble
        while let Some(value) = stack.pop() {
            match value.value() {
                Self::Default(v) => queue.push(QueueItem::Primitive(v.clone())),
                Self::Object(values) => {
                    queue.push(QueueItem::Array { len: values.len() });
                    stack.extend(values.iter().cloned().map(ValueCellRef::Pointer));
                },
                Self::Bytes(bytes) => {
                    queue.push(QueueItem::Bytes(bytes.clone()));
                }
                Self::Map(map) => {
                    queue.push(QueueItem::Map { len: map.len() });
                    stack.reserve(map.len() * 2);
                    stack.extend(map.iter().flat_map(|(k, v)| [ValueCellRef::Owned(k.clone()), ValueCellRef::Pointer(v.clone())]));
                }
            }
        };

        let mut stack = Vec::with_capacity(queue.len());
        // Assemble back
        while let Some(item) = queue.pop() {
            match item {
                QueueItem::Primitive(v) => {
                    stack.push(ValueCell::Default(v));
                },
                QueueItem::Array { len } => {
                    let values = stack.split_off(stack.len() - len);
                    stack.push(ValueCell::Object(values.into_iter().map(Into::into).collect()));
                },
                QueueItem::Bytes(bytes) => {
                    stack.push(ValueCell::Bytes(bytes));
                }
                QueueItem::Map { len } => {
                    let map = stack.split_off(stack.len() - len * 2)
                        .into_iter()
                        .tuples()
                        .map(|(k, v)| (k, v.into()))
                        .collect();

                    stack.push(ValueCell::Map(Box::new(map)));
                }
            }
        }

        debug_assert!(stack.len() == 1);
        stack.remove(0)
    }
}

impl fmt::Display for ValueCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Default(v) => write!(f, "{}", v),
            Self::Object(values) => {
                let s: Vec<String> = values.iter().map(|v| format!("{}", v.as_ref())).collect();
                write!(f, "[{}]", s.join(", "))
            },
            Self::Bytes(bytes) => {
                write!(f, "bytes[{:?}]", bytes)
            },
            Self::Map(map) => {
                let s: Vec<String> = map.iter().map(|(k, v)| format!("{}: {}", k, v.as_ref())).collect();
                write!(f, "map{}{}{}", "{", s.join(", "), "}")
            }
        }
    }
}

#[cfg(feature = "infinite-cell-depth")]
impl Clone for ValueCell {
    fn clone(&self) -> Self {
        self.deep_clone()
    }
}

#[cfg(all(feature = "infinite-cell-depth", test))]
mod tests {
    use super::*;

    #[test]
    fn test_drop_and_clone() {
        let v = ValueCell::default();
        drop(v.clone());
        drop(v);

        let v = ValueCell::Object(vec![ValueCell::default()]);
        drop(v.clone());
        drop(v);

        // Create a array with a huge depth of 100000
        let mut v = ValueCell::Object(vec![ValueCell::default()]);
        for _ in 0..100_000 {
            v = ValueCell::Object(vec![v]);
        }

        drop(v.clone());
        drop(v);

        // Create a map with a huge depth of 100000
        let mut v = ValueCell::Map(HashMap::new());
        for _ in 0..100_000 {
            let mut inner_map = HashMap::new();
            inner_map.insert(Primitive::U8(10).into(), v);
            v = ValueCell::Map(inner_map);
        }

        drop(v.clone());
        drop(v);
    }

    #[test]
    fn test_max_depth() {
        let mut map = ValueCell::Map(HashMap::new());
        for _ in 0..100 {
            let mut inner_map = HashMap::new();
            inner_map.insert(Primitive::U8(10).into(), map.into());
            map = ValueCell::Map(inner_map);
        }

        assert!(matches!(map.calculate_depth(100), Ok(100)));
        assert!(matches!(map.calculate_depth(99), Err(ValueError::MaxDepthReached)));
    }

    #[test]
    fn test_std_hash() {
        // Create a map that contains a map that contains a map...
        let mut map = ValueCell::Map(HashMap::new());
        for _ in 0..100_000 {
            let mut inner_map = HashMap::new();
            inner_map.insert(Primitive::U8(10).into(), map.into());
            map = ValueCell::Map(inner_map);
        }

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        map.hash(&mut hasher);
    }
}