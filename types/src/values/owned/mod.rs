#[cfg(test)]
mod tests;

mod serialization;

use std::{
    borrow::Cow,
    fmt,
    hash::{Hash, Hasher},
    mem,
};

use indexmap::IndexMap;
use schemars::JsonSchema;
use serde::Deserialize;

use crate::{Opaque, Type, PrimitiveRange, Either, OpaqueWrapper, PrimitiveInteger, U256, ValueError, Primitive};

use crate::{RuntimeValue, ValuePointer, Heap};
use super::serde_map;

/// An owned value for constants, inputs, and outputs.
///
/// Nested values are also `OwnedValue` (no `ValuePointer` / `UnsafeCell`).
/// `Clone`, `Drop`, `PartialEq`, and `Hash` are all iterative to prevent stack
/// overflow for deeply nested structures. Opaque payloads supply their own
/// implementations of these operations.
#[derive(Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case", tag = "type", content = "value")]
pub enum OwnedValue {
    Opaque(OpaqueWrapper),
    Bytes(Vec<u8>),
    Object(Vec<OwnedValue>),
    #[serde(deserialize_with = "serde_map::deserialize")]
    #[schemars(with = "Vec<(OwnedValue, OwnedValue)>")]
    Map(IndexMap<OwnedValue, OwnedValue>),
    #[serde(untagged)]
    Primitive(Primitive),
}

impl Eq for OwnedValue {}

impl PartialEq for OwnedValue {
    fn eq(&self, other: &Self) -> bool {
        // fast check:
        if mem::discriminant(self) != mem::discriminant(other) {
            return false;
        }

        match (self, other) {
            (Self::Primitive(a), Self::Primitive(b)) => return a == b,
            (Self::Bytes(a), Self::Bytes(b)) => return a == b,
            _ => {}
        }

        let mut stack = vec![(self, other)];
        while let Some((l, r)) = stack.pop() {
            match (l, r) {
                (Self::Primitive(a), Self::Primitive(b)) => {
                    if a != b {
                        return false;
                    }
                }
                (Self::Opaque(a), Self::Opaque(b)) => {
                    if a != b {
                        return false;
                    }
                }
                (Self::Bytes(a), Self::Bytes(b)) => {
                    if a != b {
                        return false;
                    }
                }
                (Self::Object(a), Self::Object(b)) => {
                    if a.len() != b.len() {
                        return false;
                    }
                    stack.extend(a.iter().zip(b.iter()).rev());
                }
                (Self::Map(a), Self::Map(b)) => {
                    if a.len() != b.len() {
                        return false;
                    }
                    stack.extend(
                        a.iter()
                            .zip(b.iter())
                            .rev()
                            .flat_map(|((ka, va), (kb, vb))| [(ka, kb), (va, vb)])
                    );
                }
                _ => return false,
            }
        }

        true
    }
}

impl Hash for OwnedValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Primitive(value) => return value.hash(state),
            Self::Bytes(value) => {
                12u8.hash(state);
                return value.hash(state);
            }
            _ => {}
        }

        let mut stack = vec![self];
        while let Some(cell) = stack.pop() {
            match cell {
                Self::Primitive(v) => v.hash(state),
                Self::Opaque(v) => {
                    10u8.hash(state);
                    v.hash(state);
                },
                Self::Bytes(bytes) => {
                    12u8.hash(state);
                    bytes.hash(state);
                }
                Self::Object(values) => {
                    11u8.hash(state);
                    stack.extend(values.iter().rev());
                }
                Self::Map(map) => {
                    13u8.hash(state);
                    stack.extend(map.iter().rev().flat_map(|(k, v)| [k, v]));
                }
            }
        }
    }
}

impl OwnedValue {
    fn take_map(results: &mut Vec<Self>, len: usize) -> IndexMap<Self, Self> {
        let mut entries = results.drain(results.len() - len * 2..);
        (0..len).map(|_| {
            let key = entries.next().expect("map key");
            let value = entries.next().expect("map value");
            (key, value)
        }).collect()
    }

    /// Copy this value into runtime nodes, allocating descendants in the heap.
    #[inline]
    pub fn to_runtime_value(&self, heap: &mut Heap) -> RuntimeValue {
        match self {
            Self::Primitive(p) => return RuntimeValue::Primitive(p.clone()),
            Self::Opaque(p) => return RuntimeValue::Opaque(p.deep_clone().into()),
            Self::Bytes(b) => return RuntimeValue::Bytes(b.clone()),
            _ => {}
        }

        self.to_nested_runtime_value(heap)
    }

    fn to_nested_runtime_value(&self, heap: &mut Heap) -> RuntimeValue {
        enum Work<'a> {
            Visit(&'a OwnedValue),
            BuildArray(usize),
            BuildMap(Vec<OwnedValue>),
        }

        let mut work: Vec<Work<'_>> = vec![Work::Visit(self)];
        let mut results: Vec<RuntimeValue> = Vec::new();

        while let Some(item) = work.pop() {
            match item {
                Work::BuildArray(len) => {
                    let start = results.len() - len;
                    let values: Vec<ValuePointer> = results.drain(start..).map(|value| heap.insert(value)).collect();
                    results.push(RuntimeValue::Object(values));
                }
                Work::BuildMap(keys) => {
                    let start = results.len() - keys.len();
                    let entries = keys.into_iter().zip(results.drain(start..)).map(|(key, value)| (key, heap.insert(value)))
                        .collect();
                    results.push(RuntimeValue::Map(Box::new(entries)));
                }
                Work::Visit(cell) => match cell {
                    Self::Primitive(p) => results.push(RuntimeValue::Primitive(p.clone())),
                    Self::Opaque(p) => results.push(RuntimeValue::Opaque(p.deep_clone().into())),
                    Self::Bytes(b) => results.push(RuntimeValue::Bytes(b.clone())),
                    Self::Object(values) => {
                        work.push(Work::BuildArray(values.len()));
                        work.extend(values.iter().rev().map(Work::Visit));
                    }
                    Self::Map(map) => {
                        work.push(Work::BuildMap(map.keys().cloned().collect()));
                        work.extend(map.values().rev().map(Work::Visit));
                    }
                }
            }
        }

        debug_assert_eq!(results.len(), 1);
        results.pop().expect("runtime value root")
    }

    // Calculate logical VM memory usage iteratively.
    pub fn calculate_memory_usage(&self, max_memory: usize) -> Result<usize, ValueError> {
        if let Self::Primitive(p) = self {
            return Ok(p.get_memory_usage());
        }

        // Opaque values retain the original primitive accounting: 8 at the root,
        // and 1 + 8 when nested in a container.
        if matches!(self, Self::Opaque(_)) {
            return Ok(8);
        }
        let mut stack: Vec<&OwnedValue> = vec![self];
        let mut memory = 0usize;

        while let Some(cell) = stack.pop() {
            if memory > max_memory {
                return Err(ValueError::MaxMemoryReached(memory, max_memory));
            }

            match cell {
                Self::Opaque(_) => memory += 9,
                Self::Primitive(p) => {
                    memory += 1;
                    memory += p.get_memory_usage();
                }
                Self::Bytes(bytes) => {
                    memory += 32;
                    memory += bytes.len();
                }
                Self::Object(values) => {
                    memory += 32;
                    stack.extend(values.iter());
                }
                Self::Map(map) => {
                    memory += 64;
                    for (k, v) in map.iter() {
                        stack.push(v);
                        stack.push(k);
                    }
                }
            }
        }

        Ok(memory)
    }
}

// Drains direct children of `cell` into `stack`, leaving `cell` with empty/null contents.
// Used by the iterative Drop impl to avoid recursive stack overflow.
fn drain_children(cell: &mut OwnedValue, stack: &mut Vec<OwnedValue>) {
    match cell {
        OwnedValue::Object(values) => stack.extend(mem::take(values)),
        OwnedValue::Map(map) => {
            stack.extend(
                mem::take(map)
                    .into_iter()
                    .flat_map(|(k, v)| [k, v])
            );
        }
        _ => {}
    }
}

// Iterative drop to prevent stack overflow on deep nesting.
impl Drop for OwnedValue {
    fn drop(&mut self) {
        match self {
            Self::Primitive(_) | Self::Opaque(_) | Self::Bytes(_) => return, // no nested heap data to drop
            Self::Object(_) | Self::Map(_) => {}
        }

        let mut stack = Vec::new();
        drain_children(self, &mut stack);
        while let Some(mut child) = stack.pop() {
            drain_children(&mut child, &mut stack);
        }
    }
}

// Iterative deep clone to avoid stack overflow on deeply nested structures.
impl Clone for OwnedValue {
    fn clone(&self) -> Self {
        match self {
            Self::Primitive(p) => return Self::Primitive(p.clone()),
            Self::Opaque(p) => return Self::Opaque(p.deep_clone()),
            Self::Bytes(b) => return Self::Bytes(b.clone()),
            _ => {}
        }

        enum Work<'a> {
            Visit(&'a OwnedValue),
            BuildArray(usize),
            BuildMap(usize),
        }

        let mut work = vec![Work::Visit(self)];
        let mut results = Vec::new();

        while let Some(item) = work.pop() {
            match item {
                Work::Visit(cell) => match cell {
                    Self::Primitive(p) => results.push(Self::Primitive(p.clone())),
                    Self::Opaque(p) => results.push(Self::Opaque(p.deep_clone())),
                    Self::Bytes(b) => results.push(Self::Bytes(b.clone())),
                    Self::Object(values) => {
                        work.push(Work::BuildArray(values.len()));
                        work.extend(values.iter().rev().map(Work::Visit));
                    }
                    Self::Map(map) => {
                        work.push(Work::BuildMap(map.len()));
                        for (k, v) in map.iter().rev() {
                            work.push(Work::Visit(v));
                            work.push(Work::Visit(k));
                        }
                    }
                },
                Work::BuildArray(len) => {
                    let start = results.len() - len;
                    let values = results.split_off(start);
                    results.push(Self::Object(values));
                }
                Work::BuildMap(len) => {
                    let map = Self::take_map(&mut results, len);
                    results.push(Self::Map(map));
                }
            }
        }

        debug_assert_eq!(results.len(), 1);
        results.pop().expect("owned value root")
    }
}

impl Default for OwnedValue {
    fn default() -> Self {
        Self::Primitive(Default::default())
    }
}

impl From<Primitive> for OwnedValue {
    fn from(p: Primitive) -> Self {
        Self::Primitive(p)
    }
}

impl fmt::Display for OwnedValue {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        enum Item<'a> {
            Value(&'a OwnedValue),
            Text(&'static str),
        }

        let mut work = vec![Item::Value(self)];
        while let Some(item) = work.pop() {
            match item {
                Item::Text(text) => formatter.write_str(text)?,
                Item::Value(value) => match value {
                    Self::Primitive(value) => value.fmt(formatter)?,
                    Self::Opaque(value) => value.fmt(formatter)?,
                    Self::Bytes(bytes) => write!(formatter, "bytes{:?}", bytes)?,
                    Self::Object(values) => {
                        formatter.write_str("[")?;
                        work.push(Item::Text("]"));
                        for (index, value) in values.iter().enumerate().rev() {
                            work.push(Item::Value(value));
                            if index > 0 {
                                work.push(Item::Text(", "));
                            }
                        }
                    }
                    Self::Map(map) => {
                        formatter.write_str("map{")?;
                        work.push(Item::Text("}"));
                        for (index, (key, value)) in map.iter().enumerate().rev() {
                            work.push(Item::Value(value));
                            work.push(Item::Text(": "));
                            work.push(Item::Value(key));
                            if index > 0 {
                                work.push(Item::Text(", "));
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }
}

// Allow using u8/u16/etc directly
macro_rules! impl_from_primitive_type {
    ($($t:ty => $variant:ident),*) => {
        $(impl From<$t> for OwnedValue {
            fn from(v: $t) -> Self {
                Self::Primitive(Primitive::Integer(PrimitiveInteger::$variant(v)))
            }
        })*
    }
}

impl_from_primitive_type!(
    u8 => U8,
    u16 => U16,
    u32 => U32,
    u64 => U64,
    u128 => U128
);

impl From<U256> for OwnedValue {
    fn from(v: U256) -> Self {
        Self::Primitive(Primitive::Integer(PrimitiveInteger::U256(v)))
    }
}

impl From<String> for OwnedValue {
    fn from(s: String) -> Self {
        Self::Primitive(Primitive::String(s))
    }
}

impl From<PrimitiveInteger> for OwnedValue {
    fn from(value: PrimitiveInteger) -> Self {
        Primitive::Integer(value).into()
    }
}

impl From<bool> for OwnedValue {
    fn from(value: bool) -> Self {
        Self::Primitive(Primitive::Boolean(value))
    }
}


impl OwnedValue {
    #[inline]
    pub fn is_null(&self) -> bool {
        match &self {
            Self::Primitive(Primitive::Null) => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        match &self {
            Self::Primitive(Primitive::String(_)) => true,
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
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U8(n))) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U8))
        }
    }

    #[inline]
    pub fn as_u16(&self) -> Result<u16, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U16(n))) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U16))
        }
    }

    #[inline]
    pub fn as_u32(&self) -> Result<u32, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U32(n))) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U32))
        }
    }

    #[inline]
    pub fn as_u64(&self) -> Result<u64, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U64(n))) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U64))
        }
    }

    #[inline]
    pub fn as_u128(&self) -> Result<u128, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U128(n))) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U128))
        }
    }

    #[inline]
    pub fn as_u256(&self) -> Result<U256, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U256(n))) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U256))
        }
    }

    #[inline]
    pub fn as_string(&self) -> Result<&str, ValueError> {
        match self {
            Self::Primitive(Primitive::String(n)) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::String))
        }
    }

    #[inline]
    pub fn as_bool(&self) -> Result<bool, ValueError> {
        match self {
            Self::Primitive(Primitive::Boolean(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Bool))
        }
    }

    #[inline]
    pub fn as_map(&self) -> Result<&IndexMap<OwnedValue, OwnedValue>, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedMap)
        }
    }

    #[inline]
    pub fn as_mut_map(&mut self) -> Result<&mut IndexMap<OwnedValue, OwnedValue>, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedMap),
        }
    }

    #[inline]
    pub fn to_map(&mut self) -> Result<IndexMap<OwnedValue, OwnedValue>, ValueError> {
        match self {
            Self::Map(n) => Ok(mem::take(n)),
            _ => Err(ValueError::ExpectedValueOfType(Type::Map(Box::new(Type::T(Some(0))), Box::new(Type::T(Some(1))))))
        }
    }

    // Check if the value is a function pointer (primitive u16 or object with 3 elements)
    // If it's a primitive u16, return the function id.
    // If it's an object with 3 elements, return the cell array (chunk id u16, is syscall bool, from chunk u16)
    #[inline]
    pub fn as_fn_ptr(&self) -> Result<Either<u16, &Vec<OwnedValue>>, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U16(id))) => Ok(Either::Left(*id)),
            Self::Object(values) if values.len() == 3 => Ok(Either::Right(values)),
            _ => Err(ValueError::ExpectedFnType)
        }
    }

    #[inline]
    pub fn as_vec(&self) -> Result<&Vec<OwnedValue>, ValueError> {
        match self {
            Self::Object(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::T(None)))))
        }
    }

    #[inline]
    pub fn as_mut_vec(&mut self) -> Result<&mut Vec<OwnedValue>, ValueError> {
        match self {
            Self::Object(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::T(None)))))
        }
    }

    #[inline]
    pub fn to_vec(&mut self) -> Result<Vec<OwnedValue>, ValueError> {
        match self {
            Self::Object(n) => Ok(mem::take(n)),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::T(None)))))
        }
    }

    #[inline]
    pub fn into_vec(mut self) -> Result<Vec<OwnedValue>, ValueError> {
        self.to_vec()
    }

    #[inline]
    pub fn as_bytes(&self) -> Result<&Vec<u8>, ValueError> {
        match self {
            Self::Bytes(bytes) => Ok(bytes),
            _ => Err(ValueError::ExpectedBytes)
        }
    }


    #[inline]
    pub fn as_bytes_mut(&mut self) -> Result<&mut Vec<u8>, ValueError> {
        match self {
            Self::Bytes(bytes) => Ok(bytes),
            _ => Err(ValueError::ExpectedBytes)
        }
    }

    #[inline]
    pub fn to_bytes(&mut self) -> Result<Vec<u8>, ValueError> {
        match self {
            Self::Bytes(bytes) => Ok(mem::take(bytes)),
            _ => Err(ValueError::ExpectedBytes)
        }
    }

    #[inline]
    pub fn take_as_optional(&mut self) -> Result<Option<Self>, ValueError> {
        match self {
            Self::Primitive(Primitive::Null) => Ok(None),
            v => {
                let value = mem::take(v);
                Ok(Some(value))
            }
        }
    }

    #[inline]
    pub fn to_u8(&self) -> Result<u8, ValueError> {
        self.as_u8()
    }

    #[inline]
    pub fn to_u16(&self) -> Result<u16, ValueError> {
        self.as_u16()
    }

    #[inline]
    pub fn to_u32(&self) -> Result<u32, ValueError> {
        self.as_u32()
    }

    #[inline]
    pub fn to_u64(&self) -> Result<u64, ValueError> {
        self.as_u64()
    }

    #[inline]
    pub fn to_u128(&self) -> Result<u128, ValueError> {
        self.as_u128()
    }

    #[inline]
    pub fn to_u256(&self) -> Result<U256, ValueError> {
        self.as_u256()
    }

    #[inline]
    pub fn to_bool(&self) -> Result<bool, ValueError> {
        match self {
            Self::Primitive(Primitive::Boolean(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Bool))
        }
    }

    #[inline]
    pub fn into_string(&mut self) -> Result<String, ValueError> {
        match self {
            Self::Primitive(Primitive::String(v)) => Ok(mem::take(v)),
            _ => Err(ValueError::ExpectedValueOfType(Type::Bool))
        }
    }

    #[inline]
    pub fn as_opaque(&self) -> Result<&OpaqueWrapper, ValueError> {
        match self {
            Self::Opaque(opaque) => Ok(opaque),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_opaque_mut(&mut self) -> Result<&mut OpaqueWrapper, ValueError> {
        match self {
            Self::Opaque(opaque) => Ok(opaque),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_opaque_type<T: Opaque>(&self) -> Result<&T, ValueError> {
        match self {
            Self::Opaque(opaque) => opaque.as_ref::<T>(),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_opaque_type_mut<T: Opaque>(&mut self) -> Result<&mut T, ValueError> {
        match self {
            Self::Opaque(opaque) => opaque.as_mut::<T>(),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn into_opaque_type<T: Opaque>(self) -> Result<T, ValueError> {
        match &self {
            Self::Opaque(opaque) => opaque.clone().into_inner::<T>(),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_enum(&self) -> Result<(u8, &[OwnedValue]), ValueError> {
        // the enum variant id is injected as first element
        let (variant, fields) = self.as_vec()?.split_first()
            .ok_or_else(|| ValueError::ExpectedEnumWithVariantId)?;
        Ok((variant.as_u8()?, fields))
    }

    #[inline]
    pub fn as_mut_enum(&mut self) -> Result<(u8, &mut [OwnedValue]), ValueError> {
        // the enum variant id is injected as first element
        let (variant, fields) = self.as_mut_vec()?.split_first_mut()
            .ok_or_else(|| ValueError::ExpectedEnumWithVariantId)?;
        Ok((variant.as_u8()?, fields))
    }

    #[inline]
    pub fn to_enum(&mut self) -> Result<(u8, Vec<OwnedValue>), ValueError> {
        // the enum variant id is injected as first element
        let mut values = self.to_vec()?;
        if values.is_empty() {
            return Err(ValueError::ExpectedEnumWithVariantId)
        }

        let variant = values.remove(0).as_u8()?;
        Ok((variant, values))
    }

    #[inline]
    pub fn as_range(&self) -> Result<&PrimitiveRange, ValueError> {
        self.as_value().and_then(Primitive::as_range)
    }

    #[inline]
    pub fn to_range(&mut self) -> Result<PrimitiveRange, ValueError> {
        self.into_value().and_then(Primitive::to_range)
    }

    // Check if the value is a number
    #[inline]
    pub fn is_number(&self) -> bool {
        match self {
            Self::Primitive(v) => v.is_number(),
            _ => false
        }
    }

    // Increment the value
    pub fn increment(&mut self) -> Result<(), ValueError> {
        match self {
            Self::Primitive(value) => value.increment(),
            _ => Err(ValueError::OperationNotNumberType),
        }
    }

    // Decrement the value
    pub fn decrement(&mut self) -> Result<(), ValueError> {
        match self {
            Self::Primitive(value) => value.decrement(),
            _ => Err(ValueError::OperationNotNumberType),
        }
    }

    // Cast value to string
    #[inline]
    pub fn cast_to_string(&mut self) -> Result<String, ValueError> {
        self.into_value().and_then(Primitive::cast_to_string)
    }

    // Transform a value to a string
    #[inline]
    pub fn as_string_formatted(&self) -> Result<Cow<'_, str>, ValueError> {
        match self {
            Self::Primitive(v) => v.as_string_formatted(),
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
            Type::U8 => self.checked_cast_to_u8().map(|value| Primitive::Integer(PrimitiveInteger::U8(value))),
            Type::U16 => self.checked_cast_to_u16().map(|value| Primitive::Integer(PrimitiveInteger::U16(value))),
            Type::U32 => self.checked_cast_to_u32().map(|value| Primitive::Integer(PrimitiveInteger::U32(value))),
            Type::U64 => self.checked_cast_to_u64().map(|value| Primitive::Integer(PrimitiveInteger::U64(value))),
            Type::U128 => self.checked_cast_to_u128().map(|value| Primitive::Integer(PrimitiveInteger::U128(value))),
            Type::U256 => self.checked_cast_to_u256().map(|value| Primitive::Integer(PrimitiveInteger::U256(value))),
            Type::String => self.cast_to_string().map(Primitive::String),
            Type::Bool => self.cast_to_bool().map(Primitive::Boolean),
            Type::Optional(inner) => {
                return if self.is_null() {
                    Ok(Self::default())
                } else {
                    self.checked_cast_to_primitive_type(inner)
                };
            },
            Type::Range(inner) => {
                self.to_range()?.checked_cast_to_primitive_type(inner)
                    .map(|range| Primitive::Range(Box::new(range)))
            },
            _ => Err(ValueError::InvalidCastType(expected.clone()))
        }.map(Self::Primitive)
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
            Self::Primitive(v) => Ok(v),
            _ => Err(ValueError::InvalidPrimitiveType)
        }
    }

    #[inline(always)]
    pub fn into_value(&mut self) -> Result<Primitive, ValueError> {
        match self {
            Self::Primitive(v) => Ok(mem::take(v)),
            _ => Err(ValueError::InvalidPrimitiveType)
        }
    }


    pub fn is_hashable(&self) -> bool {
        if matches!(self, Self::Primitive(_) | Self::Bytes(_)) {
            return true;
        }
        let mut work = vec![self];
        while let Some(value) = work.pop() {
            match value {
                Self::Map(_) => return false,
                Self::Opaque(value) if !value.is_hashable() => return false,
                Self::Object(children) => work.extend(children),
                _ => {}
            }
        }
        true
    }

}
impl From<OpaqueWrapper> for OwnedValue {
    fn from(value: OpaqueWrapper) -> Self {
        Self::Opaque(value)
    }
}
impl<T: Opaque> From<T> for OwnedValue {
    fn from(value: T) -> Self {
        Self::Opaque(value.into())
    }
}
