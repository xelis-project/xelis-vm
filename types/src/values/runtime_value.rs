use std::{
    borrow::Cow,
    fmt,
    mem
};
use indexmap::IndexMap;
use crate::{RuntimeOpaqueWrapper, OpaqueValue,
    opaque::OpaqueWrapper,
    Opaque,
    Type,
    U256,
    Heap,
    OwnedValue,
    ValuePointer,
};
use super::{ Primitive, PrimitiveInteger, PrimitiveRange, ValueError};


pub enum Either<L, R> {
    Left(L),
    Right(R),
}

// Runtime node. Child handles are resolved exclusively through the VM's heap.
#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Primitive(Primitive),
    Opaque(RuntimeOpaqueWrapper),
    Bytes(Vec<u8>),
    Object(Vec<ValuePointer>),
    // Map cannot be used as a key in another map
    // Key must be immutable also!
    Map(Box<IndexMap<OwnedValue, ValuePointer>>),
}

impl Default for RuntimeValue {
    fn default() -> Self {
        Self::Primitive(Default::default())
    }
}

impl From<Primitive> for RuntimeValue {
    fn from(value: Primitive) -> Self {
        Self::Primitive(value)
    }
}

impl<T: Opaque> From<T> for RuntimeValue {
    fn from(value: T) -> Self {
        Self::Opaque(value.into())
    }
}

impl From<OpaqueWrapper> for RuntimeValue {
    fn from(value: OpaqueWrapper) -> Self {
        Self::Opaque(value.into())
    }
}

impl RuntimeValue {
    // Check if our type contains any sub values
    // that could be used to get a pointer from
    pub fn has_sub_values(&self) -> bool {
        match self {
            Self::Object(_)
            | Self::Map(_) => true,
            _ => false,
        }
    }

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
    pub fn as_map(&self) -> Result<&IndexMap<OwnedValue, ValuePointer>, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedMap)
        }
    }

    #[inline]
    pub fn as_mut_map(&mut self) -> Result<&mut IndexMap<OwnedValue, ValuePointer>, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedMap),
        }
    }

    #[inline]
    pub fn to_map<'a>(&'a mut self) -> Result<IndexMap<OwnedValue, ValuePointer>, ValueError> {
        match mem::take(self) {
            Self::Map(n) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Map(Box::new(Type::T(Some(0))), Box::new(Type::T(Some(1))))))
        }
    }

    // Check if the value is a function pointer (primitive u16 or object with 3 elements)
    // If it's a primitive u16, return the function id.
    // If it's an object with 3 elements, return the cell array (chunk id u16, is syscall bool, from chunk u16)
    #[inline]
    pub fn as_fn_ptr<'a>(&'a self) -> Result<Either<u16, &'a Vec<ValuePointer>>, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U16(id))) => Ok(Either::Left(*id)),
            Self::Object(values) if values.len() == 3 => Ok(Either::Right(values)),
            _ => Err(ValueError::ExpectedFnType)
        }
    }

    #[inline]
    pub fn as_vec<'a>(&'a self) -> Result<&'a Vec<ValuePointer>, ValueError> {
        match self {
            Self::Object(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::T(None)))))
        }
    }

    #[inline]
    pub fn as_mut_vec<'a>(&'a mut self) -> Result<&'a mut Vec<ValuePointer>, ValueError> {
        match self {
            Self::Object(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::T(None)))))
        }
    }

    #[inline]
    pub fn to_vec<'a>(&'a mut self) -> Result<Vec<ValuePointer>, ValueError> {
        match mem::take(self) {
            Self::Object(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::T(None)))))
        }
    }

    #[inline]
    pub fn into_vec(self) -> Result<Vec<ValuePointer>, ValueError> {
        match self {
            Self::Object(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::T(None)))))
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
    pub fn to_bytes<'a>(&'a mut self) -> Result<Vec<u8>, ValueError> {
        match mem::take(self) {
            Self::Bytes(bytes) => Ok(bytes),
            _ => Err(ValueError::ExpectedBytes)
        }
    }

    #[inline]
    pub fn take_as_optional(&mut self) -> Result<Option<Self>, ValueError> {
        match self {
            Self::Primitive(Primitive::Null) => Ok(None),
            v => {
                let value = std::mem::take(v);
                Ok(Some(value))
            }
        }
    }

    #[inline]
    pub fn to_u8(&self) -> Result<u8, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U8(n))) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U8))
        }
    }

    #[inline]
    pub fn to_u16(&self) -> Result<u16, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U16(n))) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U16))
        }
    }

    #[inline]
    pub fn to_u32(&self) -> Result<u32, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U32(n))) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U32))
        }
    }

    #[inline]
    pub fn to_u64(&self) -> Result<u64, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U64(n))) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U64))
        }
    }

    #[inline]
    pub fn to_u128(&self) -> Result<u128, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U128(n))) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U128))
        }
    }

    #[inline]
    pub fn to_u256(&self) -> Result<U256, ValueError> {
        match self {
            Self::Primitive(Primitive::Integer(PrimitiveInteger::U256(n))) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U256))
        }
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
    pub fn as_opaque(&self) -> Result<&RuntimeOpaqueWrapper, ValueError> {
        match self {
            Self::Opaque(opaque) => Ok(opaque),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_opaque_mut(&mut self) -> Result<&mut RuntimeOpaqueWrapper, ValueError> {
        match self {
            Self::Opaque(opaque) => Ok(opaque),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_opaque_type<T: OpaqueValue>(&self) -> Result<&T, ValueError> {
        match self {
            Self::Opaque(opaque) => opaque.as_ref::<T>(),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_opaque_type_mut<T: OpaqueValue>(&mut self) -> Result<&mut T, ValueError> {
        match self {
            Self::Opaque(opaque) => opaque.as_mut::<T>(),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn into_opaque_type<T: OpaqueValue>(self) -> Result<T, ValueError> {
        match self {
            Self::Opaque(opaque) => opaque.into_inner::<T>(),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_enum<'a>(&'a self, heap: &Heap) -> Result<(u8, &'a [ValuePointer]), ValueError> {
        // the enum variant id is injected as first element
        let values = self.as_vec()?;
        if values.is_empty() {
            return Err(ValueError::ExpectedEnumWithVariantId)
        }

        Ok((values[0].as_ref(heap).as_u8()?, &values[1..]))
    }

    #[inline]
    pub fn as_mut_enum<'a>(&'a mut self, heap: &Heap) -> Result<(u8, &'a mut [ValuePointer]), ValueError> {
        // the enum variant id is injected as first element
        let values = self.as_mut_vec()?;
        if values.is_empty() {
            return Err(ValueError::ExpectedEnumWithVariantId)
        }

        Ok((values[0].as_ref(heap).as_u8()?, &mut values[1..]))
    }

    #[inline]
    pub fn to_enum(&mut self, heap: &Heap) -> Result<(u8, Vec<ValuePointer>), ValueError> {
        // the enum variant id is injected as first element
        let values = self.to_vec()?;
        if values.is_empty() {
            return Err(ValueError::ExpectedEnumWithVariantId)
        }

        Ok((values[0].as_ref(heap).as_u8()?, values[1..].into()))
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
        Ok(match self {
            Self::Primitive(v) => v.increment()?,
            _ => return Err(ValueError::OperationNotNumberType)
        })
    }

    // Decrement the value
    pub fn decrement(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Self::Primitive(v) => v.decrement()?,
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
                if self.is_null() {
                    return Ok(Default::default())
                } else {
                    return self.checked_cast_to_primitive_type(inner)
                }
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

}

impl From<PrimitiveInteger> for RuntimeValue {
    fn from(value: PrimitiveInteger) -> Self {
        Primitive::Integer(value).into()
    }
}

enum Node<'a> {
    Runtime(&'a RuntimeValue),
    Owned(&'a OwnedValue),
}

impl RuntimeValue {
    pub fn equals(&self, other: &Self, heap: &Heap) -> bool {
        match (self, other) {
            (Self::Primitive(left), Self::Primitive(right)) => return left == right,
            (Self::Bytes(left), Self::Bytes(right)) => return left == right,
            _ => {}
        }
        let mut work = vec![(self, other)];
        while let Some((left, right)) = work.pop() {
            match (left, right) {
                (Self::Primitive(a), Self::Primitive(b)) if a == b => {}
                (Self::Opaque(a), Self::Opaque(b)) if a == b => {}
                (Self::Bytes(a), Self::Bytes(b)) if a == b => {}
                (Self::Object(a), Self::Object(b)) if a.len() == b.len() => {
                    work.extend(a.iter().zip(b).map(|(a, b)| (a.as_ref(heap), b.as_ref(heap))));
                }
                (Self::Map(a), Self::Map(b)) if a.len() == b.len() => {
                    for ((ak, av), (bk, bv)) in a.iter().zip(b.iter()) {
                        if ak != bk {
                            return false;
                        }
                        work.push((av.as_ref(heap), bv.as_ref(heap)));
                    }
                }
                _ => return false,
            }
        }
        true
    }

    #[inline]
    pub fn calculate_memory_usage(&self, heap: &Heap, max: usize) -> Result<usize, ValueError> {
        if let Self::Primitive(value) = self {
            return Ok(value.get_memory_usage());
        }
        if matches!(self, Self::Opaque(_)) {
            return Ok(8);
        }
        self.calculate_nested_memory_usage(heap, max)
    }

    fn calculate_nested_memory_usage(&self, heap: &Heap, max: usize) -> Result<usize, ValueError> {
        let mut memory = 0;
        let mut work = vec![Node::Runtime(self)];
        while let Some(node) = work.pop() {
            if memory > max {
                return Err(ValueError::MaxMemoryReached(memory, max));
            }
            match node {
                Node::Runtime(Self::Primitive(value)) | Node::Owned(OwnedValue::Primitive(value)) => memory += 1 + value.get_memory_usage(),
                Node::Runtime(Self::Opaque(_)) | Node::Owned(OwnedValue::Opaque(_)) => memory += 9,
                Node::Runtime(Self::Bytes(value)) | Node::Owned(OwnedValue::Bytes(value)) => memory += 32 + value.len(),
                Node::Runtime(Self::Object(children)) => {
                    memory += 32;
                    work.extend(children.iter().map(|id| Node::Runtime(id.as_ref(heap))));
                }
                Node::Runtime(Self::Map(children)) => {
                    memory += 64;
                    work.extend(children.iter().flat_map(|(key, id)| [Node::Owned(key), Node::Runtime(id.as_ref(heap))]));
                }
                Node::Owned(OwnedValue::Object(children)) => {
                    memory += 32;
                    work.extend(children.iter().map(Node::Owned));
                }
                Node::Owned(OwnedValue::Map(children)) => {
                    memory += 64;
                    work.extend(children.iter().flat_map(|(key, value)| [Node::Owned(key), Node::Owned(value)]));
                }
            }
        }
        Ok(memory)
    }

    pub fn display<'a>(&'a self, heap: &'a Heap) -> impl fmt::Display + 'a {
        struct Display<'a>(&'a RuntimeValue, &'a Heap);
        impl fmt::Display for Display<'_> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                enum Item<'a> {
                    Value(&'a RuntimeValue),
                    Owned(&'a OwnedValue),
                    Text(&'static str),
                }
                let mut work = vec![Item::Value(self.0)];
                while let Some(item) = work.pop() {
                    match item {
                        Item::Text(text) => f.write_str(text)?,
                        Item::Owned(value) => value.fmt(f)?,
                        Item::Value(value) => match value {
                            RuntimeValue::Primitive(value) => value.fmt(f)?,
                            RuntimeValue::Opaque(value) => value.fmt(f)?,
                            RuntimeValue::Bytes(value) => write!(f, "bytes{:?}", value)?,
                            RuntimeValue::Object(values) => {
                                f.write_str("[")?;
                                work.push(Item::Text("]"));
                                for (index, value) in values.iter().enumerate().rev() {
                                    work.push(Item::Value(self.1.get(value)));
                                    if index > 0 {
                                        work.push(Item::Text(", "));
                                    }
                                }
                            }
                            RuntimeValue::Map(values) => {
                                f.write_str("map{")?;
                                work.push(Item::Text("}"));
                                for (index, (key, value)) in values.iter().enumerate().rev() {
                                    work.push(Item::Value(self.1.get(value)));
                                    work.push(Item::Text(": "));
                                    work.push(Item::Owned(key));
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
        Display(self, heap)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{HeapError, PrimitiveInteger, StackValue};

    #[test]
    fn handles_preserve_identity_and_removed_buffers() {
        let mut heap = unsafe { Heap::new() };
        let bytes = vec![1, 2, 3];
        let buffer = bytes.as_ptr();
        let id = heap.insert(RuntimeValue::Bytes(bytes));
        let mut array = RuntimeValue::Object(vec![id.clone()]);
        let removed = array.as_mut_vec().unwrap().swap_remove(0);
        assert_eq!(id, removed);
        assert_eq!(removed.as_ref(&heap).as_bytes().unwrap().as_ptr(), buffer);
        assert_eq!(std::mem::size_of::<ValuePointer>(), 8);
        let value: StackValue = removed.into();
        assert_eq!(value.estimate_memory_usage(&heap, 0).unwrap(), 8);
    }

    #[test]
    fn exporting_an_owned_byte_buffer_reuses_its_allocation() {
        let mut heap = unsafe { Heap::new() };
        let bytes = vec![1, 2, 3];
        let original = bytes.as_ptr();
        let result = heap.export(StackValue::Owned(RuntimeValue::Bytes(bytes))).unwrap();
        assert_eq!(result.as_bytes().unwrap().as_ptr(), original);
    }

    #[test]
    fn snapshots_allow_shared_children() {
        let mut heap = unsafe { Heap::new() };
        let id = heap.insert(PrimitiveInteger::U64(42).into());
        let root = RuntimeValue::Object(vec![id.clone(), id.clone()]);
        let snapshot = heap.snapshot(&root).unwrap();
        assert_eq!(snapshot.as_vec().unwrap()[0].as_u64().unwrap(), 42);
        assert_eq!(snapshot.as_vec().unwrap()[1].as_u64().unwrap(), 42);
    }

    #[test]
    fn explicit_deep_clone_detaches_nested_slots() {
        let mut heap = unsafe { Heap::new() };
        let shared = heap.insert(RuntimeValue::Bytes(vec![1, 2, 3]));
        let map_child = shared.clone();
        let map = RuntimeValue::Map(Box::new([(OwnedValue::from(7u8), map_child)].into_iter().collect()));
        let map = heap.insert(map);
        let first = shared.clone();
        let original = RuntimeValue::Object(vec![first, map]);
        let copy = original.clone();
        let detached = heap.detach(copy).unwrap();
        let children = detached.as_vec().unwrap();
        let detached_first = children[0].clone();
        let detached_map = children[1].clone();
        assert_ne!(detached_first, shared);
        let nested = detached_map.as_ref(&heap).as_map().unwrap().values().next().unwrap().clone();
        assert_ne!(nested, shared);
        assert_ne!(nested, detached_first);
        *heap.get_mut(&detached_first) = RuntimeValue::Bytes(vec![9]);
        assert_eq!(shared.as_ref(&heap).as_bytes().unwrap(), &[1, 2, 3]);
        assert_eq!(nested.as_ref(&heap).as_bytes().unwrap(), &[1, 2, 3]);
    }

    #[test]
    fn runtime_opaque_export_does_not_traverse_its_children() {
        #[derive(Debug, Clone, Opaque)]
        #[opaque(runtime, children = std::iter::once(&self.0))]
        struct Retained(ValuePointer);
        let mut heap = unsafe { Heap::new() };
        let child = heap.insert(RuntimeValue::default());
        let id = heap.insert(RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(Retained(child))));
        let root = RuntimeValue::Object(vec![id]);
        assert!(matches!(heap.snapshot(&root), Err(ValueError::HeapError(HeapError::NonPortableValue))));

    }

    #[test]
    fn clearing_a_heap_resets_its_storage() {
        let mut heap = unsafe { Heap::new() };
        {
            let id = heap.insert(PrimitiveInteger::U64(1).into());
            assert_eq!(heap.get(&id).as_u64().unwrap(), 1);
        }
        heap.clear();
        assert!(heap.is_empty());
        assert_eq!(heap.len(), 0);
        let id = heap.insert(PrimitiveInteger::U64(2).into());
        assert_eq!(heap.get(&id).as_u64().unwrap(), 2);
    }

    #[test]
    fn heap_growth_keeps_handles_valid() {
        let mut heap = unsafe { Heap::new() };
        let first = heap.insert(PrimitiveInteger::U64(42).into());
        for i in 0..10_000 {
            heap.insert(PrimitiveInteger::U64(i).into());
        }
        assert_eq!(heap.get(&first).as_u64().unwrap(), 42);
        *heap.get_mut(&first) = PrimitiveInteger::U64(99).into();
        assert_eq!(heap.get(&first).as_u64().unwrap(), 99);
    }
}
