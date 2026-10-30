mod stack_value;
mod arena;
mod pointer;
mod heap;
mod heap_error;
pub(crate) mod owned_value;

use std::{
    borrow::Cow,
    mem
};
use indexmap::IndexMap;
use crate::{
    opaque::OpaqueWrapper,
    Opaque,
    Type,
    U256
};
use super::{ Primitive, PrimitiveInteger, PrimitiveRange, ValueError};

pub use stack_value::*;
pub use pointer::*;
pub use heap::*;
pub use heap_error::*;
pub use owned_value::*;

pub enum Either<L, R> {
    Left(L),
    Right(R),
}

// Runtime node. Child handles are resolved exclusively through the VM's heap.
#[derive(Debug)]
pub enum RuntimeValue {
    Primitive(Primitive),
    Opaque(OpaqueWrapper),
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
    fn from(value: T) -> Self { Self::Opaque(value.into()) }
}

impl From<OpaqueWrapper> for RuntimeValue {
    fn from(value: OpaqueWrapper) -> Self { Self::Opaque(value) }
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

    /// Create a clone while keeping the same references for sub values (i.e. shallow clone)
    #[inline(always)]
    pub fn clone_ref(&self) -> Self {
        match self {
            Self::Bytes(v) => return Self::Bytes(v.clone()),
            Self::Primitive(v) => return Self::Primitive(v.clone()),
            Self::Opaque(v) => return Self::Opaque(v.clone()),
            Self::Object(values) => return Self::Object(values.clone()),
            Self::Map(map) => return Self::Map(map.clone()),
        }
    }
}

impl Clone for RuntimeValue {
    fn clone(&self) -> Self {
        self.clone_ref()
    }
}

impl From<PrimitiveInteger> for RuntimeValue {
    fn from(value: PrimitiveInteger) -> Self { Primitive::Integer(value).into() }
}
