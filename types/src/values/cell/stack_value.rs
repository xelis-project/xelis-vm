use std::mem;
use super::{Heap, HeapError, RuntimeValue, ValuePointer};

use crate::{
    PrimitiveInteger,
    PrimitiveRange,
    U256,
    ValueError,
    Opaque,
    OpaqueWrapper,
    Primitive,
    Type
};

// Logical reference size for gas and memory accounting, independent of handle layout.
pub const REFERENCE_SIZE: usize = 8;

// StackValue represent a value on the stack
// It can be either an owned value or a pointer to a value
// Note that the pointer can be nested (pointer to pointer)
// because of the RuntimeValue format (Object containing ValuePointers)
// Resolving a handle borrows the heap. Mutable resolution requires exclusive
// heap access, so cloned handles cannot create overlapping Rust references.
#[derive(Debug, Clone)]
pub enum StackValue {
    // Value is on stack directly
    Owned(RuntimeValue),
    Pointer {
        ptr: ValuePointer,
        depth: usize
    }
}

impl Default for StackValue {
    fn default() -> Self {
        Self::Owned(Default::default())
    }
}

impl StackValue {
    pub fn as_u8<'a>(&'a self, heap: &'a Heap) -> Result<u8, ValueError> { self.as_ref(heap).as_u8() }
    pub fn as_u16<'a>(&'a self, heap: &'a Heap) -> Result<u16, ValueError> { self.as_ref(heap).as_u16() }
    pub fn as_u128<'a>(&'a self, heap: &'a Heap) -> Result<u128, ValueError> { self.as_ref(heap).as_u128() }
    pub fn as_u256<'a>(&'a self, heap: &'a Heap) -> Result<U256, ValueError> { self.as_ref(heap).as_u256() }
    pub fn is_null(&self, heap: &Heap) -> bool { self.as_ref(heap).is_null() }
    pub fn as_string<'a>(&'a self, heap: &'a Heap) -> Result<&'a str, ValueError> { self.as_ref(heap).as_string() }
    pub fn as_bytes<'a>(&'a self, heap: &'a Heap) -> Result<&'a Vec<u8>, ValueError> { self.as_ref(heap).as_bytes() }
    pub fn as_vec<'a>(&'a self, heap: &'a Heap) -> Result<&'a Vec<ValuePointer>, ValueError> { self.as_ref(heap).as_vec() }
    pub fn as_range<'a>(&'a self, heap: &'a Heap) -> Result<&'a PrimitiveRange, ValueError> { self.as_ref(heap).as_range() }
    pub fn as_value<'a>(&'a self, heap: &'a Heap) -> Result<&'a Primitive, ValueError> { self.as_ref(heap).as_value() }
    pub fn as_opaque_type<'a, T: Opaque>(&'a self, heap: &'a Heap) -> Result<&'a T, ValueError> { self.as_ref(heap).as_opaque_type::<T>() }
    pub fn as_opaque_type_mut<'a, T: Opaque>(&'a mut self, heap: &'a mut Heap) -> Result<&'a mut T, ValueError> { self.as_mut(heap).as_opaque_type_mut::<T>() }
    pub fn as_mut_vec<'a>(&'a mut self, heap: &'a mut Heap) -> Result<&'a mut Vec<ValuePointer>, ValueError> { self.as_mut(heap).as_mut_vec() }

    #[inline(always)]
    pub fn as_bool(&self, heap: &Heap) -> Result<bool, ValueError> {
        self.as_ref(heap).as_bool()
    }

    #[inline(always)]
    pub fn as_u32(&self, heap: &Heap) -> Result<u32, ValueError> {
        self.as_ref(heap).as_u32()
    }

    #[inline(always)]
    pub fn as_u64(&self, heap: &Heap) -> Result<u64, ValueError> {
        self.as_ref(heap).as_u64()
    }

    #[inline(always)]
    pub fn estimate_memory_usage(&self, heap: &Heap, max: usize) -> Result<usize, ValueError> {
        match self {
            Self::Owned(v) => v.calculate_memory_usage(heap, max),
            Self::Pointer { .. } => {
                // Otherwise, we consider only the pointer size
                // Logical accounting remains independent of heap storage.
                Ok(REFERENCE_SIZE)
            }
        }
    }

    // Get the sub value at index requested
    pub fn get_at_index(self, heap: &Heap, index: usize) -> Result<StackValue, ValueError> {
        match self {
            Self::Owned(v) => match v {
                RuntimeValue::Object(mut values) => {
                    let len = values.len();
                    if index >= len {
                        return Err(ValueError::OutOfBounds(index, len))
                    }

                    Ok(Self::Pointer { ptr: values.swap_remove(index), depth: 1 })
                },
                RuntimeValue::Bytes(mut bytes) => {
                    let len = bytes.len();
                    if index >= len {
                        return Err(ValueError::OutOfBounds(index, len))
                    }

                    Ok(Self::Owned(RuntimeValue::Primitive(Primitive::Integer(PrimitiveInteger::U8(bytes.swap_remove(index))))))
                },
                _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
            },
            Self::Pointer { ptr, depth } => {
                let cell = ptr.as_ref(heap);

                Ok(match cell {
                    RuntimeValue::Object(values) => {
                        let len = values.len();
                        let at_index = values
                            .get(index)
                            .ok_or_else(|| ValueError::OutOfBounds(index, len))?;

                        Self::Pointer {
                            ptr: at_index.clone(),
                            depth: depth + 1
                        }
                    },
                    RuntimeValue::Bytes(bytes) => {
                        let at_index = bytes.get(index)
                            .copied()
                            .ok_or_else(|| ValueError::OutOfBounds(index, bytes.len()))?;

                        Self::Owned(RuntimeValue::Primitive(Primitive::Integer(PrimitiveInteger::U8(at_index))))
                    },
                    _ => return Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
                })
            }
        }
    }

    #[inline(always)]
    pub fn reference(&mut self, heap: &mut Heap) -> Result<Self, HeapError> {
        Ok(match self {
            Self::Pointer { ptr, depth } => Self::Pointer {
                ptr: ptr.clone(),
                depth: *depth,
            },
            Self::Owned(value) => {
                let pointer = heap.insert_from(value)?;
                *self = Self::Pointer {
                    ptr: pointer.clone(),
                    depth: 0
                };

                Self::Pointer {
                    ptr: pointer,
                    depth: 0
                }
            }
        })
    }

    // Get an owned variant from it
    #[inline(always)]
    pub fn to_owned(&self, heap: &mut Heap) -> Result<Self, HeapError> {
        let value = self.try_as_ref(heap)?.clone_ref();
        Ok(Self::Owned(heap.detach(value)?))
    }

    // Get the value of the path
    // Detach the value from shared heap slots.
    #[inline(always)]
    pub fn into_owned(self, heap: &mut Heap) -> Result<RuntimeValue, HeapError> {
        match self {
            Self::Owned(v) => heap.detach(v),
            Self::Pointer { ptr, .. } => heap.detach(ptr.to_owned(heap)?)
        }
    }

    #[inline(always)]
    pub fn raw_owned(self, heap: &Heap) -> Result<RuntimeValue, HeapError> {
        match self {
            Self::Owned(v) => Ok(v),
            Self::Pointer { ptr, .. } => ptr.raw_owned(heap)
        }
    }

    // Get a mutable reference to the value
    // Exclusive access is enforced by the mutable heap borrow.
    #[inline(always)]
    pub fn as_mut<'a>(&'a mut self, heap: &'a mut Heap) -> &'a mut RuntimeValue {
        match self {
            Self::Owned(v) => v,
            Self::Pointer { ptr, .. } => {
                ptr.as_mut(heap)
            }
        }
    }

    // Take the ownership by stealing the value from the pointer
    // and replace our current pointer as a owned value
    #[inline(always)]
    pub fn take_ownership(&mut self, heap: &mut Heap) {
        if let Self::Pointer { ptr, .. } = self {
            let cell = ptr.as_mut(heap);
            let owned = mem::take(cell);
            *self = owned.into();
        }
    }

    // Make the path owned if the pointer is the same
    #[inline(always)]
    pub fn make_owned_if_same_ptr(&mut self, heap: &mut Heap, other: ValuePointer) -> Result<(), HeapError> {
        if let Self::Pointer { ptr,  .. } = self {
            if *ptr == other {
                let cell = heap.get(*ptr)?;
                *self = heap.detach(cell.clone_ref())?.into();
            }
        }
        Ok(())
    }

    // Transform the StackValue into an Owned variant if its a pointer
    // Do nothing if its a Owned variant already
    #[inline(always)]
    pub fn make_owned(&mut self, heap: &mut Heap) -> Result<bool, HeapError> {
        Ok(if let Self::Pointer { ptr, .. } = self {
            let cell = heap.get(*ptr)?;
            *self = heap.detach(cell.clone_ref())?.into();

            true
        } else {
            false
        })
    }

    // Is it a owned value
    #[inline(always)]
    pub fn is_owned(&self) -> bool {
        matches!(self, Self::Owned(_))
    }

    // Verify if two stack value have the same pointer
    #[inline]
    pub fn ptr_eq(&self, other: &Self) -> bool {
        if let (Self::Pointer { ptr: a, .. }, Self::Pointer { ptr: b, .. }) = (self, other) {
            a.ptr_eq(*b)
        } else {
            false
        }
    }

    pub fn id(&self) -> Option<ValuePointer> {
        match self {
            Self::Owned(_) => None,
            Self::Pointer { ptr, .. } => Some(*ptr),
        }
    }

    pub fn try_as_ref<'a>(&'a self, heap: &'a Heap) -> Result<&'a RuntimeValue, HeapError> {
        match self {
            Self::Owned(value) => Ok(value),
            Self::Pointer { ptr, .. } => heap.get(*ptr),
        }
    }

    pub fn as_ref<'a>(&'a self, heap: &'a Heap) -> &'a RuntimeValue {
        match self {
            Self::Owned(value) => value,
            Self::Pointer { ptr, .. } => ptr.as_ref(heap),
        }
    }

    pub fn into_pointer(self, heap: &mut Heap) -> Result<ValuePointer, HeapError> {
        match self {
            Self::Owned(value) => heap.insert(value),
            Self::Pointer { ptr, .. } => Ok(ptr),
        }
    }

    // Retrieve the depth of the pointer
    #[inline(always)]
    pub fn depth(&self) -> usize {
        match self {
            Self::Owned(_) => 0,
            Self::Pointer { depth, .. } => *depth
        }
    }
}

impl From<RuntimeValue> for StackValue {
    fn from(value: RuntimeValue) -> Self {
        Self::Owned(value)
    }
}

impl From<Primitive> for StackValue {
    fn from(value: Primitive) -> Self {
        Self::Owned(value.into())
    }
}

impl From<ValuePointer> for StackValue {
    fn from(ptr: ValuePointer) -> Self {
        Self::Pointer { ptr, depth: 0 }
    }
}

impl<T: Opaque> From<T> for StackValue {
    fn from(value: T) -> Self {
        Self::Owned(RuntimeValue::Opaque(value.into()))
    }
}

impl From<OpaqueWrapper> for StackValue {
    fn from(value: OpaqueWrapper) -> Self {
        Self::Owned(RuntimeValue::Opaque(value))
    }
}

impl From<PrimitiveInteger> for StackValue {
    fn from(value: PrimitiveInteger) -> Self { Primitive::Integer(value).into() }
}
