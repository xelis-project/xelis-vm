use std::mem;
use crate::{
    Heap, HeapError, RuntimeValue, ValuePointer,
    PrimitiveInteger,
    PrimitiveRange,
    U256,
    ValueError,
    Opaque, OpaqueValue,
    OpaqueWrapper,
    Primitive,
    Type
};

// Logical reference size for gas and memory accounting, independent of handle layout.
pub const REFERENCE_SIZE: usize = 8;

// A stack value owns a runtime node directly or holds an owning heap handle.
// Resolving a handle borrows the heap. Mutable resolution requires exclusive
// heap access, so copied IDs cannot create overlapping Rust references.
// Ordinary Clone retains owning handles, and ordinary Drop releases them.
#[derive(Debug, Clone)]
pub enum StackValue {
    // Value is on stack directly
    Owned(RuntimeValue),
    Pointer {
        ptr: ValuePointer
    }
}

impl Default for StackValue {
    fn default() -> Self {
        Self::Owned(Default::default())
    }
}

impl StackValue {
    /// Move this owner out, leaving null for the caller's cleanup.
    pub fn take(&mut self) -> Self {
        mem::take(self)
    }

    pub fn as_u8<'a>(&'a self, heap: &'a Heap) -> Result<u8, ValueError> {
        self.as_ref(heap).as_u8()
    }

    pub fn as_u16<'a>(&'a self, heap: &'a Heap) -> Result<u16, ValueError> {
        self.as_ref(heap).as_u16()
    }

    pub fn as_u128<'a>(&'a self, heap: &'a Heap) -> Result<u128, ValueError> {
        self.as_ref(heap).as_u128()
    }

    pub fn as_u256<'a>(&'a self, heap: &'a Heap) -> Result<U256, ValueError> {
        self.as_ref(heap).as_u256()
    }

    pub fn is_null(&self, heap: &Heap) -> bool {
        self.as_ref(heap).is_null()
    }

    pub fn as_string<'a>(&'a self, heap: &'a Heap) -> Result<&'a str, ValueError> {
        self.as_ref(heap).as_string()
    }

    pub fn as_bytes<'a>(&'a self, heap: &'a Heap) -> Result<&'a Vec<u8>, ValueError> {
        self.as_ref(heap).as_bytes()
    }

    pub fn as_vec<'a>(&'a self, heap: &'a Heap) -> Result<&'a Vec<ValuePointer>, ValueError> {
        self.as_ref(heap).as_vec()
    }

    pub fn as_range<'a>(&'a self, heap: &'a Heap) -> Result<&'a PrimitiveRange, ValueError> {
        self.as_ref(heap).as_range()
    }

    pub fn as_value<'a>(&'a self, heap: &'a Heap) -> Result<&'a Primitive, ValueError> {
        self.as_ref(heap).as_value()
    }

    pub fn as_opaque_type<'a, T: OpaqueValue>(&'a self, heap: &'a Heap) -> Result<&'a T, ValueError> {
        self.as_ref(heap).as_opaque_type::<T>()
    }

    pub fn as_opaque_type_mut<'a, T: OpaqueValue>(&'a mut self, heap: &'a mut Heap) -> Result<&'a mut T, ValueError> {
        self.as_mut(heap).as_opaque_type_mut::<T>()
    }

    pub fn as_mut_vec<'a>(&'a mut self, heap: &'a mut Heap) -> Result<&'a mut Vec<ValuePointer>, ValueError> {
        self.as_mut(heap).as_mut_vec()
    }

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
    pub fn get_at_index(self, heap: &mut Heap, index: usize) -> Result<StackValue, ValueError> {
        match self {
            Self::Owned(v) => match v {
                RuntimeValue::Object(mut values) => {
                    let len = values.len();
                    if index >= len {
                        return Err(ValueError::OutOfBounds(index, len))
                    }

                    let ptr = values.swap_remove(index);
                    Ok(Self::Pointer { ptr })
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
            Self::Pointer { ptr } => {
                let value = ptr.as_ref(heap);

                let result = match value {
                    RuntimeValue::Object(values) => {
                        let len = values.len();
                        let at_index = values
                            .get(index)
                            .ok_or_else(|| ValueError::OutOfBounds(index, len))?;

                        Self::Pointer {
                            ptr: at_index.clone()
                        }
                    },
                    RuntimeValue::Bytes(bytes) => {
                        let at_index = bytes.get(index)
                            .copied()
                            .ok_or_else(|| ValueError::OutOfBounds(index, bytes.len()))?;

                        Self::Owned(RuntimeValue::Primitive(Primitive::Integer(PrimitiveInteger::U8(at_index))))
                    },
                    _ => return Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
                };
                Ok(result)
            }
        }
    }

    #[inline(always)]
    pub fn reference(&mut self, heap: &mut Heap) -> Self {
        Self::Pointer {
            ptr: self.reference_pointer(heap),
        }
    }

    /// Retain a heap handle, moving a direct value into the heap if necessary.
    #[inline(always)]
    pub fn reference_pointer(&mut self, heap: &mut Heap) -> ValuePointer {
        match self {
            Self::Pointer { ptr } => ptr.clone(),
            Self::Owned(value) => {
                let pointer = heap.insert_from(value);
                *self = Self::Pointer {
                    ptr: pointer.clone()
                };

                pointer
            }
        }
    }

    /// Clone and detach the runtime graph from existing aliases.
    #[inline(always)]
    pub fn to_owned(&self, heap: &mut Heap) -> Result<Self, HeapError> {
        let value = self.as_ref(heap).clone();
        Ok(Self::Owned(heap.detach(value)?))
    }

    /// Consume and detach the runtime graph from existing aliases.
    #[inline(always)]
    pub fn into_owned(self, heap: &mut Heap) -> Result<RuntimeValue, HeapError> {
        let value = self.into_value(heap);
        heap.detach(value)
    }

    /// Consume the wrapper, moving unique payloads or shallow-cloning shared ones.
    /// Child handles stay shared; use into_owned when detachment is required.
    #[inline(always)]
    pub fn into_value(self, heap: &mut Heap) -> RuntimeValue {
        match self {
            Self::Owned(v) => v,
            Self::Pointer { ptr } => heap.take(ptr)
        }
    }

    // Get a mutable reference to the value
    // Exclusive access is enforced by the mutable heap borrow.
    #[inline(always)]
    pub fn as_mut<'a>(&'a mut self, heap: &'a mut Heap) -> &'a mut RuntimeValue {
        match self {
            Self::Owned(v) => v,
            Self::Pointer { ptr } => {
                ptr.as_mut(heap)
            }
        }
    }

    // Transform the StackValue into an Owned variant if its a pointer
    // Do nothing if its a Owned variant already
    #[inline(always)]
    pub fn make_owned(&mut self, heap: &mut Heap) -> Result<bool, HeapError> {
        Ok(if let Self::Pointer { .. } = self {
            let owned = self.to_owned(heap)?;
            *self = owned;

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
            a.ptr_eq(b)
        } else {
            false
        }
    }

    pub fn id(&self) -> Option<&ValuePointer> {
        match self {
            Self::Owned(_) => None,
            Self::Pointer { ptr } => Some(ptr),
        }
    }

    #[inline(always)]
    pub fn as_ref<'a>(&'a self, heap: &'a Heap) -> &'a RuntimeValue {
        match self {
            Self::Owned(value) => value,
            Self::Pointer { ptr } => ptr.as_ref(heap),
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
        Self::Pointer { ptr }
    }
}

impl<T: Opaque> From<T> for StackValue {
    fn from(value: T) -> Self {
        Self::Owned(RuntimeValue::Opaque(value.into()))
    }
}

impl From<OpaqueWrapper> for StackValue {
    fn from(value: OpaqueWrapper) -> Self {
        Self::Owned(RuntimeValue::Opaque(value.into()))
    }
}

impl From<PrimitiveInteger> for StackValue {
    fn from(value: PrimitiveInteger) -> Self {
        Primitive::Integer(value).into()
    }
}
