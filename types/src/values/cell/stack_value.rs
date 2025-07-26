use std::mem;

use crate::{values::ValueError, Constant, Primitive, Type};
use super::ValueCell;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ValuePointer(*mut ValueCell);

impl ValuePointer {
    // WARNING: Put only ValueCell that is managed by one thread only
    pub unsafe fn new(cell: &mut ValueCell) -> Self {
        Self(cell as _)
    }

    #[inline(always)]
    pub unsafe fn as_mut<'a>(self) -> Result<&'a mut ValueCell, ValueError> {
        self.0.as_mut()
            .ok_or(ValueError::InvalidPointer)
    }

    #[inline(always)]
    pub unsafe fn as_ref<'a>(self) -> Result<&'a ValueCell, ValueError> {
        self.0.as_ref()
            .ok_or(ValueError::InvalidPointer)
    }
}

// SAFETY: it is up to the caller to ensure
// a ValuePointer is safe to be Send
unsafe impl Send for ValuePointer {}

#[derive(Debug)]
pub enum StackValue {
    // Value is on stack directly
    Owned(ValueCell),
    Pointer {
        origin: Option<ValuePointer>,
        ptr: ValuePointer,
        depth: usize
    }
}

impl StackValue {
    #[inline(always)]
    pub fn as_bool<'a>(&'a self) -> Result<bool, ValueError> {
        self.as_ref()?.as_bool()
    }

    #[inline(always)]
    pub fn as_u32<'a>(&'a self) -> Result<u32, ValueError> {
        self.as_ref()?.as_u32()
    }

    #[inline(always)]
    pub fn as_u64<'a>(&'a self) -> Result<u64, ValueError> {
        self.as_ref()?.as_u64()
    }

    // Get the sub value at index requested
    pub fn get_at_index(self, index: usize) -> Result<StackValue, ValueError> {
        match self {
            Self::Owned(v) => {
                let at_index = match v {
                    ValueCell::Object(mut values) => {
                        let len = values.len();
                        if index >= len {
                            return Err(ValueError::OutOfBounds(index, len))
                        }

                        values.swap_remove(index)
                    },
                    ValueCell::Bytes(mut bytes) => {
                        let len = bytes.len();
                        if index >= len {
                            return Err(ValueError::OutOfBounds(index, len))
                        }

                        ValueCell::Default(Primitive::U8(bytes.swap_remove(index)))
                    },
                    _ => return Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
                };

                Ok(Self::Owned(at_index))
            },
            Self::Pointer { origin, ptr, depth } => unsafe {
                let cell = ptr.as_mut()?;

                Ok(match cell {
                    ValueCell::Object(values) => {
                        let len = values.len();
                        let at_index = values
                            .get_mut(index)
                            .ok_or_else(|| ValueError::OutOfBounds(index, len))?;

                        Self::Pointer {
                            origin: origin.or(Some(ptr)),
                            ptr: ValuePointer::new(at_index),
                            depth: depth + 1
                        }
                    },
                    ValueCell::Bytes(bytes) => {
                        let at_index = bytes.get(index)
                            .copied()
                            .ok_or_else(|| ValueError::OutOfBounds(index, bytes.len()))?;

                        Self::Owned(ValueCell::Default(Primitive::U8(at_index)))
                    },
                    _ => return Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
                })
            }
        }
    }

    pub fn reference(&mut self) -> Self {
        match self {
            Self::Owned(value) => Self::Pointer {
                origin: None,
                // SAFETY: We actually own the value
                ptr: unsafe { ValuePointer::new(value) },
                depth: 0
            },
            Self::Pointer { origin, ptr, depth } => Self::Pointer {
                origin: *origin,
                ptr: *ptr,
                depth: *depth
            }
        }
    }

    // Get an owned variant from it
    #[inline(always)]
    pub fn to_owned(&self) -> Result<Self, ValueError> {
        Ok(Self::Owned(self.as_ref()?.clone()))
    }

    // Get the value of the path
    #[inline(always)]
    pub fn into_owned(self) -> Result<ValueCell, ValueError> {
        match self {
            Self::Owned(v) => Ok(v),
            Self::Pointer { ptr, .. } => unsafe {
                ptr.as_ref()
                    .cloned()
            }
        }
    }

    // Take the ownership by stealing the value from the pointer
    // and replace our current pointer as a owned value
    pub fn take_ownership(&mut self) -> Result<(), ValueError> {
        if let Self::Pointer { ptr, .. } = self {
            unsafe {
                let cell = ptr.as_mut()?;
                let owned = mem::take(cell);
                *self = owned.into();
            }
        }

        Ok(())
    }

    // Make the path owned if the pointer is the same
    pub fn make_owned_if_same_ptr(&mut self, other: ValuePointer) -> Result<(), ValueError> {
        if let Self::Pointer { origin, ptr,  .. } = self {
            unsafe {
                if *ptr == other || *origin == Some(other) {
                    let cell = ptr.as_ref()?;
                    *self = cell.clone().into();
                }
            }
        }

        Ok(())
    }

    // Transform the StackValue into an Owned variant if its a pointer
    // Do nothing if its a Owned variant already
    pub fn make_owned(&mut self) -> Result<bool, ValueError> {
        if let Self::Pointer { ptr, .. } = self {
            unsafe {
                let cell = ptr.as_ref()?;
                *self = cell.clone().into();
            }

            Ok(true)
        } else {
            Ok(false)
        }
    }

    // Get a reference to the value
    #[inline(always)]
    pub fn as_ref<'b>(&'b self) -> Result<&'b ValueCell, ValueError> {
        Ok(match self {
            Self::Owned(v) => v,
            Self::Pointer { ptr, .. } => unsafe {
                ptr.as_ref()?
            }
        })
    }

    // Get a mutable reference to the value
    #[inline(always)]
    pub fn as_mut<'b>(&'b mut self) -> Result<&'b mut ValueCell, ValueError> {
        Ok(match self {
            Self::Owned(v) => v,
            Self::Pointer { ptr, .. } => unsafe {
                ptr.as_mut()?
            }
        })
    }

    #[inline(always)]
    pub fn ptr(&mut self) -> ValuePointer {
        match self {
            // SAFETY: we own the value
            Self::Owned(v) => unsafe { ValuePointer::new(v) },
            Self::Pointer { ptr, .. } => *ptr
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

impl From<ValueCell> for StackValue {
    fn from(value: ValueCell) -> Self {
        Self::Owned(value)
    }
}

impl From<Primitive> for StackValue {
    fn from(value: Primitive) -> Self {
        Self::Owned(value.into())
    }
}

impl From<Constant> for StackValue {
    fn from(value: Constant) -> Self {
        Self::Owned(value.into())
    }
}