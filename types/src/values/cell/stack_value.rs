use std::mem;

use crate::{values::ValueError, Constant, Primitive};
use super::ValueCell;

#[derive(Debug)]
pub enum StackValue {
    // Value is on stack directly
    Owned(ValueCell),
    Pointer(*mut ValueCell)
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
                let mut values = v.to_vec()?;
                let len = values.len();
                if index >= len {
                    return Err(ValueError::OutOfBounds(index, len))
                }

                let at_index = values.remove(index);
                Ok(Self::Owned(at_index))
            },
            Self::Pointer(v) => unsafe {
                let cell = v.as_mut()
                    .ok_or(ValueError::InvalidPointer)?;
                let values = cell.as_mut_vec()?;
                let len = values.len();
                let at_index = values
                    .get_mut(index)
                    .ok_or_else(|| ValueError::OutOfBounds(index, len))?;

                Ok(Self::Pointer(at_index as *mut ValueCell))
            }
        }
    }

    pub fn reference(&mut self) -> Self {
        match self {
            Self::Owned(value) => Self::Pointer(value as *mut ValueCell),
            Self::Pointer(ptr) => Self::Pointer(*ptr)
        }
    }

    // Get an owned variant from it
    pub fn to_owned(&self) -> Result<Self, ValueError> {
        Ok(Self::Owned(self.as_ref()?.clone()))
    }

    // Get the internal value
    pub fn into_inner(self) -> ValueCell {
        self.into_owned().unwrap()
    }

    // Get the value of the path
    #[inline(always)]
    pub fn into_owned(self) -> Result<ValueCell, ValueError> {
        match self {
            Self::Owned(v) => Ok(v),
            Self::Pointer(v) => unsafe {
                v.as_ref()
                    .ok_or(ValueError::InvalidPointer)
                    .cloned()
            }
        }
    }

    // Take the ownership by stealing the value from the pointer
    // and replace our current pointer as a owned value
    pub fn take_ownership(&mut self) -> Result<(), ValueError> {
        if let Self::Pointer(ptr) = self {
            unsafe {
                let cell = ptr.as_mut()
                    .ok_or(ValueError::InvalidPointer)?;
                let owned = mem::take(cell);
                *self = owned.into();
            }
        }

        Ok(())
    }

    // Make the path owned if the pointer is the same
    pub fn make_owned_if_same_ptr(&mut self, other: *mut ValueCell) -> Result<(), ValueError> {
        if let Self::Pointer(ptr) = self {
            unsafe {
                if *ptr == other {
                    let cell = ptr.as_mut()
                        .ok_or(ValueError::InvalidPointer)?;
                    *self = cell.into();
                }
            }
        }

        Ok(())
    }

    // Get a reference to the value
    #[inline(always)]
    pub fn as_ref<'b>(&'b self) -> Result<&'b ValueCell, ValueError> {
        Ok(match self {
            Self::Owned(v) => v,
            Self::Pointer(v) => unsafe {
                v.as_ref().ok_or(ValueError::InvalidPointer)?
            }
        })
    }

    // Get a mutable reference to the value
    #[inline(always)]
    pub fn as_mut<'b>(&'b mut self) -> Result<&'b mut ValueCell, ValueError> {
        Ok(match self {
            Self::Owned(v) => v,
            Self::Pointer(v) => unsafe {
                v.as_mut().ok_or(ValueError::InvalidPointer)?
            }
        })
    }

    #[inline(always)]
    pub fn ptr(&mut self) -> *mut ValueCell {
        match self {
            Self::Owned(v) => v as _,
            Self::Pointer(ptr) => *ptr
        }
    }
}

impl From<ValueCell> for StackValue {
    fn from(value: ValueCell) -> Self {
        Self::Owned(value)
    }
}

impl From<&mut ValueCell> for StackValue {
    fn from(value: &mut ValueCell) -> Self {
        Self::Pointer(value as *mut _)
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