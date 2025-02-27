mod handle;

use std::mem;

use crate::{values::ValueError, Constant, Value};
pub use handle::{
    ValueHandle,
    ValueHandleMut
};

use super::ValueCell;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StackValue {
    // Value is on stack directly
    Owned(Box<ValueCell>),
    Pointer(*mut ValueCell)
}

impl StackValue {
    #[inline(always)]
    pub fn as_bool<'a>(&'a self) -> Result<bool, ValueError> {
        self.as_ref().as_bool()
    }

    #[inline(always)]
    pub fn as_u32<'a>(&'a self) -> Result<u32, ValueError> {
        self.as_ref().as_u32()
    }

    #[inline(always)]
    pub fn as_u64<'a>(&'a self) -> Result<u64, ValueError> {
        self.as_ref().as_u64()
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
                Ok(Self::Owned(Box::new(at_index)))
            },
            Self::Pointer(v) => unsafe {
                let cell = v.as_mut().unwrap();
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
            Self::Owned(value) => Self::Pointer(value.as_mut() as *mut ValueCell),
            Self::Pointer(ptr) => Self::Pointer(*ptr)
        }
    }

    // Get the internal value
    pub fn into_inner(self) -> ValueCell {
        self.into_owned().unwrap()
    }

    // Get the value of the path
    #[inline(always)]
    pub fn into_owned(self) -> Result<ValueCell, ValueError> {
        match self {
            Self::Owned(v) => Ok(*v),
            Self::Pointer(v) => unsafe {
                Ok(v.as_ref().unwrap().clone())
            }
        }
    }

    pub fn take_ownership(&mut self) {
        if let Self::Pointer(ptr) = self {
            println!("MAKE OWNED {:?}", ptr);
            unsafe {
                let cell = ptr.as_mut().unwrap();
                let owned = mem::take(cell);
                *self = owned.into();
            }
        }
    }

    // Make the path owned if the pointer is the same
    pub fn make_owned_if_same_ptr(&mut self, other: &StackValue) {
        if self.is_same_ptr(other) {
            println!("SAMMEEE");
            if let Self::Pointer(ptr) = self {
                println!("MAKE OWNED {:?}", ptr);
                unsafe {
                    let cell = ptr.as_mut().unwrap();
                    *self = cell.clone().into();
                }
            }
        }
    }

    pub fn is_wrapped(&self) -> bool {
        matches!(self, Self::Pointer(_))
    }

    // Get a reference to the value
    #[inline(always)]
    pub fn as_ref<'b>(&'b self) -> &'b ValueCell {
        match self {
            Self::Owned(v) => v,
            Self::Pointer(v) => unsafe {
                v.as_ref().unwrap()
            }
        }
    }

    // Get a mutable reference to the value
    #[inline(always)]
    pub fn as_mut<'b>(&'b mut self) -> &'b mut ValueCell {
        match self {
            Self::Owned(v) => v,
            Self::Pointer(v) => unsafe {
                v.as_mut().unwrap()
            }
        }
    }

    // Verify if its the same pointer
    #[inline]
    pub fn is_same_ptr<'b>(&'b self, other: &'b StackValue) -> bool {
        self.ptr() == other.ptr()
    }

    #[inline]
    pub fn ptr(&self) -> *const ValueCell {
        self.as_ref() as *const ValueCell
    }
}

impl From<ValueCell> for StackValue {
    fn from(value: ValueCell) -> Self {
        Self::Owned(Box::new(value))
    }
}

impl From<&mut ValueCell> for StackValue {
    fn from(value: &mut ValueCell) -> Self {
        Self::Pointer(value as *mut _)
    }
}

impl From<Value> for StackValue {
    fn from(value: Value) -> Self {
        Self::Owned(Box::new(value.into()))
    }
}

impl From<Constant> for StackValue {
    fn from(value: Constant) -> Self {
        Self::Owned(Box::new(value.into()))
    }
}