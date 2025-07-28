use std::mem;

use crate::{
    values::{cell::pointer::ValuePointer, ValueError},
    Constant,
    Primitive,
    Type
};
use super::ValueCell;

#[derive(Debug, Clone)]
pub enum StackValue {
    // Value is on stack directly
    Owned(ValueCell),
    Pointer {
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
            Self::Owned(v) => match v {
                ValueCell::Object(mut values) => {
                    let len = values.len();
                    if index >= len {
                        return Err(ValueError::OutOfBounds(index, len))
                    }

                    Ok(Self::Pointer { ptr: values.swap_remove(index), depth: 1 })
                },
                ValueCell::Bytes(mut bytes) => {
                    let len = bytes.len();
                    if index >= len {
                        return Err(ValueError::OutOfBounds(index, len))
                    }

                    Ok(Self::Owned(ValueCell::Default(Primitive::U8(bytes.swap_remove(index)))))
                },
                _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
            },
            Self::Pointer { ptr, depth } => {
                let cell = ptr.as_ref()?;

                Ok(match cell {
                    ValueCell::Object(values) => {
                        let len = values.len();
                        let at_index = values
                            .get(index)
                            .ok_or_else(|| ValueError::OutOfBounds(index, len))?;

                        Self::Pointer {
                            ptr: at_index.clone(),
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
        if let Self::Owned(value) = self {
            let v = mem::take(value);
            let pointer = ValuePointer::new(v);
            *self = Self::Pointer {
                ptr: pointer.clone(),
                depth: 0
            };
        }

        self.clone()
    }

    // Get an owned variant from it
    #[inline(always)]
    pub fn to_owned(&self) -> Result<Self, ValueError> {
        Ok(Self::Owned(self.as_ref()?.deep_clone()))
    }

    // Get the value of the path
    #[inline(always)]
    pub fn into_owned(self) -> Result<ValueCell, ValueError> {
        match self {
            Self::Owned(v) => Ok(v.deep_clone()),
            Self::Pointer { ptr, .. } => ptr.as_ref().map(ValueCell::deep_clone)
        }
    }

    // Take the ownership by stealing the value from the pointer
    // and replace our current pointer as a owned value
    pub fn take_ownership(&mut self) -> Result<(), ValueError> {
        if let Self::Pointer { ptr, .. } = self {
            let cell = ptr.as_mut()?;
            let owned = mem::take(cell);
            *self = owned.into();
        }

        Ok(())
    }

    // Make the path owned if the pointer is the same
    pub fn make_owned_if_same_ptr(&mut self, other: ValuePointer) -> Result<(), ValueError> {
        if let Self::Pointer { ptr,  .. } = self {
            if *ptr == other {
                let cell = ptr.as_ref()?;
                *self = cell.deep_clone().into();
            }
        }

        Ok(())
    }

    // Transform the StackValue into an Owned variant if its a pointer
    // Do nothing if its a Owned variant already
    pub fn make_owned(&mut self) -> Result<bool, ValueError> {
        if let Self::Pointer { ptr, .. } = self {
            let cell = ptr.as_ref()?;
            *self = cell.deep_clone().into();

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
            Self::Pointer { ptr, .. } => ptr.as_ref()?
        })
    }

    // Get a mutable reference to the value
    #[inline(always)]
    pub fn as_mut<'b>(&'b mut self) -> Result<&'b mut ValueCell, ValueError> {
        Ok(match self {
            Self::Owned(v) => v,
            Self::Pointer { ptr, .. } => ptr.as_mut()?
        })
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

impl From<ValuePointer> for StackValue {
    fn from(ptr: ValuePointer) -> Self {
        Self::Pointer { ptr, depth: 0 }
    }
}