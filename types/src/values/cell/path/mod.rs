mod handle;

use crate::{values::ValueError, Constant, SubValue, Value};
pub use handle::{
    ValueHandle,
    ValueHandleMut
};

use super::ValueCell;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Path {
    Owned(ValueCell),
    Wrapper(SubValue)
}

impl Path {
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

    // Share the value
    // This will create a new reference to the value
    pub fn shareable(&mut self) -> Path {
        match self {
            Self::Owned(v) => {
                let dst = std::mem::take(v);
                let inner = SubValue::new(dst);
                *self = Self::Wrapper(inner.reference());
                Self::Wrapper(inner)
            },
            Self::Wrapper(v) => Self::Wrapper(v.reference())
        }
    }

    // Get the sub value of the path
    pub fn get_sub_variable(self, index: usize) -> Result<Path, ValueError> {
        match self {
            Self::Owned(v) => {
                let mut values = v.to_sub_vec()?;
                let len = values.len();
                if index >= len {
                    return Err(ValueError::OutOfBounds(index, len))
                }

                let at_index = values.remove(index);
                Ok(at_index)
            },
            Self::Wrapper(v) => {
                let mut values = v.borrow_mut();
                let values = values.as_mut_sub_vec()?;
                let len = values.len();
                let at_index = values
                    .get_mut(index)
                    .ok_or_else(|| ValueError::OutOfBounds(index, len))?;

                Ok(at_index.shareable())
            }
        }
    }

    // Get the sub value of the path
    // This will return a new Path::Owned
    #[inline(always)]
    pub fn to_owned(&self) -> Path {
        match self {
            Self::Owned(v) => Self::Owned(v.clone()),
            Self::Wrapper(v) => Self::Owned(v.borrow().clone())
        }
    }

    // Get the internal value
    pub fn into_inner(self) -> ValueCell {
        match self {
            Self::Owned(v) => v,
            Self::Wrapper(v) => v.into_inner()
        }
    }

    // Get the value of the path
    #[inline(always)]
    pub fn into_owned(self) -> Result<ValueCell, ValueError> {
        match self {
            Self::Owned(v) => Ok(v),
            Self::Wrapper(v) => v.into_owned()
        }
    }

    // Make the path owned if the pointer is the same
    pub fn make_owned_if_same_ptr(&mut self, other: &Path) {
        if self.is_same_ptr(other) {
            *self = other.to_owned();
        }
    }

    pub fn is_wrapped(&self) -> bool {
        matches!(self, Self::Wrapper(_))
    }

    // Get a reference to the value
    #[inline(always)]
    pub fn as_ref<'b>(&'b self) -> ValueHandle<'b> {
        match self {
            Self::Owned(v) => ValueHandle::Borrowed(v),
            Self::Wrapper(v) => ValueHandle::Ref(v.borrow())
        }
    }

    // Get a mutable reference to the value
    #[inline(always)]
    pub fn as_mut<'b>(&'b mut self) -> ValueHandleMut<'b> {
        match self {
            Self::Owned(v) => ValueHandleMut::Borrowed(v),
            Self::Wrapper(v) => ValueHandleMut::RefMut(v.borrow_mut())
        }
    }

    // Verify if its the same pointer
    #[inline]
    pub fn is_same_ptr<'b>(&'b self, other: &'b Path) -> bool {
        self.ptr() == other.ptr()
    }

    #[inline]
    pub fn ptr(&self) -> *const ValueCell {
        self.as_ref().as_value() as *const ValueCell
    }
}

impl From<ValueCell> for Path {
    fn from(value: ValueCell) -> Self {
        Self::Owned(value)
    }
}

impl From<SubValue> for Path {
    fn from(value: SubValue) -> Self {
        Self::Wrapper(value)
    }
}

impl From<Value> for Path {
    fn from(value: Value) -> Self {
        Self::Owned(value.into())
    }
}

impl From<Constant> for Path {
    fn from(value: Constant) -> Self {
        Self::Owned(value.into())
    }
}