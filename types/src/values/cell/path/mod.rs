mod handle;

use crate::{values::ValueError, Constant, SubValue, Value};
pub use handle::{
    ValueHandle,
    ValueHandleMut
};

use super::ValueCell;

#[derive(Debug, Clone)]
pub enum Path<'a> {
    Owned(ValueCell),
    // Used for constants
    Borrowed(&'a ValueCell),
    Wrapper(SubValue)
}

impl<'a> Path<'a> {
    #[inline(always)]
    pub fn as_bool<'b: 'a>(&'b self) -> Result<bool, ValueError> {
        self.as_ref().as_bool()
    }

    #[inline(always)]
    pub fn as_u32(&'a self) -> Result<u32, ValueError> {
        self.as_ref().as_u32()
    }

    #[inline(always)]
    pub fn as_u64(&'a self) -> Result<u64, ValueError> {
        self.as_ref().as_u64()
    }

    // Share the value
    // This will create a new reference to the value
    pub fn shareable(&mut self) -> Path<'a> {
        match self {
            Self::Owned(v) => {
                let dst = std::mem::take(v);
                let inner = SubValue::new(dst);
                *self = Self::Wrapper(inner.reference());
                Self::Wrapper(inner)
            },
            Self::Borrowed(v) => { 
                let shared = SubValue::new(v.clone());
                *self = Self::Wrapper(shared.reference());
                Self::Wrapper(shared)
            },
            Self::Wrapper(v) => Self::Wrapper(v.reference())
        }
    }

    // Get the sub value of the path
    pub fn get_sub_variable(self, index: usize) -> Result<Path<'a>, ValueError> {
        match self {
            Self::Owned(v) => {
                let mut values = v.to_sub_vec()?;
                let len = values.len();
                if index >= len {
                    return Err(ValueError::OutOfBounds(index, len))
                }

                let at_index = values.remove(index);
                Ok(Path::Wrapper(at_index))
            },
            Self::Borrowed(v) => {
                let values = v.as_sub_vec()?;
                let len = values.len();
                let at_index = values
                    .get(index)
                    .ok_or_else(|| ValueError::OutOfBounds(index, len))?;

                Ok(Path::Wrapper(at_index.reference()))
            },
            Self::Wrapper(v) => {
                let values = v.borrow();
                let values = values.as_sub_vec()?;
                let len = values.len();
                let at_index = values
                    .get(index)
                    .ok_or_else(|| ValueError::OutOfBounds(index, len))?;

                Ok(Path::Wrapper(at_index.reference()))
            }
        }
    }

    // Get the value of the path
    #[inline(always)]
    pub fn into_owned(self) -> ValueCell {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v.clone(),
            Self::Wrapper(v) => v.into_owned()
        }
    }

    // Get a reference to the value
    #[inline(always)]
    pub fn as_ref<'b>(&'b self) -> ValueHandle<'b> {
        match self {
            Self::Owned(v) => ValueHandle::Borrowed(v),
            Self::Borrowed(v) => ValueHandle::Borrowed(v),
            Self::Wrapper(v) => ValueHandle::Ref(v.borrow())
        }
    }

    // Get a mutable reference to the value
    #[inline(always)]
    pub fn as_mut<'b>(&'b mut self) -> ValueHandleMut<'b> {
        match self {
            Self::Owned(v) => ValueHandleMut::Borrowed(v),
            Self::Borrowed(v) => {
                let v = v.clone();
                *self = Self::Owned(v);
                match self {
                    Self::Owned(v) => ValueHandleMut::Borrowed(v),
                    _ => unreachable!()
                }
            },
            Self::Wrapper(v) => ValueHandleMut::RefMut(v.borrow_mut())
        }
    }

    // Verify if its the same pointer
    #[inline]
    pub fn is_same_ptr<'b>(&'b self, other: &'b Path<'a>) -> bool {
        self.as_ref().as_value() as *const ValueCell == other.as_ref().as_value() as *const ValueCell
    }
}

impl From<ValueCell> for Path<'_> {
    fn from(value: ValueCell) -> Self {
        Self::Owned(value)
    }
}

impl<'a> From<&'a ValueCell> for Path<'a> {
    fn from(value: &'a ValueCell) -> Self {
        Self::Borrowed(value)
    }
}

impl From<SubValue> for Path<'_> {
    fn from(value: SubValue) -> Self {
        Self::Wrapper(value)
    }
}

impl From<Value> for Path<'_> {
    fn from(value: Value) -> Self {
        Self::Owned(value.into())
    }
}

impl From<Constant> for Path<'_> {
    fn from(value: Constant) -> Self {
        Self::Owned(value.into())
    }
}