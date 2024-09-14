use std::{cell::RefCell, rc::Rc};

use crate::{
    handle::{ValueHandle, ValueHandleMut},
    values::Value,
    IdentifierType
};
use super::InterpreterError;

#[derive(Debug)]
pub enum Path<'a> {
    Owned(Value),
    // Used for constants
    Borrowed(&'a Value),
    Wrapper(Rc<RefCell<Value>>)
}

impl<'a> Path<'a> {
    #[inline(always)]
    pub fn as_bool<'b: 'a>(&'b self) -> Result<bool, InterpreterError> {
        self.as_ref().as_bool()
    }

    #[inline(always)]
    pub fn as_u64(&'a self) -> Result<u64, InterpreterError> {
        self.as_ref().as_u64()
    }

    pub fn shareable(&mut self) -> Path<'a> {
        match self {
            Self::Owned(v) => {
                let dst = std::mem::replace(v, Value::Null);
                let shared = Rc::new(RefCell::new(dst));
                *self = Self::Wrapper(shared.clone());
                Self::Wrapper(shared)
            },
            Self::Borrowed(v) => { 
                let shared = Rc::new(RefCell::new(v.clone()));
                *self = Self::Wrapper(shared.clone());
                Self::Wrapper(shared)
            },
            Self::Wrapper(v) => Self::Wrapper(v.clone())
        }
    }

    pub fn get_index_at(self, index: usize) -> Result<Path<'a>, InterpreterError> {
        match self {
            Self::Owned(v) => {
                let mut values = v.to_vec()?;
                let len = values.len();
                if index >= len {
                    return Err(InterpreterError::OutOfBounds(index, len))
                }

                let at_index = values.remove(index);
                Ok(Path::Wrapper(at_index))
            },
            Self::Borrowed(v) => {
                let values = v.as_vec()?;
                let len = values.len();
                let at_index = values
                    .get(index)
                    .ok_or_else(|| InterpreterError::OutOfBounds(index, len))?;

                Ok(Path::Wrapper(at_index.clone()))
            },
            Self::Wrapper(v) => {
                let mut values = v.borrow_mut();
                let values = values.as_mut_vec()?;
                let len = values.len();
                let at_index = values
                    .get_mut(index)
                    .ok_or_else(|| InterpreterError::OutOfBounds(index, len))?;

                Ok(Path::Wrapper(at_index.clone()))
            }
        }
    }

    pub fn get_sub_variable(self, name: &IdentifierType) -> Result<Path<'a>, InterpreterError> {
        match self {
            Self::Owned(v) => {
                let mut values = v.to_map()?;
                let sub_value = values
                    .remove(name)
                    .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))?;

                Ok(Path::Wrapper(sub_value.clone()))
            },
            Self::Borrowed(v) => {
                let values = v.as_map()?;
                let sub_value = values
                    .get(name)
                    .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))?;

                Ok(Path::Wrapper(sub_value.clone()))
            },
            Self::Wrapper(v) => {
                let mut values = v.borrow_mut();
                let values = values.as_mut_map()?;
                let sub_value = values
                    .get_mut(name)
                    .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))?;

                Ok(Path::Wrapper(sub_value.clone()))
            }
        }
    }

    pub fn into_owned(self) -> Value {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v.clone(),
            Self::Wrapper(v) => v.borrow().clone()
        }
    }

    pub fn as_ref<'b>(&'b self) -> ValueHandle<'b> {
        match self {
            Self::Owned(v) => ValueHandle::Borrowed(v),
            Self::Borrowed(v) => ValueHandle::Borrowed(v),
            Self::Wrapper(v) => ValueHandle::Ref(v.borrow())
        }
    }

    pub fn as_mut<'b: 'a>(&'b mut self) -> ValueHandleMut<'b> {
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
}