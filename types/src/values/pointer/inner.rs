use std::rc::Rc;

use crate::{values::cell::ValueCell, ValueHandle, ValueHandleMut};
use super::{SubValue, ValuePointer};

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum ValuePointerInner {
    Owned(Box<ValueCell>),
    Shared(SubValue)
}

impl Default for ValuePointerInner {
    fn default() -> Self {
        Self::Owned(Box::new(Default::default()))
    }
}

impl ValuePointerInner {
    // Convert into a owned value
    // Take the value from the Rc
    // if it can't be taken, replace it by Value::Null
    pub fn take_value(self) -> ValueCell {
        match self {
            Self::Owned(v) => *v,
            Self::Shared(v) => match Rc::try_unwrap(v.into_inner()) {
                Ok(value) => value.into_inner(),
                Err(rc) => {
                    let mut value = rc.borrow_mut();
                    std::mem::take(&mut value)
                }
            }
        }
    }

    // Get the inner value, or clone it if it's shared
    pub fn into_value(self) -> ValueCell {
        match self {
            Self::Owned(v) => *v,
            Self::Shared(v) => match Rc::try_unwrap(v.into_inner()) {
                Ok(value) => value.into_inner(),
                Err(rc) => rc.borrow().clone()
            }
        }
    }

    // Clone the Rc value to fully own it
    pub fn into_owned(self) -> ValuePointer {
        ValuePointer(match self {
            Self::Owned(_) => self,
            Self::Shared(v) => Self::Owned(Box::new(match Rc::try_unwrap(v.into_inner()) {
                Ok(value) => value.into_inner(),
                Err(rc) => rc.borrow().clone()
            }))
        })
    }

    // Transform the value into a shared value
    pub fn transform(&mut self) -> ValuePointer {
        ValuePointer(match self {
            Self::Owned(v) => {
                let dst = std::mem::take(v);
                let shared = Self::Shared(SubValue::new(*dst));
                *self = shared.clone();
                shared
            },
            Self::Shared(v) => Self::Shared(v.reference())
        })
    }

    // Wrap the value into an handle to be casted to a reference of the value
    pub fn handle<'a>(&'a self) -> ValueHandle<'a> {
        match self {
            Self::Owned(v) => ValueHandle::Borrowed(v),
            Self::Shared(v) => ValueHandle::Ref(v.borrow())
        }
    }

    // Wrap the value into an handle to be casted to a mutable reference of the value
    pub fn handle_mut<'a>(&'a mut self) -> ValueHandleMut<'a> {
        match self {
            Self::Owned(v) => ValueHandleMut::Borrowed(v),
            Self::Shared(v) => ValueHandleMut::RefMut(v.borrow_mut())
        }
    }
}