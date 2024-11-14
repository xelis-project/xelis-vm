use std::{cell::{Ref, RefCell, RefMut}, hash::{Hash, Hasher}, rc::Rc};

use crate::{ValueHandle, ValueHandleMut};

use super::Value;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InnerValue(Rc<RefCell<Value>>);

impl InnerValue {
    #[inline(always)]
    pub fn new(value: Value) -> Self {
        InnerValue(Rc::new(RefCell::new(value)))
    }

    #[inline(always)]
    pub fn borrow<'a>(&'a self) -> Ref<'a, Value> {
        self.0.borrow()
    }

    #[inline(always)]
    pub fn borrow_mut<'a>(&'a self) -> RefMut<'a, Value> {
        self.0.borrow_mut()
    }

    #[inline(always)]
    pub fn into_inner(self) -> Rc<RefCell<Value>> {
        self.0
    }
}

impl Hash for InnerValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.borrow().hash(state)
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum ValueOwnable {
    Owned(Box<Value>),
    Rc(InnerValue)
}

impl ValueOwnable {
    // Convert into a owned value
    // Clone if the value is shared and can't be moved
    pub fn into_inner(self) -> Value {
        match self {
            ValueOwnable::Owned(v) => *v,
            ValueOwnable::Rc(v) => match Rc::try_unwrap(v.into_inner()) {
                Ok(value) => value.into_inner(),
                Err(rc) => rc.borrow().clone()
            }
        }
    }

    // Clone the Rc value to fully own it
    pub fn into_ownable(self) -> ValueOwnable {
        match self {
            ValueOwnable::Owned(_) => self,
            ValueOwnable::Rc(v) => ValueOwnable::Owned(Box::new(match Rc::try_unwrap(v.into_inner()) {
                Ok(value) => value.into_inner(),
                Err(rc) => rc.borrow().clone()
            }))
        }
    }

    // Transform the value into a shared value
    pub fn transform(&mut self) -> ValueOwnable {
        match self {
            ValueOwnable::Owned(v) => {
                let dst = std::mem::replace(v, Box::new(Value::Null));
                let shared = Self::Rc(InnerValue::new(*dst));
                *self = shared.clone();
                shared
            },
            ValueOwnable::Rc(v) => Self::Rc(v.clone())
        }
    }

    // Wrap the value into an handle to be casted to a reference of the value
    pub fn handle<'a>(&'a self) -> ValueHandle<'a> {
        match self {
            ValueOwnable::Owned(v) => ValueHandle::Borrowed(v),
            ValueOwnable::Rc(v) => ValueHandle::Ref(v.borrow())
        }
    }

    // Wrap the value into an handle to be casted to a mutable reference of the value
    pub fn handle_mut<'a>(&'a mut self) -> ValueHandleMut<'a> {
        match self {
            ValueOwnable::Owned(v) => ValueHandleMut::Borrowed(v),
            ValueOwnable::Rc(v) => ValueHandleMut::RefMut(v.borrow_mut())
        }
    }
}