use std::{borrow::Borrow, cell::{Ref, RefCell}, ops::Deref, rc::Rc};

use crate::values::Value;

use super::context::ValueHandleMut;


pub type SharedValue = Rc<RefCell<Value>>;

pub enum ValueHandle<'a> {
    Value(&'a Value),
    Reference(Ref<'a, Value>)
}

impl Deref for ValueHandle<'_> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Value(v) => v,
            Self::Reference(v) => &**v,
        }
    }
}

// Variable value is used to store a value that can be either owned or borrowed
// in the context
#[derive(Debug)]
pub enum VarValue<'a> {
    Owned(Value),
    Borrowed(&'a mut Value),
    Sharable(SharedValue),
}

impl<'a> Deref for VarValue<'a> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v,
            Self::Sharable(v) => &*v.try_borrow().unwrap(),
        }
    }
}

impl<'a> DerefMut for VarValue<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v,
            Self::Sharable(v) => &mut *v.try_borrow_mut().unwrap(),
        }
    }
}

impl<'a> Into<VarValue<'a>> for Value {
    fn into(self) -> VarValue<'a> {
        VarValue::Owned(self)
    }
}

impl<'a> From<&'a mut Value> for VarValue<'a> {
    fn from(value: &'a mut Value) -> Self {
        VarValue::Borrowed(value)
    }
}

impl<'a> VarValue<'a> {
    pub fn into_owned(self) -> Value {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v.clone(),
            Self::Sharable(v) => v.try_borrow().unwrap().clone(),
        }
    }

    pub fn to_owned(&self) -> Value {
        match self {
            Self::Owned(v) => v.clone(),
            Self::Borrowed(v) => (**v).clone(),
            Self::Sharable(v) => {
                v.try_borrow().unwrap().clone()
            },
        }
    }

    pub fn as_value(&'a self) -> ValueHandle {
        match self {
            Self::Owned(v) => ValueHandle::Value(v),
            Self::Borrowed(v) => ValueHandle::Value(v),
            Self::Sharable(v) => ValueHandle::Reference(v.try_borrow().unwrap()),            
        }
    }

    pub fn as_mut_value(&'a mut self) -> ValueHandleMut {
        match self {
            Self::Owned(v) => ValueHandleMut::Value(v),
            Self::Borrowed(v) => ValueHandleMut::Value(v),
            Self::Sharable(v) => ValueHandleMut::Reference(v.borrow_mut()),
        }
    }
}