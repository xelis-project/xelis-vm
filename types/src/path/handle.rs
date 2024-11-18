use std::{
    fmt,
    cell::{Ref, RefMut},
    ops::{Deref, DerefMut}
};

use crate::values::Value;

pub enum ValueHandle<'a> {
    Borrowed(&'a Value),
    Ref(Ref<'a, Value>)
}

pub enum ValueHandleMut<'a> {
    Borrowed(&'a mut Value),
    RefMut(RefMut<'a, Value>)
}

impl<'a> ValueHandle<'a> {
    #[inline(always)]
    pub fn as_value<'b>(&'b self) -> &'b Value {
        match self {
            Self::Borrowed(v) => v,
            Self::Ref(v) => v
        }
    }
}

impl<'a> ValueHandleMut<'a> {
    #[inline(always)]
    pub fn as_value(&self) -> &Value {
        match self {
            Self::Borrowed(v) => v,
            Self::RefMut(v) => v
        }
    }

    #[inline(always)]
    pub fn as_value_mut(&mut self) -> &mut Value {
        match self {
            Self::Borrowed(v) => v,
            Self::RefMut(v) => v
        }
    }
}

impl<'a> From<&'a Value> for ValueHandle<'a> {
    fn from(value: &'a Value) -> Self {
        Self::Borrowed(value)
    }
}

impl<'a> From<Ref<'a, Value>> for ValueHandle<'a> {
    fn from(value: Ref<'a, Value>) -> Self {
        Self::Ref(value)
    }
}

impl AsRef<Value> for ValueHandle<'_> {
    fn as_ref(&self) -> &Value {
        self.as_value()
    }
}

impl Deref for ValueHandle<'_> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        self.as_value()
    }
}

impl<'a> From<&'a mut Value> for ValueHandleMut<'a> {
    fn from(value: &'a mut Value) -> Self {
        Self::Borrowed(value)
    }
}

impl<'a> From<RefMut<'a, Value>> for ValueHandleMut<'a> {
    fn from(value: RefMut<'a, Value>) -> Self {
        Self::RefMut(value)
    }
}

impl AsRef<Value> for ValueHandleMut<'_> {
    fn as_ref(&self) -> &Value {
        self.as_value()
    }
}

impl Deref for ValueHandleMut<'_> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        self.as_value()
    }
}

impl DerefMut for ValueHandleMut<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_value_mut()
    }
}

impl AsMut<Value> for ValueHandleMut<'_> {
    fn as_mut(&mut self) -> &mut Value {
        self.as_value_mut()
    }
}

impl fmt::Display for ValueHandle<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_value())
    }
}