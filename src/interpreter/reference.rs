use std::cell::Ref;

use crate::{interpreter::VariableWrapper, values::{SharedValue, Value}};

use super::handle::ValueHandle;

pub enum Reference<'a> {
    Owned(Value),
    Borrowed(&'a Value),
    Wrapper(VariableWrapper<'a>),
    Shared(SharedValue),
}

impl<'a> Reference<'a> {
    /// Verify if the value is owned
    pub fn is_owned(&self) -> bool {
        matches!(self, Self::Owned(_))
    }

    /// Verify if the value is borrowed
    pub fn is_borrowed(&self) -> bool {
        !self.is_owned()
    }

    /// Convert the value to owned
    pub fn into_owned(self) -> Value {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v.clone(),
            Self::Wrapper(wrapper) => wrapper.get().as_value().clone(),
            Self::Shared(shared) => shared.borrow().clone(),
        }
    }

    pub fn handle<'b>(&'b self) -> ValueHandle<'b> {
        match self {
            Self::Owned(v) => ValueHandle::Borrowed(v),
            Self::Borrowed(v) => ValueHandle::Borrowed(v),
            Self::Wrapper(wrapper) => ValueHandle::Ref(Ref::map(wrapper.get(), |v| v.as_value())),
            Self::Shared(shared) => ValueHandle::Ref(shared.borrow()),
        }
    }
}