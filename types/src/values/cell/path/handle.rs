use std::{
    fmt,
    cell::{Ref, RefMut},
    ops::{Deref, DerefMut}
};
use crate::ValueCell;

pub enum ValueHandle<'a> {
    Borrowed(&'a ValueCell),
    Ref(Ref<'a, ValueCell>)
}

pub enum ValueHandleMut<'a> {
    Borrowed(&'a mut ValueCell),
    RefMut(RefMut<'a, ValueCell>)
}

impl<'a> ValueHandle<'a> {
    #[inline(always)]
    pub fn as_value<'b>(&'b self) -> &'b ValueCell {
        match self {
            Self::Borrowed(v) => v,
            Self::Ref(v) => v
        }
    }
}

impl<'a> ValueHandleMut<'a> {
    #[inline(always)]
    pub fn as_value(&self) -> &ValueCell {
        match self {
            Self::Borrowed(v) => v,
            Self::RefMut(v) => v
        }
    }

    #[inline(always)]
    pub fn as_value_mut(&mut self) -> &mut ValueCell {
        match self {
            Self::Borrowed(v) => v,
            Self::RefMut(v) => v
        }
    }
}

impl<'a> From<&'a ValueCell> for ValueHandle<'a> {
    fn from(value: &'a ValueCell) -> Self {
        Self::Borrowed(value)
    }
}

impl<'a> From<Ref<'a, ValueCell>> for ValueHandle<'a> {
    fn from(value: Ref<'a, ValueCell>) -> Self {
        Self::Ref(value)
    }
}

impl AsRef<ValueCell> for ValueHandle<'_> {
    fn as_ref(&self) -> &ValueCell {
        self.as_value()
    }
}

impl Deref for ValueHandle<'_> {
    type Target = ValueCell;

    fn deref(&self) -> &Self::Target {
        self.as_value()
    }
}

impl<'a> From<&'a mut ValueCell> for ValueHandleMut<'a> {
    fn from(value: &'a mut ValueCell) -> Self {
        Self::Borrowed(value)
    }
}

impl<'a> From<RefMut<'a, ValueCell>> for ValueHandleMut<'a> {
    fn from(value: RefMut<'a, ValueCell>) -> Self {
        Self::RefMut(value)
    }
}

impl AsRef<ValueCell> for ValueHandleMut<'_> {
    fn as_ref(&self) -> &ValueCell {
        self.as_value()
    }
}

impl Deref for ValueHandleMut<'_> {
    type Target = ValueCell;

    fn deref(&self) -> &Self::Target {
        self.as_value()
    }
}

impl DerefMut for ValueHandleMut<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_value_mut()
    }
}

impl AsMut<ValueCell> for ValueHandleMut<'_> {
    fn as_mut(&mut self) -> &mut ValueCell {
        self.as_value_mut()
    }
}

impl fmt::Display for ValueHandle<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_value())
    }
}