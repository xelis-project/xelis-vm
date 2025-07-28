use std::{cell::UnsafeCell, rc::Rc};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use crate::{Constant, Primitive, ValueCell, ValueError};

// ValuePointer is a simple wrapper around the raw mut pointer
#[derive(Debug, Clone)]
pub struct ValuePointer(Rc<UnsafeCell<ValueCell>>);

impl ValuePointer {
    // WARNING: Put only ValueCell that is managed by one thread only
    #[inline(always)]
    pub fn new(cell: ValueCell) -> Self {
        Self(Rc::new(UnsafeCell::new(cell as _)))
    }

    #[inline(always)]
    pub fn as_mut<'a>(&'a mut self) -> Result<&'a mut ValueCell, ValueError> {
        let ptr = self.0.get();
        unsafe {
            Ok(ptr.as_mut().unwrap())
        }
    }

    #[inline(always)]
    pub fn as_ref<'a>(&'a self) -> Result<&'a ValueCell, ValueError> {
        let ptr = self.0.get();
        unsafe {
            Ok(ptr.as_ref().unwrap())
        }
    }

    #[inline]
    pub fn as_u16(&self) -> Result<u16, ValueError> {
        self.as_ref().and_then(ValueCell::as_u16)
    }

    #[inline]
    pub fn as_bool(&self) -> Result<bool, ValueError> {
        self.as_ref().and_then(ValueCell::as_bool)
    }

    #[inline]
    pub fn into_owned(&self) -> Result<ValueCell, ValueError> {
        self.as_ref().map(ValueCell::deep_clone)
    }
}

impl PartialEq for ValuePointer {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0) || self.as_ref().unwrap() == other.as_ref().unwrap()
    }
}

impl Eq for ValuePointer {}

impl Serialize for ValuePointer {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_ref()
            .map_err(|e| serde::ser::Error::custom(e.to_string()))?
            .serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ValuePointer {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = ValueCell::deserialize(deserializer)?;
        Ok(Self::new(value))
    }
}

impl From<Constant> for ValuePointer {
    fn from(value: Constant) -> Self {
        Self::new(value.into())
    }
}

impl From<Primitive> for ValuePointer {
    fn from(value: Primitive) -> Self {
        Self::new(value.into())
    }
}

impl From<ValueCell> for ValuePointer {
    fn from(value: ValueCell) -> Self {
        Self::new(value)
    }
}