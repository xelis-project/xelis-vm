use core::fmt;
use std::{cell::UnsafeCell, sync::Arc};
use schemars::JsonSchema;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use crate::{Constant, Primitive, ValueCell, ValueError};

// ValuePointer is a simple wrapper around the raw mut pointer
#[derive(Clone, JsonSchema)]
#[schemars(with = "ValueCell")]
pub struct ValuePointer(Arc<UnsafeCell<ValueCell>>);

// SAFETY: ValueCell is Sync + Send
// https://github.com/rust-lang/rust/issues/95439
unsafe impl Sync for ValuePointer {}
unsafe impl Send for ValuePointer {}

impl ValuePointer {
    #[inline(always)]
    pub fn new(cell: ValueCell) -> Self {
        Self(Arc::new(UnsafeCell::new(cell as _)))
    }

    #[inline(always)]
    pub unsafe fn as_mut<'a>(&'a mut self) -> &'a mut ValueCell {
        let ptr = self.0.get();
        // SAFETY: the pointer is valid as long as it is borrowed
        ptr.as_mut().unwrap_unchecked()
    }

    #[inline(always)]
    pub fn ptr(&self) -> *const ValueCell {
        self.0.get() as _
    }

    #[inline(always)]
    pub fn strong_count(&self) -> usize {
        Arc::strong_count(&self.0)
    }

    #[inline(always)]
    pub fn is_alone(&self) -> bool {
        Arc::strong_count(&self.0) == 1
    }

    #[inline(always)]
    pub fn as_ref<'a>(&'a self) -> &'a ValueCell {
        let ptr = self.0.get();
        // SAFETY: the pointer is valid as long as it is
        // borrowed
        unsafe {
            ptr.as_ref().unwrap_unchecked()
        }
    }

    #[inline(always)]
    pub fn as_u16(&self) -> Result<u16, ValueError> {
        self.as_ref().as_u16()
    }

    #[inline(always)]
    pub fn as_bool(&self) -> Result<bool, ValueError> {
        self.as_ref().as_bool()
    }

    #[inline(always)]
    pub fn into_owned(&self) -> ValueCell {
        self.as_ref().deep_clone()
    }

    #[inline(always)]
    pub fn to_owned(&self) -> ValueCell {
        self.as_ref().clone()
    }

    #[inline(always)]
    pub fn ptr_eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.0, &other.0)
    }
}

impl Default for ValuePointer {
    fn default() -> Self {
        Self::new(Default::default())
    }
}

impl PartialEq for ValuePointer {
    fn eq(&self, other: &Self) -> bool {
        self.ptr_eq(other) || self.as_ref() == other.as_ref()
    }
}

impl Eq for ValuePointer {}

impl Serialize for ValuePointer {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.as_ref().serialize(serializer)
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

impl fmt::Debug for ValuePointer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ValuePointer({:?})", self.0.get())
    }
}