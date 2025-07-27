use std::{cell::RefCell, rc::Rc};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use crate::{Constant, Primitive, ValueCell, ValueError};

// ValuePointer is a simple wrapper around the raw mut pointer
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValuePointer(Rc<RefCell<ValueCell>>);

impl ValuePointer {
    // WARNING: Put only ValueCell that is managed by one thread only
    #[inline(always)]
    pub fn new(cell: ValueCell) -> Self {
        Self(Rc::new(RefCell::new(cell as _)))
    }

    #[inline(always)]
    pub fn as_mut<'a>(&'a mut self) -> Result<&'a mut ValueCell, ValueError> {
        todo!()
    }

    #[inline(always)]
    pub fn as_ref<'a>(&'a self) -> Result<&'a ValueCell, ValueError> {
        todo!()
    }
}

impl Serialize for ValuePointer {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.borrow().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ValuePointer {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value = ValueCell::deserialize(deserializer)?;
        Ok(ValuePointer(Rc::new(RefCell::new(value))))
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