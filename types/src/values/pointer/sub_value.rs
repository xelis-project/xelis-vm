use std::{
    cell::{Ref, RefCell, RefMut},
    hash::{Hash, Hasher},
    rc::Rc
};
use crate::{Value, ValueCell, ValueType};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubValue(Rc<RefCell<ValueCell>>);

impl SubValue {
    #[inline(always)]
    pub fn new(v: ValueCell) -> Self {
        Self(Rc::new(RefCell::new(v)))
    }

    #[inline(always)]
    pub fn from(v: Rc<RefCell<ValueCell>>) -> Self {
        Self(v)
    }

    #[inline(always)]
    pub fn borrow<'a>(&'a self) -> Ref<'a, ValueCell> {
        self.0.borrow()
    }

    #[inline(always)]
    pub fn borrow_mut<'a>(&'a self) -> RefMut<'a, ValueCell> {
        self.0.borrow_mut()
    }

    #[inline(always)]
    pub fn into_inner(self) -> Rc<RefCell<ValueCell>> {
        self.0
    }

    #[inline(always)]
    pub fn into_owned(self) -> ValueCell {
        match Rc::try_unwrap(self.0) {
            Ok(value) => value.into_inner(),
            Err(rc) => rc.borrow().clone()
        }
    }
}

impl Hash for SubValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.borrow().hash(state);
    }
}

impl From<ValueCell> for SubValue {
    fn from(v: ValueCell) -> Self {
        Self::new(v)
    }
}

impl From<Value> for SubValue {
    fn from(v: Value) -> Self {
        Self::new(v.into())
    }
}

impl From<ValueType> for SubValue {
    fn from(v: ValueType) -> Self {
        Self::new(v.into())
    }
}