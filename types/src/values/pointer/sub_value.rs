use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashSet,
    hash::{Hash, Hasher},
    rc::Rc
};
use crate::Value;


#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SubValue(Rc<RefCell<Value>>);

impl SubValue {
    #[inline(always)]
    pub fn new(value: Value) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }

    #[inline(always)]
    pub fn from(value: Rc<RefCell<Value>>) -> Self {
        Self(value)
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


impl Hash for SubValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.borrow().hash_with_tracked_pointers(state, &mut HashSet::new());
    }
}
