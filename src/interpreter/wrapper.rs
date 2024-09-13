use std::{cell::{Ref, RefCell, RefMut}, rc::Rc};

use crate::values::Value;

use super::variable::Variable;

pub struct VariableWrapper<'a> {
    origin: Rc<RefCell<Variable<'a>>>,
}

impl<'a> VariableWrapper<'a> {
    pub fn new(origin: Rc<RefCell<Variable<'a>>>) -> Self {
        Self {
            origin,
        }
    }

    pub fn get<'b>(&'b self) -> Ref<'b, Variable<'a>> {
        self.origin.borrow()
    }

    pub fn get_mut<'b>(&'b self) -> RefMut<'b, Variable<'a>> {
        self.origin.borrow_mut()
    }

    pub fn as_value<'b>(&'b self) -> Ref<'b, Value> {
        Ref::map(self.get(), |o| o.as_value())
    }

    pub fn as_value_mut<'b>(&'b self) -> RefMut<'b, Value> {
        RefMut::map(self.get_mut(), |o| o.as_mut())
    }
}