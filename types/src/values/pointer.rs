use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashSet,
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
    rc::Rc
};

use crate::{ValueHandle, ValueHandleMut};

use super::Value;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InnerValue(Rc<RefCell<Value>>);

impl InnerValue {
    #[inline(always)]
    pub fn new(value: Value) -> Self {
        InnerValue(Rc::new(RefCell::new(value)))
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

impl Hash for InnerValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.borrow().hash_with_tracked_pointers(state, &mut HashSet::new());
    }
}

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub enum ValuePointerInner {
    Owned(Box<Value>),
    Shared(InnerValue)
}

impl Default for ValuePointerInner {
    fn default() -> Self {
        Self::Owned(Box::new(Value::Null))
    }
}

// Value Pointer is a wrapper around the real Value Pointer
// It was introduced to allow to implement a custom Drop to prevent any stackoverflow
// that could happen with huge nested values
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValuePointer(ValuePointerInner);

impl ValuePointer {
    #[inline(always)]
    pub fn owned(value: Value) -> Self {
        Self(ValuePointerInner::Owned(Box::new(value)))
    }

    #[inline(always)]
    pub fn shared(value: InnerValue) -> Self {
        Self(ValuePointerInner::Shared(value))
    }
}

impl AsRef<ValuePointerInner> for ValuePointer {
    fn as_ref(&self) -> &ValuePointerInner {
        &self.0
    }
}

impl AsMut<ValuePointerInner> for ValuePointer {
    fn as_mut(&mut self) -> &mut ValuePointerInner {
        &mut self.0
    }
}

impl Deref for ValuePointer {
    type Target = ValuePointerInner;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ValuePointer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl ValuePointer {
    #[inline(always)]
    pub fn into_inner(&mut self) -> Value {
        let v = std::mem::take(&mut self.0);
        v.into_inner()
    }

    #[inline(always)]
    pub fn into_ownable(&mut self) -> ValuePointer {
        let v = std::mem::take(&mut self.0);
        v.into_ownable()
    }
}

impl Hash for ValuePointer {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.handle()
            .hash_with_tracked_pointers(state, &mut HashSet::new());
    }
}

impl Drop for ValuePointer {
    fn drop(&mut self) {
        let mut stack = vec![self.into_inner()];
        while let Some(value) = stack.pop() {
            match value {
                Value::Map(map) => {
                    stack.extend(map.into_iter().flat_map(|(k, mut v)| vec![k, v.into_inner()]));
                },
                Value::Array(array) => {
                    stack.extend(array.into_iter().map(|mut v| v.into_inner()));
                },
                Value::Optional(Some(mut v)) => {
                    stack.push(v.into_inner());
                },
                Value::Struct(fields, _) => {
                    stack.extend(fields.into_iter().map(|mut v| v.into_inner()));
                },
                Value::Enum(fields, _) => {
                    stack.extend(fields.into_iter().map(|mut v| v.into_inner()));
                }
                _ => {}
            }
        }
    }
}

impl ValuePointerInner {
    // Convert into a owned value
    // Clone if the value is shared and can't be moved
    pub fn into_inner(self) -> Value {
        match self {
            Self::Owned(v) => *v,
            Self::Shared(v) => match Rc::try_unwrap(v.into_inner()) {
                Ok(value) => value.into_inner(),
                Err(rc) => rc.borrow().clone()
            }
        }
    }

    // Clone the Rc value to fully own it
    pub fn into_ownable(self) -> ValuePointer {
        ValuePointer(match self {
            Self::Owned(_) => self,
            Self::Shared(v) => Self::Owned(Box::new(match Rc::try_unwrap(v.into_inner()) {
                Ok(value) => value.into_inner(),
                Err(rc) => rc.borrow().clone()
            }))
        })
    }

    // Transform the value into a shared value
    pub fn transform(&mut self) -> ValuePointer {
        ValuePointer(match self {
            Self::Owned(v) => {
                let dst = std::mem::replace(v, Box::new(Value::Null));
                let shared = Self::Shared(InnerValue::new(*dst));
                *self = shared.clone();
                shared
            },
            Self::Shared(v) => Self::Shared(v.clone())
        })
    }

    // Wrap the value into an handle to be casted to a reference of the value
    pub fn handle<'a>(&'a self) -> ValueHandle<'a> {
        match self {
            Self::Owned(v) => ValueHandle::Borrowed(v),
            Self::Shared(v) => ValueHandle::Ref(v.borrow())
        }
    }

    // Wrap the value into an handle to be casted to a mutable reference of the value
    pub fn handle_mut<'a>(&'a mut self) -> ValueHandleMut<'a> {
        match self {
            Self::Owned(v) => ValueHandleMut::Borrowed(v),
            Self::Shared(v) => ValueHandleMut::RefMut(v.borrow_mut())
        }
    }
}