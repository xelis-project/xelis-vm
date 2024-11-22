mod sub_value;
mod inner;

use std::{
    collections::HashSet,
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
    ptr,
};

use super::ValueCell;

pub use sub_value::SubValue;
pub use inner::ValuePointerInner;

// Value Pointer is a wrapper around the real Value Pointer
// It was introduced to allow to implement a custom Drop to prevent any stackoverflow
// that could happen with huge nested values
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValuePointer(ValuePointerInner);

impl ValuePointer {
    // Create a new value pointer from a value
    #[inline(always)]
    pub fn owned(value: ValueCell) -> Self {
        Self(ValuePointerInner::Owned(Box::new(value)))
    }

    // Create a shared value pointer
    #[inline(always)]
    pub fn shared(value: SubValue) -> Self {
        Self(ValuePointerInner::Shared(value))
    }

    // Get the inner value pointer
    #[inline(always)]
    pub fn get_value_ptr(&self) -> *const ValueCell {
        ptr::from_ref(self.handle().as_value())
    }

    // Take the value, even if it's shared, replace it by Null
    #[inline(always)]
    pub fn take_value(&mut self) -> ValueCell {
        let v = std::mem::take(&mut self.0);
        v.take_value()
    }

    // Clone the inner value
    #[inline(always)]
    pub fn to_value(&self) -> ValueCell {
        self.handle().as_value().clone()
    }
}

#[cfg(not(feature = "value_pointer_drop"))]
impl ValuePointer {
    // Convert into a owned Pointer to fully own the value
    #[inline(always)]
    pub fn into_owned(self) -> ValuePointer {
        self.0.into_owned()
    }

    // Get the owned value or clone it if it's shared
    #[inline(always)]
    pub fn into_value(self) -> ValueCell {
        self.0.into_value()
    }
}

#[cfg(feature = "value_pointer_drop")]
impl ValuePointer {
    // Convert into a owned Pointer to fully own the value
    #[inline(always)]
    pub fn into_owned(&mut self) -> ValuePointer {
        let v = std::mem::take(&mut self.0);
        v.into_owned()
    }

    // Get the owned value or clone it if it's shared
    #[inline(always)]
    pub fn into_value(&mut self) -> Value {
        let v = std::mem::take(&mut self.0);
        v.into_value()
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

impl From<ValueCell> for ValuePointer {
    fn from(value: ValueCell) -> Self {
        Self::owned(value)
    }
}

impl Hash for ValuePointer {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.handle()
            .hash_with_pointers(state, &mut HashSet::new());
    }
}

#[cfg(feature = "value_pointer_drop")]
impl Drop for ValuePointer {
    fn drop(&mut self) {
        let ptr = self.get_value_ptr();
        let mut stack = vec![self.into_value()];

        // Only take the value from the Rc<RefCell>> if it's the same pointer
        fn fetch_value(mut pointer: ValuePointer, ptr: *const Value) -> Value {
            if pointer.get_value_ptr() == ptr {
                pointer.take_value()
            } else {
                pointer.into_value()
            }
        }

        while let Some(value) = stack.pop() {
            match value {
                Value::Map(map) => {
                    for (k, v) in map {
                        stack.push(k);
                        stack.push(fetch_value(v, ptr));
                    }
                },
                Value::Array(array) => {
                    stack.extend(array.into_iter().map(|v| fetch_value(v, ptr)));
                },
                Value::Optional(Some(v)) => {
                    stack.push(fetch_value(v, ptr));
                },
                Value::Struct(fields, _) => {
                    stack.extend(fields.into_iter().map(|v| fetch_value(v, ptr)));
                },
                Value::Enum(fields, _) => {
                    stack.extend(fields.into_iter().map(|v| fetch_value(v, ptr)));
                }
                _ => {}
            }
        }
    }
}