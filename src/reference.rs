use std::{borrow::Borrow, cell::Ref, fmt::{Display, Formatter}, ops::Deref};

use crate::values::Value;

pub enum Reference<'a> {
    Owned(Value),
    Borrowed(&'a Value),
    Ref(Ref<'a, Value>),
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

    /// If the value is borrowed, we will clone it
    pub fn to_mut(&mut self) -> &mut Value {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => {
                let v = v.clone();
                *self = Self::Owned(v);
                match self {
                    Self::Owned(v) => v,
                    _ => unreachable!(),
                }
            }
            Self::Ref(v) => {
                let v = v.clone();
                *self = Self::Owned(v);
                match self {
                    Self::Owned(v) => v,
                    _ => unreachable!(),
                }
            }
        }
    }

    /// Convert the value to owned
    pub fn into_owned(self) -> Value {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v.clone(),
            Self::Ref(v) => v.clone(),
        }
    }
}

impl AsRef<Value> for Reference<'_> {
    fn as_ref(&self) -> &Value {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v,
            Self::Ref(v) => v,
        }
    }
}

impl Deref for Reference<'_> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl Borrow<Value> for Reference<'_> {
    fn borrow(&self) -> &Value {
        self.as_ref()
    }
}

impl Display for Reference<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_ref())
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use super::*;

    #[test]
    fn test_as_ref() {
        let value = Value::Null;
        let reference = Reference::Owned(value.clone());
        assert_eq!(reference.as_ref(), &value);

        let reference = Reference::Borrowed(&value);
        assert_eq!(reference.as_ref(), &value);

        let cell = RefCell::new(value.clone());
        let borrow = cell.borrow();
        let reference = Reference::Ref(borrow);
        assert_eq!(reference.as_ref(), &value);
    }
}