use std::{borrow::{Borrow, Cow}, ops::{Deref, DerefMut}};

use crate::values::Value;

#[derive(Debug)]
pub enum VarValue<'a> {
    Owned(Value),
    Ref(&'a Value),
    Mut(&'a mut Value)
}

impl<'a> Into<VarValue<'a>> for Value {
    fn into(self) -> VarValue<'a> {
        VarValue::Owned(self)
    }
}

impl<'a> From<&'a mut Value> for VarValue<'a> {
    fn from(value: &'a mut Value) -> Self {
        VarValue::Mut(value)
    }
}

impl<'a> AsMut<Value> for VarValue<'a> {
    fn as_mut(&mut self) -> &mut Value {
        match self {
            Self::Owned(v) => v,
            Self::Ref(v) => {
                *self = VarValue::Owned((**v).clone());
                match self {
                    Self::Owned(v) => v,
                    _ => unreachable!()
                }
            },
            Self::Mut(v) => v,
        }
    }
}

impl<'a> VarValue<'a> {
    pub fn into_owned(self) -> Value {
        match self {
            Self::Owned(v) => v,
            Self::Ref(v) => v.clone(),
            Self::Mut(v) => v.clone(),
        }
    }

    pub fn to_owned(&self) -> Value {
        match self {
            Self::Owned(v) => v.clone(),
            Self::Ref(v) => (*v).clone(),
            Self::Mut(v) => (**v).clone(),
        }
    }

    pub fn into_cow(self) -> Cow<'a, Value> {
        match self {
            Self::Owned(v) => Cow::Owned(v),
            Self::Ref(v) => Cow::Borrowed(v),
            Self::Mut(v) => Cow::Borrowed(v),
        }
    }

    pub fn as_value(&'a self) -> &'a Value {
        match self {
            Self::Owned(v) => v,
            Self::Ref(v) => v,
            Self::Mut(v) => v,
        }
    }

    pub fn as_mut_value(&'a mut self) -> &'a mut Value {
        match self {
            Self::Owned(v) => v,
            Self::Ref(v) => {
                *self = VarValue::Owned((**v).clone());
                match self {
                    Self::Owned(v) => v,
                    _ => unreachable!()
                }
            },
            Self::Mut(v) => v,
        }
    }
}

impl Borrow<Value> for VarValue<'_> {
    fn borrow(&self) -> &Value {
        match self {
            Self::Owned(v) => v,
            Self::Ref(v) => v,
            Self::Mut(v) => v,
        }
    }
}

impl Deref for VarValue<'_> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Owned(v) => v,
            Self::Ref(v) => v,
            Self::Mut(v) => v,
        }
    }
}

impl DerefMut for VarValue<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match self {
            Self::Owned(v) => v,
            Self::Ref(v) => {
                *self = VarValue::Owned((**v).clone());
                match self {
                    Self::Owned(v) => v,
                    _ => unreachable!()
                }
            },
            Self::Mut(v) => v,
        }
    }
}