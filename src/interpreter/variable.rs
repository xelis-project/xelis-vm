use std::{borrow::Cow, cell::RefMut, ops::{Deref, DerefMut}};

use crate::{values::Value, IdentifierType, Reference};

use super::InterpreterError;

pub enum VariablePath<'a> {
    Mut(&'a mut Value),
    RefMut(RefMut<'a, Value>)
}

impl<'a> VariablePath<'a> {
    pub fn get_index_at(self, index: usize) -> Result<VariablePath<'a>, InterpreterError> {
        match self {
            Self::Mut(v) => {
                let values = v.as_mut_vec()?;
                let len = values.len();
                let at_index = values
                    .get_mut(index)
                    .ok_or_else(|| InterpreterError::OutOfBounds(index, len))?;

                Ok(VariablePath::Mut(at_index))
            },
            Self::RefMut(v) => {
                RefMut::filter_map(v, |origin| {
                    let values = origin.as_mut_vec().ok()?;
                    values
                        .get_mut(index)
                }).map(Self::RefMut)
                .map_err(|_| InterpreterError::Unknown)
            }
        }
    }

    pub fn get_reference_sub_variable(self, name: &IdentifierType) -> Result<Reference<'a>, InterpreterError> {
        match self {
            Self::Mut(v) => {
                let values = v.as_mut_map()?;
                let at_index = values
                    .get_mut(name)
                    .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))?;

                Ok(Reference::Borrowed(at_index))
            },
            Self::RefMut(v) => {
                RefMut::filter_map(v, |origin| {
                    let values = origin.as_mut_map().ok()?;
                    values
                        .get_mut(name)
                }).map(Reference::RefMut)
                .map_err(|_| InterpreterError::Unknown)
            }
        }
    }

    pub fn get_sub_variable(self, name: &IdentifierType) -> Result<VariablePath<'a>, InterpreterError> {
        match self {
            Self::Mut(v) => {
                let values = v.as_mut_map()?;
                let at_index = values
                    .get_mut(name)
                    .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))?;

                Ok(VariablePath::Mut(at_index))
            },
            Self::RefMut(v) => {
                RefMut::filter_map(v, |origin| {
                    let values = origin.as_mut_map().ok()?;
                    values
                        .get_mut(name)
                }).map(Self::RefMut)
                .map_err(|_| InterpreterError::Unknown)
            }
        }
    }
}

impl AsRef<Value> for VariablePath<'_> {
    fn as_ref(&self) -> &Value {
        match self {
            Self::Mut(v) => v,
            Self::RefMut(v) => v
        }
    }
}

impl AsMut<Value> for VariablePath<'_> {
    fn as_mut(&mut self) -> &mut Value {
        match self {
            Self::Mut(v) => v,
            Self::RefMut(v) => v
        }
    }
}

impl Deref for VariablePath<'_> {
    type Target = Value;

    fn deref(&self) -> &Self::Target {
        self.as_ref()
    }
}

impl DerefMut for VariablePath<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.as_mut()
    }
}

#[derive(Debug)]
pub enum Variable<'a> {
    Owned(Value),
    Borrowed(&'a Value),
    Mut(&'a mut Value)
}

impl<'a> Variable<'a> {
    pub fn as_value<'b>(&'b self) -> &'b Value {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => v,
            Self::Mut(v) => v
        }
    }

    // If the value is borrowed, we will clone it
    pub fn as_mut(&mut self) -> &mut Value {
        match self {
            Self::Owned(v) => v,
            Self::Borrowed(v) => {
                let v = v.clone();
                *self = Self::Owned(v);
                match self {
                    Self::Owned(v) => v,
                    _ => unreachable!()
                }
            },
            Self::Mut(v) => v
        }
    }
}

impl<'a> From<Value> for Variable<'a> {
    fn from(value: Value) -> Self {
        Self::Owned(value)
    }
}

impl<'a> From<&'a Value> for Variable<'a> {
    fn from(value: &'a Value) -> Self {
        Self::Borrowed(value)
    }
}

impl<'a> From<Cow<'a, Value>> for Variable<'a> {
    fn from(value: Cow<'a, Value>) -> Self {
        match value {
            Cow::Borrowed(v) => Self::Borrowed(v),
            Cow::Owned(v) => Self::Owned(v)
        }
    }
}

impl<'a> From<Reference<'a>> for Variable<'a> {
    fn from(reference: Reference<'a>) -> Self {
        match reference {
            Reference::Owned(v) => Self::Owned(v),
            Reference::Borrowed(v) => Self::Borrowed(v),
            Reference::Ref(v) => Self::Owned(v.clone()),
            Reference::RefMut(v) => Self::Owned(v.clone())
        }
    }
}

impl<'a> From<&'a mut Value> for Variable<'a> {
    fn from(value: &'a mut Value) -> Self {
        Self::Mut(value)
    }
}
