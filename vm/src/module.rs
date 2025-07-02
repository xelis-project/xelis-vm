use std::{ops::Deref, sync::Arc};

use xelis_bytecode::Module;

// Module with provided metadata
#[derive(Debug)]
pub struct ModuleMetadata<'a, M> {
    pub module: Reference<'a, Module>,
    pub metadata: Reference<'a, M>
}

impl<'a, M> Clone for ModuleMetadata<'a, M> {
    fn clone(&self) -> Self {
        Self {
            module: self.module.clone(),
            metadata: self.metadata.clone()
        }
    }
}

#[derive(Debug)]
pub enum Reference<'a, T> {
    Borrowed(&'a T),
    Shared(Arc<T>),
}

impl<'a, T> Clone for Reference<'a, T> {
    fn clone(&self) -> Self {
        match self {
            Self::Borrowed(v) => Self::Borrowed(v),
            Self::Shared(v) => Self::Shared(v.clone())
        }
    }
}

impl<'a, T> From<Arc<T>> for Reference<'a, T> {
    fn from(value: Arc<T>) -> Self {
        Self::Shared(value)
    }
}

impl<'a, T> From<&'a T> for Reference<'a, T> {
    fn from(value: &'a T) -> Self {
        Self::Borrowed(value)
    }
}

impl<'a, T> Deref for Reference<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match self {
            Reference::Borrowed(v) => v,
            Reference::Shared(v) => v,
        }
    }
}

impl<'a, T> AsRef<T> for Reference<'a, T> {
    fn as_ref(&self) -> &T {
        match self {
            Self::Borrowed(v) => v,
            Self::Shared(v) => v,
        }
    }
}