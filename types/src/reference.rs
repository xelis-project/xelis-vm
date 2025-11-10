use std::{borrow::Cow, ops::Deref, sync::Arc};

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

impl<'a, T: ToOwned> From<Cow<'a, T>> for Reference<'a, T>
where
    T::Owned: Into<T>,
{
    fn from(value: Cow<'a, T>) -> Self {
        match value {
            Cow::Borrowed(v) => Self::Borrowed(v),
            Cow::Owned(v) => Self::Shared(Arc::new(v.into())),
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