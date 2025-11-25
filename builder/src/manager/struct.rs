use std::borrow::Cow;

use xelis_types::{IdentifierType, StructType, Type};

use crate::Builder;

use super::TypeManager;

#[derive(Debug, Clone)]
pub struct StructBuilder {
    inner: StructType
}

impl Builder for StructBuilder {
    type Data = (Vec<Cow<'static, str>>, Vec<(Cow<'static, str>, Type)>);
    type Type = StructType;

    fn build_with(id: IdentifierType, name: impl Into<Cow<'static, str>>, data: Self::Data) -> Self {
        let (generics, fields) = data;
        Self {
            inner: if generics.is_empty() {
                StructType::new(id, name, fields)
            } else {
                StructType::new_with_generics(id, name, generics, fields)
            }
        }
    }

    fn get_type(&self) -> &Self::Type {
        &self.inner
    }

    fn names<'a>(&'a self) -> impl Iterator<Item = &'a str> {
        self.inner.fields().iter().map(|(k, _)| k.as_ref())
    }

    fn to_type(&self) -> Self::Type {
        self.inner.clone()
    }

    fn type_id(&self) -> IdentifierType {
        self.inner.id()
    }
}

pub type StructManager<'a> = TypeManager<'a, StructBuilder>;