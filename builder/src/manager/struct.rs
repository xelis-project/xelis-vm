use xelis_types::{IdentifierType, StructType, Type};

use super::{Builder, BuilderType, TypeManager};

#[derive(Debug, Clone)]
pub struct StructBuilder<'a> {
    inner: StructType,
    fields_names: Vec<&'a str>
}

pub type StructManager<'a> = TypeManager<'a, StructBuilder<'a>>;

impl<'a> Builder<'a> for StructBuilder<'a> {
    type Data = Type;
    type BuilderType = StructType;
    type Type = StructType;

    fn new(inner: Self::BuilderType, fields_names: Vec<&'a str>) -> Self {
        Self {
            inner,
            fields_names
        }
    }

    fn builder_type(&self) -> &Self::BuilderType {
        &self.inner
    }

    fn names(&self) -> &Vec<&'a str> {
        &self.fields_names
    }

    fn get_type(&self) -> &Self::Type {
        &self.inner
    }

    fn to_type(&self) -> Self::Type {
        self.inner.clone()
    }

    fn type_id(&self) -> IdentifierType {
        self.inner.id()
    }
}

impl BuilderType<Type> for StructType {
    fn with(id: IdentifierType, fields: Vec<Type>) -> Self {
        StructType::new(id, fields)
    }

    fn type_id(&self) -> IdentifierType {
        self.id()
    }
}