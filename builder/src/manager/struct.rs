use xelis_types::{IdentifierType, StructType, Type};

use super::{Builder, BuilderType, TypeManager};

#[derive(Debug, Clone)]
pub struct StructBuilder<'a> {
    inner: StructType,
    fields_names: Vec<&'a str>
}

pub type StructManager<'a> = TypeManager<'a, Type, StructBuilder<'a>>;

impl<'a> Builder<'a, Type> for StructBuilder<'a> {
    type InnerType = StructType;

    fn new(inner: Self::InnerType, fields_names: Vec<&'a str>) -> Self {
        Self {
            inner,
            fields_names
        }
    }

    fn fields_names(&self) -> &Vec<&'a str> {
        &self.fields_names
    }

    fn inner(&self) -> &StructType {
        &self.inner
    }

    fn into_inner(self) -> Self::InnerType {
        self.inner
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