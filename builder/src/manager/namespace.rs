use xelis_types::{IdentifierType, NamespaceType, Type};
use super::{Builder, BuilderType, TypeManager};

#[derive(Debug, Clone)]
pub struct NamespaceBuilder<'a> {
    inner: NamespaceType,
    path_segments: Vec<&'a str>,
}

pub type NamespaceManager<'a> = TypeManager<'a, NamespaceBuilder<'a>>;

impl<'a> Builder<'a> for NamespaceBuilder<'a> {
    type Data = String;
    type BuilderType = NamespaceType;
    type Type = NamespaceType;

    fn new(inner: Self::BuilderType, path_segments: Vec<&'a str>) -> Self {
        Self { inner, path_segments }
    }

    fn names(&self) -> &Vec<&'a str> {
        &self.path_segments
    }

    fn builder_type(&self) -> &Self::BuilderType {
        &self.inner
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

impl BuilderType<String> for NamespaceType {
    fn with(id: IdentifierType, data: Vec<String>) -> Self {
        NamespaceType::new(id, data)
    }

    fn type_id(&self) -> IdentifierType {
        self.id()
    }
}