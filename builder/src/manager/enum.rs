use xelis_types::{EnumType, EnumVariant, IdentifierType};
use super::{Builder, BuilderType, TypeManager};

#[derive(Debug, Clone)]
pub struct EnumBuilder<'a> {
    inner: EnumType,
    fields_names: Vec<&'a str>
}

pub type EnumManager<'a> = TypeManager<'a, EnumVariant, EnumBuilder<'a>>;

impl<'a> Builder<'a, EnumVariant> for EnumBuilder<'a> {
    type InnerType = EnumType;

    fn new(inner: Self::InnerType, fields_names: Vec<&'a str>) -> Self {
        Self {
            inner,
            fields_names
        }
    }

    fn fields_names(&self) -> &Vec<&'a str> {
        &self.fields_names
    }

    fn inner(&self) -> &EnumType {
        &self.inner
    }

    fn into_inner(self) -> Self::InnerType {
        self.inner
    }
}

impl BuilderType<EnumVariant> for EnumType {
    fn with(id: IdentifierType, fields: Vec<EnumVariant>) -> Self {
        EnumType::new(id, fields)
    }

    fn type_id(&self) -> IdentifierType {
        self.id()
    }
}