use std::borrow::Cow;

use xelis_types::{EnumType, EnumVariant, IdentifierType};
use super::{Builder, TypeManager};

#[derive(Debug)]
pub struct EnumBuilder {
    inner: EnumType
}

impl EnumBuilder {
    // This function is used to get the variant by its id
    pub fn get_variant_by_id<'a>(&'a self, id: u8) -> Option<&'a (Cow<'static, str>, EnumVariant)> {
        self.inner.get_variant(id)
    }

    pub fn get_variant_by_name(&self, name: &str) -> Option<(u8, &EnumVariant)> {
        self.inner.variants()
            .iter()
            .enumerate()
            .find(|(_, (k, _))| k == name)
            .map(|(index, (_, v))| (index as u8, v))
    }

    pub fn variants(&self) -> &Vec<(Cow<'static, str>, EnumVariant)> {
        &self.inner.variants()
    }
}

pub type EnumManager<'a> = TypeManager<'a, EnumBuilder>;

impl Builder for EnumBuilder {
    type Data = Vec<(Cow<'static, str>, EnumVariant)>;
    type Type = EnumType;

    fn build_with(id: IdentifierType, name: impl Into<Cow<'static, str>>, data: Self::Data) -> Self {
        Self {
            inner: EnumType::new(id, name, data)
        }
    }

    fn names<'a>(&'a self) -> impl Iterator<Item = &'a str> {
        self.inner.variants().iter().map(|(name, _)| name.as_ref())
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