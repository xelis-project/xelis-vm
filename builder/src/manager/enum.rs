use xelis_types::{EnumType, EnumVariant, IdentifierType, Type};
use super::{Builder, BuilderType, TypeManager};

#[derive(Debug)]
pub struct EnumBuilder<'a> {
    inner: EnumTypeBuilder<'a>,
    variants_names: Vec<&'a str>
}

#[derive(Debug)]
pub struct EnumTypeBuilder<'a> {
    inner: EnumType,
    variants: Vec<EnumVariantBuilder<'a>>
}

impl<'a> EnumBuilder<'a> {

    // This function is used to get the variant by its id
    pub fn get_variant_by_id(&'a self, id: u8) -> Option<&'a EnumVariantBuilder<'a>> {
        self.inner.variants.get(id as usize)
    }

    // This function is used to get the variant and its id by its name
    pub fn get_variant_by_name(&'a self, name: &str) -> Option<(u8, &'a EnumVariantBuilder<'a>)> {
        self.variants_names.iter()
            .position(|n| n == &name)
            .map(|i| (i as u8, &self.inner.variants[i]))
    }
}

impl EnumTypeBuilder<'_> {
    pub fn variants(&self) -> &Vec<EnumVariantBuilder> {
        &self.variants
    }
}

pub type EnumVariantBuilder<'a> = Vec<(&'a str, Type)>;

pub type EnumManager<'a> = TypeManager<'a, EnumBuilder<'a>>;

impl<'a> Builder<'a> for EnumBuilder<'a> {
    type Data = EnumVariantBuilder<'a>;
    type BuilderType = EnumTypeBuilder<'a>;
    type Type = EnumType;

    fn new(inner: Self::BuilderType, variants_names: Vec<&'a str>) -> Self {
        Self {
            inner,
            variants_names
        }
    }

    fn names(&self) -> &Vec<&'a str> {
        &self.variants_names
    }

    fn builder_type(&self) -> &Self::BuilderType {
        &self.inner
    }

    fn get_type(&self) -> &Self::Type {
        &self.inner.inner
    }

    fn to_type(&self) -> Self::Type {
        self.inner.inner.clone()
    }

    fn type_id(&self) -> IdentifierType {
        self.inner.inner.id()
    }
}

impl<'a> BuilderType<EnumVariantBuilder<'a>> for EnumTypeBuilder<'a> {
    fn with(id: IdentifierType, variants: Vec<EnumVariantBuilder<'a>>) -> Self {
        let types = variants.iter()
            .map(|v| EnumVariant::new(v.iter()
                .map(|(_, t)| t.clone())
                .collect()
            ).clone())
            .collect();

        Self {
            inner: EnumType::new(id, types),
            variants
        }
    }

    fn type_id(&self) -> IdentifierType {
        self.inner.id()
    }
}