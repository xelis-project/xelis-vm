pub mod traits;

use std::{
    any::TypeId,
    borrow::Cow,
    collections::HashSet,
    fmt,
    hash::{Hash, Hasher}
};
use schemars::*;
use serde::{Deserialize, Serialize, ser::SerializeStruct};
use serde_json::Value;
use crate::{IdentifierType, ValueError};
use traits::*;
use super::Type;

/// Type-level markers that can be declared on an [`OpaqueType`] descriptor.
/// These drive static type-checking behaviour (method-dispatch compatibility,
/// etc.) and are **not** related to the runtime `dyn Opaque` value traits.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OpaqueTypeTrait {
    /// The type is an iterable container (e.g. `Iterator<T>`).  Any
    /// `Array<T>`, `Range<T>`, or other `Iterable` opaque with a compatible
    /// element type may substitute for it during method-dispatch matching.
    Iterable,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct OpaqueTypeInner {
    id: IdentifierType,
    name: Cow<'static, str>,
    allow_external_input: bool,
    /// How many generic parameters this type accepts (0 = non-generic template)
    #[serde(default)]
    generics_count: u8,
    /// Concrete generic parameters when instantiated (empty for template)
    #[serde(default)]
    generics: Vec<Type>,
    /// Type-level trait markers declared on this opaque type descriptor.
    #[serde(default, skip_serializing_if = "HashSet::is_empty")]
    traits: HashSet<OpaqueTypeTrait>,
}

impl PartialEq for OpaqueTypeInner {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.generics == other.generics
    }
}
impl Eq for OpaqueTypeInner {}

impl Hash for OpaqueTypeInner {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.generics.hash(state);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct OpaqueType(OpaqueTypeInner);

impl OpaqueType {
    pub fn new(id: IdentifierType, name: &'static str) -> Self {
        Self::with(id, name, false)
    }

    pub fn with(id: IdentifierType, name: &'static str, allow_external_input: bool) -> Self {
        Self(OpaqueTypeInner {
            id,
            name: name.into(),
            allow_external_input,
            generics_count: 0,
            generics: Vec::new(),
            traits: HashSet::new(),
        })
    }

    /// Create a generic template that accepts `generics_count` type parameters.
    pub fn with_generics_count(id: IdentifierType, name: &'static str, allow_external_input: bool, generics_count: u8) -> Self {
        Self(OpaqueTypeInner {
            id,
            name: name.into(),
            allow_external_input,
            generics_count,
            generics: Vec::new(),
            traits: HashSet::new(),
        })
    }

    /// Instantiate this (template) OpaqueType with concrete type arguments.
    pub fn with_generics(&self, generics: Vec<Type>) -> Self {
        Self(OpaqueTypeInner {
            id: self.0.id,
            name: self.0.name.clone(),
            allow_external_input: self.0.allow_external_input,
            generics_count: self.0.generics_count,
            generics,
            traits: self.0.traits.clone(),
        })
    }

    /// Builder: add a type-level trait marker to this opaque type descriptor.
    /// Duplicate entries are silently ignored.
    pub fn with_trait(mut self, t: OpaqueTypeTrait) -> Self {
        self.0.traits.insert(t);
        self
    }

    /// Check whether this opaque type has the given type-level trait marker.
    pub fn has_trait(&self, t: OpaqueTypeTrait) -> bool {
        self.0.traits.contains(&t)
    }

    /// Convenience: whether this type carries the [`OpaqueTypeTrait::Iterable`] marker.
    pub fn is_iterable(&self) -> bool {
        self.has_trait(OpaqueTypeTrait::Iterable)
    }

    pub fn id(&self) -> IdentifierType {
        self.0.id
    }

    pub fn name(&self) -> &str {
        &self.0.name
    }

    pub fn allow_external_input(&self) -> bool {
        self.0.allow_external_input
    }

    /// How many generic parameters the type declares (0 = non-generic).
    pub fn generics_count(&self) -> u8 {
        self.0.generics_count
    }

    /// The concrete generic parameters (empty when this is a template).
    pub fn generics(&self) -> &[Type] {
        &self.0.generics
    }
}

impl fmt::Display for OpaqueType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.name)?;
        if !self.0.generics.is_empty() {
            write!(f, "<")?;
            for (i, g) in self.0.generics.iter().enumerate() {
                if i > 0 { write!(f, ", ")?; }
                write!(f, "{}", g)?;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

impl Serialize for OpaqueType {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}

impl<'a> Deserialize<'a> for OpaqueType {
    fn deserialize<D: serde::Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(Self(OpaqueTypeInner::deserialize(deserializer)?))
    }
}

pub trait Opaque: DynType + DynHash + DynEq + JSONHelper + Serializable + fmt::Debug + Sync + Send {
    fn clone_box(&self) -> Box<dyn Opaque>;

    fn display(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Opaque")
    }

    fn validate(&self) -> Result<(), ValueError> {
        Ok(())
    }
}

impl Hash for dyn Opaque {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.dyn_hash(state);
    }
}

/// An Opaque value that can be used to store any type
/// This allow environments to provide custom types to the VM
#[derive(Debug, Hash, JsonSchema)]
pub struct OpaqueWrapper {
    #[schemars(skip)]
    inner: Box<dyn Opaque>
}

impl PartialEq for OpaqueWrapper {
    fn eq(&self, other: &Self) -> bool {
        let other = other.inner.as_eq();
        self.inner.as_ref()
            .is_equal(other)
    }
}

impl Eq for OpaqueWrapper {}

impl OpaqueWrapper {
    /// Create a new OpaqueWrapper
    /// OpaqueType id is the index of the type in the OpaqueManager
    #[inline]
    pub fn new<T: Opaque>(value: T) -> Self {
        Self {
            inner: Box::new(value)
        }
    }

    /// Get the TypeId of the OpaqueWrapper
    #[inline]
    pub fn get_type_id(&self) -> TypeId {
        self.inner.get_type()
    }

    // Downcast the OpaqueWrapper to a specific type
    pub fn as_ref<T: Opaque>(&self) -> Result<&T, ValueError> {
        (*self.inner).as_any()
            .downcast_ref::<T>()
            .ok_or(ValueError::InvalidOpaqueTypeMismatch)
    }

    // Downcast the OpaqueWrapper to a specific type
    pub fn as_mut<T: Opaque>(&mut self) -> Result<&mut T, ValueError> {
        (*self.inner).as_any_mut()
            .downcast_mut::<T>()
            .ok_or(ValueError::InvalidOpaqueTypeMismatch)
    }

    // Downcast the OpaqueWrapper to a specific type
    pub fn into_inner<T: Opaque>(self) -> Result<T, ValueError> {
        self.inner.into_any()
            .downcast::<T>()
            .map(|value| *value)
            .map_err(|_| ValueError::InvalidOpaqueTypeMismatch)
    }

    #[inline]
    pub fn is_serializable(&self) -> bool {
        self.inner.is_serializable()
    }

    #[inline]
    pub fn is_json_serializable(&self) -> bool {
        self.inner.is_json_supported()
    }

    #[inline]
    pub fn is_hashable(&self) -> bool {
        self.inner.as_ref().is_hashable()
    }

    #[inline]
    pub fn get_size(&self) -> usize {
        self.inner.get_size()
    }

    #[inline]
    pub fn inner(&self) -> &dyn Opaque {
        self.inner.as_ref()
    }

    #[inline]
    pub fn validate(&self) -> Result<(), ValueError> {
        self.inner.validate()
    }
}

impl Serialize for OpaqueWrapper {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut structure = serializer.serialize_struct("OpaqueWrapper", 2)?;
        structure.serialize_field("type", &self.inner.get_type_name())?;

        let value = if self.inner.is_json_supported() {
            self.inner.serialize_json()
                .map_err(serde::ser::Error::custom)?
        } else {
            Value::String("JSON not supported".to_owned())
        };

        structure.serialize_field("value", &value)?;
        structure.end()
    }
}

impl<'a> Deserialize<'a> for OpaqueWrapper {
    fn deserialize<D: serde::Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        #[derive(Deserialize)]
        struct OpaqueWrapperData {
            #[serde(rename = "type")]
            type_: String,
            value: Value,
        }

        let data = OpaqueWrapperData::deserialize(deserializer)?;
        let type_name = data.type_;
        let value = data.value;

        let registry = JSON_REGISTRY.read()
            .map_err(serde::de::Error::custom)?;

        let deserialize_fn = registry.get(&type_name.as_str())
            .ok_or(serde::de::Error::custom(format!("Unknown type '{}' for registry", type_name)))?;

        deserialize_fn(value)
            .map_err(serde::de::Error::custom)
    }
}

impl Clone for OpaqueWrapper {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone_box(),
        }
    }
}

impl<T: Opaque> From<T> for OpaqueWrapper {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl fmt::Display for OpaqueWrapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.display(f)
    }
}

#[cfg(test)]
mod tests {
    use std::{any::TypeId, hash::DefaultHasher};
    use indexmap::IndexMap;

    use crate::{
        impl_opaque,
        register_opaque_json,
        Primitive,
        Type,
        ValueCell
    };
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
    struct CustomOpaque {
        value: i32,
    }

    impl_opaque!(
        "CustomOpaque",
        CustomOpaque,
        display,
        json
    );

    impl fmt::Display for CustomOpaque {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "CustomOpaque({})", self.value)
        }
    }

    impl Serializable for CustomOpaque {
        fn get_size(&self) -> usize {
            4
        }
    }

    #[test]
    fn test_opaque_wrapper() {
        let opaque = OpaqueWrapper::new(CustomOpaque { value: 42 });
        assert_eq!(opaque.to_string(), "CustomOpaque(42)");
        assert_eq!(opaque.get_type_id(), TypeId::of::<CustomOpaque>());

        // Test hashing
        let mut hasher = DefaultHasher::new();
        opaque.hash(&mut hasher);

        // Test equality
        let opaque2 = opaque.clone();
        assert_eq!(opaque, opaque2);
    }

    #[test]
    fn test_opaque_serde() {
        {
            let mut registry = JSON_REGISTRY.write().unwrap();
            register_opaque_json!(registry, "CustomOpaque", CustomOpaque);
        }

        let opaque = OpaqueWrapper::new(CustomOpaque { value: 42 });
        let json = serde_json::to_string(&opaque).unwrap();
        assert_eq!(json, r#"{"type":"CustomOpaque","value":{"value":42}}"#);

        let opaque2: OpaqueWrapper = serde_json::from_str(&json).unwrap();
        assert_eq!(opaque, opaque2);
    }

    #[test]
    fn test_opaque_type_serde() {
        let opaque_type = OpaqueType::new(42, "Foo");
        let json = serde_json::to_string(&opaque_type).unwrap();
        assert_eq!(json, r#"{"id":42,"name":"Foo","allow_external_input":false,"generics_count":0,"generics":[]}"#);

        let opaque_type2: OpaqueType = serde_json::from_str(&json).unwrap();
        assert_eq!(opaque_type, opaque_type2);

        // Backwards-compat: old format (no generics fields) must still deserialise.
        let old_json = r#"{"id":42,"name":"Foo","allow_external_input":false}"#;
        let opaque_type3: OpaqueType = serde_json::from_str(old_json).unwrap();
        assert_eq!(opaque_type, opaque_type3);

        let opaque_type = Type::Opaque(OpaqueType::new(42, "Foo"));
        let json = serde_json::to_string(&opaque_type).unwrap();
        assert_eq!(json, r#"{"type":"opaque","value":{"id":42,"name":"Foo","allow_external_input":false,"generics_count":0,"generics":[]}}"#);
    }

    #[test]
    fn test_opaque_downcast() {
        let mut opaque = OpaqueWrapper::new(CustomOpaque { value: 42 });
        let _: &CustomOpaque = opaque.as_ref().unwrap();
        let _: &mut CustomOpaque = opaque.as_mut().unwrap();
        let _: CustomOpaque = opaque.into_inner().unwrap();
    }

    #[test]
    fn test_map_opaque_as_key() {
        {
            let mut registry = JSON_REGISTRY.write().unwrap();
            register_opaque_json!(registry, "CustomOpaque", CustomOpaque);
        }

        let opaque = OpaqueWrapper::new(CustomOpaque { value: 42 });
        let mut map = IndexMap::new();
        map.insert(ValueCell::Primitive(Primitive::Opaque(opaque)), ValueCell::Primitive(Primitive::String("hello world".to_owned())).into());
        map.insert(ValueCell::Primitive(Primitive::String("key test".to_owned())), ValueCell::Primitive(Primitive::String("hello world 2".to_owned())).into());

        let map = ValueCell::Map(Box::new(map));
        let json = serde_json::to_string(&map).unwrap();
        let map2: ValueCell = serde_json::from_str(&json).unwrap();

        assert_eq!(map, map2);
    }
}