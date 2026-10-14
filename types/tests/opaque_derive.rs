use silex_types::{
    traits::{DynType, JSONHelper},
    Opaque, OpaqueWrapper,
};

#[derive(Debug, Clone, Opaque)]
struct Plain;

#[derive(Debug, Clone, Opaque)]
#[opaque(children = std::iter::once(&self.0 as &dyn Opaque))]
struct OpaqueParent(Plain);

#[test]
fn direct_opaque_children_use_the_same_derive_option() {
    let value = silex_types::ValueCell::Primitive(silex_types::Primitive::Opaque(
        OpaqueWrapper::new(OpaqueParent(Plain)),
    ));
    assert_eq!(value.calculate_depth(1).unwrap(), 1);
    assert!(matches!(
        value.calculate_depth(0),
        Err(silex_types::ValueError::MaxDepthReached)
    ));
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, Opaque)]
#[opaque(name = "Record", display, json, hash, eq, size = 4)]
struct Record<T>(T)
where
    T: PartialEq;

impl<T: std::fmt::Display + PartialEq> std::fmt::Display for Record<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "record:{}", self.0)
    }
}

#[derive(Debug, Clone, Opaque)]
#[opaque(serializable)]
struct Binary(u32);

impl octet::Serializable for Binary {
    fn size(&self) -> usize {
        octet::Serializable::size(&self.0)
    }
    fn write<W: octet::Writable>(
        &self,
        writer: &mut octet::Writer<W>,
    ) -> Result<(), matriochka::Error> {
        octet::Serializable::write(&self.0, writer)
    }
    fn read<R: octet::Readable>(reader: &mut octet::Reader<R>) -> Result<Self, matriochka::Error> {
        octet::Serializable::read(reader).map(Self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Opaque)]
#[opaque(skip_json, crate = "::silex_types")]
enum CustomJson {
    Value,
}

impl JSONHelper for CustomJson {
    fn is_json_supported(&self) -> bool {
        true
    }
    fn serialize_json(&self) -> anyhow::Result<serde_json::Value> {
        Ok(serde_json::json!("custom"))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, silex_types::DynType)]
#[opaque(name = "Manual")]
struct Manual;

impl JSONHelper for Manual {}
impl Opaque for Manual {
    fn clone_box(&self) -> Box<dyn Opaque> {
        Box::new(self.clone())
    }
    fn get_size(&self) -> usize {
        123
    }
}

#[test]
fn default_capabilities_and_identity() {
    let opaque = OpaqueWrapper::new(Plain);
    assert_eq!(opaque.inner().get_type_name(), "Plain");
    assert_eq!(opaque.get_type_id(), std::any::TypeId::of::<Plain>());
    assert_eq!(opaque.to_string(), "Opaque(Plain)");
    assert!(!opaque.is_serializable());
    assert!(!opaque.is_json_serializable());
    assert!(opaque.as_hashable().is_none());
    assert!(opaque.as_eq().is_none());
    assert!(!opaque.is_hashable());
    assert_eq!(opaque, opaque);
    assert_ne!(opaque.clone(), opaque);
}

#[test]
fn selected_capabilities_on_generic_type() {
    let opaque = OpaqueWrapper::new(Record(42u32));
    assert_eq!(opaque.inner().get_type_name(), "Record");
    assert_eq!(opaque.get_type_id(), std::any::TypeId::of::<Record<u32>>());
    assert_eq!(opaque.to_string(), "record:42");
    assert_eq!(opaque.get_size(), 4);
    assert!(opaque.is_json_serializable());
    assert_eq!(
        opaque.inner().serialize_json().unwrap(),
        serde_json::json!(42)
    );
    assert_eq!(opaque.clone(), opaque);
    assert!(opaque.is_hashable());
    assert!(opaque.as_eq().is_some());
    use std::hash::{Hash, Hasher};
    let hash = |value: &OpaqueWrapper| {
        let mut hasher = std::hash::DefaultHasher::new();
        value.hash(&mut hasher);
        hasher.finish()
    };
    assert_eq!(hash(&opaque), hash(&opaque.clone()));
    let mut map = std::collections::HashMap::new();
    map.insert(opaque, "value");
    assert_eq!(map.get(&OpaqueWrapper::new(Record(42u32))), Some(&"value"));
    assert_ne!(
        OpaqueWrapper::new(Record(42u32)),
        OpaqueWrapper::new(Record(43u32))
    );
}

#[test]
fn map_keys_require_both_capabilities() {
    #[derive(Debug, Clone, Hash, Opaque)]
    #[opaque(hash)]
    struct HashOnly;
    #[derive(Debug, Clone, PartialEq, Eq, Opaque)]
    #[opaque(eq)]
    struct EqOnly;

    let hash_only = OpaqueWrapper::new(HashOnly);
    assert!(hash_only.as_hashable().is_some());
    assert!(hash_only.as_eq().is_none());
    assert!(!hash_only.is_hashable());
    assert_ne!(hash_only, hash_only.clone());

    let eq_only = OpaqueWrapper::new(EqOnly);
    assert!(eq_only.as_eq().is_some());
    assert!(eq_only.as_hashable().is_none());
    assert!(!eq_only.is_hashable());
    assert_eq!(eq_only, eq_only.clone());
}

#[test]
fn binary_capability_uses_octet_adapter() {
    let opaque = OpaqueWrapper::new(Binary(42));
    let mut bytes = vec![99];
    assert_eq!(
        opaque
            .as_serializable()
            .unwrap()
            .serialize(&mut bytes)
            .unwrap(),
        4
    );
    assert_eq!(
        &bytes[1..],
        octet::Serializable::to_bytes(&42u32).unwrap().as_ref()
    );
    assert_eq!(bytes[0], 99);
    assert_eq!(opaque.get_size(), 4);
}

#[test]
fn custom_implementations_are_preserved() {
    assert_eq!(
        CustomJson::Value.serialize_json().unwrap(),
        serde_json::json!("custom")
    );
    let opaque = OpaqueWrapper::new(Manual);
    assert_eq!(opaque.inner().get_type_name(), "Manual");
    assert_eq!(opaque.get_size(), 123);
    assert_eq!(Manual.get_type(), std::any::TypeId::of::<Manual>());
}
