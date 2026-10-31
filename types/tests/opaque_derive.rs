use silex_types::RuntimeOpaqueWrapper;
use silex_types::OpaqueValue;
use silex_types::{
    DynType, PrimitiveInteger, register_opaque_json,
    traits::{DynType, JSONHelper, JSON_REGISTRY},
    OwnedValue, Opaque, OpaqueWrapper, Primitive, StackValue, RuntimeValue, ValueError, ValuePointer, Heap, HeapError,
};

#[derive(Debug, Clone, Opaque)]
struct Plain;

#[test]
fn opaque_heap_errors_propagate_through_import_clone_and_export() {
    #[derive(Debug, Clone, Opaque)]
    #[opaque(runtime, deep_clone_in = Err::<Self, HeapError>(HeapError::CapacityExceeded)?)]
    struct FailingClone;

    let mut heap = unsafe { Heap::new() };
    let value = RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(FailingClone));
    let copy = value.clone();
    assert!(matches!(heap.detach(copy), Err(HeapError::CapacityExceeded)));
    assert!(matches!(heap.export(value.into()), Err(ValueError::HeapError(HeapError::NonPortableValue))));
    assert!(heap.is_empty());
}

#[derive(Debug, Clone, Opaque)]
#[opaque(
    runtime,
    children = std::iter::once(&self.0),
    deep_clone_in = { let cell = self.0.clone_value(heap); let cell = heap.detach(cell)?; Self(heap.insert(cell)) }
)]
struct RetainedValue(ValuePointer);

#[test]
fn ownership_conversion_detaches_opaque_payloads_at_every_level() {
    let mut heap = unsafe { Heap::new() };
    for nested in [false, true] {
        for mode in 0..4 {
            let original = heap.insert(Primitive::Integer(PrimitiveInteger::U64(1)).into());
            let mut value = RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(
                RetainedValue(original.clone()),
            ));
            if nested {
                value = RuntimeValue::Object(vec![heap.insert(value)]);
            }
            let snapshot = match mode {
                0 => {
                    let copy = value.clone();
                    let detached = heap.detach(copy).unwrap();
                    drop(value);
                    detached
                },
                1 => heap.detach(value).unwrap(),
                2 => StackValue::Owned(value).into_owned(&mut heap).unwrap(),
                3 => StackValue::from(heap.insert(value)).into_owned(&mut heap).unwrap(),
                _ => unreachable!(),
            };
            let retained = if nested {
                snapshot.as_vec().unwrap()[0].as_ref(&heap)
            } else {
                &snapshot
            }
            .as_opaque_type::<RetainedValue>()
            .unwrap().0.clone();
            assert!(!retained.ptr_eq(&original));
            // No payload references are live during this mutation.
            *original.as_mut(&mut heap) = Primitive::Integer(PrimitiveInteger::U64(2)).into();
            assert_eq!(retained.as_ref(&heap).as_u64().unwrap(), 1);
            drop(retained);
            drop(snapshot);
            drop(original);
            assert!(heap.is_empty());
        }
    }
}

#[derive(Debug, Clone, Opaque)]
#[opaque(children = std::iter::once(&self.0 as &dyn OpaqueValue))]
struct OpaqueParent(Plain);

#[test]
fn direct_opaque_children_use_the_same_derive_option() {
    let value = OpaqueParent(Plain);
    let mut children = 0;
    value.visit_children(&mut |_| children += 1);
    assert_eq!(children, 1);
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
    fn serialize_json(&self) -> Result<serde_json::Value, matriochka::Error> {
        Ok(serde_json::json!("custom"))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, DynType)]
#[opaque(name = "Manual")]
struct Manual;

impl JSONHelper for Manual {}
impl Opaque for Manual {
    fn clone_box(&self) -> Box<dyn Opaque> {
        Box::new(self.clone())
    }
}

impl OpaqueValue for Manual {

    fn get_size(&self) -> usize {
        123
    }
}

#[derive(Debug, Clone, serde::Deserialize, Opaque)]
#[opaque(json)]
struct InvalidJson;

impl serde::Serialize for InvalidJson {
    fn serialize<S: serde::Serializer>(&self, _: S) -> Result<S::Ok, S::Error> {
        Err(serde::ser::Error::custom("fixture serialization failure"))
    }
}

#[test]
fn json_serialization_preserves_context_and_cause() {
    let error = InvalidJson.serialize_json().unwrap_err();
    assert!(error.to_string().contains("InvalidJson"));
    assert!(error.downcast_ref::<serde_json::Error>().is_some());
    assert!(format!("{error:#}").contains("fixture serialization failure"));

    let error = serde_json::to_value(OpaqueWrapper::new(InvalidJson)).unwrap_err();
    let diagnostic = error.to_string();
    assert!(diagnostic.contains("InvalidJson"));
    assert!(diagnostic.contains("fixture serialization failure"));
}

#[test]
fn json_deserialization_preserves_context_and_cause() {
    {
        let mut registry = JSON_REGISTRY.write().unwrap();
        register_opaque_json!(registry, "InvalidJson", InvalidJson);
    }

    let error = JSON_REGISTRY.read().unwrap()["InvalidJson"](
        serde_json::json!(true),
    ).unwrap_err();
    assert!(error.to_string().contains("InvalidJson"));
    assert!(error.downcast_ref::<serde_json::Error>().is_some());

    let error = serde_json::from_value::<OpaqueWrapper>(serde_json::json!({
        "type": "InvalidJson",
        "value": true,
    })).unwrap_err();
    let diagnostic = error.to_string();
    assert!(diagnostic.contains("InvalidJson"));
    assert!(diagnostic.contains("invalid type: boolean"));
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

#[test]
fn constant_opaque_conversion_preserves_portable_values() {
    let mut heap = unsafe { Heap::new() };
    let constant = OwnedValue::Opaque(OpaqueWrapper::new(Record(7u64)));
    let value = constant.to_runtime_value(&mut heap);
    assert_eq!(value.as_opaque_type::<Record<u64>>().unwrap().0, 7);
    let output = heap.export(value.into()).unwrap();
    assert_eq!(output.as_opaque_type::<Record<u64>>().unwrap().0, 7);
}
