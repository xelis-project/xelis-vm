use silex_types::{traits::DynSerializable, Opaque, OwnedValue, Primitive, PrimitiveRange, U256};

#[derive(Debug, Clone, Opaque)]
struct Unsupported;

#[derive(Debug, Clone, serde::Serialize, Opaque)]
#[opaque(json)]
struct JsonOnly;

#[derive(Debug, Clone, serde::Serialize, Opaque)]
#[opaque(serializable)]
struct BinaryOnly(u32);

impl DynSerializable for BinaryOnly {
    fn get_size(&self) -> usize {
        self.0.get_size()
    }

    fn serialize(&self, buffer: &mut Vec<u8>) -> Result<usize, matriochka::Error> {
        DynSerializable::serialize(&self.0, buffer)
    }
}

#[derive(Debug, Clone, serde::Serialize, Opaque)]
#[opaque(json, serializable)]
struct Both(u32);

impl DynSerializable for Both {
    fn get_size(&self) -> usize {
        self.0.get_size()
    }

    fn serialize(&self, buffer: &mut Vec<u8>) -> Result<usize, matriochka::Error> {
        DynSerializable::serialize(&self.0, buffer)
    }
}

fn assert_capabilities(value: &OwnedValue, binary: bool, json: bool) {
    assert_eq!(
        value.is_serializable(),
        binary,
        "binary capability: {value:?}"
    );
    assert_eq!(
        value.is_json_serializable(),
        json,
        "JSON capability: {value:?}"
    );
}

fn opaque_cases() -> [(OwnedValue, bool, bool); 4] {
    [
        (OwnedValue::from(Unsupported), false, false),
        (OwnedValue::from(JsonOnly), false, true),
        (OwnedValue::from(BinaryOnly(42)), true, false),
        (OwnedValue::from(Both(42)), true, true),
    ]
}

#[test]
fn builtins_support_both_serialization_formats() {
    let values = [
        OwnedValue::default(),
        OwnedValue::from(true),
        OwnedValue::from(String::from("hello")),
        OwnedValue::from(1u8),
        OwnedValue::from(1u16),
        OwnedValue::from(1u32),
        OwnedValue::from(u64::MAX),
        OwnedValue::from(u128::MAX),
        OwnedValue::from(U256::MAX),
        OwnedValue::from(Primitive::Range(Box::new(PrimitiveRange::U64(0..10)))),
        OwnedValue::Bytes(vec![0, 255]),
        OwnedValue::Object(Vec::new()),
        OwnedValue::Map(Default::default()),
    ];
    for value in values {
        assert_capabilities(&value, true, true);
    }
}

#[test]
fn opaque_capabilities_are_independent() {
    for (value, binary, json) in opaque_cases() {
        assert_capabilities(&value, binary, json);
    }
}

#[test]
fn object_capabilities_include_every_nested_element() {
    for (opaque, binary, json) in opaque_cases() {
        for index in 0..3 {
            let mut children = vec![OwnedValue::from(1u64); 3];
            children[index] = OwnedValue::Object(vec![opaque.clone()]);
            assert_capabilities(&OwnedValue::Object(children), binary, json);
        }
    }
}

#[test]
fn map_capabilities_include_nested_keys_and_values() {
    for (opaque, binary, json) in opaque_cases() {
        // Nested structured keys must be checked as well as direct opaque keys.
        for key in [opaque.clone(), OwnedValue::Object(vec![opaque.clone()])] {
            let map = OwnedValue::Map([(key, OwnedValue::from(1u64))].into_iter().collect());
            assert_capabilities(&map, binary, json);
        }
        let map = OwnedValue::Map(
            [
                (OwnedValue::from(1u8), OwnedValue::from(1u64)),
                (OwnedValue::from(2u8), OwnedValue::Object(vec![opaque])),
            ]
            .into_iter()
            .collect(),
        );
        assert_capabilities(&map, binary, json);
    }
}

#[test]
fn all_map_entries_must_support_the_requested_format() {
    let map = OwnedValue::Map(
        [
            (OwnedValue::from(1u8), OwnedValue::from(BinaryOnly(42))),
            (OwnedValue::from(2u8), OwnedValue::from(JsonOnly)),
            (OwnedValue::from(3u8), OwnedValue::from(Both(42))),
        ]
        .into_iter()
        .collect(),
    );
    assert_capabilities(&map, false, false);
}

#[test]
fn deeply_nested_capability_checks_are_iterative() {
    for (mut value, binary, json) in opaque_cases() {
        for _ in 0..10_000 {
            value = OwnedValue::Object(vec![value]);
        }
        // Avoid formatting this tree on the success path.
        assert_eq!(value.is_serializable(), binary);
        assert_eq!(value.is_json_serializable(), json);
    }
}

#[test]
fn capability_checks_do_not_run_serializers() {
    #[derive(Debug, Clone, Opaque)]
    #[opaque(json, serializable)]
    struct PanickingSerializers;

    impl DynSerializable for PanickingSerializers {
        fn get_size(&self) -> usize {
            panic!("capability checks must not calculate serialized size");
        }

        fn serialize(&self, _: &mut Vec<u8>) -> Result<usize, matriochka::Error> {
            panic!("capability checks must not run binary serialization");
        }
    }

    impl serde::Serialize for PanickingSerializers {
        fn serialize<S: serde::Serializer>(&self, _: S) -> Result<S::Ok, S::Error> {
            panic!("capability checks must not run JSON serialization");
        }
    }

    assert_capabilities(&OwnedValue::from(PanickingSerializers), true, true);
}

#[test]
fn runtime_capabilities_include_owned_keys_and_shared_runtime_children() {
    use silex_types::{Heap, RuntimeValue};

    for (opaque, binary, json) in opaque_cases() {
        // SAFETY: the heap and every runtime handle remain on this thread.
        let mut heap = unsafe { Heap::new() };
        let key = OwnedValue::Object(vec![opaque.clone()]);
        let child = heap.import_owned(opaque);
        let child = heap.insert(child);
        let object = heap.insert(RuntimeValue::Object(vec![child.clone(), child]));
        let map = RuntimeValue::Map(Box::new([(key, object)].into_iter().collect()));
        let slots = heap.len();
        assert_eq!(map.is_serializable(&heap), binary);
        assert_eq!(map.is_json_serializable(&heap), json);
        assert_eq!(
            heap.len(),
            slots,
            "capability checks must not allocate runtime slots"
        );
    }
}

#[test]
fn runtime_capabilities_do_not_require_an_owned_snapshot() {
    use silex_types::{Heap, RuntimeOpaqueWrapper, RuntimeValue};

    #[derive(Debug, Clone, serde::Serialize, Opaque)]
    #[opaque(runtime, json)]
    struct LocalJson(u32);

    // SAFETY: the heap and runtime opaque stay local and are used sequentially.
    let heap = unsafe { Heap::new() };
    let value = RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(LocalJson(42)));
    assert!(
        heap.snapshot(&value).is_err(),
        "runtime-only opaques cannot be exported"
    );
    assert!(!value.is_serializable(&heap));
    assert!(value.is_json_serializable(&heap));
}

#[test]
fn deep_runtime_capability_checks_are_iterative() {
    use silex_types::{Heap, RuntimeValue};

    // SAFETY: every handle remains in this sequential, acyclic runtime graph.
    let mut heap = unsafe { Heap::new() };
    let mut value = RuntimeValue::from(Both(42));
    for _ in 0..10_000 {
        let child = heap.insert(value);
        value = RuntimeValue::Object(vec![child]);
    }
    assert!(value.is_serializable(&heap));
    assert!(value.is_json_serializable(&heap));
}
