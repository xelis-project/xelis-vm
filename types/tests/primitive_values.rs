use std::hash::{Hash, Hasher};

use indexmap::IndexMap;
use matriochka::ResultExt as _;
use serde_json::{json, Value};
use silex_types::{
    traits::JSON_REGISTRY, Opaque, OpaqueWrapper, Primitive,
    PrimitiveInteger, PrimitiveRange, Type, OwnedValue, ValueError, U256,
};

#[derive(Default)]
struct HashBytes(Vec<u8>);
impl Hasher for HashBytes {
    fn finish(&self) -> u64 {
        0
    }
    fn write(&mut self, bytes: &[u8]) {
        self.0.extend_from_slice(bytes);
    }
}
fn hash_bytes(value: &impl Hash) -> Vec<u8> {
    let mut hasher = HashBytes::default();
    value.hash(&mut hasher);
    hasher.0
}

#[test]
fn integers_and_ranges_serialize_current_variants_and_keep_costs() {
    macro_rules! check {
        ($variant:ident, $start:expr, $end:expr, $width:expr, $tag:expr, $wire_start:expr, $wire_end:expr) => {{
            let start = $start;
            let end = $end;
            let integer = PrimitiveInteger::$variant(start);
            let primitive = Primitive::Integer(integer);
            let integer_json = json!({"type": stringify!($variant).to_lowercase(), "value": $wire_start});
            assert_eq!(serde_json::to_value(integer).unwrap(), integer_json);
            assert_eq!(serde_json::from_value::<PrimitiveInteger>(integer_json.clone()).unwrap(), integer);
            assert_eq!(serde_json::to_value(&primitive).unwrap(), integer_json);
            assert_eq!(serde_json::from_value::<Primitive>(integer_json.clone()).unwrap(), primitive);
            let owned = OwnedValue::Primitive(primitive.clone());
            assert_eq!(serde_json::to_value(&owned).unwrap(), integer_json);
            assert_eq!(serde_json::from_value::<OwnedValue>(integer_json.clone()).unwrap(), owned);
            let mut expected_hash = HashBytes::default();
            ($tag as u8).hash(&mut expected_hash);
            start.hash(&mut expected_hash);
            assert_eq!(hash_bytes(&primitive), expected_hash.0);
            assert_eq!(integer.get_memory_usage(), $width);
            assert_eq!(primitive.get_memory_usage(), $width);

            let range = Primitive::Range(Box::new(PrimitiveRange::$variant(start..end)));
            let expected = json!({"type": "range", "value": [
                {"type": stringify!($variant).to_lowercase(), "value": $wire_start},
                {"type": stringify!($variant).to_lowercase(), "value": $wire_end}
            ]});
            assert_eq!(serde_json::to_value(&range).unwrap(), expected);
            assert_eq!(serde_json::from_value::<Primitive>(expected).unwrap(), range);
            assert_eq!(range.get_memory_usage(), 16 + 2 * $width);
            assert_eq!(range.as_range().unwrap().start(), integer);
            let mut expected_hash = HashBytes::default();
            9u8.hash(&mut expected_hash);
            ($tag as u8).hash(&mut expected_hash);
            start.hash(&mut expected_hash);
            ($tag as u8).hash(&mut expected_hash);
            end.hash(&mut expected_hash);
            assert_eq!(hash_bytes(&range), expected_hash.0);
        }};
    }
    check!(U8, 1u8, 3u8, 1, 1, 1, 3);
    check!(U16, 1u16, 3u16, 2, 2, 1, 3);
    check!(U32, 1u32, 3u32, 4, 3, 1, 3);
    check!(U64, 1u64, 3u64, 8, 4, "1", "3");
    check!(U128, 1u128, 3u128, 16, 5, "1", "3");
    check!(
        U256,
        U256::ONE,
        U256::from(3u8),
        32,
        6,
        serde_json::to_value(U256::ONE).unwrap(),
        serde_json::to_value(U256::from(3u8)).unwrap()
    );
}

#[test]
fn invalid_ranges_are_rejected_at_construction_and_deserialization() {
    assert!(matches!(
        PrimitiveRange::new(PrimitiveInteger::U8(0), PrimitiveInteger::U64(1)),
        Err(ValueError::InvalidRange)
    ));
    assert!(Primitive::new_range(Primitive::Null, Primitive::Boolean(true)).is_err());
    for endpoint in [
        json!({"type":"string", "value":"invalid"}),
        json!({"type":"u8", "value":1}),
        json!({"type":"range", "value":[{"type":"u64", "value":"0"}, {"type":"u64", "value":"1"}]}),
        json!({"type":"opaque", "value":{"type":"unknown", "value":null}}),
    ] {
        let invalid = json!({"type":"range", "value":[{"type":"u64", "value":"0"}, endpoint]});
        assert!(serde_json::from_value::<Primitive>(invalid.clone()).is_err());
        assert!(serde_json::from_value::<OwnedValue>(invalid.clone()).is_err());
    }
    for (start, end) in [(0, 0), (3, 1)] {
        assert!(
            PrimitiveRange::new(PrimitiveInteger::U64(start), PrimitiveInteger::U64(end)).is_ok()
        );
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize, Opaque)]
#[opaque(name = "PrimitiveSplitFixture", json, hash, eq)]
struct Fixture(u32);

#[test]
fn opaques_serialize_as_owned_variants_without_changing_costs() {
    JSON_REGISTRY.write().unwrap().insert(
        "PrimitiveSplitFixture",
        Box::new(|value: Value| {
            Ok(OpaqueWrapper::new(serde_json::from_value::<Fixture>(
                value,
            ).context("deserializing PrimitiveSplitFixture from JSON")?))
        }),
    );
    let opaque = OpaqueWrapper::new(Fixture(42));
    let expected = json!({"type":"opaque", "value":{"type":"PrimitiveSplitFixture", "value":42}});
    let cell = OwnedValue::Opaque(opaque.clone());
    assert_eq!(serde_json::to_value(&cell).unwrap(), expected);
    assert_eq!(
        serde_json::from_value::<OwnedValue>(expected.clone()).unwrap(),
        cell
    );
    assert!(serde_json::from_value::<Primitive>(expected.clone()).is_err());
    let mut expected_hash = HashBytes::default();
    10u8.hash(&mut expected_hash);
    opaque.hash(&mut expected_hash);
    assert_eq!(hash_bytes(&cell), expected_hash.0);
    assert_eq!(cell.calculate_memory_usage(100).unwrap(), 8);
    let nested = OwnedValue::Object(vec![cell.into()]);
    assert_eq!(nested.calculate_memory_usage(100).unwrap(), 41);
    let schema = serde_json::to_value(schemars::schema_for!(OwnedValue)).unwrap();
    assert_eq!(schema["title"], "OwnedValue");
    assert!(schema.to_string().contains("opaque"));
    assert_eq!(
        Primitive::Integer(PrimitiveInteger::U8(1))
            .get_type()
            .unwrap(),
        Type::U8
    );
}

#[test]
fn range_casts_keep_endpoint_width_and_overflow_checks() {
    let source = Primitive::Range(Box::new(PrimitiveRange::U64(1..4)));
    let target = Type::Range(Box::new(Type::U8));
    let expected = Primitive::Range(Box::new(PrimitiveRange::U8(1..4)));
    assert_eq!(
        source
            .clone()
            .checked_cast_to_primitive_type(&target)
            .unwrap(),
        expected
    );
    assert_eq!(
        OwnedValue::Primitive(source.clone())
            .checked_cast_to_primitive_type(&target)
            .unwrap(),
        OwnedValue::Primitive(expected.clone())
    );
    let overflow = Primitive::Range(Box::new(PrimitiveRange::U64(0..256)));
    assert!(matches!(
        overflow.checked_cast_to_primitive_type(&target),
        Err(ValueError::CastError)
    ));
    assert_eq!(
        PrimitiveInteger::U8(1).partial_cmp(&PrimitiveInteger::U64(1)),
        None
    );
}


#[test]
fn maps_and_bytes_serialize_without_proxy_value_types() {
    let value = OwnedValue::Map(IndexMap::from_iter([
        (OwnedValue::Object(vec![PrimitiveInteger::U8(1).into()]), OwnedValue::Bytes(vec![0, 255])),
    ]));
    let encoded = serde_json::to_value(&value).unwrap();
    assert_eq!(encoded, json!({"type":"map", "value":[[
        {"type":"object", "value":[{"type":"u8", "value":1}]},
        {"type":"bytes", "value":[0, 255]}
    ]]}));
    assert_eq!(serde_json::from_value::<OwnedValue>(encoded).unwrap(), value);

}

#[test]
fn redundant_scalar_wrappers_are_rejected() {
    assert!(serde_json::from_value::<Primitive>(json!({"type":"integer", "value":{"type":"u64", "value":"1"}})).is_err());
    assert!(serde_json::from_value::<OwnedValue>(json!({
        "type":"primitive", "value":{"type":"opaque", "value":null}
    })).is_err());
}


#[test]
fn all_scalar_owned_values_use_the_primitive_tags() {
    let values = [
        Primitive::Null,
        Primitive::Boolean(true),
        Primitive::String("text".into()),
        Primitive::Range(Box::new(PrimitiveRange::U64(1..3))),
    ];
    for value in values {
        let encoded = serde_json::to_value(&value).unwrap();
        let owned = OwnedValue::Primitive(value.clone());
        assert_eq!(serde_json::to_value(&owned).unwrap(), encoded);
        assert_eq!(serde_json::from_value::<OwnedValue>(encoded.clone()).unwrap(), owned);
    }
}

#[test]
fn integer_schemas_match_string_encoded_large_values() {
    let schema = serde_json::to_value(schemars::schema_for!(PrimitiveInteger)).unwrap();
    let variants = schema["oneOf"].as_array().unwrap();
    for tag in ["u64", "u128"] {
        let variant = variants.iter().find(|variant| variant["properties"]["type"]["const"] == tag).unwrap();
        assert_eq!(variant["properties"]["value"]["type"], "string");
    }
}
