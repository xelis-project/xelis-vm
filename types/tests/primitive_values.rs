use std::hash::{Hash, Hasher};

use serde_json::{json, Value};
use silex_types::{
    traits::JSON_REGISTRY, Constant, ConstantValueCell, Opaque, OpaqueWrapper, Primitive,
    PrimitiveInteger, PrimitiveRange, Type, ValueCell, ValueError, U256,
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
fn integers_and_ranges_preserve_serialization_hashes_and_memory_costs() {
    macro_rules! check {
        ($variant:ident, $start:expr, $end:expr, $width:expr, $tag:expr, $wire_start:expr, $wire_end:expr) => {{
            let start = $start;
            let end = $end;
            let integer = PrimitiveInteger::$variant(start);
            let primitive = Primitive::Integer(integer);
            let expected = json!({"type": stringify!($variant).to_lowercase(), "value": $wire_start});
            assert_eq!(serde_json::to_value(&primitive).unwrap(), expected);
            assert_eq!(serde_json::from_value::<Primitive>(expected).unwrap(), primitive);
            let mut legacy_hash = HashBytes::default();
            ($tag as u8).hash(&mut legacy_hash);
            start.hash(&mut legacy_hash);
            assert_eq!(hash_bytes(&primitive), legacy_hash.0);
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
            let mut legacy_hash = HashBytes::default();
            9u8.hash(&mut legacy_hash);
            ($tag as u8).hash(&mut legacy_hash);
            start.hash(&mut legacy_hash);
            ($tag as u8).hash(&mut legacy_hash);
            end.hash(&mut legacy_hash);
            assert_eq!(hash_bytes(&range), legacy_hash.0);
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
        let cell = json!({"type":"primitive", "value":invalid});
        assert!(serde_json::from_value::<ValueCell>(cell.clone()).is_err());
        assert!(serde_json::from_value::<Constant>(cell.clone()).is_err());
        assert!(serde_json::from_value::<ConstantValueCell>(cell).is_err());
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
fn lifted_opaques_keep_their_legacy_wire_shape_hash_and_memory_cost() {
    JSON_REGISTRY.write().unwrap().insert(
        "PrimitiveSplitFixture",
        Box::new(|value: Value| {
            Ok(OpaqueWrapper::new(serde_json::from_value::<Fixture>(
                value,
            )?))
        }),
    );
    let opaque = OpaqueWrapper::new(Fixture(42));
    let expected = json!({"type":"primitive", "value":{
        "type":"opaque", "value":{"type":"PrimitiveSplitFixture", "value":42}
    }});
    let cell = ValueCell::Opaque(opaque.clone());
    let constant = Constant::Opaque(opaque.clone());
    let constant_cell = ConstantValueCell::Opaque(opaque.clone());
    assert_eq!(serde_json::to_value(&cell).unwrap(), expected);
    assert_eq!(serde_json::to_value(&constant).unwrap(), expected);
    assert_eq!(serde_json::to_value(&constant_cell).unwrap(), expected);
    assert_eq!(
        serde_json::from_value::<ValueCell>(expected.clone()).unwrap(),
        cell
    );
    assert_eq!(
        serde_json::from_value::<Constant>(expected.clone()).unwrap(),
        constant
    );
    assert_eq!(
        serde_json::from_value::<ConstantValueCell>(expected.clone()).unwrap(),
        constant_cell
    );
    assert!(serde_json::from_value::<Primitive>(expected["value"].clone()).is_err());
    let mut legacy_hash = HashBytes::default();
    10u8.hash(&mut legacy_hash);
    opaque.hash(&mut legacy_hash);
    assert_eq!(hash_bytes(&cell), legacy_hash.0);
    assert_eq!(hash_bytes(&constant), legacy_hash.0);
    assert_eq!(hash_bytes(&constant_cell), legacy_hash.0);
    assert_eq!(cell.calculate_memory_usage(100).unwrap(), 8);
    assert_eq!(constant_cell.calculate_memory_usage(100).unwrap(), 8);
    let nested = ValueCell::Object(vec![cell.into()]);
    assert_eq!(nested.calculate_memory_usage(100).unwrap(), 41);
    assert_eq!(
        nested.calculate_depth(0).unwrap_err().to_string(),
        "max depth reached"
    );
    assert_eq!(nested.calculate_depth(1).unwrap(), 1);
    let schema = serde_json::to_value(schemars::schema_for!(ValueCell)).unwrap();
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
        Constant::Primitive(source.clone())
            .checked_cast_to_primitive_type(&target)
            .unwrap(),
        Constant::Primitive(expected.clone())
    );
    assert_eq!(
        ValueCell::Primitive(source)
            .checked_cast_to_primitive_type(&target)
            .unwrap(),
        ValueCell::Primitive(expected)
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
