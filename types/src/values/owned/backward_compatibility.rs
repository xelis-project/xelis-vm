//! Decode current OwnedValue JSON and the ValueCell JSON from master.
//!
//! Serialization and the schema remain in the current format. To remove legacy
//! support, restore derived Deserialize on OwnedValue (including serde_map's
//! deserialize_with on Map) and remove this module.

use indexmap::IndexMap;
use serde::{Deserialize, Deserializer};

use super::OwnedValue;
use crate::values::serde_map;
use crate::{OpaqueWrapper, Primitive};

#[derive(Deserialize)]
#[serde(rename_all = "snake_case", tag = "type", content = "value")]
enum CompatibleValue {
    Opaque(OpaqueWrapper),
    Bytes(CompatibleBytes),
    Object(Vec<OwnedValue>),
    #[serde(deserialize_with = "serde_map::deserialize")]
    Map(IndexMap<OwnedValue, OwnedValue>),
    // ValueCell wrapped all primitives, including opaques, in this tag.
    Primitive(LegacyPrimitive),
    #[serde(untagged)]
    CurrentPrimitive(Primitive),
}

#[derive(Deserialize)]
#[serde(untagged)]
enum CompatibleBytes {
    Current(Vec<u8>),
    Legacy(#[serde(with = "hex::serde")] Vec<u8>),
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case", tag = "type", content = "value")]
enum LegacyPrimitive {
    Opaque(OpaqueWrapper),
    // Integer tags and range endpoints already match the current primitives.
    #[serde(untagged)]
    Primitive(Primitive),
}

impl<'de> Deserialize<'de> for OwnedValue {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(match CompatibleValue::deserialize(deserializer)? {
            CompatibleValue::Opaque(value) => Self::Opaque(value),
            CompatibleValue::Bytes(
                CompatibleBytes::Current(value) | CompatibleBytes::Legacy(value),
            ) => Self::Bytes(value),
            CompatibleValue::Object(values) => Self::Object(values),
            CompatibleValue::Map(values) => Self::Map(values),
            CompatibleValue::Primitive(LegacyPrimitive::Opaque(value)) => Self::Opaque(value),
            CompatibleValue::Primitive(LegacyPrimitive::Primitive(value))
            | CompatibleValue::CurrentPrimitive(value) => Self::Primitive(value),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{PrimitiveRange, U256};
    use serde_json::{json, Value};

    fn assert_decodes(input: Value, expected: &OwnedValue) {
        assert_eq!(
            &serde_json::from_value::<OwnedValue>(input.clone()).unwrap(),
            expected
        );
        assert_eq!(
            &serde_json::from_str::<OwnedValue>(&input.to_string()).unwrap(),
            expected
        );
    }

    #[test]
    fn primitives_decode_in_both_formats_and_keep_current_serialization() {
        let cases = [
            (json!({"type": "null"}), OwnedValue::default()),
            (json!({"type": "boolean", "value": true}), true.into()),
            (
                json!({"type": "string", "value": "hello"}),
                String::from("hello").into(),
            ),
            (json!({"type": "u8", "value": 255}), u8::MAX.into()),
            (json!({"type": "u16", "value": 65535}), u16::MAX.into()),
            (
                json!({"type": "u32", "value": 4294967295u32}),
                u32::MAX.into(),
            ),
            (
                json!({"type": "u64", "value": "18446744073709551615"}),
                u64::MAX.into(),
            ),
            (
                json!({"type": "u128", "value": u128::MAX.to_string()}),
                u128::MAX.into(),
            ),
            (
                json!({"type": "u256", "value": U256::MAX.to_string()}),
                U256::MAX.into(),
            ),
        ];
        for (current, expected) in cases {
            assert_decodes(current.clone(), &expected);
            assert_decodes(json!({"type": "primitive", "value": current}), &expected);
            assert_eq!(serde_json::to_value(&expected).unwrap(), current);
        }
    }

    #[test]
    fn ranges_decode_in_both_formats() {
        let ranges = [
            PrimitiveRange::U8(1..2),
            PrimitiveRange::U16(1..2),
            PrimitiveRange::U32(1..2),
            PrimitiveRange::U64(1..u64::MAX),
            PrimitiveRange::U128(1..u128::MAX),
            PrimitiveRange::U256(U256::from(1u8)..U256::MAX),
        ];
        for range in ranges {
            let current = json!({"type": "range", "value": [range.start(), range.end()]});
            let expected = OwnedValue::from(Primitive::Range(Box::new(range)));
            assert_decodes(current.clone(), &expected);
            assert_decodes(json!({"type": "primitive", "value": current}), &expected);
            assert_eq!(serde_json::to_value(&expected).unwrap(), current);
        }
    }

    #[test]
    fn bytes_decode_arrays_and_legacy_hex() {
        for (hex, bytes) in [
            ("", vec![]),
            ("000aff", vec![0, 10, 255]),
            ("00Aaff", vec![0, 170, 255]),
        ] {
            let current = json!({"type": "bytes", "value": bytes});
            let expected = OwnedValue::Bytes(bytes);
            assert_decodes(current.clone(), &expected);
            assert_decodes(json!({"type": "bytes", "value": hex}), &expected);
            assert_eq!(serde_json::to_value(&expected).unwrap(), current);
        }
    }

    #[test]
    fn compatibility_applies_to_nested_objects_and_both_sides_of_maps() {
        let legacy = json!({"type": "object", "value": [
            {"type": "map", "value": [[
                {"type": "primitive", "value": {"type": "string", "value": "key"}},
                {"type": "object", "value": [
                    {"type": "bytes", "value": "00ff"},
                    {"type": "primitive", "value": {"type": "u64", "value": "42"}},
                    {"type": "boolean", "value": true}
                ]}
            ]]},
            {"type": "object", "value": []},
            {"type": "map", "value": []}
        ]});
        let expected = OwnedValue::Object(vec![
            OwnedValue::Map(
                [(
                    String::from("key").into(),
                    OwnedValue::Object(vec![
                        OwnedValue::Bytes(vec![0, 255]),
                        42u64.into(),
                        true.into(),
                    ]),
                )]
                .into_iter()
                .collect(),
            ),
            OwnedValue::Object(vec![]),
            OwnedValue::Map(IndexMap::new()),
        ]);
        assert_decodes(legacy, &expected);
        assert_decodes(serde_json::to_value(&expected).unwrap(), &expected);
    }

    #[test]
    fn legacy_opaques_move_out_of_the_primitive_wrapper() {
        #[derive(
            Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, Deserialize, crate::Opaque,
        )]
        #[opaque(json, eq, hash)]
        struct LegacyJsonOpaque(u32);

        {
            let mut registry = crate::traits::JSON_REGISTRY.write().unwrap();
            crate::register_opaque_json!(registry, "LegacyJsonOpaque", LegacyJsonOpaque);
        }
        let current = json!({"type": "opaque", "value": {"type": "LegacyJsonOpaque", "value": 42}});
        let expected = OwnedValue::from(LegacyJsonOpaque(42));
        assert_decodes(current.clone(), &expected);
        assert_decodes(json!({"type": "primitive", "value": current}), &expected);
        assert_eq!(serde_json::to_value(&expected).unwrap(), current);
    }

    #[test]
    fn malformed_legacy_values_are_rejected() {
        for input in [
            json!({"type": "bytes", "value": "0"}),
            json!({"type": "bytes", "value": "zz"}),
            json!({"type": "bytes", "value": [256]}),
            json!({"type": "primitive", "value": {"type": "u8", "value": 256}}),
            json!({"type": "primitive", "value": {"type": "u64", "value": 42}}),
            json!({"type": "primitive", "value": {"type": "object", "value": []}}),
            json!({"type": "primitive", "value": {"type": "range", "value": [
                {"type": "u8", "value": 1}, {"type": "u16", "value": 2}
            ]}}),
            json!({"type": "map", "value": [[{"type": "primitive", "value": {"type": "null"}}]]}),
        ] {
            assert!(
                serde_json::from_value::<OwnedValue>(input.clone()).is_err(),
                "{input}"
            );
        }
    }
}
