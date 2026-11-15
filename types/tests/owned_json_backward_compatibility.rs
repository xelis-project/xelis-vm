use serde::Deserialize;
use silex_types::OwnedValue;

#[test]
fn owned_value_directly_decodes_legacy_and_current_json() {
    for input in [
        r#"{"type":"primitive","value":{"type":"u64","value":"42"}}"#,
        r#"{"type":"u64","value":"42"}"#,
    ] {
        let value = serde_json::from_str::<OwnedValue>(input).unwrap();
        assert_eq!(value, OwnedValue::from(42u64));
        assert_eq!(
            serde_json::to_string(&value).unwrap(),
            r#"{"type":"u64","value":"42"}"#,
        );
    }
}

#[test]
fn derived_deserialization_uses_compatibility_for_owned_value_fields() {
    #[derive(Deserialize)]
    struct Input {
        value: OwnedValue,
    }

    let input: Input = serde_json::from_str(
        r#"{"value":{"type":"object","value":[{"type":"bytes","value":"00ff"},{"type":"primitive","value":{"type":"u8","value":7}}]}}"#,
    )
    .unwrap();
    assert_eq!(
        input.value,
        OwnedValue::Object(vec![OwnedValue::Bytes(vec![0, 255]), OwnedValue::from(7u8)]),
    );
}
