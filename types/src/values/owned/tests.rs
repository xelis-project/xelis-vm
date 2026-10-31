use std::hash::{DefaultHasher, Hash, Hasher};
use indexmap::IndexMap;
use schemars::Schema;
use serde_json::json;
use crate::{Heap, OwnedValue, Primitive, PrimitiveInteger, RuntimeValue};

#[test]
fn test_clone() {
    let cell = OwnedValue::from(42u64);
    let clone = cell.clone();
    assert_eq!(cell, clone);
}

#[test]
fn test_drop_deep_array() {
    let depth = 10_000;
    let mut cell = OwnedValue::default();
    for _ in 0..depth {
        cell = OwnedValue::Object(vec![cell]);
    }
    drop(cell); // should not stack overflow
}

#[test]
fn test_into_runtime_value() {
    let cell = OwnedValue::Object(vec![
        OwnedValue::from(1u8),
        OwnedValue::from(2u8),
    ]);
    let mut heap = unsafe { Heap::new() };
    let vc = cell.to_runtime_value(&mut heap);
    match vc {
        RuntimeValue::Object(arr) => {
            assert_eq!(arr[0].as_ref(&heap).as_u8().unwrap(), 1);
            assert_eq!(arr[1].as_ref(&heap).as_u8().unwrap(), 2);
        }
        _ => panic!("expected Object"),
    }
}

#[test]
fn display_keeps_nested_separators_and_map_order() {
    let value = OwnedValue::Object(vec![
        OwnedValue::Map(IndexMap::from_iter([
            (OwnedValue::from(1u8), OwnedValue::Object(vec![OwnedValue::from(2u8), OwnedValue::from(3u8)])),
            (OwnedValue::from(4u8), OwnedValue::Map(IndexMap::new())),
        ])),
        OwnedValue::Bytes(vec![5, 6]),
        OwnedValue::Object(Vec::new()),
    ]);
    assert_eq!(value.to_string(), "[map{1: [2, 3], 4: map{}}, bytes[5, 6], []]");
}

#[test]
fn enum_extraction_moves_the_owned_fields() {
    let bytes = vec![1, 2, 3];
    let allocation = bytes.as_ptr();
    let mut value = OwnedValue::Object(vec![OwnedValue::from(7u8), OwnedValue::Bytes(bytes)]);
    let (variant, fields) = value.to_enum().unwrap();
    assert_eq!(variant, 7);
    assert_eq!(fields[0].as_bytes().unwrap().as_ptr(), allocation);
    assert!(value.as_vec().unwrap().is_empty());
}




#[cfg(feature = "infinite-cell-depth")]
const MAX_ITERS: usize = 100_000;
#[cfg(not(feature = "infinite-cell-depth"))]
const MAX_ITERS: usize = 1000;

#[test]
fn test_drop_and_clone() {
    let v = OwnedValue::default();
    drop(v.clone());
    drop(v);

    let v = OwnedValue::Object(vec![Default::default()]);
    drop(v.clone());
    drop(v);

    // Exercise deeply nested arrays.
    let mut v = OwnedValue::Object(vec![Default::default()]);
    for _ in 0..MAX_ITERS {
        v = OwnedValue::Object(vec![v]);
    }

    drop(v.clone());
    drop(v);

    // Exercise deeply nested maps.
    let mut v = OwnedValue::Map(IndexMap::new());
    for _ in 0..MAX_ITERS {
        let mut inner_map = IndexMap::new();
        inner_map.insert(OwnedValue::from(10u8), v);
        v = OwnedValue::Map(inner_map);
    }

    drop(v.clone());
    drop(v);
}

#[test]
fn test_std_hash() {
    // Create a map that contains a map that contains a map...
    let mut map = OwnedValue::Map(IndexMap::new());
    for _ in 0..MAX_ITERS {
        let mut inner_map = IndexMap::new();
        inner_map.insert(OwnedValue::from(10u8), map);
        map = OwnedValue::Map(inner_map);
    }

    let mut hasher = DefaultHasher::new();
    map.hash(&mut hasher);

    // Try the Eq impl
    assert_eq!(map, map.clone());
}

#[test]
fn test_display_deep_nesting() {
    // Test that Display doesn't stack overflow on deeply nested structures
    let mut map = OwnedValue::Map(IndexMap::new());
    for _ in 0..MAX_ITERS {
        let mut inner_map = IndexMap::new();
        inner_map.insert(OwnedValue::from(10u8), map);
        map = OwnedValue::Map(inner_map);
    }

    // This should not stack overflow
    let display = format!("{}", map);
    assert!(display.starts_with("map{"));
    assert!(display.ends_with("}"));

    // Test with deeply nested arrays
    let mut array = OwnedValue::Object(vec![Primitive::Integer(PrimitiveInteger::U64(42)).into()]);
    for _ in 0..MAX_ITERS {
        array = OwnedValue::Object(vec![array]);
    }

    let display = format!("{}", array);
    assert!(display.starts_with("["));
    assert!(display.ends_with("]"));
}

#[test]
fn test_debug_deep_nesting() {
    // Test that Debug doesn't stack overflow on deeply nested structures via OwnedValue
    let mut map = OwnedValue::Map(IndexMap::new());
    for _ in 0..100 {  // Use smaller depth for Debug since it's more verbose
        let mut inner_map = IndexMap::new();
        inner_map.insert(OwnedValue::from(10u8), map);
        map = OwnedValue::Map(inner_map);
    }

    // This should not stack overflow
    let debug = format!("{:?}", map);
    assert!(debug.starts_with("Map("));
    assert!(debug.ends_with(")"));
}

#[test]
fn test_serde() {
    let mut map = OwnedValue::Map(IndexMap::new());
    for _ in 0..200 {
        let mut inner_map = IndexMap::new();
        inner_map.insert(OwnedValue::from(10u8), map);
        map = OwnedValue::Map(inner_map);
    }

    let serialized = serde_json::to_value(&map).unwrap();

    let got: OwnedValue = serde_json::from_value(serialized).unwrap();
    assert_eq!(map, got);
}

#[test]
fn test_serde_huge_map() {
    let mut map: IndexMap<OwnedValue, OwnedValue> = IndexMap::new();
    for i in 0..10_000u64 {
        let inner_map = IndexMap::new();
        map.insert(Primitive::Integer(PrimitiveInteger::U64(i)).into(), OwnedValue::Map(inner_map).into());
    }

    let map = OwnedValue::Map(map);
    let serialized = json!(map);

    let got: OwnedValue = serde_json::from_value(serialized).unwrap();
    assert_eq!(map, got);
}

#[test]
fn test_serde_stackoverflow() {
    let key = serde_json::to_string(&OwnedValue::from(10u8)).unwrap();
    let mut input = String::from(r#"{"type":"map","value":[]}"#);
    for _ in 0..100 {
        input = format!(r#"{{"type":"map","value":[[{key},{input}]]}}"#);
    }
    assert!(serde_json::from_str::<OwnedValue>(&input)
        .is_err_and(|error| error.to_string().contains("recursion limit exceeded")));
}

#[test]
fn test_schema() {
    let _: Schema = schemars::schema_for!(OwnedValue);
}

#[test]
fn test_hashing_types() {
    let value = OwnedValue::from(10u8);

    let single = OwnedValue::Object(vec![value.clone(), value.clone()]);
    let twice = OwnedValue::Object(vec![value.clone()]);

    let mut hasher = DefaultHasher::new();
    single.hash(&mut hasher);
    let hash = hasher.finish();

    hasher = DefaultHasher::new();
    twice.hash(&mut hasher);
    let hash2 = hasher.finish();

    assert_ne!(hash, hash2);
}

#[test]
fn deeply_nested_owned_values_format_iteratively_and_stop_at_the_serializer_boundary() {
    let mut value = OwnedValue::from(42u64);
    for _ in 0..4096 {
        value = OwnedValue::Object(vec![value]);
    }
    assert_eq!(format!("{value:?}").matches("Object([").count(), 4096);
    let error = serde_json::to_string(&value).unwrap_err();
    assert!(error.to_string().contains("serializer nesting limit"));
}
