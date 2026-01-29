use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::{NumberType, Primitive, ValueCell, ValueError, ValuePointer};

/// A packed type representation
/// Used for representing types in a compact way
#[derive(Debug, PartialEq, Eq, Clone, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "snake_case", tag = "type", content = "value")]
pub enum TypePacked {
    // Support anything
    Any,
    // Optional type
    Optional(Box<TypePacked>),
    // Primitive types
    Number(NumberType),
    String,
    Bool,
    Bytes,
    Array(Box<TypePacked>),
    // Tuples of packed types
    // it can represent enum, struct or tuples
    Tuples(Vec<TypePacked>),
    // Single
    Range(Box<NumberType>),
    // Map of packed types
    Map(Box<TypePacked>, Box<TypePacked>),
    // Maximum u8::MAX variants
    OneOf(Vec<Vec<TypePacked>>),
}

impl TypePacked {
    /// Verify the TypePacked is valid
    /// This is iterative to prevent stack overflow with deeply nested types
    /// Returns an error if:
    /// - The depth exceeds max_depth
    /// - OneOf has no variants or more than u8::MAX variants
    /// - OneOf has a variant with no types (use empty vec for unit variants)
    /// - Map key type is Map (maps cannot be keys)
    pub fn verify(&self, max_depth: usize) -> Result<(), ValueError> {
        // Stack contains (type, current_depth)
        let mut stack: Vec<(&TypePacked, usize, bool)> = vec![(self, 0, false)];

        while let Some((tp, depth, is_map_key)) = stack.pop() {
            if depth > max_depth {
                return Err(ValueError::MaxDepthReached);
            }

            match tp {
                TypePacked::Any
                | TypePacked::Number(_)
                | TypePacked::String
                | TypePacked::Bool
                | TypePacked::Bytes
                | TypePacked::Range(_) => {
                    // Primitive types are always valid
                },
                TypePacked::Optional(inner) => {
                    stack.push((inner, depth + 1, is_map_key));
                },
                TypePacked::Array(inner) => {
                    stack.push((inner, depth + 1, is_map_key));
                },
                TypePacked::Tuples(types) => {
                    if types.len() > u8::MAX as usize {
                        return Err(ValueError::UnknownType);
                    }

                    for t in types.iter() {
                        stack.push((t, depth + 1, is_map_key));
                    }
                },
                TypePacked::OneOf(variants) => {
                    // Must have at least one variant
                    if variants.is_empty() {
                        return Err(ValueError::UnknownType);
                    }

                    // Cannot have more than u8::MAX variants
                    if variants.len() > u8::MAX as usize {
                        return Err(ValueError::UnknownType);
                    }

                    for variant_types in variants.iter() {
                        for t in variant_types.iter() {
                            stack.push((t, depth + 1, is_map_key));
                        }
                    }
                },
                TypePacked::Map(key_type, value_type) => {
                    // Map keys cannot be maps themselves
                    if is_map_key {
                        return Err(ValueError::UnknownType);
                    }

                    stack.push((key_type, depth + 1, true));
                    stack.push((value_type, depth + 1, is_map_key));
                },
            }
        }

        Ok(())
    }

    // Convert from Primitive to TypePacked
    pub fn from_primitive(p: &Primitive) -> Option<TypePacked> {
        Some(match p {
            Primitive::U8(_) => TypePacked::Number(NumberType::U8),
            Primitive::U16(_) => TypePacked::Number(NumberType::U16),
            Primitive::U32(_) => TypePacked::Number(NumberType::U32),
            Primitive::U64(_) => TypePacked::Number(NumberType::U64),
            Primitive::U128(_) => TypePacked::Number(NumberType::U128),
            Primitive::U256(_) => TypePacked::Number(NumberType::U256),
            Primitive::Boolean(_) => TypePacked::Bool,
            Primitive::String(_) => TypePacked::String,
            Primitive::Null => TypePacked::Optional(Box::new(TypePacked::Any)),
            _ => return None
        })
    }

    /// Check if a ValueCell matches the TypePacked
    /// This is iterative to prevent stack overflow with deeply nested types
    pub fn check(&self, value: &ValueCell) -> bool {
        let mut stack: Vec<(&TypePacked, &ValueCell)> = vec![(self, value)];

        while let Some((tp, val)) = stack.pop() {
            match (tp, val) {
                (TypePacked::Optional(_), ValueCell::Primitive(Primitive::Null)) => {},
                (TypePacked::Optional(inner), v) => {
                    stack.push((inner, v));
                },
                (TypePacked::Number(nt), ValueCell::Primitive(p)) => {
                    if !TypePacked::from_primitive(p)
                        .is_some_and(|v| v == TypePacked::Number(*nt)) {
                        return false;
                    }
                },
                (TypePacked::String, ValueCell::Primitive(Primitive::String(_))) => {},
                (TypePacked::Bool, ValueCell::Primitive(Primitive::Boolean(_))) => {},
                (TypePacked::Bytes, ValueCell::Bytes(_)) => {},
                (TypePacked::Any, _) => {},
                (TypePacked::Array(inner), ValueCell::Object(values)) => {
                    for v in values.iter() {
                        stack.push((inner, v.as_ref()));
                    }
                },
                (TypePacked::Tuples(types), ValueCell::Object(values)) => {
                    if types.len() != values.len() {
                        return false;
                    }
                    for (t, v) in types.iter().zip(values.iter()) {
                        stack.push((t, v.as_ref()));
                    }
                },
                (TypePacked::OneOf(variants), ValueCell::Object(values)) => {
                    // First element should be the variant index (u8)
                    let Some(ValueCell::Primitive(Primitive::U8(index))) = values.get(0).map(ValuePointer::as_ref) else {
                        return false;
                    };

                    let variant_index = *index as usize;
                    if variant_index >= variants.len() {
                        return false;
                    }

                    let variant_types = &variants[variant_index];
                    // The remaining elements should match the variant types
                    let data = &values[1..];
                    if data.len() != variant_types.len() {
                        return false;
                    }

                    for (t, v) in variant_types.iter().zip(data.iter()) {
                        stack.push((t, v.as_ref()));
                    }
                },
                (TypePacked::Map(key_type, value_type), ValueCell::Map(map)) => {
                    for (k, v) in map.iter() {
                        stack.push((key_type, k));
                        stack.push((value_type, v.as_ref()));
                    }
                },
                _ => return false,
            }
        }

        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{U256, ValueCell, ValuePointer};

    #[test]
    fn test_bytes() {
        let tp_bytes = TypePacked::Bytes;
        let val_bytes = ValueCell::Bytes(vec![1, 2, 3]);
        assert!(tp_bytes.check(&val_bytes));
    }

    #[test]
    fn test_bool() {
        let tp_bool = TypePacked::Bool;
        let val_bool = ValueCell::Primitive(Primitive::Boolean(true));
        assert!(tp_bool.check(&val_bool));
    }

    #[test]
    fn test_string() {
        let tp_string = TypePacked::String;
        let val_string = ValueCell::Primitive(Primitive::String("Test".to_string()));
        assert!(tp_string.check(&val_string));
    }

    #[test]
    fn test_u8() {
        let tp_number_u8 = TypePacked::Number(NumberType::U8);
        let val_number_u8 = ValueCell::Primitive(Primitive::U8(255));
        assert!(tp_number_u8.check(&val_number_u8));
    }

    #[test]
    fn test_u16() {
        let tp_number_u16 = TypePacked::Number(NumberType::U16);
        let val_number_u16 = ValueCell::Primitive(Primitive::U16(1000));
        assert!(tp_number_u16.check(&val_number_u16));
    }

    #[test]
    fn test_u32() {
        let tp_number_u32 = TypePacked::Number(NumberType::U32);
        let val_number_u32 = ValueCell::Primitive(Primitive::U32(1_000_000));
        assert!(tp_number_u32.check(&val_number_u32));
    }

    #[test]
    fn test_u64() {
        let tp_number_u64 = TypePacked::Number(NumberType::U64);
        let val_number_u64 = ValueCell::Primitive(Primitive::U64(1_000_000_000));
        assert!(tp_number_u64.check(&val_number_u64));
    }

    #[test]
    fn test_u128() {
        let tp_number_u128 = TypePacked::Number(NumberType::U128);
        let val_number_u128 = ValueCell::Primitive(Primitive::U128(1_000_000_000_000));
        assert!(tp_number_u128.check(&val_number_u128));
    }

    #[test]
    fn test_u256() {
        let tp_number_u256 = TypePacked::Number(NumberType::U256);
        let val_number_u256 = ValueCell::Primitive(Primitive::U256(U256::from(42u64)));
        assert!(tp_number_u256.check(&val_number_u256));
    }

    #[test]
    fn test_optional() {
        let tp_optional = TypePacked::Optional(Box::new(TypePacked::Number(NumberType::U8)));
        let val_null = ValueCell::Primitive(Primitive::Null);
        let val_number_u8 = ValueCell::Primitive(Primitive::U8(100));
        assert!(tp_optional.check(&val_null));
        assert!(tp_optional.check(&val_number_u8));

        assert!(!tp_optional.check(&ValueCell::Primitive(Primitive::String("Test".to_string()))));
    }

    #[test]
    fn test_any() {
        let tp_any = TypePacked::Any;
        let val_string = ValueCell::Primitive(Primitive::String("Test".to_string()));
        let val_bool = ValueCell::Primitive(Primitive::Boolean(true));
        let val_number_u8 = ValueCell::Primitive(Primitive::U8(100));
        assert!(tp_any.check(&val_string));
        assert!(tp_any.check(&val_bool));
        assert!(tp_any.check(&val_number_u8));
    }

    // Array tests
    #[test]
    fn test_array_empty() {
        let tp_array = TypePacked::Array(Box::new(TypePacked::Number(NumberType::U8)));
        let val_array = ValueCell::Object(vec![]);
        assert!(tp_array.check(&val_array));
    }

    #[test]
    fn test_array_u8() {
        let tp_array = TypePacked::Array(Box::new(TypePacked::Number(NumberType::U8)));
        let val_array = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(1))),
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(2))),
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(3))),
        ]);
        assert!(tp_array.check(&val_array));
    }

    #[test]
    fn test_array_string() {
        let tp_array = TypePacked::Array(Box::new(TypePacked::String));
        let val_array = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::String("hello".to_string()))),
            ValuePointer::from(ValueCell::Primitive(Primitive::String("world".to_string()))),
        ]);
        assert!(tp_array.check(&val_array));
    }

    #[test]
    fn test_array_mixed_types_fails() {
        let tp_array = TypePacked::Array(Box::new(TypePacked::Number(NumberType::U8)));
        let val_array = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(1))),
            ValuePointer::from(ValueCell::Primitive(Primitive::String("invalid".to_string()))),
        ]);
        assert!(!tp_array.check(&val_array));
    }

    #[test]
    fn test_array_nested() {
        let tp_array = TypePacked::Array(Box::new(TypePacked::Array(Box::new(TypePacked::Number(NumberType::U32)))));
        let inner_array1 = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U32(1))),
            ValuePointer::from(ValueCell::Primitive(Primitive::U32(2))),
        ]);
        let inner_array2 = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U32(3))),
            ValuePointer::from(ValueCell::Primitive(Primitive::U32(4))),
        ]);
        let val_array = ValueCell::Object(vec![
            ValuePointer::from(inner_array1),
            ValuePointer::from(inner_array2),
        ]);
        assert!(tp_array.check(&val_array));
    }

    #[test]
    fn test_array_with_any() {
        let tp_array = TypePacked::Array(Box::new(TypePacked::Any));
        let val_array = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(1))),
            ValuePointer::from(ValueCell::Primitive(Primitive::String("mixed".to_string()))),
            ValuePointer::from(ValueCell::Primitive(Primitive::Boolean(true))),
        ]);
        assert!(tp_array.check(&val_array));
    }

    #[test]
    fn test_array_with_optional() {
        let tp_array = TypePacked::Array(Box::new(TypePacked::Optional(Box::new(TypePacked::Number(NumberType::U8)))));
        let val_array = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(1))),
            ValuePointer::from(ValueCell::Primitive(Primitive::Null)),
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(3))),
        ]);
        assert!(tp_array.check(&val_array));
    }

    // Tuple tests (also represents struct)
    #[test]
    fn test_tuple_empty() {
        let tp_tuple = TypePacked::Tuples(vec![]);
        let val_tuple = ValueCell::Object(vec![]);
        assert!(tp_tuple.check(&val_tuple));
    }

    #[test]
    fn test_tuple_single() {
        let tp_tuple = TypePacked::Tuples(vec![TypePacked::Number(NumberType::U8)]);
        let val_tuple = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(42))),
        ]);
        assert!(tp_tuple.check(&val_tuple));
    }

    #[test]
    fn test_tuple_mixed_types() {
        let tp_tuple = TypePacked::Tuples(vec![
            TypePacked::Number(NumberType::U8),
            TypePacked::String,
            TypePacked::Bool,
        ]);
        let val_tuple = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(42))),
            ValuePointer::from(ValueCell::Primitive(Primitive::String("hello".to_string()))),
            ValuePointer::from(ValueCell::Primitive(Primitive::Boolean(true))),
        ]);
        assert!(tp_tuple.check(&val_tuple));
    }

    #[test]
    fn test_tuple_wrong_length() {
        let tp_tuple = TypePacked::Tuples(vec![
            TypePacked::Number(NumberType::U8),
            TypePacked::String,
        ]);
        let val_tuple = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(42))),
        ]);
        assert!(!tp_tuple.check(&val_tuple));
    }

    #[test]
    fn test_tuple_wrong_type() {
        let tp_tuple = TypePacked::Tuples(vec![
            TypePacked::Number(NumberType::U8),
            TypePacked::String,
        ]);
        let val_tuple = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(42))),
            ValuePointer::from(ValueCell::Primitive(Primitive::Boolean(true))), // should be String
        ]);
        assert!(!tp_tuple.check(&val_tuple));
    }

    #[test]
    fn test_tuple_nested() {
        let tp_tuple = TypePacked::Tuples(vec![
            TypePacked::Number(NumberType::U64),
            TypePacked::Tuples(vec![
                TypePacked::String,
                TypePacked::Bool,
            ]),
        ]);
        let inner_tuple = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::String("nested".to_string()))),
            ValuePointer::from(ValueCell::Primitive(Primitive::Boolean(false))),
        ]);
        let val_tuple = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U64(999))),
            ValuePointer::from(inner_tuple),
        ]);
        assert!(tp_tuple.check(&val_tuple));
    }

    // Struct tests (using Tuples representation)
    #[test]
    fn test_struct_person() {
        // Struct { name: String, age: u8, active: bool }
        let tp_struct = TypePacked::Tuples(vec![
            TypePacked::String,
            TypePacked::Number(NumberType::U8),
            TypePacked::Bool,
        ]);
        let val_struct = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::String("Alice".to_string()))),
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(30))),
            ValuePointer::from(ValueCell::Primitive(Primitive::Boolean(true))),
        ]);
        assert!(tp_struct.check(&val_struct));
    }

    #[test]
    fn test_struct_with_optional_field() {
        // Struct { id: u64, nickname: optional<String> }
        let tp_struct = TypePacked::Tuples(vec![
            TypePacked::Number(NumberType::U64),
            TypePacked::Optional(Box::new(TypePacked::String)),
        ]);
        
        // With some nickname
        let val_with_nickname = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U64(1))),
            ValuePointer::from(ValueCell::Primitive(Primitive::String("nick".to_string()))),
        ]);
        assert!(tp_struct.check(&val_with_nickname));
        
        // Without nickname (null)
        let val_without_nickname = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U64(2))),
            ValuePointer::from(ValueCell::Primitive(Primitive::Null)),
        ]);
        assert!(tp_struct.check(&val_without_nickname));
    }

    #[test]
    fn test_struct_with_array_field() {
        // Struct { name: String, scores: Array<u32> }
        let tp_struct = TypePacked::Tuples(vec![
            TypePacked::String,
            TypePacked::Array(Box::new(TypePacked::Number(NumberType::U32))),
        ]);
        let scores_array = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U32(100))),
            ValuePointer::from(ValueCell::Primitive(Primitive::U32(95))),
            ValuePointer::from(ValueCell::Primitive(Primitive::U32(87))),
        ]);
        let val_struct = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::String("Bob".to_string()))),
            ValuePointer::from(scores_array),
        ]);
        assert!(tp_struct.check(&val_struct));
    }

    // Enum tests (using OneOf representation)
    #[test]
    fn test_enum_simple_variants() {
        // enum Status { Active, Inactive, Pending }
        // Represented as OneOf with empty tuples for each variant
        let tp_enum = TypePacked::OneOf(vec![
            vec![], // Active
            vec![], // Inactive
            vec![], // Pending
        ]);
        
        // Each variant is represented as (variant_index, variant_data)
        // Active variant (index 0)
        let val_active = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(0))),
        ]);
        assert!(tp_enum.check(&val_active));
        
        // Inactive variant (index 1)
        let val_inactive = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(1))),
        ]);
        assert!(tp_enum.check(&val_inactive));
        
        // Pending variant (index 2)
        let val_pending = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(2))),
        ]);
        assert!(tp_enum.check(&val_pending));
    }

    #[test]
    fn test_enum_with_data() {
        // enum Message { Text(String), Number(u64), Pair(u8, bool) }
        let tp_enum = TypePacked::OneOf(vec![
            vec![TypePacked::String],                                    // Text(String)
            vec![TypePacked::Number(NumberType::U64)],                   // Number(u64)
            vec![TypePacked::Number(NumberType::U8), TypePacked::Bool],  // Pair(u8, bool)
        ]);
        
        // Text variant (index 0)
        let val_text = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(0))),
            ValuePointer::from(ValueCell::Primitive(Primitive::String("Hello".to_string()))),
        ]);
        assert!(tp_enum.check(&val_text));
        
        // Number variant (index 1)
        let val_number = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(1))),
            ValuePointer::from(ValueCell::Primitive(Primitive::U64(12345))),
        ]);
        assert!(tp_enum.check(&val_number));
        
        // Pair variant (index 2)
        let val_pair = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(2))),
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(42))),
            ValuePointer::from(ValueCell::Primitive(Primitive::Boolean(true))),
        ]);
        assert!(tp_enum.check(&val_pair));
    }

    #[test]
    fn test_enum_invalid_variant_index() {
        // enum Color { Red, Green, Blue }
        let tp_enum = TypePacked::OneOf(vec![
            vec![], // Red
            vec![], // Green  
            vec![], // Blue
        ]);
        
        // Invalid variant index (3, out of bounds)
        let val_invalid = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(3))),
        ]);
        assert!(!tp_enum.check(&val_invalid));
    }

    #[test]
    fn test_enum_wrong_data_type() {
        // enum Value { Int(u64), Text(String) }
        let tp_enum = TypePacked::OneOf(vec![
            vec![TypePacked::Number(NumberType::U64)], // Int(u64)
            vec![TypePacked::String],                   // Text(String)
        ]);
        
        // Int variant with String data (wrong type)
        let val_wrong = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(0))), // Int variant
            ValuePointer::from(ValueCell::Primitive(Primitive::String("wrong".to_string()))), // Should be u64
        ]);
        assert!(!tp_enum.check(&val_wrong));
    }

    #[test]
    fn test_enum_wrong_data_count() {
        // enum Data { Single(u8), Double(u8, u8) }
        let tp_enum = TypePacked::OneOf(vec![
            vec![TypePacked::Number(NumberType::U8)],                                    // Single(u8)
            vec![TypePacked::Number(NumberType::U8), TypePacked::Number(NumberType::U8)], // Double(u8, u8)
        ]);
        
        // Single variant with two values (wrong count)
        let val_wrong = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(0))), // Single variant
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(1))),
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(2))), // Extra value
        ]);
        assert!(!tp_enum.check(&val_wrong));
    }

    #[test]
    fn test_enum_nested_struct() {
        // enum Result { Ok(Person), Err(String) }
        // where Person is { name: String, age: u8 }
        let tp_enum = TypePacked::OneOf(vec![
            vec![TypePacked::Tuples(vec![
                TypePacked::String,
                TypePacked::Number(NumberType::U8),
            ])], // Ok(Person)
            vec![TypePacked::String], // Err(String)
        ]);
        
        // Ok variant with Person struct
        let person = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::String("Charlie".to_string()))),
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(25))),
        ]);
        let val_ok = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(0))),
            ValuePointer::from(person),
        ]);
        assert!(tp_enum.check(&val_ok));
        
        // Err variant
        let val_err = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(1))),
            ValuePointer::from(ValueCell::Primitive(Primitive::String("Something went wrong".to_string()))),
        ]);
        assert!(tp_enum.check(&val_err));
    }

    #[test]
    fn test_enum_option_like() {
        // enum Option<T> { None, Some(T) } where T = u64
        let tp_enum = TypePacked::OneOf(vec![
            vec![],                                      // None
            vec![TypePacked::Number(NumberType::U64)],   // Some(u64)
        ]);
        
        // None variant
        let val_none = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(0))),
        ]);
        assert!(tp_enum.check(&val_none));
        
        // Some variant
        let val_some = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::U8(1))),
            ValuePointer::from(ValueCell::Primitive(Primitive::U64(42))),
        ]);
        assert!(tp_enum.check(&val_some));
    }

    // Map tests
    #[test]
    fn test_map_empty() {
        use indexmap::IndexMap;
        let tp_map = TypePacked::Map(
            Box::new(TypePacked::String),
            Box::new(TypePacked::Number(NumberType::U64)),
        );
        let val_map = ValueCell::Map(Box::new(IndexMap::new()));
        assert!(tp_map.check(&val_map));
    }

    #[test]
    fn test_map_string_to_u64() {
        use indexmap::IndexMap;
        let tp_map = TypePacked::Map(
            Box::new(TypePacked::String),
            Box::new(TypePacked::Number(NumberType::U64)),
        );
        let mut map = IndexMap::new();
        map.insert(
            ValueCell::Primitive(Primitive::String("key1".to_string())),
            ValuePointer::from(ValueCell::Primitive(Primitive::U64(100))),
        );
        map.insert(
            ValueCell::Primitive(Primitive::String("key2".to_string())),
            ValuePointer::from(ValueCell::Primitive(Primitive::U64(200))),
        );
        let val_map = ValueCell::Map(Box::new(map));
        assert!(tp_map.check(&val_map));
    }

    #[test]
    fn test_map_wrong_key_type() {
        use indexmap::IndexMap;
        let tp_map = TypePacked::Map(
            Box::new(TypePacked::String),
            Box::new(TypePacked::Number(NumberType::U64)),
        );
        let mut map = IndexMap::new();
        map.insert(
            ValueCell::Primitive(Primitive::U8(1)), // Wrong key type (should be String)
            ValuePointer::from(ValueCell::Primitive(Primitive::U64(100))),
        );
        let val_map = ValueCell::Map(Box::new(map));
        assert!(!tp_map.check(&val_map));
    }

    #[test]
    fn test_map_wrong_value_type() {
        use indexmap::IndexMap;
        let tp_map = TypePacked::Map(
            Box::new(TypePacked::String),
            Box::new(TypePacked::Number(NumberType::U64)),
        );
        let mut map = IndexMap::new();
        map.insert(
            ValueCell::Primitive(Primitive::String("key".to_string())),
            ValuePointer::from(ValueCell::Primitive(Primitive::String("wrong".to_string()))), // Wrong value type
        );
        let val_map = ValueCell::Map(Box::new(map));
        assert!(!tp_map.check(&val_map));
    }

    #[test]
    fn test_map_u64_to_struct() {
        use indexmap::IndexMap;
        // Map<u64, Person> where Person is { name: String, active: bool }
        let tp_map = TypePacked::Map(
            Box::new(TypePacked::Number(NumberType::U64)),
            Box::new(TypePacked::Tuples(vec![
                TypePacked::String,
                TypePacked::Bool,
            ])),
        );
        let person = ValueCell::Object(vec![
            ValuePointer::from(ValueCell::Primitive(Primitive::String("Dave".to_string()))),
            ValuePointer::from(ValueCell::Primitive(Primitive::Boolean(true))),
        ]);
        let mut map = IndexMap::new();
        map.insert(
            ValueCell::Primitive(Primitive::U64(1)),
            ValuePointer::from(person),
        );
        let val_map = ValueCell::Map(Box::new(map));
        assert!(tp_map.check(&val_map));
    }

    // Verify tests
    #[test]
    fn test_verify_primitive_types() {
        assert!(TypePacked::Any.verify(10).is_ok());
        assert!(TypePacked::String.verify(10).is_ok());
        assert!(TypePacked::Bool.verify(10).is_ok());
        assert!(TypePacked::Bytes.verify(10).is_ok());
        assert!(TypePacked::Number(NumberType::U8).verify(10).is_ok());
        assert!(TypePacked::Number(NumberType::U256).verify(10).is_ok());
        assert!(TypePacked::Range(Box::new(NumberType::U64)).verify(10).is_ok());
    }

    #[test]
    fn test_verify_optional() {
        let tp = TypePacked::Optional(Box::new(TypePacked::String));
        assert!(tp.verify(10).is_ok());
    }

    #[test]
    fn test_verify_array() {
        let tp = TypePacked::Array(Box::new(TypePacked::Number(NumberType::U32)));
        assert!(tp.verify(10).is_ok());
    }

    #[test]
    fn test_verify_tuples() {
        let tp = TypePacked::Tuples(vec![
            TypePacked::String,
            TypePacked::Number(NumberType::U8),
            TypePacked::Bool,
        ]);
        assert!(tp.verify(10).is_ok());
    }

    #[test]
    fn test_verify_tuples_empty() {
        let tp = TypePacked::Tuples(vec![]);
        assert!(tp.verify(10).is_ok());
    }

    #[test]
    fn test_verify_oneof() {
        let tp = TypePacked::OneOf(vec![
            vec![TypePacked::String],
            vec![TypePacked::Number(NumberType::U64)],
            vec![], // unit variant
        ]);
        assert!(tp.verify(10).is_ok());
    }

    #[test]
    fn test_verify_oneof_empty_fails() {
        let tp = TypePacked::OneOf(vec![]);
        assert!(tp.verify(10).is_err());
    }

    #[test]
    fn test_verify_map() {
        let tp = TypePacked::Map(
            Box::new(TypePacked::String),
            Box::new(TypePacked::Number(NumberType::U64)),
        );
        assert!(tp.verify(10).is_ok());
    }

    #[test]
    fn test_verify_map_key_cannot_be_map() {
        let tp = TypePacked::Map(
            Box::new(TypePacked::Map(
                Box::new(TypePacked::String),
                Box::new(TypePacked::String),
            )),
            Box::new(TypePacked::Number(NumberType::U64)),
        );
        assert!(tp.verify(10).is_err());
    }

    #[test]
    fn test_verify_depth_exceeded() {
        // Create a deeply nested type: Optional<Optional<Optional<...>>>
        let mut tp = TypePacked::String;
        for _ in 0..5 {
            tp = TypePacked::Optional(Box::new(tp));
        }
        // Depth 5 nested, should fail with max_depth 3
        assert!(tp.verify(3).is_err());
        // Should pass with max_depth 10
        assert!(tp.verify(10).is_ok());
    }

    #[test]
    fn test_verify_depth_array_nested() {
        // Array<Array<Array<u8>>>
        let tp = TypePacked::Array(Box::new(
            TypePacked::Array(Box::new(
                TypePacked::Array(Box::new(TypePacked::Number(NumberType::U8)))
            ))
        ));
        // Depth is 3, should fail with max_depth 2
        assert!(tp.verify(2).is_err());
        // Should pass with max_depth 3
        assert!(tp.verify(3).is_ok());
    }

    #[test]
    fn test_verify_depth_tuple_nested() {
        // Tuples with nested tuples
        let tp = TypePacked::Tuples(vec![
            TypePacked::String,
            TypePacked::Tuples(vec![
                TypePacked::Bool,
                TypePacked::Tuples(vec![
                    TypePacked::Number(NumberType::U8),
                ]),
            ]),
        ]);
        // Depth is 3, should fail with max_depth 2
        assert!(tp.verify(2).is_err());
        // Should pass with max_depth 3
        assert!(tp.verify(3).is_ok());
    }

    #[test]
    fn test_verify_depth_map_nested() {
        // Map<String, Map<String, u64>>
        let tp = TypePacked::Map(
            Box::new(TypePacked::String),
            Box::new(TypePacked::Map(
                Box::new(TypePacked::String),
                Box::new(TypePacked::Number(NumberType::U64)),
            )),
        );
        // Depth is 2, should fail with max_depth 1
        assert!(tp.verify(1).is_err());
        // Should pass with max_depth 2
        assert!(tp.verify(2).is_ok());
    }

    #[test]
    fn test_verify_depth_oneof_nested() {
        // OneOf with nested types
        let tp = TypePacked::OneOf(vec![
            vec![TypePacked::Tuples(vec![
                TypePacked::Optional(Box::new(TypePacked::String)),
            ])],
        ]);
        // Depth is 3 (OneOf -> Tuples -> Optional -> String)
        assert!(tp.verify(2).is_err());
        assert!(tp.verify(3).is_ok());
    }

    #[test]
    fn test_verify_complex_valid() {
        // Complex nested structure that should be valid
        let tp = TypePacked::Map(
            Box::new(TypePacked::String),
            Box::new(TypePacked::OneOf(vec![
                vec![TypePacked::Tuples(vec![
                    TypePacked::String,
                    TypePacked::Optional(Box::new(TypePacked::Number(NumberType::U64))),
                ])],
                vec![TypePacked::Array(Box::new(TypePacked::Bool))],
            ])),
        );
        assert!(tp.verify(10).is_ok());
    }

    #[test]
    fn test_verify_zero_depth() {
        // With max_depth 0, only primitives at root should pass
        assert!(TypePacked::String.verify(0).is_ok());
        assert!(TypePacked::Bool.verify(0).is_ok());
        
        // Nested types should fail at depth 0
        let tp = TypePacked::Optional(Box::new(TypePacked::String));
        assert!(tp.verify(0).is_err());
    }

    #[test]
    fn test_verify_map_key_nested_map_in_tuple_fails() {
        // Map<Tuple<Map<String, String>>, u64> - map inside tuple as key should fail
        let tp = TypePacked::Map(
            Box::new(TypePacked::Tuples(vec![
                TypePacked::Map(
                    Box::new(TypePacked::String),
                    Box::new(TypePacked::String),
                ),
            ])),
            Box::new(TypePacked::Number(NumberType::U64)),
        );
        assert!(tp.verify(10).is_err());
    }

    #[test]
    fn test_verify_map_key_nested_map_in_array_fails() {
        // Map<Array<Map<String, String>>, u64> - map inside array as key should fail
        let tp = TypePacked::Map(
            Box::new(TypePacked::Array(Box::new(
                TypePacked::Map(
                    Box::new(TypePacked::String),
                    Box::new(TypePacked::String),
                ),
            ))),
            Box::new(TypePacked::Number(NumberType::U64)),
        );
        assert!(tp.verify(10).is_err());
    }

    #[test]
    fn test_verify_map_key_nested_map_in_optional_fails() {
        // Map<Optional<Map<String, String>>, u64> - map inside optional as key should fail
        let tp = TypePacked::Map(
            Box::new(TypePacked::Optional(Box::new(
                TypePacked::Map(
                    Box::new(TypePacked::String),
                    Box::new(TypePacked::String),
                ),
            ))),
            Box::new(TypePacked::Number(NumberType::U64)),
        );
        assert!(tp.verify(10).is_err());
    }

    #[test]
    fn test_verify_map_key_nested_map_in_oneof_fails() {
        // Map<OneOf<[Map<String, String>]>, u64> - map inside oneof as key should fail
        let tp = TypePacked::Map(
            Box::new(TypePacked::OneOf(vec![
                vec![TypePacked::Map(
                    Box::new(TypePacked::String),
                    Box::new(TypePacked::String),
                )],
            ])),
            Box::new(TypePacked::Number(NumberType::U64)),
        );
        assert!(tp.verify(10).is_err());
    }

    #[test]
    fn test_verify_map_value_can_contain_map() {
        // Map<String, Map<String, u64>> - map in value position is valid
        let tp = TypePacked::Map(
            Box::new(TypePacked::String),
            Box::new(TypePacked::Map(
                Box::new(TypePacked::String),
                Box::new(TypePacked::Number(NumberType::U64)),
            )),
        );
        assert!(tp.verify(10).is_ok());
    }

    #[test]
    fn test_verify_map_value_nested_map_in_tuple() {
        // Map<String, Tuple<Map<String, u64>>> - map nested in value is valid
        let tp = TypePacked::Map(
            Box::new(TypePacked::String),
            Box::new(TypePacked::Tuples(vec![
                TypePacked::Map(
                    Box::new(TypePacked::String),
                    Box::new(TypePacked::Number(NumberType::U64)),
                ),
            ])),
        );
        assert!(tp.verify(10).is_ok());
    }

    #[test]
    fn test_verify_map_key_complex_valid() {
        // Map<Tuple<String, u64, Array<Bool>>, String> - complex key without map is valid
        let tp = TypePacked::Map(
            Box::new(TypePacked::Tuples(vec![
                TypePacked::String,
                TypePacked::Number(NumberType::U64),
                TypePacked::Array(Box::new(TypePacked::Bool)),
            ])),
            Box::new(TypePacked::String),
        );
        assert!(tp.verify(10).is_ok());
    }

    #[test]
    fn test_verify_deeply_nested_map_key_fails() {
        // Map<Tuple<Array<Optional<Map<...>>>>, u64> - deeply nested map in key fails
        let tp = TypePacked::Map(
            Box::new(TypePacked::Tuples(vec![
                TypePacked::Array(Box::new(
                    TypePacked::Optional(Box::new(
                        TypePacked::Map(
                            Box::new(TypePacked::String),
                            Box::new(TypePacked::String),
                        ),
                    )),
                )),
            ])),
            Box::new(TypePacked::Number(NumberType::U64)),
        );
        assert!(tp.verify(10).is_err());
    }
}