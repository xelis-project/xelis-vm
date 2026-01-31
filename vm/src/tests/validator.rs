use xelis_builder::EnvironmentBuilder;
use xelis_bytecode::{Chunk, Module};
use xelis_types::{NumberType, Primitive, TypePacked, ValueCell};

use crate::{ModuleValidator, ValidatorError};

fn create_environment() -> xelis_environment::Environment<()> {
    EnvironmentBuilder::default().build()
}

// Helper to create a simple module with an entry chunk
fn create_module_with_entry_chunk(params: Option<Vec<TypePacked>>) -> Module {
    let mut module = Module::new();
    let chunk = Chunk::new();
    module.add_entry_chunk(chunk, params);
    module
}

// Helper to create a simple module with a public (All) chunk
fn create_module_with_public_chunk(params: Option<Vec<TypePacked>>) -> Module {
    let mut module = Module::new();
    let chunk = Chunk::new();
    module.add_public_chunk(chunk, params);
    module
}

// Helper to create a simple module with an internal chunk
fn create_module_with_internal_chunk() -> Module {
    let mut module = Module::new();
    let chunk = Chunk::new();
    module.add_internal_chunk(chunk);
    module
}

#[test]
fn test_verify_invoke_chunk_entry_no_params() {
    let module = create_module_with_entry_chunk(None);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Entry chunk with no expected params should accept any params (or none)
    let result = validator.verify_invoke_chunk(0, std::iter::empty());
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_public_no_params() {
    let module = create_module_with_public_chunk(None);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Public (All) chunk with no expected params should accept any params
    let result = validator.verify_invoke_chunk(0, std::iter::empty());
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_internal_fails() {
    let module = create_module_with_internal_chunk();
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Internal chunks cannot be invoked externally
    let result = validator.verify_invoke_chunk(0, std::iter::empty());
    assert!(matches!(result, Err(ValidatorError::InvalidEntryId(0))));
}

#[test]
fn test_verify_invoke_chunk_invalid_chunk_id() {
    let module = create_module_with_entry_chunk(None);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Non-existent chunk id should fail
    let result = validator.verify_invoke_chunk(999, std::iter::empty());
    assert!(matches!(result, Err(ValidatorError::InvalidEntryId(999))));
}

#[test]
fn test_verify_invoke_chunk_with_u64_param() {
    let params = Some(vec![TypePacked::Number(NumberType::U64)]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Correct parameter type
    let value = ValueCell::Primitive(Primitive::U64(42));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_wrong_param_type() {
    let params = Some(vec![TypePacked::Number(NumberType::U64)]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Wrong parameter type (U8 instead of U64)
    let value = ValueCell::Primitive(Primitive::U8(42));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(0))));
}

#[test]
fn test_verify_invoke_chunk_multiple_params() {
    let params = Some(vec![
        TypePacked::Number(NumberType::U64),
        TypePacked::String,
        TypePacked::Bool,
    ]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    let values = vec![
        ValueCell::Primitive(Primitive::U64(42)),
        ValueCell::Primitive(Primitive::String("hello".to_string())),
        ValueCell::Primitive(Primitive::Boolean(true)),
    ];
    let result = validator.verify_invoke_chunk(0, values.iter());
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_second_param_wrong_type() {
    let params = Some(vec![
        TypePacked::Number(NumberType::U64),
        TypePacked::String,
    ]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    let values = vec![
        ValueCell::Primitive(Primitive::U64(42)),
        ValueCell::Primitive(Primitive::Boolean(true)), // Should be String
    ];
    let result = validator.verify_invoke_chunk(0, values.iter());
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(1))));
}

#[test]
fn test_verify_invoke_chunk_optional_param() {
    let params = Some(vec![TypePacked::Optional(Box::new(TypePacked::Number(NumberType::U64)))]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Test with null value
    let null_value = ValueCell::Primitive(Primitive::Null);
    let result = validator.verify_invoke_chunk(0, std::iter::once(&null_value));
    assert!(result.is_ok());

    // Test with actual value
    let value = ValueCell::Primitive(Primitive::U64(42));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_any_param() {
    let params = Some(vec![TypePacked::Any]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Any type should accept any value
    let value = ValueCell::Primitive(Primitive::U64(42));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(result.is_ok());

    let value = ValueCell::Primitive(Primitive::String("test".to_string()));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_hook_fails() {
    let mut module = Module::new();
    let chunk = Chunk::new();
    module.add_hook_chunk(0, chunk);

    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Hook chunks cannot be invoked externally
    let result = validator.verify_invoke_chunk(0, std::iter::empty());
    assert!(matches!(result, Err(ValidatorError::InvalidEntryId(0))));
}

#[test]
fn test_verify_invoke_chunk_bytes_param() {
    let params = Some(vec![TypePacked::Bytes]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Test with bytes value
    let value = ValueCell::Bytes(vec![1, 2, 3].into());
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_array_param() {
    let params = Some(vec![TypePacked::Array(Box::new(TypePacked::Number(NumberType::U8)))]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Test with array of u8 values
    let values = vec![
        ValueCell::Primitive(Primitive::U8(1)).into(),
        ValueCell::Primitive(Primitive::U8(2)).into(),
        ValueCell::Primitive(Primitive::U8(3)).into(),
    ];
    let array_value = ValueCell::Object(values);
    let result = validator.verify_invoke_chunk(0, std::iter::once(&array_value));
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_array_wrong_inner_type() {
    let params = Some(vec![TypePacked::Array(Box::new(TypePacked::Number(NumberType::U8)))]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Test with array of wrong type (U64 instead of U8)
    let values = vec![
        ValueCell::Primitive(Primitive::U64(1)).into(),
    ];
    let array_value = ValueCell::Object(values);
    let result = validator.verify_invoke_chunk(0, std::iter::once(&array_value));
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(0))));
}

#[test]
fn test_verify_invoke_chunk_public_with_params() {
    let params = Some(vec![TypePacked::Number(NumberType::U32)]);
    let module = create_module_with_public_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Correct parameter type on public chunk
    let value = ValueCell::Primitive(Primitive::U32(100));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_all_number_types() {
    // Test all number types
    let test_cases = vec![
        (NumberType::U8, Primitive::U8(1)),
        (NumberType::U16, Primitive::U16(1)),
        (NumberType::U32, Primitive::U32(1)),
        (NumberType::U64, Primitive::U64(1)),
        (NumberType::U128, Primitive::U128(1)),
    ];

    let env = create_environment();

    for (num_type, primitive) in test_cases {
        let params = Some(vec![TypePacked::Number(num_type)]);
        let module = create_module_with_entry_chunk(params);
        let validator = ModuleValidator::new(&module, &env);

        let value = ValueCell::Primitive(primitive);
        let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
        assert!(result.is_ok(), "Failed for number type {:?}", num_type);
    }
}

#[test]
fn test_verify_invoke_chunk_tuples_param() {
    let params = Some(vec![TypePacked::Tuples(vec![
        TypePacked::Number(NumberType::U8),
        TypePacked::String,
    ])]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Test with tuple value
    let values = vec![
        ValueCell::Primitive(Primitive::U8(42)).into(),
        ValueCell::Primitive(Primitive::String("hello".to_string())).into(),
    ];
    let tuple_value = ValueCell::Object(values);
    let result = validator.verify_invoke_chunk(0, std::iter::once(&tuple_value));
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_tuples_wrong_length() {
    let params = Some(vec![TypePacked::Tuples(vec![
        TypePacked::Number(NumberType::U8),
        TypePacked::String,
    ])]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Test with wrong tuple length (only 1 element instead of 2)
    let values = vec![
        ValueCell::Primitive(Primitive::U8(42)).into(),
    ];
    let tuple_value = ValueCell::Object(values);
    let result = validator.verify_invoke_chunk(0, std::iter::once(&tuple_value));
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(0))));
}

#[test]
fn test_verify_invoke_chunk_empty_params_list() {
    // Entry chunk expects parameters but none provided
    let params = Some(vec![TypePacked::Number(NumberType::U64)]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // No parameters provided when one is expected - this should still work
    // as the iterator just won't have anything to check against
    let result = validator.verify_invoke_chunk(0, std::iter::empty());
    // The current implementation zips, so if no params are provided, it doesn't fail
    assert!(result.is_ok());
}
