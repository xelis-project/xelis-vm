use indexmap::IndexMap;
use xelis_builder::EnvironmentBuilder;
use xelis_bytecode::{Chunk, Module};
use xelis_types::{impl_opaque, traits::{JSONHelper, Serializable}, NumberType, OpaqueWrapper, Primitive, TypePacked, ValueCell, ValuePointer};
use xelis_lexer::Lexer;
use xelis_parser::Parser;
use xelis_compiler::Compiler;

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

    // No parameters provided when one is expected
    let result = validator.verify_invoke_chunk(0, std::iter::empty());
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParamsSize(_, _))));
}

fn compile_module_with_enforce(code: &str) -> (xelis_bytecode::Module, xelis_environment::Environment<()>) {
    let tokens = Lexer::new(code).get().unwrap();
    let env_builder = EnvironmentBuilder::<()>::default();
    let (program, _) = Parser::new(tokens, &env_builder).parse().unwrap();
    let env = env_builder.build();
    let module = Compiler::new(&program, &env)
        .with_enforce_public_parameters(true)
        .compile()
        .unwrap();
    (module, env)
}

fn compile_module_without_enforce(code: &str) -> (xelis_bytecode::Module, xelis_environment::Environment<()>) {
    let tokens = Lexer::new(code).get().unwrap();
    let env_builder = EnvironmentBuilder::<()>::default();
    let (program, _) = Parser::new(tokens, &env_builder).parse().unwrap();
    let env = env_builder.build();
    let module = Compiler::new(&program, &env).compile().unwrap();
    (module, env)
}

#[test]
fn test_compiler_entry_enforce_correct_params() {
    let (module, env) = compile_module_with_enforce(
        "entry main(a: u64, b: u64) -> u64 { return a + b }"
    );
    let validator = ModuleValidator::new(&module, &env);

    let args = vec![
        ValueCell::Primitive(Primitive::U64(10)),
        ValueCell::Primitive(Primitive::U64(20)),
    ];
    let result = validator.verify_invoke_chunk(0, args.iter());
    assert!(result.is_ok());
}

#[test]
fn test_compiler_entry_enforce_wrong_type() {
    let (module, env) = compile_module_with_enforce(
        "entry main(a: u64, b: u64) -> u64 { return a + b }"
    );
    let validator = ModuleValidator::new(&module, &env);

    // Pass u32 instead of u64 for first param
    let args = vec![
        ValueCell::Primitive(Primitive::U32(10)),
        ValueCell::Primitive(Primitive::U64(20)),
    ];
    let result = validator.verify_invoke_chunk(0, args.iter());
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(0))));
}

#[test]
fn test_compiler_entry_enforce_second_param_wrong_type() {
    let (module, env) = compile_module_with_enforce(
        "entry main(a: u64, b: u64) -> u64 { return a + b }"
    );
    let validator = ModuleValidator::new(&module, &env);

    let args = vec![
        ValueCell::Primitive(Primitive::U64(10)),
        ValueCell::Primitive(Primitive::U32(20)), // wrong: u32 instead of u64
    ];
    let result = validator.verify_invoke_chunk(0, args.iter());
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(1))));
}

#[test]
fn test_compiler_pub_fn_enforce_correct_params() {
    let (module, env) = compile_module_with_enforce(
        "pub fn add(a: u64, b: u64) -> u64 { return a + b }"
    );
    let validator = ModuleValidator::new(&module, &env);

    let args = vec![
        ValueCell::Primitive(Primitive::U64(5)),
        ValueCell::Primitive(Primitive::U64(7)),
    ];
    let result = validator.verify_invoke_chunk(0, args.iter());
    assert!(result.is_ok());
}

#[test]
fn test_compiler_pub_fn_enforce_wrong_type() {
    let (module, env) = compile_module_with_enforce(
        "pub fn add(a: u64, b: u64) -> u64 { return a + b }"
    );
    let validator = ModuleValidator::new(&module, &env);

    let args = vec![
        ValueCell::Primitive(Primitive::U64(5)),
        ValueCell::Primitive(Primitive::Boolean(true)), // wrong: bool instead of u64
    ];
    let result = validator.verify_invoke_chunk(0, args.iter());
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(1))));
}

#[test]
fn test_compiler_no_enforce_skips_validation() {
    // Without enforce_public_parameters, parameters() == None → validation is skipped
    let (module, env) = compile_module_without_enforce(
        "pub fn add(a: u64, b: u64) -> u64 { return a + b }"
    );
    let validator = ModuleValidator::new(&module, &env);

    // Wrong types but since parameters() is None, validator does not check
    let args = vec![
        ValueCell::Primitive(Primitive::Boolean(true)),
        ValueCell::Primitive(Primitive::String("oops".to_string())),
    ];
    let result = validator.verify_invoke_chunk(0, args.iter());
    assert!(result.is_ok());
}

#[test]
fn test_compiler_private_fn_rejected() {
    // Private fn has Access::Private → verify_invoke_chunk should reject it
    let (module, env) = compile_module_with_enforce(
        "fn helper(x: u64) -> u64 { return x }"
    );
    let validator = ModuleValidator::new(&module, &env);

    let result = validator.verify_invoke_chunk(0, std::iter::empty());
    assert!(matches!(result, Err(ValidatorError::InvalidEntryId(0))));
}

#[test]
fn test_compiler_entry_no_params_enforce() {
    let (module, env) = compile_module_with_enforce(
        "entry main() -> u64 { return 42 }"
    );
    let validator = ModuleValidator::new(&module, &env);

    let result = validator.verify_invoke_chunk(0, std::iter::empty());
    assert!(result.is_ok());
}

#[test]
fn test_compiler_entry_enforce_has_type_packed() {
    // Verify the compiled module actually stores TypePacked when enforce is enabled
    let (module, _env) = compile_module_with_enforce(
        "entry main(a: u64, b: u64) -> u64 { return a + b }"
    );
    let chunk = module.get_chunk_at(0).unwrap();
    assert_eq!(
        chunk.access.parameters(),
        Some(&vec![TypePacked::Number(NumberType::U64), TypePacked::Number(NumberType::U64)])
    );
}

#[test]
fn test_compiler_no_enforce_has_no_type_packed() {
    // Without enforce, parameters() must be None
    let (module, _env) = compile_module_without_enforce(
        "pub fn add(a: u64, b: u64) -> u64 { return a + b }"
    );
    let chunk = module.get_chunk_at(0).unwrap();
    assert_eq!(chunk.access.parameters(), None);
}

#[test]
fn test_verify_invoke_chunk_map_param_correct() {
    // Map<u64, string> parameter
    let params = Some(vec![TypePacked::Map(
        Box::new(TypePacked::Number(NumberType::U64)),
        Box::new(TypePacked::String),
    )]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    let mut map = IndexMap::new();
    map.insert(
        ValueCell::Primitive(Primitive::U64(1)),
        ValuePointer::from(ValueCell::Primitive(Primitive::String("hello".to_string()))),
    );
    map.insert(
        ValueCell::Primitive(Primitive::U64(2)),
        ValuePointer::from(ValueCell::Primitive(Primitive::String("world".to_string()))),
    );
    let value = ValueCell::Map(Box::new(map));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_map_wrong_key_type() {
    // Expects Map<u64, string> but key is u8
    let params = Some(vec![TypePacked::Map(
        Box::new(TypePacked::Number(NumberType::U64)),
        Box::new(TypePacked::String),
    )]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    let mut map = IndexMap::new();
    map.insert(
        ValueCell::Primitive(Primitive::U8(1)), // wrong key type
        ValuePointer::from(ValueCell::Primitive(Primitive::String("hello".to_string()))),
    );
    let value = ValueCell::Map(Box::new(map));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(0))));
}

#[test]
fn test_verify_invoke_chunk_map_wrong_value_type() {
    // Expects Map<u64, string> but value is bool
    let params = Some(vec![TypePacked::Map(
        Box::new(TypePacked::Number(NumberType::U64)),
        Box::new(TypePacked::String),
    )]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    let mut map = IndexMap::new();
    map.insert(
        ValueCell::Primitive(Primitive::U64(1)),
        ValuePointer::from(ValueCell::Primitive(Primitive::Boolean(true))), // wrong value type
    );
    let value = ValueCell::Map(Box::new(map));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(0))));
}

#[test]
fn test_verify_invoke_chunk_oneof_correct_variant() {
    // OneOf representing an enum: variant 0 = (u64), variant 1 = (string, bool)
    let params = Some(vec![TypePacked::OneOf(vec![
        vec![TypePacked::Number(NumberType::U64)],
        vec![TypePacked::String, TypePacked::Bool],
    ])]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Use variant 0 with a u64 payload
    let values = vec![
        ValueCell::Primitive(Primitive::U8(0)).into(), // variant index
        ValueCell::Primitive(Primitive::U64(42)).into(),
    ];
    let result = validator.verify_invoke_chunk(0, std::iter::once(&ValueCell::Object(values)));
    assert!(result.is_ok());

    // Use variant 1 with (string, bool) payload
    let values = vec![
        ValueCell::Primitive(Primitive::U8(1)).into(), // variant index
        ValueCell::Primitive(Primitive::String("hi".to_string())).into(),
        ValueCell::Primitive(Primitive::Boolean(false)).into(),
    ];
    let result = validator.verify_invoke_chunk(0, std::iter::once(&ValueCell::Object(values)));
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_oneof_wrong_variant_payload() {
    // OneOf with variant 0 = (u64), but we supply a bool payload
    let params = Some(vec![TypePacked::OneOf(vec![
        vec![TypePacked::Number(NumberType::U64)],
        vec![TypePacked::String, TypePacked::Bool],
    ])]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    let values = vec![
        ValueCell::Primitive(Primitive::U8(0)).into(), // variant 0
        ValueCell::Primitive(Primitive::Boolean(true)).into(), // wrong: bool instead of u64
    ];
    let result = validator.verify_invoke_chunk(0, std::iter::once(&ValueCell::Object(values)));
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(0))));
}

#[test]
fn test_compiler_entry_enforce_struct_param() {
    // Structs are represented as Tuples in TypePacked
    let (module, env) = compile_module_with_enforce(
        "struct Point { x: u64, y: u64 } entry main(p: Point) -> u64 { return p.x + p.y }"
    );
    let validator = ModuleValidator::new(&module, &env);

    let struct_value = ValueCell::Object(vec![
        ValueCell::Primitive(Primitive::U64(3)).into(),
        ValueCell::Primitive(Primitive::U64(4)).into(),
    ]);
    let result = validator.verify_invoke_chunk(0, std::iter::once(&struct_value));
    assert!(result.is_ok());
}

#[test]
fn test_compiler_entry_enforce_struct_param_wrong_field_type() {
    let (module, env) = compile_module_with_enforce(
        "struct Point { x: u64, y: u64 } entry main(p: Point) -> u64 { return p.x + p.y }"
    );
    let validator = ModuleValidator::new(&module, &env);

    // y field is string instead of u64
    let struct_value = ValueCell::Object(vec![
        ValueCell::Primitive(Primitive::U64(3)).into(),
        ValueCell::Primitive(Primitive::String("bad".to_string())).into(),
    ]);
    let result = validator.verify_invoke_chunk(0, std::iter::once(&struct_value));
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(0))));
}

#[test]
fn test_verify_invoke_chunk_optional_wrong_inner_type() {
    // Optional<u64> but the non-null value is a string → should fail
    let params = Some(vec![TypePacked::Optional(Box::new(TypePacked::Number(NumberType::U64)))]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    let value = ValueCell::Primitive(Primitive::String("not a u64".to_string()));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(0))));
}

#[test]
fn test_verify_invoke_chunk_tuples_wrong_element_type() {
    // Tuple (u8, string) but second element is bool instead of string
    let params = Some(vec![TypePacked::Tuples(vec![
        TypePacked::Number(NumberType::U8),
        TypePacked::String,
    ])]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    let value = ValueCell::Object(vec![
        ValueCell::Primitive(Primitive::U8(1)).into(),
        ValueCell::Primitive(Primitive::Boolean(true)).into(), // wrong: bool instead of string
    ]);
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(0))));
}

#[test]
fn test_verify_invoke_chunk_oneof_out_of_range_variant() {
    let params = Some(vec![TypePacked::OneOf(vec![
        vec![TypePacked::Number(NumberType::U64)],
    ])]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Variant index 5 but only variant 0 exists
    let values = vec![
        ValueCell::Primitive(Primitive::U8(5)).into(),
        ValueCell::Primitive(Primitive::U64(42)).into(),
    ];
    let result = validator.verify_invoke_chunk(0, std::iter::once(&ValueCell::Object(values)));
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(0))));
}

#[test]
fn test_verify_invoke_chunk_oneof_missing_variant_index() {
    // Object with no elements, missing the leading u8 variant index
    let params = Some(vec![TypePacked::OneOf(vec![
        vec![TypePacked::Number(NumberType::U64)],
    ])]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    let result = validator.verify_invoke_chunk(0, std::iter::once(&ValueCell::Object(vec![])));
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(0))));
}

#[test]
fn test_verify_invoke_chunk_oneof_unit_variant() {
    // Enum variant with no payload (unit variant)
    let params = Some(vec![TypePacked::OneOf(vec![
        vec![],  // unit variant 0
        vec![TypePacked::Number(NumberType::U64)],
    ])]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    // Only the variant index, no payload
    let values = vec![ValueCell::Primitive(Primitive::U8(0)).into()];
    let result = validator.verify_invoke_chunk(0, std::iter::once(&ValueCell::Object(values)));
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_map_empty_is_ok() {
    // An empty map should satisfy any Map<K,V> constraint
    let params = Some(vec![TypePacked::Map(
        Box::new(TypePacked::Number(NumberType::U64)),
        Box::new(TypePacked::String),
    )]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    let value = ValueCell::Map(Box::new(IndexMap::new()));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_range_param() {
    // TypePacked::Range(U64) should accept a Primitive::Range of u64 values
    let params = Some(vec![TypePacked::Range(Box::new(NumberType::U64))]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment();
    let validator = ModuleValidator::new(&module, &env);

    let value = ValueCell::Primitive(Primitive::Range(Box::new((
        Primitive::U64(0),
        Primitive::U64(100),
    ))));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(result.is_ok());
}

#[test]
fn test_verify_invoke_chunk_opaque_param_correct() {
    #[derive(Debug, PartialEq, Eq, Hash, Clone)]
    struct TestOpaque;

    impl JSONHelper for TestOpaque {
        fn is_json_supported(&self) -> bool { false }
        fn serialize_json(&self) -> Result<serde_json::Value, anyhow::Error> { todo!() }
    }

    impl Serializable for TestOpaque {
        fn get_size(&self) -> usize { 0 }
        fn is_serializable(&self) -> bool { false }
        fn serialize(&self, _: &mut Vec<u8>) -> usize { 0 }
    }

    impl_opaque!("TestOpaque", TestOpaque);

    let mut env_builder = EnvironmentBuilder::<()>::default();
    let opaque_type = env_builder.register_opaque::<TestOpaque>("TestOpaque", true);
    let env = env_builder.build();

    // TypePacked::Opaque index matching the registered opaque id
    let params = Some(vec![TypePacked::Opaque(opaque_type.id())]);
    let mut module = Module::new();
    module.add_entry_chunk(Chunk::new(), params);

    let validator = ModuleValidator::new(&module, &env);

    let value = ValueCell::Primitive(Primitive::Opaque(OpaqueWrapper::new(TestOpaque)));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(result.is_ok(), "opaque type id={} env={:?}", opaque_type.id(), env.get_opaques());
}

#[test]
fn test_verify_invoke_chunk_opaque_wrong_type() {
    // Opaque param but a plain u64 is provided → should fail
    let params = Some(vec![TypePacked::Opaque(0)]);
    let module = create_module_with_entry_chunk(params);
    let env = create_environment(); // no opaques registered

    let validator = ModuleValidator::new(&module, &env);

    let value = ValueCell::Primitive(Primitive::U64(42));
    let result = validator.verify_invoke_chunk(0, std::iter::once(&value));
    assert!(matches!(result, Err(ValidatorError::InvalidChunkParam(0))));
}

#[test]
fn test_compiler_entry_enforce_enum_all_unit_variants() {
    // enum Foo { A, B } — both variants have no payload
    // TypePacked should be OneOf([ [], [] ])
    let (module, env) = compile_module_with_enforce(
        "enum Foo { A, B } entry main(f: Foo) -> u64 { return 0u64 }"
    );
    let validator = ModuleValidator::new(&module, &env);

    // Variant A (index 0) — unit: only the variant index
    let variant_a = ValueCell::Object(vec![
        ValueCell::Primitive(Primitive::U8(0)).into(),
    ]);
    assert!(validator.verify_invoke_chunk(0, std::iter::once(&variant_a)).is_ok());

    // Variant B (index 1) — unit: only the variant index
    let variant_b = ValueCell::Object(vec![
        ValueCell::Primitive(Primitive::U8(1)).into(),
    ]);
    assert!(validator.verify_invoke_chunk(0, std::iter::once(&variant_b)).is_ok());

    // Out-of-range variant index → should fail
    let bad_variant = ValueCell::Object(vec![
        ValueCell::Primitive(Primitive::U8(2)).into(),
    ]);
    assert!(matches!(
        validator.verify_invoke_chunk(0, std::iter::once(&bad_variant)),
        Err(ValidatorError::InvalidChunkParam(0))
    ));

    // Unit variant with unexpected payload → should fail
    let variant_a_with_payload = ValueCell::Object(vec![
        ValueCell::Primitive(Primitive::U8(0)).into(),
        ValueCell::Primitive(Primitive::U64(42)).into(), // unexpected
    ]);
    assert!(matches!(
        validator.verify_invoke_chunk(0, std::iter::once(&variant_a_with_payload)),
        Err(ValidatorError::InvalidChunkParam(0))
    ));
}

#[test]
fn test_compiler_entry_enforce_enum_mixed_variants() {
    // enum Foo { A(string), B } — A has a string payload, B is unit
    // TypePacked should be OneOf([ [String], [] ])
    let (module, env) = compile_module_with_enforce(
        "enum Foo { A(string), B } entry main(f: Foo) -> u64 { return 0u64 }"
    );
    let validator = ModuleValidator::new(&module, &env);

    // Variant A (index 0) with correct string payload
    let variant_a = ValueCell::Object(vec![
        ValueCell::Primitive(Primitive::U8(0)).into(),
        ValueCell::Primitive(Primitive::String("hello".to_string())).into(),
    ]);
    assert!(validator.verify_invoke_chunk(0, std::iter::once(&variant_a)).is_ok());

    // Variant B (index 1) — unit
    let variant_b = ValueCell::Object(vec![
        ValueCell::Primitive(Primitive::U8(1)).into(),
    ]);
    assert!(validator.verify_invoke_chunk(0, std::iter::once(&variant_b)).is_ok());

    // Variant A with wrong payload type (u64 instead of string) → should fail
    let variant_a_bad = ValueCell::Object(vec![
        ValueCell::Primitive(Primitive::U8(0)).into(),
        ValueCell::Primitive(Primitive::U64(99)).into(),
    ]);
    assert!(matches!(
        validator.verify_invoke_chunk(0, std::iter::once(&variant_a_bad)),
        Err(ValidatorError::InvalidChunkParam(0))
    ));

    // Variant B with unexpected payload → should fail
    let variant_b_with_payload = ValueCell::Object(vec![
        ValueCell::Primitive(Primitive::U8(1)).into(),
        ValueCell::Primitive(Primitive::String("unexpected".to_string())).into(),
    ]);
    assert!(matches!(
        validator.verify_invoke_chunk(0, std::iter::once(&variant_b_with_payload)),
        Err(ValidatorError::InvalidChunkParam(0))
    ));
}
