use silex_builder::EnvironmentBuilder;
use silex_bytecode::{Chunk, Module, OpCode};
use silex_decompiler::{Decompiler, FunctionSignature};
use silex_types::Type;

fn module(bytes: &[u8]) -> Module {
    let mut chunk = Chunk::new();
    for byte in bytes {
        chunk.write_u8(*byte);
    }
    let mut module = Module::new();
    module.add_internal_chunk(chunk);
    module
}

#[test]
fn malformed_and_unsupported_bytecode_has_locations() {
    let env = EnvironmentBuilder::<()>::new();
    for (bytes, message) in [
        (vec![255], "invalid opcode"),
        (vec![OpCode::Constant.as_byte(), 0], "truncated"),
        (vec![OpCode::Constant.as_byte(), 0, 0], "unknown constant"),
        (vec![OpCode::SysCall.as_byte(), 0, 0], "unknown syscall"),
        (
            vec![OpCode::InvokeChunk.as_byte(), 1, 0, 0],
            "unknown chunk",
        ),
        (vec![OpCode::MemoryLoad.as_byte(), 3, 0], "unknown register"),
        (vec![OpCode::Add.as_byte()], "stack underflow"),
        (
            vec![OpCode::Jump.as_byte(), 2, 0, 0, 0],
            "instruction boundary",
        ),
        (
            vec![OpCode::Jump.as_byte(), 0, 0, 0, 0],
            "unstructured jump",
        ),
        (vec![OpCode::DynamicCall.as_byte(), 0], "unsupported opcode"),
        (vec![OpCode::Cast.as_byte(), 255], "invalid parameter cast"),
    ] {
        let module = module(&bytes);
        let err = Decompiler::new(&module, &env).decompile().unwrap_err();
        assert_eq!(err.chunk, 0);
        assert_eq!(err.offset, 0);
        assert!(err.message.contains(message), "{err}");
    }
}

#[test]
fn empty_module_and_unknown_hook() {
    let env = EnvironmentBuilder::<()>::new();
    assert_eq!(
        Decompiler::new(&Module::new(), &env).decompile().unwrap(),
        ""
    );
    let mut module = Module::new();
    let mut chunk = Chunk::new();
    chunk.emit_opcode(OpCode::Return);
    module.add_hook_chunk(42, chunk);
    assert!(Decompiler::new(&module, &env)
        .decompile()
        .unwrap_err()
        .message
        .contains("unknown hook"));
}

#[test]
fn explicit_signatures_are_checked_by_the_parser() {
    let env = EnvironmentBuilder::<()>::new();
    let module = module(&[
        OpCode::MemorySet.as_byte(),
        0,
        0,
        OpCode::MemoryLoad.as_byte(),
        0,
        0,
        OpCode::Return.as_byte(),
    ]);
    let hint = FunctionSignature {
        name: "identity".into(),
        parameters: vec![Type::U64],
        return_type: Some(Type::U64),
    };
    let source = Decompiler::new(&module, &env)
        .with_function_signature(0, hint.clone())
        .decompile()
        .unwrap();
    assert!(source.contains("fn identity(arg0: u64) -> u64"));
    let invalid = FunctionSignature {
        name: "not a name".into(),
        ..hint.clone()
    };
    assert!(Decompiler::new(&module, &env)
        .with_function_signature(0, invalid)
        .decompile()
        .is_err());
    assert!(Decompiler::new(&module, &env)
        .with_function_signature(8, hint)
        .decompile()
        .is_err());
}

#[test]
fn arbitrary_single_opcodes_do_not_panic() {
    let env = EnvironmentBuilder::<()>::new();
    for byte in 0..=255 {
        let module = module(&[byte]);
        let _ = Decompiler::new(&module, &env).decompile();
    }
}

#[test]
fn deeply_nested_expressions_are_bounded_before_parsing() {
    let env = EnvironmentBuilder::<()>::new();
    let mut module = Module::new();
    module.add_constant(silex_types::Primitive::U64(1));
    let mut chunk = Chunk::new();
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(0);
    for _ in 0..150 {
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(0);
        chunk.emit_opcode(OpCode::Add);
    }
    chunk.emit_opcode(OpCode::Return);
    module.add_internal_chunk(chunk);
    assert!(Decompiler::new(&module, &env)
        .decompile()
        .unwrap_err()
        .message
        .contains("nesting exceeds"));
}
