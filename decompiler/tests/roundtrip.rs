use silex_builder::EnvironmentBuilder;
use silex_bytecode::Module;
use silex_compiler::Compiler;
use silex_decompiler::Decompiler;
use silex_environment::ModuleMetadata;
use silex_lexer::Lexer;
use silex_parser::Parser;
use silex_types::Primitive;
use xelis_vm::{ModuleValidator, VM};

fn compile(source: &str, env: &EnvironmentBuilder<()>, packed: bool, fold: bool) -> Module {
    let tokens = Lexer::new(source)
        .get()
        .unwrap_or_else(|e| panic!("{e}\n{source}"));
    let mut parser = Parser::new(tokens, env);
    parser.set_const_upgrading_disabled(!fold);
    let (program, _) = parser.parse().unwrap_or_else(|e| panic!("{e}\n{source}"));
    Compiler::new(&program, env.environment())
        .with_enforce_public_parameters(packed)
        .compile()
        .unwrap()
}

fn roundtrip(source: &str) -> String {
    let env = EnvironmentBuilder::default();
    let module = compile(source, &env, false, false);
    let recovered = Decompiler::new(&module, &env)
        .decompile()
        .unwrap_or_else(|e| panic!("{e}\n{source}"));
    let rebuilt = compile(&recovered, &env, false, false);
    if let Some(entry) = module
        .chunks()
        .iter()
        .position(|c| matches!(c.access, silex_bytecode::Access::Entry { .. }))
    {
        assert_eq!(
            execute(&module, &env, entry),
            execute(&rebuilt, &env, entry),
            "{source}\n{recovered}"
        );
    }
    recovered
}

fn execute(module: &Module, env: &EnvironmentBuilder<()>, entry: usize) -> Primitive {
    ModuleValidator::new(module, env.environment())
        .verify()
        .unwrap();
    let mut vm = VM::default();
    vm.append_module(ModuleMetadata {
        module: module.into(),
        environment: env.environment().into(),
        metadata: (&()).into(),
    })
    .unwrap();
    vm.context_mut().set_gas_limit(1_000_000);
    vm.invoke_chunk_id_unchecked(entry).unwrap();
    vm.run_blocking().unwrap().into_value().unwrap()
}

#[test]
fn source_grammar() {
    for source in [
        "entry main() { return 42 }",
        "pub fn add(a: u64, b: u64) -> u64 { return a + b } entry main() { return add(2, 7) }",
        "fn add(a: u64, b: u64) -> u64 { return a + b } entry main() { return add(2, 7) }",
        "entry main() { let x: u64 = 2 x += 3 return x * 2 }",
        "entry main() { let x: u64 = 2 if x > 1 { return 7 } else { return 3 } }",
        "entry main() { let x: u64 = 2 if x > 1 { x += 1 } return x }",
        "entry main() { let x: u64 = 0 while x < 4 { x += 1 } return x }",
        "entry main() { let x: u64 = 0 while x < 4 { if x == 2 { break } x += 1 } return x }",
        "entry main() { let x: u64 = 0 foreach v in 0..4 { x += v } return x }",
        "entry main() { let x: u64 = 0 for i: u64 = 0; i < 4; i += 1 { x += i } return x }",
        "entry main() { let x: u64 = 2 return x > 1 ? 7 : 3 }",
        "entry main() { let x: u64 = 2 if x > 0 && x < 4 { return 7 } return 3 }",
        "entry main() { let x: u64 = 2 if x == 0 || x < 4 { return 7 } return 3 }",
        "entry main() { let a: u64[] = [2, 3] a[0] += 1 return a[0] }",
        "entry main() { let s = \"hello\" return s.len() as u64 }",
        "entry main() { let a: u64[] = [2, 9] return (a[0] + 1) + a.remove(0) }",
        "entry main() { let a: u64[] = [2, 9] if false && a.pop().unwrap() == 9 { return 0 } return a.len() as u64 }",
        "entry main() { let a: u64[] = [0, 1, 2] let n: u64 = 0 while a.pop().unwrap() > 0 { n += 1 } return n }",
        "entry main() { let x: u64 = 0 while x < 4 { x += 1 if x == 2 { continue } } return x }",
        "entry main() { let x: u64 = 0 foreach v in [1, 2, 3] { if v == 2 { continue } x += v } return x }",
        "entry main() { let a = bytes::from_hex(\"0102\") return a.len() as u64 }",
        "fn empty() {} entry main() { empty() return 7 }",
        "fn length(a: u64[]) -> u32 { return a.len() } entry main() { return length([1, 2]) as u64 }",
        "fn factorial(n: u64) -> u64 { if n <= 1 { return 1 } return n * factorial(n - 1) } entry main() { return factorial(5) }",
        "entry main() { let a: u64 = 7 return ((a & 3) | 8) ^ (a >> 1) }",
        "entry main() { let m: map<u64, u64> = {1: 2, 3: 4} return m.get(3).unwrap() }",
        "entry main() { let t: (u64, string) = (7, \"hello\") return t.0 }",
        "entry main() { let (a, b) = (7, 9) return a * 10 + b }",
        "entry main() { let (a, _, b) = (7, \"hello\", 9) return a * 10 + b }",
        "entry main() { let m: map<u64, u64> = {} m.insert(1, 5) return m.get(1).unwrap() }",
        "entry main() -> string { let x: u64 = 1 if x > 0 { return \"a\nb\" } return \"other\" }",
    ] { roundtrip(source); }
}

#[test]
fn native_names() {
    let source = roundtrip(
        "entry main() { let s = \"hello\" println(s.to_uppercase()) return s.len() as u64 }",
    );
    assert!(source.contains("println("));
    assert!(source.contains(".to_uppercase("));
    assert!(source.contains(".len("));
}

#[test]
fn packed_parameters_and_hooks() {
    let mut env = EnvironmentBuilder::default();
    env.register_hook(
        "constructor",
        vec![("value", silex_types::Type::U64)],
        Some(silex_types::Type::U64),
    );
    let module = compile("pub fn test(v: u64) -> u64 { return v + 1 } hook constructor(v: u64) -> u64 { return test(v) }", &env, true, false);
    let recovered = Decompiler::new(&module, &env).decompile().unwrap();
    assert!(recovered.contains("hook constructor(arg0: u64) -> u64"));
    compile(&recovered, &env, true, false);
}

#[test]
fn folded_constants_and_escaping() {
    let env = EnvironmentBuilder::default();
    for source in [
        "entry main() { return 1 + 2 }",
        "entry main() { let a: u64[] = [2, 3] return a[1] }",
        "entry main() { let s = \"a\\\"b\\\\c\nété\" return s.len() as u64 }",
        "entry main() { let b = b\"abc\" return b.len() as u64 }",
    ] {
        let module = compile(source, &env, false, true);
        let recovered = Decompiler::new(&module, &env).decompile().unwrap();
        let rebuilt = compile(&recovered, &env, false, true);
        assert_eq!(
            execute(&module, &env, 0),
            execute(&rebuilt, &env, 0),
            "{source}\n{recovered}"
        );
    }
}

#[test]
fn custom_environment_resolves_ids_without_default_std() {
    use silex_environment::{FunctionHandler, SysCallResult};
    use silex_types::{Constant, Type};
    let mut env = EnvironmentBuilder::new();
    env.register_native_function(
        "host_value",
        None,
        vec![],
        FunctionHandler::Sync(|_, _, _, _| Ok(SysCallResult::Return(Primitive::U64(42).into()))),
        1,
        Some(Type::U64),
    );
    env.register_native_function(
        "scaled",
        Some(Type::U64),
        vec![("scale", Type::U64)],
        FunctionHandler::Sync(|v, args, _, _| {
            Ok(SysCallResult::Return(
                Primitive::U64(v?.as_u64()? * args[0].as_u64()?).into(),
            ))
        }),
        1,
        Some(Type::U64),
    );
    env.register_constant(
        Type::U64,
        "ANSWER",
        Constant::Primitive(Primitive::U64(42)),
        Type::U64,
    );
    let module = compile(
        "entry main() { return host_value().scaled(2) }",
        &env,
        false,
        false,
    );
    let recovered = Decompiler::new(&module, &env).decompile().unwrap();
    assert!(recovered.contains("host_value()"));
    assert!(recovered.contains(".scaled(2u64)"));
    let rebuilt = compile(&recovered, &env, false, false);
    assert_eq!(execute(&module, &env, 0), execute(&rebuilt, &env, 0));
}

#[test]
fn calls_do_not_create_synthetic_locals() {
    let env = EnvironmentBuilder::default();
    let source = "fn value() -> u64 { return 2 } entry main() { return value() + 1 }";
    let module = compile(source, &env, false, false);
    let recovered = Decompiler::new(&module, &env).decompile().unwrap();

    assert!(recovered.contains("function0()"), "{recovered}");
    assert!(recovered.contains("1u64"), "{recovered}");
    assert!(!recovered.contains("let"));

    let tuple_source = "fn tuple() -> (u64, (u64, u64)) { return (1, (2, 3)) } entry main() { let (a, (b, c)) = tuple() return a + b + c }";
    let env = EnvironmentBuilder::default();
    let tuple_module = compile(tuple_source, &env, false, false);
    let tuple_recovered = Decompiler::new(&tuple_module, &env).decompile().unwrap();
    assert_eq!(tuple_recovered.matches("function0()").count(), 2);
    assert!(!tuple_recovered.contains("field"), "{tuple_recovered}");
    let tuple_rebuilt = compile(&tuple_recovered, &env, false, false);
    assert_eq!(
        execute(&tuple_module, &env, 1),
        execute(&tuple_rebuilt, &env, 1),
        "{tuple_recovered}"
    );
}

#[test]
fn discarded_values_do_not_create_bindings() {
    let recovered =
        roundtrip("fn value() -> u64 { return 2 } entry main() { value(); value(); return 7 }");
    assert!(!recovered.contains("let"), "{recovered}");
    for source in [
        "entry main() { let a: u64[] = [2, 9] a.remove(0); a.pop(); return a.len() as u64 }",
        "entry main() { let x: u64 = 2 let _ = x + 1 let _ = x let _ = 7 return x }",
        "entry main() { let _ = [1, 2] let _ = {1: 2} let _ = (1, true) return 7 }",
    ] {
        let recovered = roundtrip(source);
        assert!(!recovered.contains("let _ ="), "{recovered}");
    }
}

#[test]
fn pop_n_preserves_expression_evaluation_order() {
    use silex_bytecode::{Chunk, OpCode};

    let env = EnvironmentBuilder::default();
    let mut module = compile(
        "entry main() { let a: u64[] = [1, 2, 3] a.remove(0); a.remove(1); return a[0] }",
        &env,
        false,
        false,
    );
    // Batch the two standalone results into one PopN without changing the
    // order of their producing instructions. There are no jumps to relocate.
    let bytes = module.chunks()[0].chunk.get_instructions();
    let mut chunk = Chunk::new();
    let mut pc = 0;
    let mut pops = 0;
    while pc < bytes.len() {
        let op = OpCode::from_byte(bytes[pc]).unwrap();
        let end = pc + 1 + op.arguments_bytes();
        if matches!(op, OpCode::Pop) {
            pops += 1;
            if pops == 2 {
                chunk.emit_opcode(OpCode::PopN);
                chunk.write_u8(2);
            }
        } else {
            for byte in &bytes[pc..end] {
                chunk.write_u8(*byte);
            }
        }
        pc = end;
    }
    assert_eq!(pops, 2);
    module.get_chunk_at_mut(0).unwrap().chunk = chunk;
    let recovered = Decompiler::new(&module, &env).decompile().unwrap();
    assert!(!recovered.contains("let _"), "{recovered}");
    let rebuilt = compile(&recovered, &env, false, false);
    assert_eq!(execute(&module, &env, 0), Primitive::U64(2));
    assert_eq!(execute(&rebuilt, &env, 0), Primitive::U64(2));
}

#[test]
fn eager_boolean_opcodes_evaluate_both_operands() {
    use silex_bytecode::{Chunk, OpCode};

    let env = EnvironmentBuilder::default();
    for (left, op) in [(false, OpCode::And), (true, OpCode::Or)] {
        let mut module = compile(
            &format!("entry main() {{ let a: u64[] = [1, 2] let _ = {left} == (a.remove(0) == 1) return a.len() as u64 }}"),
            &env,
            false,
            false,
        );
        let bytes = module.chunks()[0].chunk.get_instructions();
        let mut chunk = Chunk::new();
        let mut pc = 0;
        let mut comparisons = 0;
        while pc < bytes.len() {
            let instruction = OpCode::from_byte(bytes[pc]).unwrap();
            let end = pc + 1 + instruction.arguments_bytes();
            if matches!(instruction, OpCode::Eq) {
                comparisons += 1;
            }
            if matches!(instruction, OpCode::Eq) && comparisons == 2 {
                chunk.emit_opcode(op);
            } else {
                for byte in &bytes[pc..end] {
                    chunk.write_u8(*byte);
                }
            }
            pc = end;
        }
        assert_eq!(comparisons, 2);
        module.get_chunk_at_mut(0).unwrap().chunk = chunk;
        let recovered = Decompiler::new(&module, &env).decompile().unwrap();
        let rebuilt = compile(&recovered, &env, false, false);
        assert_eq!(execute(&module, &env, 0), Primitive::U64(1));
        assert_eq!(execute(&rebuilt, &env, 0), Primitive::U64(1));
    }
}

#[test]
fn pop_n_does_not_create_bindings() {
    use silex_bytecode::{Chunk, OpCode};

    let env = EnvironmentBuilder::default();
    let mut module = Module::new();
    let constant = module.add_constant(Primitive::U64(7)) as u16;
    let mut chunk = Chunk::new();
    for _ in 0..3 {
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(constant);
    }
    chunk.emit_opcode(OpCode::PopN);
    chunk.write_u8(2);
    chunk.emit_opcode(OpCode::Return);
    module.add_entry_chunk(chunk, None);

    let recovered = Decompiler::new(&module, &env).decompile().unwrap();
    assert!(!recovered.contains("let"), "{recovered}");
    let rebuilt = compile(&recovered, &env, false, false);
    assert_eq!(execute(&module, &env, 0), execute(&rebuilt, &env, 0));
}

#[test]
fn discarded_expressions_still_trap() {
    let env = EnvironmentBuilder::default();
    let module = compile(
        "entry main() { let zero: u64 = 0 let _ = 1 / zero return 7 }",
        &env,
        false,
        false,
    );
    let recovered = Decompiler::new(&module, &env).decompile().unwrap();
    assert!(!recovered.contains("let _ ="), "{recovered}");
    let rebuilt = compile(&recovered, &env, false, false);
    for module in [&module, &rebuilt] {
        let mut vm = VM::default();
        vm.append_module(ModuleMetadata {
            module: module.into(),
            environment: env.environment().into(),
            metadata: (&()).into(),
        })
        .unwrap();
        vm.context_mut().set_gas_limit(1_000_000);
        vm.invoke_chunk_id_unchecked(0).unwrap();
        assert!(vm.run_blocking().is_err(), "{recovered}");
    }
}

#[test]
fn voidable_syscalls_follow_standalone_and_value_contexts() {
    use silex_environment::{FunctionHandler, SysCallResult};
    use silex_types::Type;

    let mut env = EnvironmentBuilder::default();
    env.register_native_function(
        "maybe_value",
        None,
        vec![],
        FunctionHandler::Sync(|_, _, _, _| Ok(SysCallResult::Return(Primitive::U64(7).into()))),
        0,
        Some(Type::Voidable(Box::new(Type::U64))),
    );
    env.register_native_function(
        "maybe_none",
        None,
        vec![],
        FunctionHandler::Sync(|_, _, _, _| Ok(SysCallResult::None)),
        0,
        Some(Type::Voidable(Box::new(Type::U64))),
    );

    let standalone = compile(
        "entry main() { maybe_value() return 3 }",
        &env,
        false,
        false,
    );
    let recovered = Decompiler::new(&standalone, &env).decompile().unwrap();
    assert!(recovered.contains("maybe_value()"));
    assert!(!recovered.contains("let value"));

    let used = compile(
        "entry main() { let value: u64 = maybe_value() return value }",
        &env,
        false,
        false,
    );
    let recovered = Decompiler::new(&used, &env).decompile().unwrap();
    assert!(recovered.contains("maybe_value()"));
    assert!(!recovered.contains("let value"));
    compile(&recovered, &env, false, false);

    let nested = compile(
        "fn consume(value: u64) -> u64 { return value } entry main() { return consume(maybe_value()) }",
        &env,
        false,
        false,
    );
    let recovered = Decompiler::new(&nested, &env).decompile().unwrap();
    assert!(
        recovered.contains("function0(maybe_value())"),
        "{recovered}"
    );
    compile(&recovered, &env, false, false);

    // Standalone voidable calls deliberately have no Pop. An explicitly
    // discarded result does have one, and must keep it after recompilation.
    for source in [
        "entry main() { maybe_value(); return 3 }",
        "entry main() { let _ = maybe_value() return 3 }",
    ] {
        let module = compile(source, &env, false, false);
        let recovered = Decompiler::new(&module, &env).decompile().unwrap();
        let rebuilt = compile(&recovered, &env, false, false);
        assert_eq!(
            module.chunks()[0].chunk.get_instructions(),
            rebuilt.chunks()[0].chunk.get_instructions(),
            "{recovered}",
        );
    }

    for source in [
        "entry main() { return maybe_value() + maybe_value() }",
        "fn combine(a: u64, b: u64) -> u64 { return a * 10 + b } entry main() { return combine(maybe_value(), 3) }",
        "fn combine(a: u64, b: u64) -> u64 { return a * 10 + b } entry main() { return combine(maybe_value(), maybe_value()) }",
        "entry main() { return maybe_value() + (true ? 1 : 2) }",
        "entry main() { return (true ? maybe_value() : maybe_value()) + 1 }",
        "entry main() { maybe_none(); let n: u64 = 0 while n < 2 { n += 1 } return n }",
        "entry main() { let n: u64 = 0 while n < 2 { maybe_none(); n += 1 } return n }",
        "entry main() { let n: u64 = 0 while n < 2 { maybe_none(); maybe_none(); n += 1 } return n }",
        "entry main() { maybe_none(); let n: u64 = 0 while n < 2 { maybe_none(); n += 1 } return n }",
    ] {
        let module = compile(source, &env, false, false);
        let recovered = Decompiler::new(&module, &env).decompile().unwrap();
        let rebuilt = compile(&recovered, &env, false, false);
        let entry = module.chunks().len() - 1;
        assert_eq!(execute(&module, &env, entry), execute(&rebuilt, &env, entry), "{recovered}");
    }
}

#[test]
fn short_circuit_expression_does_not_create_a_local() {
    let env = EnvironmentBuilder::default();
    let module = compile(
        "fn truth() -> bool { return true } fn check() -> bool { return false && truth() } entry main() { if check() { return 1 } return 0 }",
        &env,
        false,
        false,
    );
    let recovered = Decompiler::new(&module, &env).decompile().unwrap();
    assert!(recovered.contains("&&"), "{recovered}");
    assert!(!recovered.contains("logic"), "{recovered}");
    compile(&recovered, &env, false, false);
}

#[test]
fn dynamic_calls_infer_erased_function_types_from_opcodes() {
    let env = EnvironmentBuilder::default();
    let module = compile(
        "fn apply(f: fn(u64) -> u64, value: u64) -> u64 { return f(value) } entry main() { return 0 }",
        &env,
        false,
        false,
    );
    let recovered = Decompiler::new(&module, &env).decompile().unwrap();
    assert!(recovered.contains("arg0(arg1)"), "{recovered}");
    compile(&recovered, &env, false, false);

    let pointer_module = compile(
        "fn add(value: u64) -> u64 { return value + 1 } entry main() { let f: fn(u64) -> u64 = add return f(2) }",
        &env,
        false,
        false,
    );
    let pointer_recovered = Decompiler::new(&pointer_module, &env).decompile().unwrap();
    assert!(
        pointer_recovered.contains("fn(u64) -> u64"),
        "{pointer_recovered}"
    );
    let pointer_rebuilt = compile(&pointer_recovered, &env, false, false);
    assert_eq!(
        execute(&pointer_module, &env, 1),
        execute(&pointer_rebuilt, &env, 1),
        "{pointer_recovered}"
    );
}

#[test]
fn dynamic_calls_preserve_supplied_signatures() {
    use silex_decompiler::FunctionSignature;
    use silex_types::{FnType, Type};

    let env = EnvironmentBuilder::default();
    for (source, result) in [
        ("pub fn apply(f: fn(u64), value: u64) { f(value) }", None),
        ("pub fn apply(f: fn(u64) -> u64, value: u64) -> u64 { let result = f(value) return result }", Some(Type::U64)),
    ] {
        let module = compile(source, &env, false, false);
        let signature = FunctionSignature {
            name: "apply".into(),
            parameters: vec![
                Type::Function(FnType::new(None, false, vec![Type::U64], result.clone())),
                Type::U64,
            ],
            return_type: result,
        };
        let recovered = Decompiler::new(&module, &env)
            .with_function_signature(0, signature)
            .decompile()
            .unwrap();
        assert!(!recovered.contains("any"), "{recovered}");
        let rebuilt = compile(&recovered, &env, false, false);
        assert_eq!(module.chunks()[0].chunk.get_instructions(), rebuilt.chunks()[0].chunk.get_instructions(), "{recovered}");
    }
}

#[test]
fn environment_struct_fields_from_signature() {
    use silex_decompiler::FunctionSignature;
    use silex_types::Type;
    let mut env = EnvironmentBuilder::<()>::new();
    let record = env.register_structure("Record", [("amount", Type::U64), ("flag", Type::Bool)]);
    let module = compile(
        "pub fn amount(record: Record) -> u64 { return record.amount }",
        &env,
        false,
        false,
    );
    let source = Decompiler::new(&module, &env)
        .with_function_signature(
            0,
            FunctionSignature {
                name: "amount".into(),
                parameters: vec![Type::Struct(record)],
                return_type: Some(Type::U64),
            },
        )
        .decompile()
        .unwrap();
    assert!(source.contains("arg0: Record"));
    assert!(source.contains(".amount"));
    compile(&source, &env, false, false);
}
