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
