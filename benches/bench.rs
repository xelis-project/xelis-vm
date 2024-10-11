use criterion::{criterion_group, criterion_main, Criterion};
use xelis_vm::{
    ast::{Program, Signature},
    bytecode::{compiler::Compiler, vm::VM, Module},
    Environment,
    EnvironmentBuilder,
    IdentifierType,
    Interpreter,
    Lexer,
    Parser,
    State
};

const DEFAULT_CODE: &str = "
entry main() {
    let a: u8 = 10;
    let b: u8 = 20;
    let c: u8 = a + b;
    return c as u64;
}";

// Find 2000 prime numbers
const PRIME_FINDER: &str = "
entry main() {
    let primes: u64[] = [2];
    let check: u64 = 3;
    let index: u64 = 1;
    while index < 2_000 {
        let is_prime: bool = true;
        foreach prime in primes {
            if (check % prime) == 0 {
                is_prime = false;
                break;
            }
        }

        if is_prime {
            primes.push(check);
            index += 1;
        }

        check += 2
    }

    return 0;
}";

fn prepare(code: &str) -> (IdentifierType, Program, Module, Environment) {
    let tokens = Lexer::new(code).get().unwrap();
    let env = EnvironmentBuilder::default();
    let (program, mapper) = Parser::new(tokens, &env).parse().unwrap();
    
    let signature = Signature::new("main".to_owned(), None, Vec::new());
    let entry = mapper.get(&signature).unwrap();

    let env = env.build();
    let module = Compiler::new(&program, &env).compile().unwrap();

    (entry, program, module, env)
}

fn bench_internal(c: &mut Criterion, code: &str, name: &str) {
    let mut group = c.benchmark_group(name);

    // Lexer
    group.bench_function("lexer", |b| b.iter(|| Lexer::new(code).get().unwrap()));

    // Parser
    let tokens = Lexer::new(code).get().unwrap();
    let env = EnvironmentBuilder::default();
    group.bench_function("parser", |b| b.iter(|| Parser::new(tokens.clone(), &env).parse().unwrap()));

    let (entry, program, module, environment) = prepare(code);

    // Interpreter
    let mut state = State::new(None, Some(100), None);
    let interpreter = Interpreter::new(&program, &environment).unwrap();
    group.bench_function("interpreter", |b| b.iter(|| {
        interpreter.call_entry_function(&entry, vec![], &mut state).unwrap();
    }));

    // VM
    let mut vm = VM::new(&module, &environment);
    group.bench_function("vm", |b| b.iter(|| {
        vm.invoke_entry_chunk(0).unwrap();
        vm.run().unwrap();
    }));
}

fn bench_simple_code(c: &mut Criterion) {
    bench_internal(c, DEFAULT_CODE, "simple_code");
}

fn bench_prime_finder(c: &mut Criterion) {
    bench_internal(c, PRIME_FINDER, "prime_finder");
}

criterion_group!(benches, bench_simple_code, bench_prime_finder);
criterion_main!(benches);