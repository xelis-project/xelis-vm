use criterion::{criterion_group, criterion_main, Criterion};
use xelis_vm::{ast::Signature, EnvironmentBuilder, Interpreter, Lexer, Parser, State};

const CODE: &str = "
entry main() {
    let a: u8 = 10;
    let b: u8 = 20;
    let c: u8 = a + b;
    return c as u64;
}
";

fn bench_lexer(c: &mut Criterion) {
    c.bench_function("lexer", |b| b.iter(|| Lexer::new(CODE).get().unwrap()));
}

fn bench_parser(c: &mut Criterion) {
    let tokens = Lexer::new(CODE).get().unwrap();
    let env = EnvironmentBuilder::new();
    c.bench_function("parser", |b| b.iter(|| Parser::new(None, tokens.clone(), &env).parse().unwrap()));
}

fn bench_vm(c: &mut Criterion) {
    let tokens = Lexer::new(CODE).get().unwrap();
    let env = EnvironmentBuilder::new();
    let (program, mapper) = Parser::new(None, tokens, &env).parse().unwrap();
    let vm = Interpreter::new(&program, env.environment()).unwrap();
    let mut state = State::new(None, Some(100), None);
    let signature = Signature::new("main".to_owned(), None, Vec::new());
    c.bench_function("vm", |b| b.iter(|| vm.call_entry_function(&mapper.get(&signature).unwrap(), vec![], &mut state).unwrap()));
}

criterion_group!(benches, bench_lexer, bench_parser, bench_vm);
criterion_main!(benches);