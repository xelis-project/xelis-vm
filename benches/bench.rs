use criterion::{criterion_group, criterion_main, Criterion};
use xelis_vm::{Environment, Lexer, Parser};

const CODE: &str = "
entry main() {
    let a: byte = 10;
    let b: byte = 20;
    let c: byte = a + b;
    return c as int;
}
";

fn bench_lexer(c: &mut Criterion) {
    c.bench_function("lexer", |b| b.iter(|| Lexer::new(CODE).get().unwrap()));
}

fn bench_parser(c: &mut Criterion) {
    let tokens = Lexer::new(CODE).get().unwrap();
    let (env, mapper) = Environment::new();
    c.bench_function("parser", |b| b.iter(|| Parser::new(tokens.clone(), &env).parse(&mut mapper.clone()).unwrap()));
}

criterion_group!(benches, bench_lexer, bench_parser);
criterion_main!(benches);