use criterion::{criterion_group, criterion_main, Criterion};
use xelis_vm::Lexer;

fn bench_lexer(c: &mut Criterion) {
    let code =
    "entry main() {
        let a: byte = 10;
        let b: byte = 20;
        let c: byte = a + b;
        return c;
    }";
    c.bench_function("lexer", |b| b.iter(|| Lexer::new(code).get().unwrap()));
}

criterion_group!(benches, bench_lexer);
criterion_main!(benches);