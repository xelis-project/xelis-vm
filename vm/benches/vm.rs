use criterion::{criterion_group, criterion_main, Criterion};
use xelis_builder::EnvironmentBuilder;
use xelis_bytecode::Module;
use xelis_compiler::Compiler;
use xelis_environment::Environment;
use xelis_lexer::Lexer;
use xelis_parser::Parser;
use xelis_vm::VM;

macro_rules! bench {
    ($group: expr, $name: expr, $code: expr) => {
        $group.bench_function($name, |b| {    
            let (module, env) = prepare($code);
            let mut vm = VM::new(&module, &env);
            b.iter(|| {
                vm.invoke_entry_chunk(0).unwrap();
                vm.run().unwrap();
            });
        });
    };
}

fn prepare(code: &str) -> (Module, Environment) {
    let tokens = Lexer::new(code).get().unwrap();
    let env = EnvironmentBuilder::default();
    let (program, _) = Parser::new(tokens, &env).parse().unwrap();
    let env = env.build();
    let module = Compiler::new(&program, &env).compile().unwrap();

    (module, env)
}

fn bench_struct(c: &mut Criterion) {
    let mut group = c.benchmark_group("struct");
    bench!(
        group,
        "creation",
        r#"
        struct Test {
            a: u8,
            b: u16,
            c: u32,
            d: u64,
            e: u128,
            f: u256,
            g: bool
        }

        entry main() {
            let _: Test = Test {
                a: 1,
                b: 2,
                c: 3,
                d: 4,
                e: 5,
                f: 6,
                g: true
            };

            return 0;
        }
        "#
    );

    bench!(
        group,
        "access",
        r#"
        struct Test {
            a: u8,
            b: u16,
            c: u32,
            d: u64,
            e: u128,
            f: u256,
            g: bool
        }

        entry main() {
            let t: Test = Test {
                a: 1,
                b: 2,
                c: 3,
                d: 4,
                e: 5,
                f: 6,
                g: true
            };

            let _: u8 = t.a;
            let _: u16 = t.b;
            let _: u32 = t.c;
            let _: u64 = t.d;
            let _: u128 = t.e;
            let _: u256 = t.f;
            let _: bool = t.g;

            return 0;
        }
        "#
    );

    bench!(
        group,
        "access hot",
        r#"
        struct Test {
            f: u256
        }

        entry main() {
            let t: Test = Test {
                f: 0
            };

            while t.f < 1000 {
                t.f += 1;
            }

            return 0;
        }
        "#
    );
}

fn prime_finder(c: &mut Criterion) {
    let mut group = c.benchmark_group("prime_finder");
    let code = r#"
        entry main() {
            let n: u32 = N;
            let primes: u32[] = [];
            let count: u32 = 0;

            for i: u32 = 2; i < n; i += 1 {
                let is_prime: bool = true;

                for j: u32 = 2; j < i; j += 1 {
                    if (i % j) == 0 {
                        is_prime = false;
                        break;
                    }
                }

                if is_prime {
                    primes.push(i);
                    count += 1;
                }
            }

            return 0;
        }
        "#;

    bench!(
        group,
        "2000",
        &code.replace("N", "2000")
    );

    bench!(
        group,
        "5000",
        &code.replace("N", "5000")
    );
}

criterion_group!(benches, bench_struct, prime_finder);
criterion_main!(benches);