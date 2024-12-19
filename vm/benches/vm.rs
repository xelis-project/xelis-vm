use criterion::{criterion_group, criterion_main, Criterion};
use xelis_builder::EnvironmentBuilder;
use xelis_bytecode::Module;
use xelis_compiler::Compiler;
use xelis_environment::Environment;
use xelis_lexer::Lexer;
use xelis_parser::Parser;
use xelis_vm::VM;

macro_rules! bench {
    ($group: expr, $name: expr, $code: expr, $id: expr) => {
        $group.bench_function($name, |b| {    
            let (module, env) = prepare($code);
            let mut vm = VM::new(&module, &env);
            b.iter(|| {
                vm.invoke_entry_chunk($id).unwrap();
                vm.run().unwrap();
            });
        });
    };
    ($group: expr, $name: expr, $code: expr) => {
        bench!($group, $name, $code, 0);
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

            let h: u8 = t.a;
            let i: u16 = t.b;
            let j: u32 = t.c;
            let k: u64 = t.d;
            let l: u128 = t.e;
            let m: u256 = t.f;
            let n: bool = t.g;

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

fn bench_foreach(c: &mut Criterion) {
    let mut group = c.benchmark_group("foreach");
    bench!(
        group,
        "array",
        r#"
        entry main() {
            let arr: u32[] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
            let sum: u32 = 0;

            foreach i in arr {
                sum += i;
            }

            return 0;
        }
        "#
    );

    bench!(
        group,
        "range",
        r#"
        entry main() {
            let sum: u32 = 0;
            foreach i in 0u32..10u32 {
                sum += i;
            }
            return 0;
        }
        "#
    );

    bench!(
        group,
        "nested",
        r#"
        entry main() {
            let sum: u32 = 0;
            foreach i in 0u32..10u32 {
                foreach j in 0u32..10u32 {
                    sum += i + j;
                }
            }
            return 0;
        }
        "#
    );
}

fn bench_for(c: &mut Criterion) {
    let mut group = c.benchmark_group("for");

    bench!(
        group,
        "array",
        r#"
        entry main() {
            let arr: u32[] = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
            let sum: u32 = 0;
            for i: u32 = 0; i < arr.len(); i += 1 {
                sum += arr[i];
            }
            return 0;
        }
        "#
    );

    bench!(
        group,
        "range",
        r#"
        entry main() {
            let sum: u32 = 0;
            for i: u32 = 0; i < 10; i += 1 {
                sum += i;
            }
            return 0;
        }
        "#
    );

    bench!(
        group,
        "nested",
        r#"
        entry main() {
            let sum: u32 = 0;
            for i: u32 = 0; i < 10; i += 1 {
                for j: u32 = 0; j < 10; j += 1 {
                    sum += i + j;
                }
            }
            return 0;
        }
        "#
    );
}

fn bench_while(c: &mut Criterion) {
    let mut group = c.benchmark_group("while");
    bench!(
        group,
        "simple",
        r#"
        entry main() {
            let sum: u32 = 0;
            let i: u32 = 0;
            while i < 10 {
                sum += i;
                i += 1;
            }
            return 0;
        }
        "#
    );

    bench!(
        group,
        "nested",
        r#"
        entry main() {
            let sum: u32 = 0;
            let i: u32 = 0;
            while i < 10 {
                let j: u32 = 0;
                while j < 10 {
                    sum += i + j;
                    j += 1;
                }
                i += 1;
            }
            return 0;
        }
        "#
    );
}

fn bench_function_call(c: &mut Criterion) {
    let mut group = c.benchmark_group("function_call");
    bench!(
        group,
        "simple",
        r#"
        fn add(a: u32, b: u32) -> u32 {
            return a + b;
        }

        entry main() {
            let a: u32 = 1;
            let b: u32 = 2;
            let c: u32 = add(a, b);
            return 0;
        }
        "#,
        1
    );

    bench!(
        group,
        "nested",
        r#"
        fn add2(a: u32, b: u32) -> u32 {
            return a + b;
        }

        fn add(a: u32, b: u32) -> u32 {
            return add2(a, b);
        }

        entry main() {
            let c: u32 = add(1, 2);
            return 0;
        }
        "#,
        2
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

criterion_group!(
    benches,
    bench_struct,
    bench_foreach,
    bench_for,
    bench_while,
    bench_function_call,
    prime_finder,
);
criterion_main!(benches);