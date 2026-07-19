use criterion::{criterion_group, criterion_main, Criterion};
use silex_builder::EnvironmentBuilder;
use silex_bytecode::Module;
use silex_compiler::Compiler;
use silex_environment::{Environment, ModuleMetadata};
use silex_lexer::Lexer;
use silex_parser::Parser;
use xelis_vm::VM;

macro_rules! bench {
    ($group: expr, $name: expr, $code: expr, $id: expr) => {
        $group.bench_function($name, |b| {
            let code = $code;
            let (module, env) = prepare(&code);
            let mut vm = VM::default();
            b.iter(|| {
                vm.append_module(ModuleMetadata {
                    module: (&module).into(),
                    environment: (&env).into(),
                    metadata: (&()).into(),
                }).expect("module");

                vm.invoke_entry_chunk($id).expect("entry");
                vm.run_blocking().expect("run");
                vm.context_mut().reset_usage();
            });
        });
    };
    ($group: expr, $name: expr, $code: expr) => {
        bench!($group, $name, $code, 0);
    };
}

fn prepare(code: &str) -> (Module, Environment<()>) {
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
            let primes: u32[] = [];
            let count: u32 = 0;
            let i: u32 = 2;

            while count < N {
                let is_prime: bool = true;

                foreach p in primes {
                    if p * p > i {
                        break;
                    }

                    if i % p == 0 {
                        is_prime = false;
                        break;
                    }
                }

                if is_prime {
                    primes.push(i);
                    count += 1;
                }

                i += 1;
            }

            return 0;
        }
        "#;

    bench!(
        group,
        "2000",
        code.replace("N", "2000")
    );

    bench!(
        group,
        "5000",
        code.replace("N", "5000")
    );
}

fn bench_iterator(c: &mut Criterion) {
    let mut group = c.benchmark_group("iterator");

    bench!(
        group,
        "collect",
        r#"
        entry main() {
            let r = 0u64..100u64;
            let arr: u64[] = r.collect();
            let result: u64[] = arr.iter().collect();
            return result.len() as u64
        }
        "#
    );

    bench!(
        group,
        "sum",
        r#"
        entry main() {
            let r = 0u64..100u64;
            let arr: u64[] = r.collect();
            return arr.iter().sum()
        }
        "#
    );

    bench!(
        group,
        "count",
        r#"
        entry main() {
            let r = 0u64..100u64;
            let arr: u64[] = r.collect();
            return arr.iter().count() as u64
        }
        "#
    );

    bench!(
        group,
        "map",
        r#"
        entry main() {
            let r = 0u64..100u64;
            let arr: u64[] = r.collect();
            let result: u64[] = arr.iter().map(|x: u64| { return x * 2 }).collect();
            return result.len() as u64
        }
        "#
    );

    bench!(
        group,
        "filter",
        r#"
        entry main() {
            let r = 0u64..100u64;
            let arr: u64[] = r.collect();
            let result: u64[] = arr.iter().filter(|x: u64| { return x % 2 == 0 }).collect();
            return result.len() as u64
        }
        "#
    );

    bench!(
        group,
        "fold",
        r#"
        entry main() {
            let r = 0u64..100u64;
            let arr: u64[] = r.collect();
            return arr.iter().fold(0u64, |acc: u64, x: u64| { return acc + x })
        }
        "#
    );

    bench!(
        group,
        "map + filter + fold",
        r#"
        entry main() {
            let r = 0u64..100u64;
            let arr: u64[] = r.collect();
            return arr.iter()
                .map(|x: u64| { return x * 2 })
                .filter(|x: u64| { return x % 4 == 0 })
                .fold(0u64, |acc: u64, x: u64| { return acc + x })
        }
        "#
    );

    bench!(
        group,
        "chain",
        r#"
        entry main() {
            let ra = 0u64..50u64;
            let rb = 50u64..100u64;
            let a: u64[] = ra.collect();
            let b: u64[] = rb.collect();
            let result: u64[] = a.iter().chain(b.iter()).collect();
            return result.len() as u64
        }
        "#
    );

    bench!(
        group,
        "skip + take",
        r#"
        entry main() {
            let r = 0u64..100u64;
            let arr: u64[] = r.collect();
            let result: u64[] = arr.iter().skip(25u32).take(50u32).collect();
            return result.len() as u64
        }
        "#
    );

    bench!(
        group,
        "find",
        r#"
        entry main() {
            let r = 0u64..100u64;
            let arr: u64[] = r.collect();
            let found: optional<u64> = arr.iter().find(|x: u64| { return x == 95 });
            return found.unwrap()
        }
        "#
    );

    bench!(
        group,
        "any",
        r#"
        entry main() {
            let r = 0u64..100u64;
            let arr: u64[] = r.collect();
            return arr.iter().any(|x: u64| { return x == 50 }) as u64
        }
        "#
    );

    bench!(
        group,
        "unfold",
        r#"
        entry main() {
            let result: u64[] = Iterator::unfold(0u64, |state: u64| {
                if state < 100 {
                    return (state, state + 1u64)
                }
                return null
            }).collect();
            return result.len() as u64
        }
        "#
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
    bench_iterator,
);
criterion_main!(benches);
