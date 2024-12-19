use xelis_compiler::Compiler;
use xelis_environment::{Environment, EnvironmentError};
use xelis_builder::EnvironmentBuilder;
use xelis_lexer::Lexer;
use xelis_parser::Parser;
use xelis_types::Value;
use super::*;

#[track_caller]
fn prepare_module(code: &str) -> (Module, Environment) {
    let tokens: Vec<_> = Lexer::new(code).into_iter().collect::<Result<_, _>>().unwrap();
    let env = EnvironmentBuilder::default();
    let (program, _) = Parser::with(tokens.into_iter(), &env).parse().unwrap();

    let env = env.build();
    let module = Compiler::new(&program, &env).compile().unwrap();

    (module, env)
}

#[track_caller]
fn try_run_code(code: &str, id: u16) -> Result<Value, VMError> {
    let (module, environment) = prepare_module(code);
    run_internal(module, &environment, id)
}

#[track_caller]
fn run_code_id(code: &str, id: u16) -> Value {
    try_run_code(code, id).unwrap()
}

#[track_caller]
fn run_code(code: &str) -> Value {
    run_code_id(code, 0)
}

#[test]
fn test_max_gas() {
    let code = r#"
        entry main() {
            while true {}

            return 0
        }
    "#;

    let (module, environment) = prepare_module(code);
    let mut vm = VM::new(&module, &environment);
    vm.context_mut().set_gas_limit(1000);
    vm.invoke_entry_chunk(0).unwrap();

    assert!(matches!(vm.run(), Err(VMError::EnvironmentError(EnvironmentError::NotEnoughGas { .. }))));
}

#[test]
fn test_pow() {
    let code = r#"
        entry main() {
            return 2u64 ** 10
        }
    "#;

    assert_eq!(run_code(code), Value::U64(1024));
}

#[test]
fn test_pow_assign() {
    let code = r#"
        entry main() {
            let x: u64 = 2;
            x **= 10;
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(1024));
}

#[test]
fn test_u256() {
    let code = r#"
        entry main() {
            let x: u256 = 10;
            let y: u64 = 20;
            return (x + y as u256) as u64
        }
    "#;

    assert_eq!(run_code(code), Value::U64(30u32.into()));
}

#[test]
fn test_ternary() {
    let code = r#"
        entry main() {
            let x: u64 = 10;
            let y: u64 = x == 10 ? 20 : 30;
            return y
        }
    "#;

    assert_eq!(run_code(code), Value::U64(20));
}

#[test]
fn test_ternary_negative() {
    let code = r#"
        entry main() {
            let x: u64 = 20;
            let y: u64 = x == 10 ? 20 : 30;
            return y
        }
    "#;

    assert_eq!(run_code(code), Value::U64(30));
}

#[test]
fn test_if() {
    let code = r#"
        entry main() {
            let x: u64 = 10;
            if x == 10 {
                x = 20
            }
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(20));
}

#[test]
fn test_and() {
    let code = r#"
        fn no_call() -> bool {
            return panic("no call")
        }

        entry main() {
            let x: u64 = 10;
            if (x != 10) && no_call() {
                panic("x is not 10")
            }
            return x
        }
    "#;

    assert_eq!(run_code_id(code, 1), Value::U64(10));
}

#[test]
fn test_and_positive() {
    let code = r#"
        fn test() -> bool {
            return true
        }

        entry main() {
            let x: u64 = 10;
            if (x == 10) && test() {
                return x
            }
            return panic("x is not 10")
        }
    "#;

    assert_eq!(run_code_id(code, 1), Value::U64(10));
}

#[test]
fn test_or() {
    let code = r#"
        fn no_call() -> bool {
            return panic("no call")
        }

        entry main() {
            let x: u64 = 10;
            if (x == 10) || no_call() {
                return 0
            }
            return panic("x is not 10")
        }
    "#;

    assert_eq!(run_code_id(code, 1), Value::U64(0));
}

#[test]
fn test_or_negative() {
    let code = r#"
        entry main() {
            let x: u64 = 10;
            if false || (x == 10) {
                return x
            }
            return panic("unexpected")
        }
    "#;

    assert_eq!(run_code(code), Value::U64(10));
}

#[test]
fn test_if_else() {
    let code = r#"
        entry main() {
            let x: u64 = 10;
            if x == 20 {
                x = 20
            } else {
                x = 30
            }
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(30));
}

#[test]
fn test_if_else_positive() {
    let code = r#"
        entry main() {
            let x: u64 = 10;
            if x == 10 {
                x = 20
            } else {
                x = 30
            }
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(20));
}

#[test]
fn test_nested_if() {
    let code = r#"
        entry main() {
            let x: u64 = 10;
            if x == 10 {
                x = 5
                if x == 5 {
                    x = 20
                }
            }
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(20));
}

#[test]
fn test_if_else_if() {
    let code = r#"
        entry main() {
            let x: u64 = 10;
            if x == 20 {
                x = 20
            } else if x == 10 {
                x = 30
            } else {
                x = 40
            }
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(30));
}

#[test]
fn test_while() {
    let code = r#"
        entry main() {
            let x: u64 = 0;
            while x < 10 {
                x = x + 1;
            }
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(10));
}

#[test]
fn test_for() {
    let code = r#"
        entry main() {
            let x: u64 = 0;
            for i: u64 = 0; i < 10; i += 1 {
                x = x + 1;
            }
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(10));
}

#[test]
fn test_struct_access() {
    let code = r#"
        struct Test {
            x: u64,
            y: u64
        }

        entry main() {
            let t: Test = Test { x: 10, y: 20 };
            return t.x + t.y
        }
    "#;

    assert_eq!(run_code(code), Value::U64(30));
}

#[test]
fn test_struct_assign() {
    let code = r#"
        struct Test {
            x: u64,
            y: u64
        }

        entry main() {
            let t: Test = Test { x: 10, y: 20 };
            t.x = 30;
            return t.x + t.y
        }
    "#;

    assert_eq!(run_code(code), Value::U64(50));
}

#[test]
fn test_self_assign() {
    let code = r#"
        entry main() {
            let x: u64 = 10;
            x = x;
            x = x + 10;
            x += x;
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(40));
}

#[test]
fn test_function_call() {
    let code = r#"
        fn add(a: u64, b: u64) -> u64 {
            return a + b
        }

        entry main() {
            return add(10, 20)
        }
    "#;

    assert_eq!(run_code_id(code, 1), Value::U64(30));
}

#[test]
fn test_array() {
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30];
            return arr[0] + arr[1] + arr[2]
        }
    "#;

    assert_eq!(run_code(code), Value::U64(60));
}

#[test]
fn test_array_in_struct() {
    let code = r#"
        struct Test {
            arr: u64[]
        }

        entry main() {
            let t: Test = Test { arr: [10, 20, 30] };
            return t.arr[0] + t.arr[1] + t.arr[2]
        }
    "#;

    assert_eq!(run_code(code), Value::U64(60));
}

#[test]
fn test_continue() {
    let code = r#"
        entry main() {
            let x: u64 = 0;
            for i: u64 = 0; i < 10; i += 1 {
                if ((i % 2) == 0) {
                    continue
                }
                x += 1
            }
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(5));
}

#[test]
fn test_break() {
    let code = r#"
        entry main() {
            let x: u64 = 0;
            for i: u64 = 0; i < 10; i += 1 {
                if (i == 5) {
                    break
                }
                x += 1
            }
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(5));
}

#[test]
fn test_nested_loops() {
    let code = r#"
        entry main() {
            let x: u64 = 0;
            for i: u64 = 0; i < 10; i += 1 {
                for j: u64 = 0; j < 10; j += 1 {
                    x = x + 1
                }
            }
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(100));
}

#[test]
fn test_for_array() {
    let code = r#"
        entry main() {
            let arr: u64[] = [10, 20, 30];
            let x: u64 = 0;
            for i: u32 = 0; i < arr.len(); i += 1 {
                let y: u64 = arr[i];
                x = x + y
            }
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(60));
}

#[test]
fn test_foreach_range() {
    let code = r#"
        entry main() {
            let x: u64 = 0;
            foreach i in 0..10 {
                x = x + i
            }
            return x
        }
    "#;

    assert_eq!(run_code(code), Value::U64(45));
}

#[test]
fn test_range_contains() {
    let code = r#"
        entry main() {
            let x: bool = (0..10).contains(5);
            return x as u64
        }
    "#;

    assert_eq!(run_code(code), Value::U64(1));
}


#[test]
fn test_range_contains_u256() {
    let code = r#"
        entry main() {
            let x: bool = (0u256..10u256).contains(5);
            return x as u64
        }
    "#;

    assert_eq!(run_code(code), Value::U64(1));
}

#[test]
fn test_range_collect() {
    let code = r#"
        entry main() {
            let x: u64[] = 0..10.collect();
            return x.len() as u64
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(10)
    );
}

#[test]
fn test_range_type() {
    let code = r#"
        entry main() {
            let x: range<u64> = 0..10;
            return x.count()
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(10)
    );
}

#[test]
fn test_stackoverflow() {
    let code = r#"
        entry main() {
            let x: u64 = 0;
            for i: u64 = 0; i < 1000000; i += 1 {
                x = x + 1
            }
            return x
        }"#;

    // assert_eq!(run_code(code), Value::U64(1000000));

    let mut code = r#"
        entry main() {
            let a: u64 = 1;
            let b: u64 = a
    "#.to_string() + "+ a + a ".repeat(100000).as_str();
    code.push_str("; return b }");

    // TODO FIXME
    todo!("Fix stack overflow test");

    assert_eq!(run_code(&code), Value::U64(10000 * 2 + 1));
}

#[test]
fn test_dangling_value_scoped() {
    let code = r#"
        entry main() {
            {
                10 + 5;
            }
            return 0
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(0)
    );
}

#[test]
fn test_dangling_value() {
    let code = r#"
        entry main() {
            10 + 5;
            return 0
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(0)
    );
}

#[test]
fn test_dangling_value_after_jump() {
    let code = r#"
        entry main() {
            if false {}
            10 + 5;
            return 0
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(0)
    );
}

#[test]
fn test_map() {
    let code = r#"
        entry main() {
            let x: map<string, u8> = {};
            x.insert("a", 10);
            let a: optional<u8> = x.get("a");
            return a.unwrap() as u64
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(10)
    );
}

#[test]
fn test_map_inline() {
    let code = r#"
        entry main() {
            return {
                "a": 10u8
            }.get("a").unwrap() as u64
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(10)
    );
}

#[test]
fn test_self_reference() {
    let code = r#"
        entry main() {
            let x: u64[][] = [[10]];
            x.push(x[0]);
            x[1][0] = 20;
            return x[0][0]
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(10)
    );
}

#[test]
fn test_self_reference_struct() {
    let code = r#"
        struct Test {
            x: u64
        }

        entry main() {
            let t: Test[] = [Test { x: 10 }];
            t.push(t.get(0).unwrap());
            t[1].x = 20;
            return t[0].x
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(10)
    );
}

#[test]
fn test_self_reference_map() {
    let code = r#"
        struct Dummy {
            x: u64
        }
        entry main() {
            let x: map<string, Dummy> = {};
            x.insert("a", Dummy { x: 10 });
            let dummy: Dummy = x.get("a").unwrap();
            x.insert("b", dummy);
            x.get("b").unwrap().x = 20;

            assert(!is_same_ptr(x.get("a").unwrap(), x.get("b").unwrap()));

            return x.get("a").unwrap().x
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(10)
    );
}

#[test]
fn test_enum() {
    let code = r#"
        enum Test {
            A,
            B { value: u64 }
        }

        entry main() {
            let x: Test = Test::B { value: 10 };
            return 10
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(10)
    );
}

#[test]
fn test_array_slice() {
    let code = r#"
        entry main() {
            let x: u64[] = [10, 20, 30, 40, 50];
            let y: u64[] = x.slice(1..4);

            assert(is_same_ptr(y[0], x[1]));
            y.push(60);
            assert(!is_same_ptr(y[3], x[4]));

            y.push(x[4]);
            assert(!is_same_ptr(y[4], x[4]));

            return y[0] + y[1] + y[2]
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(90)
    );
}

#[test]
fn test_recursive_call() {
    let code = r#"
        fn fib(n: u64) -> u64 {
            if n == 0 {
                return 0
            } else if n == 1 {
                return 1
            }

            return fib(n - 1) + fib(n - 2)
        }

        entry main() {
            return fib(10)
        }
    "#;

    assert_eq!(
        run_code_id(code, 1),
        Value::U64(55)
    );
}

#[test]
fn test_const() {
    let code = r#"
        const X: u64 = 10;

        entry main() {
            return X
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(10)
    );
}

#[test]
fn test_const_add() {
    let code = r#"
        const ZERO: u64 = 0
        const HELLO_WORLD: string = "Hello World"

        entry main() {
            let message: string = HELLO_WORLD + " " + ZERO
            return message.len() as u64
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(13)
    );
}

#[test]
fn test_optional_cast() {
    let code = r#"
        struct Test { value: u64 }
        entry main() {
            let x: optional<Test> = Test { value: 10 };
            let y: optional<u64> = x.unwrap().value;
            let v: u64 = y.unwrap();
            return v
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(10)
    );
}


#[test]
fn test_optional_unwrap_or() {
    let code = r#"
        entry main() {
            let x: optional<u8> = null;
            return x.unwrap_or(10) as u64
        }
    "#;

    assert_eq!(
        run_code(code),
        Value::U64(10)
    );
}

#[test]
fn test_div_by_zero() {
    let code = r#"
        entry main() {
            let x: u64 = 10;
            let y: u64 = 0;
            return x / y
        }
    "#;

    assert!(
        matches!(
            try_run_code(code, 0),
            Err(VMError::DivisionByZero)
        )
    );
}

#[test]
fn test_path() {
    let code = r#"
        struct Value {
            value: string
        }

        fn (v Value) my_value() -> string {
            return v.value
        }

        struct Message {
            value: Value
        }

        fn (m Message) to_string() -> string {
            return m.value.value
        }

        entry main() {
            let message: Message = Message { value: Value { value: "Hello World!" } }
            assert(message.to_string() == "Hello World!")
            message.value.value += " from path"
            assert(message.to_string() == "Hello World! from path")
            return 0
        }
    "#;

    assert_eq!(
        run_code_id(code, 2),
        Value::U64(0)
    );
}