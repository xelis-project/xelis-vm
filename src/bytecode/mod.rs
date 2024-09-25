mod chunk;
mod opcode;
mod module;
pub mod vm;
pub mod compiler;
pub mod assembler;

pub use chunk::Chunk;
pub use opcode::OpCode;
pub use module::Module;

#[cfg(test)]
mod tests {
    use compiler::Compiler;
    use vm::VM;

    use crate::{Environment, EnvironmentBuilder, Lexer, Parser, Value};

    use super::*;

    #[track_caller]
    fn prepare_module(code: &str) -> (Module, Environment) {
        let tokens = Lexer::new(code).get().unwrap();
        let env = EnvironmentBuilder::default();
        let (program, _) = Parser::new(tokens, &env).parse().unwrap();

        let env = env.build();
        let module = Compiler::new(&program, &env).compile().unwrap();

        (module, env)
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

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(20));
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

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(30));
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

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(20));
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

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(20));
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

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(30));
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

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(10));
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

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(10));
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

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(30));
    }

    #[test]
    fn test_function_call() {
        let code = r#"
            func add(a: u64, b: u64): u64 {
                return a + b
            }

            entry main() {
                return add(10, 20)
            }
        "#;

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(1).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(30));
    }

    #[test]
    fn test_array() {
        let code = r#"
            entry main() {
                let arr: u64[] = [10, 20, 30];
                return arr[0] + arr[1] + arr[2]
            }
        "#;

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(60));
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

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(60));
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

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(5));
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

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(5));
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

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(100));
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

        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(60));
    }
}
