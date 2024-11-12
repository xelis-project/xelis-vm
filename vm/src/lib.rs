mod chunk;
mod error;
mod iterator;
mod instructions;
mod stack;

use xelis_environment::Environment;
pub use error::VMError;
pub use chunk::*;
use instructions::{InstructionResult, InstructionTable};
use stack::Stack;

use xelis_types::{Struct, Value, Path};
use xelis_bytecode::Module;

// 64 elements maximum in the call stack
const CALL_STACK_SIZE: usize = 64;

// Backend of the VM
pub struct Backend<'a> {
    // The module to execute
    module: &'a Module,
    // The environment of the VM
    environment: &'a Environment,
    // The instruction table of the VM
    table: InstructionTable<'a>,
}

impl<'a> Backend<'a> {
    // Get a struct with an id
    // TODO: support env structs
    pub fn get_struct_with_id(&self, id: u16) -> Result<&Struct, VMError> {
        self.module.get_struct_at(id as usize).ok_or(VMError::StructNotFound)
    }

    // Get a constant with an id
    pub fn get_constant_with_id(&self, id: u16) -> Result<&Value, VMError> {
        self.module.get_constant_at(id as usize).ok_or(VMError::ConstantNotFound)
    }
}

// Virtual Machine to execute the bytecode from chunks of a Module.
pub struct VM<'a> {
    backend: Backend<'a>,
    // The call stack of the VM
    // Every chunks to proceed are stored here
    call_stack: Vec<ChunkManager<'a>>,
    // The stack of the VM
    // Every values are stored here
    stack: Stack<'a>,
}

impl<'a> VM<'a> {
    // Create a new VM
    pub fn new(module: &'a Module, environment: &'a Environment) -> Self {
        VM {
            backend: Backend {
                module,
                environment,
                table: InstructionTable::new(),
            },
            call_stack: Vec::with_capacity(4),
            stack: Stack::new(),
        }
    }

    // Get the stack
    #[inline]
    pub fn get_stack(&self) -> &Stack<'a> {
        &self.stack
    }

    // Invoke a chunk using its id
    pub(crate) fn invoke_chunk_id(&mut self, id: u16) -> Result<(), VMError> {
        if self.call_stack.len() >= CALL_STACK_SIZE {
            return Err(VMError::CallStackOverflow);
        }

        let chunk = self.backend.module.get_chunk_at(id as usize)
            .ok_or(VMError::ChunkNotFound)?;

        let manager = ChunkManager::new(chunk);
        self.call_stack.push(manager);
        Ok(())
    }

    // Invoke a chunk using its id and arguments
    pub fn invoke_chunk_with_args(&mut self, id: u16, args: Vec<Path<'a>>) -> Result<(), VMError> {
        self.stack.extend_stack(args.into_iter())?;
        self.invoke_chunk_id(id)
    }

    // Invoke an entry chunk using its id
    pub fn invoke_entry_chunk(&mut self, id: u16) -> Result<(), VMError> {
        if !self.backend.module.is_entry_chunk(id as usize) {
            return Err(VMError::ChunkNotEntry);
        }
        self.invoke_chunk_id(id)
    }

    // Invoke an entry chunk using its id
    pub fn invoke_entry_chunk_with_args(&mut self, id: u16, args: Vec<Path<'a>>) -> Result<(), VMError> {
        self.invoke_entry_chunk(id)?;
        self.stack.extend_stack(args.into_iter())?;
        Ok(())
    }

    // Run the VM
    // It will execute the bytecode
    // First chunk executed should always return a value
    pub fn run(&mut self) -> Result<Value, VMError> {
        while let Some(mut manager) = self.call_stack.pop() {
            while let Ok(opcode) = manager.read_u8() {
                match self.backend.table.execute(opcode, &self.backend, &mut self.stack, &mut manager)? {
                    InstructionResult::Nothing => {},
                    InstructionResult::InvokeChunk(id) => {
                        self.call_stack.push(manager);
                        self.invoke_chunk_id(id)?;
                        break;
                    },
                    InstructionResult::Break => {
                        break;
                    }
                }
            }
        }

        let end_value = self.stack.pop_stack()?.into_owned();
        assert!(self.stack.count() == 0);

        Ok(end_value)
    }
}


#[cfg(test)]
mod tests {
    use xelis_bytecode::{Chunk, Module, OpCode};
    use xelis_types::{Type, Value, ValueOwnable};

    use super::*;

    fn run(module: Module) -> Value {
        let env = Environment::new();
        let mut vm = VM::new(&module, &env);
        vm.invoke_chunk_id(0).unwrap();
        vm.run().unwrap()
    }

    #[test]
    fn test_casting() {
        let mut module = Module::new();
        let mut chunk = Chunk::new();
        let index = module.add_constant(Value::U8(10));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        chunk.emit_opcode(OpCode::Cast);
        chunk.write_u8(Type::String.primitive_byte().unwrap());

        module.add_chunk(chunk);

        assert_eq!(run(module), Value::String("10".to_string()));
    }

    #[test]
    fn test_array_call() {
        let mut module = Module::new();
        let mut chunk = Chunk::new();

        // Push element 1
        let index = module.add_constant(Value::U8(10));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        // Push element 2
        let index = module.add_constant(Value::U8(20));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        // Create a new array with 2 elements
        chunk.emit_opcode(OpCode::NewArray);
        chunk.write_u32(2);

        // Load the first element
        let index = module.add_constant(Value::U16(0));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        chunk.emit_opcode(OpCode::ArrayCall);
        module.add_chunk(chunk);

        assert_eq!(run(module), Value::U8(10));
    }

    #[test]
    fn test_multi_depth_array_call() {
        let mut module = Module::new();
        let mut chunk = Chunk::new();

        let values = vec![
            Value::U8(10),
            Value::U8(20),
            Value::U8(30),
        ].into_iter().map(|v| ValueOwnable::Owned(Box::new(v))).collect();

        // Push element 1
        let index = module.add_constant(Value::Array(values));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        // Create a new array with 2 elements
        chunk.emit_opcode(OpCode::NewArray);
        chunk.write_u32(1);

        // Load the first element of the first array
        let index = module.add_constant(Value::U16(0));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        chunk.emit_opcode(OpCode::ArrayCall);

        // Load the last element
        let index = module.add_constant(Value::U16(2));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        chunk.emit_opcode(OpCode::ArrayCall);

        module.add_chunk(chunk);

        assert_eq!(run(module), Value::U8(30));
    }

    #[test]
    fn test_struct() {
        // Create a new struct
        let new_struct = Struct {
            fields: vec![
                Type::U8,
                Type::U16
            ]
        };

        let mut module = Module::new();
        module.add_struct(new_struct);

        let mut chunk = Chunk::new();
        // Push the first field
        let index = module.add_constant(Value::U8(10));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        // Push the second field
        let index = module.add_constant(Value::U16(20));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        chunk.emit_opcode(OpCode::NewStruct);
        // struct id
        chunk.write_u16(0);

        chunk.emit_opcode(OpCode::Return);

        module.add_chunk(chunk);

        let env = Environment::new();
        let mut vm = VM::new(&module, &env);
        vm.invoke_chunk_id(0).unwrap();
        assert_eq!(vm.run().unwrap(), Value::Struct(0, vec![
            ValueOwnable::Owned(Box::new(Value::U8(10))),
            ValueOwnable::Owned(Box::new(Value::U16(20)))
        ].into()));

        let chunk = module.get_chunk_at_mut(0).unwrap();
        chunk.pop_instruction();

        // Store the struct in the memory
        chunk.emit_opcode(OpCode::MemorySet);
        chunk.write_u16(0);

        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(0);

        // load first field
        chunk.emit_opcode(OpCode::SubLoad);
        chunk.write_u16(0);

        // cast first field into u16
        chunk.emit_opcode(OpCode::Cast);
        chunk.write_u8(Type::U16.primitive_byte().unwrap());

        // Load the struct again
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(0);


        // load second field
        chunk.emit_opcode(OpCode::SubLoad);
        chunk.write_u16(1);


        // Sum the two fields
        chunk.emit_opcode(OpCode::Add);

        chunk.emit_opcode(OpCode::Return);

        let mut vm = VM::new(&module, &env);
        vm.invoke_chunk_id(0).unwrap();
        assert_eq!(vm.run().unwrap(), Value::U16(30));
    }

    #[test]
    fn test_function_call() {
        let mut module = Module::new();

        // First function should return "true"
        let mut bool_fn = Chunk::new();
        let index = module.add_constant(Value::Boolean(true));
        bool_fn.emit_opcode(OpCode::Constant);
        bool_fn.write_u16(index as u16);

        bool_fn.emit_opcode(OpCode::Return);

        // This chunk should call the bool fn
        let mut first = Chunk::new();
        first.emit_opcode(OpCode::InvokeChunk);
        // Call the bool_fn which is at index 1 in chunks list
        first.write_u16(1);

        // not on a value
        first.write_bool(false);

        // 0 args
        first.write_u8(0);


        // return its value
        first.emit_opcode(OpCode::Return);

        module.add_chunk(first);
        module.add_chunk(bool_fn);

        let env = Environment::new();
        let mut vm = VM::new(&module, &env);
        vm.invoke_chunk_id(0).unwrap();
        assert_eq!(vm.run().unwrap(), Value::Boolean(true));
    }

    #[test]
    fn test_function_call_on_value() {
        let mut module = Module::new();

        // Create a struct with u64 field
        let new_struct = Struct {
            fields: vec![
                Type::U64
            ]
        };

        module.add_struct(new_struct);

        // Create a function on a struct
        // When called, the first stack value should be the struct
        let mut struct_fn = Chunk::new();
        struct_fn.emit_opcode(OpCode::SubLoad);
        // Read field 0
        struct_fn.write_u16(0);

        // Main function
        let mut main = Chunk::new();
        // Create a struct
        let index = module.add_constant(Value::Struct(0, vec![
            ValueOwnable::Owned(Box::new(Value::U64(10)))
        ].into()));

        main.emit_opcode(OpCode::Constant);
        main.write_u16(index as u16);

        // Call the struct_fn which is at index 1 in chunks list
        main.emit_opcode(OpCode::InvokeChunk);
        main.write_u16(1);
        main.write_bool(true);
        main.write_u8(0);

        module.add_chunk(main);
        module.add_chunk(struct_fn);

        let env = Environment::new();
        let mut vm = VM::new(&module, &env);
        vm.invoke_chunk_id(0).unwrap();
        assert_eq!(vm.run().unwrap(), Value::U64(10));
    }

    #[test]
    fn test_memory() {
        let mut module = Module::new();
        let mut chunk = Chunk::new();

        // Push element 1
        let index = module.add_constant(Value::U8(10));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        // Store the value in the memory
        chunk.emit_opcode(OpCode::MemorySet);
        chunk.write_u16(0);

        // Load the value from the memory
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(0);

        module.add_chunk(chunk);

        assert_eq!(run(module), Value::U8(10));
    }

    #[test]
    fn test_for_each_index() {
        let mut module = Module::new();
        let mut chunk = Chunk::new();

        // Push element 1
        let index = module.add_constant(Value::U8(10));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        // Push element 2
        let index = module.add_constant(Value::U8(20));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        // Create a new array with 2 elements
        chunk.emit_opcode(OpCode::NewArray);
        chunk.write_u32(2);

        // Store the array in the memory
        chunk.emit_opcode(OpCode::MemorySet);
        chunk.write_u16(0);

        let sum_index = module.add_constant(Value::U8(0));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(sum_index as u16);

        // Store sum in memory
        chunk.emit_opcode(OpCode::MemorySet);
        chunk.write_u16(1);

        let index = module.add_constant(Value::U32(0));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        // Store in memory
        chunk.emit_opcode(OpCode::MemorySet);
        chunk.write_u16(2);

        // Load the array
        chunk.emit_opcode(OpCode::MemoryLoad);
        let jump_at = chunk.last_index();
        chunk.write_u16(0);

        // load the index
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(2);

        // Load element
        chunk.emit_opcode(OpCode::ArrayCall);

        // Load the sum
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(1);

        chunk.emit_opcode(OpCode::Add);

        // Store the sum
        chunk.emit_opcode(OpCode::MemorySet);
        chunk.write_u16(1);

        // Load the index again
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(2);

        
        // Increment the index
        chunk.emit_opcode(OpCode::Inc);

        // Copy the value
        chunk.emit_opcode(OpCode::Copy);

        // Store the index
        chunk.emit_opcode(OpCode::MemorySet);
        chunk.write_u16(2);

        // Load the array
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(0);

        chunk.emit_opcode(OpCode::IterableLength);

        // pop len, pop index, push index >= len
        chunk.emit_opcode(OpCode::Gte);

        // jump to beginning if we are less than len
        chunk.emit_opcode(OpCode::JumpIfFalse);
        chunk.write_u32(jump_at as u32);

        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(1);

        module.add_chunk(chunk);

        assert_eq!(run(module), Value::U8(30));
    }

    #[test]
    fn test_for_each_iterator() {
        let mut module = Module::new();
        let mut chunk = Chunk::new();

        let index = module.add_constant(Value::Array(vec![
            ValueOwnable::Owned(Box::new(Value::U8(10))),
            ValueOwnable::Owned(Box::new(Value::U8(20))),
            ValueOwnable::Owned(Box::new(Value::U8(30))),
            ValueOwnable::Owned(Box::new(Value::U8(40))),
            ValueOwnable::Owned(Box::new(Value::U8(50))),
        ].into()));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        // Create a new sum variable in memory
        let index = module.add_constant(Value::U8(0));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        // Store the sum in memory
        chunk.emit_opcode(OpCode::MemorySet);
        chunk.write_u16(0);

        // Start iterator
        chunk.emit_opcode(OpCode::IteratorBegin);
        chunk.emit_opcode(OpCode::IteratorNext);
        let start_iterator_index = chunk.last_index();
        chunk.write_u32(0);
        let patch_addr = chunk.last_index();

        // Load the sum
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(0);

        // Swap both so sum is left and value is right
        chunk.emit_opcode(OpCode::Swap);
        chunk.write_u8(1);

        // Add the value to the sum
        chunk.emit_opcode(OpCode::AssignAdd);

        // Iterator end
        chunk.emit_opcode(OpCode::Jump);
        chunk.write_u32(start_iterator_index as u32);
        
        // Patch the iterator next jump
        chunk.patch_jump(patch_addr, chunk.index() as u32);

        // Load the sum
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(0);

        chunk.emit_opcode(OpCode::Return);

        // Execute
        module.add_chunk(chunk);
        assert_eq!(run(module), Value::U8(150));
    }
}

#[cfg(test)]
mod full_tests {
    use super::*;
    use xelis_compiler::Compiler;
    use xelis_environment::Environment;
    use xelis_builder::EnvironmentBuilder;
    use xelis_lexer::Lexer;
    use xelis_parser::Parser;
    use xelis_types::Value;

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
    fn test_u256() {
        let code = r#"
            entry main() {
                let x: u256 = 10;
                let y: u64 = 20;
                return (x + y as u256) as u64
            }
        "#;
    
        let (module, environment) = prepare_module(code);
    
        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(30u32.into()));
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
    
        let (module, environment) = prepare_module(code);
    
        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(20));
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
    
        let (module, environment) = prepare_module(code);
    
        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(30));
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
    
        let (module, environment) = prepare_module(code);
    
        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(1).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(10));
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
    
        let (module, environment) = prepare_module(code);
    
        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(1).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(10));
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
    
        let (module, environment) = prepare_module(code);
    
        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(1).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(0));
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
    
        let (module, environment) = prepare_module(code);
    
        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(10));
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
    
        let (module, environment) = prepare_module(code);
    
        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(50));
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
    
        let (module, environment) = prepare_module(code);
    
        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(45));
    }

    #[test]
    fn test_range_contains() {
        let code = r#"
            entry main() {
                let x: bool = (0..10).contains(5);
                return x as u64
            }
        "#;
    
        let (module, environment) = prepare_module(code);
    
        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(1));
    }


    #[test]
    fn test_range_contains_u256() {
        let code = r#"
            entry main() {
                let x: bool = (0u256..10u256).contains(5);
                return x as u64
            }
        "#;
    
        let (module, environment) = prepare_module(code);
    
        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(1));
    }

    #[test]
    fn test_range_collect() {
        let code = r#"
            entry main() {
                let x: u64[] = 0..10.collect();
                return x.len() as u64
            }
        "#;
    
        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(10));
    }

    #[test]
    fn test_range_type() {
        let code = r#"
            entry main() {
                let x: range<u64> = 0..10;
                return x.count()
            }
        "#;
    
        let (module, environment) = prepare_module(code);

        let mut vm = VM::new(&module, &environment);
        vm.invoke_chunk_id(0).unwrap();
        let value = vm.run().unwrap();
        assert_eq!(value, Value::U64(10));
    }
}