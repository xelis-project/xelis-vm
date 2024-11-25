mod chunk;
mod error;
mod iterator;
mod stack;
mod validator;

pub mod instructions;
pub use validator::*;

use xelis_environment::{Environment, Context};
use instructions::{InstructionResult, InstructionTable};
use stack::Stack;

use xelis_types::{EnumType, Path, StructType, Constant};
use xelis_bytecode::Module;

pub use error::VMError;
pub use chunk::*;

// 64 elements maximum in the call stack
const CALL_STACK_SIZE: usize = 64;

// Backend of the VM
// This is the immutable part of the VM
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
    #[inline]
    pub fn get_struct_with_id(&self, mut id: usize) -> Result<&StructType, VMError> {
        let env_structs = self.environment.get_structures();
        if let Some(struct_type) = env_structs.get_index(id) {
            return Ok(struct_type);
        } else {
            id -= env_structs.len();
        }

        self.module.get_struct_at(id).ok_or(VMError::StructNotFound)
    }

    // Get a constant registered in the module using its id
    #[inline(always)]
    pub fn get_constant_with_id(&self, id: usize) -> Result<&Constant, VMError> {
        self.module.get_constant_at(id).ok_or(VMError::ConstantNotFound)
    }

    // Get an enum with an id
    #[inline]
    pub fn get_enum_with_id(&self, mut id: usize) -> Result<&EnumType, VMError> {
        let env_enums = self.environment.get_enums();
        if let Some(enum_type) = env_enums.get_index(id) {
            return Ok(enum_type);
        } else {
            id -= env_enums.len();
        }

        self.module.get_enum_at(id).ok_or(VMError::EnumNotFound)
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
    // Context given to each instruction
    context: Context<'a>,
}

impl<'a> VM<'a> {
    // Create a new VM
    pub fn new(module: &'a Module, environment: &'a Environment) -> Self {
        Self::with(module, environment, InstructionTable::new(), Context::default())
    }

    // Create a new VM with a given table and context
    pub fn with(module: &'a Module, environment: &'a Environment, table: InstructionTable<'a>, context: Context<'a>) -> Self {
        Self {
            backend: Backend {
                module,
                environment,
                table,
            },
            call_stack: Vec::with_capacity(4),
            stack: Stack::new(),
            context,
        }
    }

    // Get the stack
    #[inline]
    pub fn get_stack(&self) -> &Stack<'a> {
        &self.stack
    }

    // Get the context
    #[inline]
    pub fn context(&self) -> &Context<'a> {
        &self.context
    }

    // Get a mutable reference to the context
    #[inline]
    pub fn context_mut(&mut self) -> &mut Context<'a> {
        &mut self.context
    }

    // Get the instruction table
    #[inline]
    pub fn table(&self) -> &InstructionTable<'a> {
        &self.backend.table
    }

    // Get a mutable reference to the instruction table
    #[inline]
    pub fn table_mut(&mut self) -> &mut InstructionTable<'a> {
        &mut self.backend.table
    }

    // Get the environment
    #[inline]
    pub fn environment(&self) -> &Environment {
        self.backend.environment
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
    pub fn run(&mut self) -> Result<Constant, VMError> {
        while let Some(mut manager) = self.call_stack.pop() {
            while let Some(opcode) = manager.next_u8() {
                match self.backend.table.execute(opcode, &self.backend, &mut self.stack, &mut manager, &mut self.context)? {
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
        if self.stack.count() != 0 {
            return Err(VMError::StackNotCleaned);
        }

        Ok(end_value.into())
    }
}


#[cfg(test)]
mod tests {
    use xelis_builder::EnvironmentBuilder;
    use xelis_bytecode::{Chunk, Module, OpCode};
    use xelis_environment::EnvironmentError;
    use xelis_types::{Type, Value, ValueError};

    use super::*;

    #[track_caller]
    fn run_internal(module: Module) -> Result<Value, VMError> {
        let env = EnvironmentBuilder::default().build();
        let mut vm = VM::new(&module, &env);
        vm.invoke_chunk_id(0).unwrap();
        vm.run().map(|v| v.into_value().unwrap())
    }

    #[track_caller]
    fn run(module: Module) -> Value {
        run_internal(module).unwrap()
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
    fn test_pop_empty_stack() {
        let mut module = Module::new();
        let mut chunk = Chunk::new();
        chunk.emit_opcode(OpCode::Pop);

        module.add_chunk(chunk);

        assert!(matches!(run_internal(module), Err(VMError::EmptyStack)));
    }


    #[test]
    fn test_pop_constant() {
        let mut module = Module::new();
        let mut chunk = Chunk::new();
        let index = module.add_constant(Value::U8(10));

        // Add a return 0
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(module.add_constant(Value::U8(0)) as u16);

        // Constant to be poped
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        chunk.emit_opcode(OpCode::Pop);


        chunk.emit_opcode(OpCode::Return);
        module.add_chunk(chunk);

        assert_eq!(run(module), Value::U8(0));
    }

    #[test]
    fn test_pop_n_out_of_bounds() {
        let mut module = Module::new();
        let mut chunk = Chunk::new();
        chunk.emit_opcode(OpCode::PopN);
        chunk.write_u8(1);

        module.add_chunk(chunk);

        assert!(matches!(run_internal(module), Err(VMError::StackIndexOutOfBounds)));
    }

    #[test]
    fn test_pop_n_constants() {
        let mut module = Module::new();
        let mut chunk = Chunk::new();
        let index = module.add_constant(Value::U8(10));

        // 0 that will be returned
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(module.add_constant(Value::U8(0)) as u16);

        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        let index = module.add_constant(Value::U8(20));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        chunk.emit_opcode(OpCode::PopN);
        chunk.write_u8(2);

        // Add a return 0
        chunk.emit_opcode(OpCode::Return);
        module.add_chunk(chunk);

        assert_eq!(run(module), Value::U8(0));
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
        chunk.write_u8(2);

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
        ].into_iter().map(|v| v.into()).collect();

        // Push element 1
        let index = module.add_constant(Constant::Array(values));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(index as u16);

        // Create a new array with 2 elements
        chunk.emit_opcode(OpCode::NewArray);
        chunk.write_u8(1);

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
        let new_struct = StructType::new(0, vec![
                Type::U8,
                Type::U16
            ]
        );

        let mut module = Module::new();
        module.add_struct(new_struct.clone());

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
        assert_eq!(
            vm.run().unwrap(),
            Constant::Struct(
                vec![
                    Value::U8(10).into(),
                    Value::U16(20).into()
                ].into(),
                new_struct
            )
        );

        let chunk = module.get_chunk_at_mut(0).unwrap();
        chunk.pop_instruction();

        // Store the struct in the memory
        chunk.emit_opcode(OpCode::MemorySet);
        chunk.write_u16(0);

        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(0);

        // load first field
        chunk.emit_opcode(OpCode::SubLoad);
        chunk.write_u8(0);

        // cast first field into u16
        chunk.emit_opcode(OpCode::Cast);
        chunk.write_u8(Type::U16.primitive_byte().unwrap());

        // Load the struct again
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(0);


        // load second field
        chunk.emit_opcode(OpCode::SubLoad);
        chunk.write_u8(1);


        // Sum the two fields
        chunk.emit_opcode(OpCode::Add);

        chunk.emit_opcode(OpCode::Return);

        let mut vm = VM::new(&module, &env);
        vm.invoke_chunk_id(0).unwrap();
        assert_eq!(vm.run().unwrap(), Value::U16(30).into());
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
        assert_eq!(vm.run().unwrap(), Value::Boolean(true).into());
    }

    #[test]
    fn test_function_call_on_value() {
        let mut module = Module::new();

        // Create a struct with u64 field
        let new_struct = StructType::new(0, vec![Type::U64]);

        module.add_struct(new_struct.clone());

        // Create a function on a struct
        // When called, the first stack value should be the struct
        let mut struct_fn = Chunk::new();
        struct_fn.emit_opcode(OpCode::SubLoad);
        // Read field 0
        struct_fn.write_u8(0);

        // Main function
        let mut main = Chunk::new();
        // Create a struct
        let index = module.add_constant(Constant::Struct(vec![
            Value::U64(10).into()
        ], new_struct));

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
        assert_eq!(vm.run().unwrap(), Value::U64(10).into());
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
        chunk.write_u8(2);

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

        let index = module.add_constant(Constant::Array(vec![
            Value::U8(10).into(),
            Value::U8(20).into(),
            Value::U8(30).into(),
            Value::U8(40).into(),
            Value::U8(50).into(),
        ]));
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

    #[test]
    fn test_bad_program_recursive_infinite() {
        let mut module = Module::new();
        let mut chunk = Chunk::new();

        // Recursive call
        chunk.emit_opcode(OpCode::InvokeChunk);
        chunk.write_u16(0);
        chunk.write_bool(false);
        chunk.write_u8(0);

        chunk.emit_opcode(OpCode::Return);

        // Execute
        module.add_chunk(chunk);
        assert!(matches!(run_internal(module), Err(VMError::CallStackOverflow)));
    }

    #[test]
    fn test_infinite_map_depth() {
        let mut module = Module::new();
        let mut chunk = Chunk::new();

        // let map = {};
        // foreach _ in 0..28000 {
        //     let mut inner_map = {};
        //     inner_map.insert("hello world", map);
        //     map = inner_map;
        // }

        let constant_id = module.add_constant(Value::String("hello world".to_string())) as u16;

        // Create the map
        chunk.emit_opcode(OpCode::NewMap);
        chunk.write_u8(0);

        // Store in memory
        chunk.emit_opcode(OpCode::MemorySet);
        chunk.write_u16(0);

        let jump_at = chunk.index();
        // Create the inner map
        chunk.emit_opcode(OpCode::NewMap);
        chunk.write_u8(0);

        // Store in memory
        chunk.emit_opcode(OpCode::MemorySet);
        chunk.write_u16(1);

        // Load the map for insert
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(1);

        // Insert the map into the inner map
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u16(constant_id);

        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(0);

        // Insert map
        chunk.emit_opcode(OpCode::SysCall);
        chunk.write_u16(79);
        chunk.write_bool(true);
        chunk.write_u8(2);

        // Drop the returned value
        chunk.emit_opcode(OpCode::Pop);

        // Load again the map
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(1);

        // Store it in memory at place of the map
        chunk.emit_opcode(OpCode::MemorySet);
        chunk.write_u16(0);

        // jump to beginning
        chunk.emit_opcode(OpCode::Jump);
        chunk.write_u32(jump_at as u32);

        // Execute
        module.add_chunk(chunk);

        assert!(matches!(run_internal(module), Err(VMError::EnvironmentError(EnvironmentError::ValueError(ValueError::MaxDepthReached)))));
    }
}

#[cfg(test)]
mod full_tests {
    use super::*;
    use xelis_compiler::Compiler;
    use xelis_environment::{Environment, EnvironmentError};
    use xelis_builder::EnvironmentBuilder;
    use xelis_lexer::Lexer;
    use xelis_parser::Parser;
    use xelis_types::Value;

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
    fn run_code_id(code: &str, id: u16) -> Value {
        let (module, environment) = prepare_module(code);
        let mut vm = VM::new(&module, &environment);
        vm.invoke_entry_chunk(id).unwrap();
        vm.run().unwrap().into_value().unwrap()
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
        vm.context_mut().set_gas_limit(Some(1000));
        vm.invoke_entry_chunk(0).unwrap();

        assert!(matches!(vm.run(), Err(VMError::EnvironmentError(EnvironmentError::NotEnoughGas { .. }))));
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

        assert_eq!(run_code(code), Value::U64(1000000));

        let mut code = r#"
            entry main() {
                let a: u64 = 1;
                let b: u64 = a
        "#.to_string() + "+ a + a ".repeat(10000).as_str();
        code.push_str("return x }");

        // TODO FIXME
        todo!("Fix stack overflow test");
        // assert_eq!(run_code(&code), Value::U64(10000 * 2 + 1));
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
}