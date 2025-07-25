use super::*;

use xelis_bytecode::{Chunk, Module, OpCode};
use xelis_types::{Type, Primitive};

#[track_caller]
fn try_run(module: Module) -> Result<Primitive, VMError> {
    try_run_id(module, 0)
}

#[track_caller]
fn try_run_id(module: Module, id: u16) -> Result<Primitive, VMError> {
    let env = EnvironmentBuilder::default().build();
    run_internal(module, &env, id)
}

#[test]
fn test_casting() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();
    let index = module.add_constant(Primitive::U8(10));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    chunk.emit_opcode(OpCode::Cast);
    chunk.write_u8(Type::String.primitive_byte().unwrap());

    module.add_entry_chunk(chunk);

    assert_eq!(run(module), Primitive::String("10".to_string()));
}

#[test]
fn test_pop_empty_stack() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();
    chunk.emit_opcode(OpCode::Pop);

    module.add_entry_chunk(chunk);

    assert!(matches!(try_run(module), Err(VMError::EmptyStack)));
}


#[test]
fn test_pop_constant() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();
    let index = module.add_constant(Primitive::U8(10));

    // Add a return 0
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(module.add_constant(Primitive::U8(0)) as u16);

    // Constant to be poped
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    chunk.emit_opcode(OpCode::Pop);


    chunk.emit_opcode(OpCode::Return);
    module.add_entry_chunk(chunk);

    assert_eq!(run(module), Primitive::U8(0));
}

#[test]
fn test_pop_n_out_of_bounds() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();
    chunk.emit_opcode(OpCode::PopN);
    chunk.write_u8(1);

    module.add_entry_chunk(chunk);

    assert!(matches!(try_run(module), Err(VMError::StackIndexOutOfBounds)));
}

#[test]
fn test_pop_n_constants() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();
    let index = module.add_constant(Primitive::U8(10));

    // 0 that will be returned
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(module.add_constant(Primitive::U8(0)) as u16);

    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    let index = module.add_constant(Primitive::U8(20));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    chunk.emit_opcode(OpCode::PopN);
    chunk.write_u8(2);

    // Add a return 0
    chunk.emit_opcode(OpCode::Return);
    module.add_entry_chunk(chunk);

    assert_eq!(run(module), Primitive::U8(0));
}

#[test]
fn test_array_call() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();

    // Push element 1
    let index = module.add_constant(Primitive::U8(10));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    // Push element 2
    let index = module.add_constant(Primitive::U8(20));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    // Create a new array with 2 elements
    chunk.emit_opcode(OpCode::NewObject);
    chunk.write_u8(2);

    // Load the first element
    let index = module.add_constant(Primitive::U32(0));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    chunk.emit_opcode(OpCode::ArrayCall);
    module.add_entry_chunk(chunk);

    assert_eq!(run(module), Primitive::U8(10));
}

#[test]
fn test_multi_depth_array_call() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();

    let values = vec![
        Primitive::U8(10),
        Primitive::U8(20),
        Primitive::U8(30),
    ].into_iter().map(|v| v.into()).collect();

    // Push element 1
    let index = module.add_constant(Constant::Array(values));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    // Create a new array with 2 elements
    chunk.emit_opcode(OpCode::NewObject);
    chunk.write_u8(1);

    // Load the first element of the first array
    let index = module.add_constant(Primitive::U32(0));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    chunk.emit_opcode(OpCode::ArrayCall);

    // Load the last element
    let index = module.add_constant(Primitive::U32(2));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    chunk.emit_opcode(OpCode::ArrayCall);

    module.add_entry_chunk(chunk);

    assert_eq!(run(module), Primitive::U8(30));
}

#[test]
fn test_struct() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();
    // Push the first field
    let index = module.add_constant(Primitive::U8(10));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    // Push the second field
    let index = module.add_constant(Primitive::U16(20));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    chunk.emit_opcode(OpCode::NewObject);
    chunk.write_u8(2);

    chunk.emit_opcode(OpCode::Return);

    module.add_entry_chunk(chunk);

    {
        let env = Environment::new();
        let mut vm = VM::new(&env);
        vm.append_module(&module, &()).expect("module");
        vm.invoke_chunk_id(0).expect("entry chunk at 0");
        assert_eq!(
            vm.run_blocking().unwrap(),
            ValueCell::Object(
                vec![
                    Primitive::U8(10).into(),
                    Primitive::U16(20).into()
                ]
            )
        );
    }

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

    let env = Environment::new();
    let mut vm = VM::new(&env);
    vm.append_module(&module, &()).expect("module");
    vm.invoke_chunk_id(0).expect("entry chunk");

    assert_eq!(vm.run_blocking().unwrap(), Primitive::U16(30).into());
}

#[test]
fn test_function_call() {
    let mut module = Module::new();

    // First function should return "true"
    let mut bool_fn = Chunk::new();
    let index = module.add_constant(Primitive::Boolean(true));
    bool_fn.emit_opcode(OpCode::Constant);
    bool_fn.write_u16(index as u16);

    bool_fn.emit_opcode(OpCode::Return);

    // This chunk should call the bool fn
    let mut first = Chunk::new();
    first.emit_opcode(OpCode::InvokeChunk);
    // Call the bool_fn which is at index 1 in chunks list
    first.write_u16(1);

    // 0 args
    first.write_u8(0);


    // return its value
    first.emit_opcode(OpCode::Return);

    module.add_internal_chunk(first);
    module.add_internal_chunk(bool_fn);

    let env = Environment::new();
    let mut vm = VM::new(&env);
    vm.append_module(&module, &()).expect("module");
    vm.invoke_chunk_id(0).expect("entry chunk");

    assert_eq!(vm.run_blocking().unwrap(), Primitive::Boolean(true).into());
}

#[test]
fn test_function_call_on_value() {
    let mut module = Module::new();

    // Create a function on a struct
    // When called, the first stack value should be the struct
    let mut struct_fn = Chunk::new();
    struct_fn.emit_opcode(OpCode::SubLoad);
    // Read field 0
    struct_fn.write_u8(0);

    // Main function
    let mut main = Chunk::new();
    // Create a struct
    let index = module.add_constant(Constant::Array(vec![
        Primitive::U64(10).into()
    ]));

    main.emit_opcode(OpCode::Constant);
    main.write_u16(index as u16);

    // Call the struct_fn which is at index 1 in chunks list
    main.emit_opcode(OpCode::InvokeChunk);
    main.write_u16(1);
    main.write_u8(1);

    module.add_entry_chunk(main);
    module.add_internal_chunk(struct_fn);

    let env = Environment::new();
    let mut vm = VM::new(&env);
    vm.append_module(&module, &()).expect("module");
    vm.invoke_chunk_id(0).expect("entry chunk");

    assert_eq!(vm.run_blocking().unwrap(), Primitive::U64(10).into());
}

#[test]
fn test_memory() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();

    // Push element 1
    let index = module.add_constant(Primitive::U8(10));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    // Store the value in the memory
    chunk.emit_opcode(OpCode::MemorySet);
    chunk.write_u16(0);

    // Load the value from the memory
    chunk.emit_opcode(OpCode::MemoryLoad);
    chunk.write_u16(0);

    module.add_entry_chunk(chunk);

    assert_eq!(run(module), Primitive::U8(10));
}

#[test]
fn test_for_each_index() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();

    // Push element 1
    let index = module.add_constant(Primitive::U8(10));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    // Push element 2
    let index = module.add_constant(Primitive::U8(20));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    // Create a new array with 2 elements
    chunk.emit_opcode(OpCode::NewObject);
    chunk.write_u8(2);

    // Store the array in the memory
    chunk.emit_opcode(OpCode::MemorySet);
    chunk.write_u16(0);

    let sum_index = module.add_constant(Primitive::U8(0));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(sum_index as u16);

    // Store sum in memory
    chunk.emit_opcode(OpCode::MemorySet);
    chunk.write_u16(1);

    let index = module.add_constant(Primitive::U32(0));
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

    module.add_entry_chunk(chunk);

    assert_eq!(run(module), Primitive::U8(30));
}

#[test]
fn test_memory_overwrite_self() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();

    // Push element 1
    let index = module.add_constant(Primitive::U64(10));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    chunk.emit_opcode(OpCode::MemorySet);
    chunk.write_u16(0);

    // load the pointer
    chunk.emit_opcode(OpCode::MemoryLoad);
    chunk.write_u16(0);

    // Store the pointer by overwriting its source 
    chunk.emit_opcode(OpCode::MemorySet);
    chunk.write_u16(0);

    // load the pointer stored again
    chunk.emit_opcode(OpCode::MemoryLoad);
    chunk.write_u16(0);

    module.add_entry_chunk(chunk);

    assert_eq!(run(module), Primitive::U64(10));
}

#[test]
fn test_safe_sub_value_pointer() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();

    // Push element 1
    module.add_constant(Constant::Array(vec![Primitive::U64(10).into()]));
    module.add_constant(Primitive::U32(0));

    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(0);

    chunk.emit_opcode(OpCode::MemorySet);
    chunk.write_u16(0);

    // load the pointer
    chunk.emit_opcode(OpCode::MemoryLoad);
    chunk.write_u16(0);

    // push index
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(1);

    // Load index 0 from array
    chunk.emit_opcode(OpCode::ArrayCall);

    // Store the pointer by overwriting its source
    chunk.emit_opcode(OpCode::MemorySet);
    chunk.write_u16(0);

    // load the pointer stored again
    chunk.emit_opcode(OpCode::MemoryLoad);
    chunk.write_u16(0);

    module.add_entry_chunk(chunk);

    assert_eq!(run(module), Primitive::U64(10));
}


#[test]
fn test_safe_sub_intermediate_value_pointer() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();

    // Push element 1
    module.add_constant(Constant::Array(vec![Constant::Array(vec![Primitive::U64(10).into()])]));
    module.add_constant(Primitive::U32(0));

    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(0);

    chunk.emit_opcode(OpCode::MemorySet);
    chunk.write_u16(0);

    // load the pointer
    chunk.emit_opcode(OpCode::MemoryLoad);
    chunk.write_u16(0);

    // push index
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(1);

    // Load index 0 from array
    chunk.emit_opcode(OpCode::ArrayCall);

    // Store the pointer by writing the pointer in #1
    // So here we have the sub array
    chunk.emit_opcode(OpCode::MemorySet);
    chunk.write_u16(1);

    // Load the stored pointer of above instruction
    chunk.emit_opcode(OpCode::MemoryLoad);
    chunk.write_u16(1);

    // Repush index
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(1);

    // Load index 0 from sub array
    chunk.emit_opcode(OpCode::ArrayCall);

    // Copy the value so the pointer live in the stack
    chunk.emit_opcode(OpCode::Copy);

    // Now overwrite the origin array on which mem#1 depends
    chunk.emit_opcode(OpCode::MemorySet);
    chunk.write_u16(0);

    module.add_entry_chunk(chunk);

    assert_eq!(run(module), Primitive::U64(10));
}

#[test]
fn test_for_each_iterator() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();

    let index = module.add_constant(Constant::Array(vec![
        Primitive::U8(10).into(),
        Primitive::U8(20).into(),
        Primitive::U8(30).into(),
        Primitive::U8(40).into(),
        Primitive::U8(50).into(),
    ]));
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(index as u16);

    // Create a new sum variable in memory
    let index = module.add_constant(Primitive::U8(0));
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
    module.add_entry_chunk(chunk);
    assert_eq!(run(module), Primitive::U8(150));
}

#[test]
fn test_bad_program_recursive_infinite() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();

    // Recursive call
    chunk.emit_opcode(OpCode::InvokeChunk);
    chunk.write_u16(0);
    chunk.write_u8(0);

    chunk.emit_opcode(OpCode::Return);

    // Execute
    module.add_internal_chunk(chunk);

    assert!(try_run(module).is_err());
}

#[test]
fn test_bad_program_bad_stack_pointers() {
    let mut module = Module::new();

    let mut add = Chunk::new();

    add.emit_opcode(OpCode::Pop);
    add.emit_opcode(OpCode::Pop);

    add.emit_opcode(OpCode::Constant);
    add.write_u16(2);

    add.emit_opcode(OpCode::MemorySet);
    add.write_u16(0);

    add.emit_opcode(OpCode::MemoryLoad);
    add.write_u16(0);

    let mut main = Chunk::new();

    module.add_constant(Primitive::U64(0));
    module.add_constant(Primitive::U64(1));
    module.add_constant(Primitive::U64(3));

    // Store first constant in our memory then load it again
    main.emit_opcode(OpCode::Constant);
    main.write_u16(0);

    main.emit_opcode(OpCode::MemorySet);
    main.write_u16(0);

    // Same for the second
    main.emit_opcode(OpCode::Constant);
    main.write_u16(1);

    main.emit_opcode(OpCode::MemorySet);
    main.write_u16(1);

    // Load their pointers
    main.emit_opcode(OpCode::MemoryLoad);
    main.write_u16(0);

    main.emit_opcode(OpCode::MemoryLoad);
    main.write_u16(1);

    main.emit_opcode(OpCode::InvokeChunk);
    main.write_u16(0);
    main.write_u8(2);

    main.emit_opcode(OpCode::Return);

    // Execute
    module.add_internal_chunk(add);
    module.add_entry_chunk(main);

    let res = try_run_id(module, 1).unwrap();
    assert!(matches!(res, Primitive::U64(3)));
}

#[test]
fn test_infinite_map_depth() {
    let mut module = Module::new();
    let mut chunk = Chunk::new();

    let max_iterations = 100_000;
    // let map = {};
    // foreach _ in 0..max_iterations {
    //     map.insert("hello world", map);
    // }

    let constant_id = module.add_constant(Primitive::String("hello world".to_string())) as u16;

    // Create the map
    chunk.emit_opcode(OpCode::NewMap);
    chunk.write_u8(0);

    // Store in memory
    chunk.emit_opcode(OpCode::MemorySet);
    chunk.write_u16(0);

    let id = module.add_constant(Primitive::U32(0)) as u16;
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(id);

    chunk.emit_opcode(OpCode::MemorySet);
    chunk.write_u16(1);

    let jump_at = chunk.index();

    // Load the index and increment it
    chunk.emit_opcode(OpCode::MemoryLoad);
    chunk.write_u16(1);

    chunk.emit_opcode(OpCode::Inc);
    chunk.emit_opcode(OpCode::Pop);

    // Now load the initial map
    chunk.emit_opcode(OpCode::MemoryLoad);
    chunk.write_u16(0);

    // Insert the map into itself
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(constant_id);

    chunk.emit_opcode(OpCode::MemoryLoad);
    chunk.write_u16(0);

    // Insert map
    chunk.emit_opcode(OpCode::SysCall);
    chunk.write_u16(89);
    chunk.write_bool(true);
    chunk.write_u8(2);

    // Drop the returned value
    chunk.emit_opcode(OpCode::Pop);

    // jump to beginning if we are less than len
    chunk.emit_opcode(OpCode::MemoryLoad);
    chunk.write_u16(1);

    let id = module.add_constant(Primitive::U32(max_iterations)) as u16;
    chunk.emit_opcode(OpCode::Constant);
    chunk.write_u16(id);

    chunk.emit_opcode(OpCode::Gte);

    chunk.emit_opcode(OpCode::JumpIfFalse);
    chunk.write_u32(jump_at as u32);

    chunk.emit_opcode(OpCode::MemoryLoad);
    chunk.write_u16(0);

    // Execute
    module.add_entry_chunk(chunk);

    assert!(try_run(module).is_err());
    // assert!(matches!(try_run(module), Err(VMError::EnvironmentError(EnvironmentError::ValueError(ValueError::MaxDepthReached)))));
}