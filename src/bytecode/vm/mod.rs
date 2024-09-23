mod chunk;
mod error;

use std::{cell::RefCell, collections::VecDeque, rc::Rc};

pub use error::VMError;
pub use chunk::*;

use crate::{interpreter::Path, types::Struct, Environment, Type, Value};

use super::{opcode::OpCode, Module};


macro_rules! op {
    ($a: expr, $b: expr, $op: tt) => {{
        Path::Owned(match ($a.into_owned(), $b.into_owned()) {
            (Value::U8(a), Value::U8(b)) => Value::U8(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::U16(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::U32(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::U64(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::U128(a $op b),
            _ => return Err(VMError::IncompatibleValues)
        })
    }};
}

macro_rules! op_bool {
    ($a: expr, $b: expr, $op: tt) => {{
        Path::Owned(match ($a.into_owned(), $b.into_owned()) {
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a $op b),
            (Value::U8(a), Value::U8(b)) => Value::Boolean(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::Boolean(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::Boolean(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::Boolean(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::Boolean(a $op b),
            _ => return Err(VMError::IncompatibleValues)
        })
    }};
}

// Virtual Machine to execute the bytecode from chunks of a Module.
// TODO: to be configurable, use a InstructionTable of 256 entries
// with functions pointers to the implementation for each u8 op code
// This will gain some performances by skipping a matching in OpCode::from_byte
pub struct VM<'a> {
    // The module to execute
    module: &'a Module,
    // The environment of the VM
    environment: &'a Environment,
    // The call stack of the VM
    call_stack: Vec<ChunkManager<'a>>,
}

impl<'a> VM<'a> {
    // Create a new VM
    pub fn new(module: &'a Module, environment: &'a Environment) -> Self {
        VM {
            module,
            environment,
            call_stack: Vec::new(),
        }
    }

    // Get a struct with an id
    // TODO: support env structs
    pub fn get_struct_with_id(&self, id: u16) -> Result<&Struct, VMError> {
        self.module.get_struct_at(id as usize).ok_or(VMError::StructNotFound)
    }

    // Invoke a chunk using its id
    pub fn invoke_chunk_id(&mut self, id: u16) -> Result<(), VMError> {
        let chunk = self.module.get_chunk_at(id as usize)
            .ok_or(VMError::ChunkNotFound)?;

        let manager = ChunkManager::new(chunk);
        self.call_stack.push(manager);
        Ok(())
    }

    // Invoke a chunk using its id and arguments
    pub fn invoke_chunk_with_args(&mut self, id: u16, args: Vec<Path<'a>>) -> Result<(), VMError> {
        let chunk = self.module.get_chunk_at(id as usize)
            .ok_or(VMError::ChunkNotFound)?;

        let mut manager = ChunkManager::new(chunk);
        manager.extend_stack(args);
        self.call_stack.push(manager);
        Ok(())
    }

    // Run the VM
    // It will execute the bytecode
    // First chunk executed should always return a value
    pub fn run(&mut self) -> Result<Value, VMError> {
        let mut final_result = None;

        'main: while let Some(mut manager) = self.call_stack.pop() {
            if let Some(value) = final_result.take() {
                manager.push_stack(value);
            }

            while let Ok(op_code) = manager.read_op_code() {
                match op_code {
                    OpCode::Constant => {
                        let index = manager.read_u8()? as usize;
                        let constant = self.module.get_constant_at(index).ok_or(VMError::ConstantNotFound)?;
                        manager.push_stack(Path::Borrowed(constant));
                    },
                    OpCode::MemoryLoad => {
                        let index = manager.read_u16()?;
                        let value = manager.from_register(index as usize)?
                            .shareable();
                        manager.push_stack(value);
                    },
                    OpCode::MemoryStore => {
                        let value = manager.pop_stack()?;
                        manager.push_register(value);
                    },
                    OpCode::MemoryAssign => {
                        let index = manager.read_u16()?;
                        let value = manager.pop_stack()?;
                        manager.to_register(index as usize, value)?;
                    },
                    OpCode::Copy => {
                        let value = manager.last_stack()?;
                        manager.push_stack(value.clone());
                    },
                    OpCode::Cast => {
                        let _type = manager.read_type()?;
                        let current = manager.pop_stack()?
                            .into_owned();

                        let value = match _type {
                            Type::U8 => Value::U8(current.cast_to_u8().unwrap()),
                            Type::U16 => Value::U16(current.cast_to_u16().unwrap()),
                            Type::U32 => Value::U32(current.cast_to_u32().unwrap()),
                            Type::U64 => Value::U64(current.cast_to_u64().unwrap()),
                            Type::U128 => Value::U128(current.cast_to_u128().unwrap()),
                            Type::String => Value::String(current.cast_to_string().unwrap()),
                            _ => return Err(VMError::UnsupportedCastType)
                        };
                        manager.push_stack(Path::Owned(value));
                    },
                    OpCode::NewArray => {
                        let length = manager.read_u32()?;
                        let mut array = VecDeque::with_capacity(length as usize);
                        for _ in 0..length {
                            array.push_front(Rc::new(RefCell::new(manager.pop_stack()?.into_owned())));
                        }

                        manager.push_stack(Path::Owned(Value::Array(array.into())));
                    },
                    OpCode::ArrayCall => {
                        let index = manager.pop_stack()?.into_owned().cast_to_u32().unwrap();
                        let value = manager.pop_stack()?;
                        manager.push_stack(value.get_sub_variable(index as usize).unwrap());
                    },
                    OpCode::InvokeChunk => {
                        let id = manager.read_u16()?;
                        let on_value = if manager.read_bool()? {
                            Some(manager.pop_stack()?)
                        } else {
                            None
                        };
                        let args = manager.read_u8()?;
    
                        let mut arguments = VecDeque::with_capacity(args as usize);
                        for _ in 0..args {
                            arguments.push_front(manager.pop_stack()?);
                        }
    
                        // Add back our current state to the stack
                        self.call_stack.push(manager);
    
                        // Find the chunk
                        let chunk = self.module.get_chunk_at(id as usize)
                            .ok_or(VMError::ChunkNotFound)?;

                        let mut new_manager = ChunkManager::new(chunk);
                        
                        new_manager.extend_stack(arguments);
                        if let Some(value) = on_value {
                            new_manager.push_stack(value);
                        }

                        self.call_stack.push(new_manager);
    
                        continue 'main;
                    },
                    OpCode::SysCall => {
                        let id = manager.read_u16()?;
                        let mut on_value = if manager.read_bool()? {
                            Some(manager.pop_stack()?)
                        } else {
                            None
                        };
                        let args = manager.read_u8()?;

                        let mut arguments = VecDeque::with_capacity(args as usize);
                        for _ in 0..args {
                            arguments.push_front(manager.pop_stack()?);
                        }

                        let func = self.environment.get_functions().get(id as usize)
                            .ok_or(VMError::UnknownSysCall)?;

                        let mut instance = match on_value.as_mut() {
                            Some(v) => Some(v.as_mut()),
                            None => None,
                        };

                        func.call_function(instance.as_deref_mut(), arguments.into(), todo!())?;
                    },
                    OpCode::NewStruct => {
                        let id = manager.read_u16()?;
                        let structure = self.get_struct_with_id(id)?;
                        let mut fields = VecDeque::new();
                        for _ in 0..structure.fields.len() {
                            fields.push_front(Rc::new(RefCell::new(manager.pop_stack()?.into_owned())));
                        }
    
                        manager.push_stack(Path::Owned(Value::Struct(id, fields.into())));
                    },
                    OpCode::Add => {
                        let right = manager.pop_stack()?;
                        let left = manager.pop_stack()?;
                        manager.push_stack(op!(left, right, +));
                    },
                    OpCode::SubLoad => {
                        let index = manager.read_u16()?;
                        let path = manager.pop_stack()?;
                        manager.push_stack(path.get_sub_variable(index as usize).unwrap());
                    },
                    OpCode::Lte => {
                        let right = manager.pop_stack()?;
                        let left = manager.pop_stack()?;
                        manager.push_stack(op_bool!(left, right, <=));
                    },
                    OpCode::Gt => {
                        let right = manager.pop_stack()?;
                        let left = manager.pop_stack()?;
                        manager.push_stack(op_bool!(left, right, >));
                    },
                    OpCode::Gte => {
                        let right = manager.pop_stack()?;
                        let left = manager.pop_stack()?;
                        manager.push_stack(op_bool!(left, right, >=));
                    },
                    OpCode::Jump => {
                        let index = manager.read_u32()?;
                        manager.set_index(index as usize);
                    },
                    OpCode::JumpIfFalse => {
                        let index = manager.read_u32()?;
                        let value = manager.pop_stack()?;
                        if !value.as_bool().unwrap() {
                            manager.set_index(index as usize);
                        }
                    },
                    OpCode::IterableLength => {
                        let value = manager.pop_stack()?;
                        let len = value.as_ref().as_vec().unwrap().len();
                        manager.push_stack(Path::Owned(Value::U32(len as u32)));
                    },
                    OpCode::Inc => {
                        let v = manager.last_mut_stack()?;
                        v.as_mut().increment().unwrap();
                    },
                    OpCode::Return => {
                        break;
                    },
                    _ => {
                        println!("Not implemented {:?}", op_code);
                        break;
                    }
                }
            }

            final_result = manager.pop_stack().ok();
        }

        Ok(final_result.ok_or(VMError::NoValue)?.into_owned())
    }
}


#[cfg(test)]
mod tests {
    use crate::bytecode::Chunk;

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
        chunk.write_u8(index as u8);

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
        chunk.write_u8(index as u8);

        // Push element 2
        let index = module.add_constant(Value::U8(20));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        // Create a new array with 2 elements
        chunk.emit_opcode(OpCode::NewArray);
        chunk.write_u32(2);

        // Load the first element
        let index = module.add_constant(Value::U16(0));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

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
        ].into_iter().map(|v| Rc::new(RefCell::new(v))).collect();

        // Push element 1
        let index = module.add_constant(Value::Array(values));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        // Create a new array with 2 elements
        chunk.emit_opcode(OpCode::NewArray);
        chunk.write_u32(1);

        // Load the first element of the first array
        let index = module.add_constant(Value::U16(0));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        chunk.emit_opcode(OpCode::ArrayCall);

        // Load the last element
        let index = module.add_constant(Value::U16(2));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

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
        chunk.write_u8(index as u8);

        // Push the second field
        let index = module.add_constant(Value::U16(20));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        chunk.emit_opcode(OpCode::NewStruct);
        // struct id
        chunk.write_u16(0);

        chunk.emit_opcode(OpCode::Return);

        module.add_chunk(chunk);

        let env = Environment::new();
        let mut vm = VM::new(&module, &env);
        vm.invoke_chunk_id(0).unwrap();
        assert_eq!(vm.run().unwrap(), Value::Struct(0, vec![
            Rc::new(RefCell::new(Value::U8(10))),
            Rc::new(RefCell::new(Value::U16(20)))
        ].into()));

        let chunk = module.get_chunk_at_mut(0).unwrap();
        chunk.pop_instruction();

        // Store the struct in the memory
        chunk.emit_opcode(OpCode::MemoryStore);

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

        let env = Environment::new();
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
        bool_fn.write_u8(index as u8);

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
            Rc::new(RefCell::new(Value::U64(10)))
        ].into()));

        main.emit_opcode(OpCode::Constant);
        main.write_u8(index as u8);

        // Call the struct_fn which is at index 1 in chunks list
        main.emit_opcode(OpCode::InvokeChunk);
        main.write_u8(0);
        main.write_bool(true);
        main.write_u16(1);

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
        chunk.write_u8(index as u8);

        // Store the value in the memory
        chunk.emit_opcode(OpCode::MemoryStore);

        // Load the value from the memory
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(0);

        module.add_chunk(chunk);

        assert_eq!(run(module), Value::U8(10));
    }

    #[test]
    fn test_for_each() {
        let mut module = Module::new();
        let mut chunk = Chunk::new();

        // Push element 1
        let index = module.add_constant(Value::U8(10));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        // Push element 2
        let index = module.add_constant(Value::U8(20));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        // Create a new array with 2 elements
        chunk.emit_opcode(OpCode::NewArray);
        chunk.write_u32(2);

        // Store the array in the memory
        chunk.emit_opcode(OpCode::MemoryStore);

        let sum_index = module.add_constant(Value::U8(0));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(sum_index as u8);

        // Store sum in memory
        chunk.emit_opcode(OpCode::MemoryStore);

        let index = module.add_constant(Value::U32(0));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        // Store in memory
        chunk.emit_opcode(OpCode::MemoryStore);

        // Load the array
        chunk.emit_opcode(OpCode::MemoryLoad);
        let jump_at = chunk.index();
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
        chunk.emit_opcode(OpCode::MemoryAssign);
        chunk.write_u16(1);

        // Load the index again
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(2);

        
        // Increment the index
        chunk.emit_opcode(OpCode::Inc);

        // Copy the value
        chunk.emit_opcode(OpCode::Copy);

        // Store the index
        chunk.emit_opcode(OpCode::MemoryAssign);
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
}