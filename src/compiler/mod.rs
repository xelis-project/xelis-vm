mod chunk;
mod opcode;

use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use chunk::{Chunk, ChunkManager};
use opcode::OpCode;

use crate::{types::Struct, Type, Value};

macro_rules! op {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (Value::U8(a), Value::U8(b)) => Value::U8(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::U16(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::U32(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::U64(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::U128(a $op b),
            _ => return None
        }
    }};
}

macro_rules! op_bool {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a $op b),
            (Value::U8(a), Value::U8(b)) => Value::Boolean(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::Boolean(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::Boolean(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::Boolean(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::Boolean(a $op b),
            _ => panic!("aaaa")
        }
    }};
}

pub enum CompilerError {
    NotImplemented
}

// A module is a collection of chunks
// It represents a program compiled in bytecode
pub struct Module {
    // TODO: have a central IndexSet of all the constants

    // Available chunks
    chunks: Vec<Chunk>,
    // registered structs
    structs: Vec<Struct>
}

impl Module {
    pub fn new() -> Self {
        Module {
            chunks: Vec::new(),
            structs: Vec::new()
        }
    }

    pub fn add_chunk(&mut self, chunk: Chunk) {
        self.chunks.push(chunk);
    }
}

// Its main purpose is to compile the program into a bytecode version
// that can be executed by the VM
pub struct Compiler;

// Simple example of how the bytecode VM would works with ArrayCall, FunctionCall and struct values
pub fn run(module: &Module, start: usize) -> Option<Value> {
    let mut chunks = Vec::new();
    chunks.push(ChunkManager::new(module.chunks.get(start).unwrap()));

    let mut final_result = None;
    'main: while let Some(mut manager) = chunks.pop() {
        if let Some(value) = final_result.take() {
            manager.push_stack(value);
        }

        while let Some(op_code) = manager.read_op_code() {
            match op_code {
                OpCode::Constant => {
                    let index = manager.read_u8().unwrap() as usize;
                    let constant = manager.get_constant(index).unwrap();
                    manager.push_stack(constant.clone());
                },
                OpCode::MemoryLoad => {
                    let index = manager.read_u16();
                    let value = manager.from_register(index as usize).unwrap().clone();
                    manager.push_stack(value);
                },
                OpCode::MemorySet => {
                    let value = manager.pop_stack().unwrap();
                    manager.push_register(value);
                },
                OpCode::MemoryAssign => {
                    let index = manager.read_u16();
                    let value = manager.pop_stack().unwrap();
                    manager.to_register(index as usize, value);
                },
                OpCode::Copy => {
                    let value = manager.last_stack().unwrap();
                    manager.push_stack(value.clone());
                },
                OpCode::Cast => {
                    let _type = manager.read_type().unwrap();
                    let current = manager.pop_stack().unwrap();
                    let value = match _type {
                        Type::U8 => Value::U8(current.cast_to_u8().ok().unwrap()),
                        Type::U16 => Value::U16(current.cast_to_u16().ok().unwrap()),
                        Type::U32 => Value::U32(current.cast_to_u32().ok().unwrap()),
                        Type::U64 => Value::U64(current.cast_to_u64().ok().unwrap()),
                        Type::U128 => Value::U128(current.cast_to_u128().ok().unwrap()),
                        Type::String => Value::String(current.cast_to_string().ok().unwrap()),
                        _ => panic!("cast unnkown type")
                    };
                    manager.push_stack(value);
                },
                OpCode::NewArray => {
                    let length = manager.read_u16();
                    let mut array = VecDeque::with_capacity(length as usize);
                    for _ in 0..length {
                        array.push_front(Rc::new(RefCell::new(manager.pop_stack().unwrap())));
                    }
    
                    manager.push_stack(Value::Array(array.into()));
                },
                OpCode::ArrayCall => {
                    let index = manager.pop_stack().unwrap().cast_to_u32().ok().unwrap();
                    let value = manager.pop_stack().unwrap();
                    let array = value.as_sub_vec().ok().unwrap();
                    manager.push_stack(array[index as usize].borrow().clone());
                },
                OpCode::InvokeChunk => {
                    let args = manager.read_u8().unwrap();
                    let on_value = manager.read_bool().unwrap();
                    let id = manager.read_u16();

                    let mut arguments = VecDeque::with_capacity(args as usize);
                    for _ in 0..args {
                        arguments.push_front(manager.pop_stack().unwrap());
                    }

                    // TODO
                    let _on_value = if on_value {
                        Some(manager.pop_stack().unwrap())
                    } else {
                        None
                    };

                    // Add our current state to the stack
                    chunks.push(manager);

                    // Find the chunk
                    let new_manager = ChunkManager::new(module.chunks.get(id as usize).unwrap());
                    chunks.push(new_manager);

                    continue 'main;
                },
                OpCode::NewStruct => {
                    let id = manager.read_u16();
                    let structure = module.structs.get(id as usize).unwrap();
                    let mut fields = VecDeque::new();
                    for _ in 0..structure.fields.len() {
                        fields.push_front(Rc::new(RefCell::new(manager.pop_stack().unwrap())));
                    }

                    manager.push_stack(Value::Struct(id, fields.into()));
                },
                OpCode::Add => {
                    let right = manager.pop_stack().unwrap();
                    let left = manager.pop_stack().unwrap();
                    manager.push_stack(op!(left, right, +));
                },
                OpCode::SubLoad => {
                    let index = manager.read_u16();
                    let value = manager.pop_stack().unwrap();
                    let sub_value = value.as_sub_vec().ok().unwrap();
                    manager.push_stack(sub_value[index as usize].borrow().clone());
                },
                OpCode::Lte => {
                    let right = manager.pop_stack().unwrap();
                    let left = manager.pop_stack().unwrap();
                    manager.push_stack(op_bool!(left, right, <=));
                },
                OpCode::Gt => {
                    let right = manager.pop_stack().unwrap();
                    let left = manager.pop_stack().unwrap();
                    manager.push_stack(op_bool!(left, right, >));
                },
                OpCode::Gte => {
                    let right = manager.pop_stack().unwrap();
                    let left = manager.pop_stack().unwrap();
                    manager.push_stack(op_bool!(left, right, >=));
                },
                OpCode::Jump => {
                    let index = manager.read_u32();
                    manager.set_index(index as usize);
                },
                OpCode::JumpIfFalse => {
                    let index = manager.read_u32();
                    let value = manager.pop_stack().unwrap();
                    if !value.as_bool().ok().unwrap() {
                        manager.set_index(index as usize);
                    }
                },
                OpCode::IterableLength => {
                    let value = manager.pop_stack().unwrap();
                    let len = value.as_vec().ok().unwrap().len();
                    manager.push_stack(Value::U32(len as u32));
                },
                OpCode::Inc => {
                    manager.last_mut_stack().unwrap().increment().unwrap();
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
        final_result = manager.pop_stack();
    }

    final_result
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(chunk: Chunk) -> Value {
        let mut module = Module::new();
        module.add_chunk(chunk);

        super::run(&module, 0).unwrap()
    }

    #[test]
    fn test_casting() {
        let mut chunk = Chunk::new();
        let index = chunk.add_constant(Value::U8(10));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        chunk.emit_opcode(OpCode::Cast);
        chunk.write_u8(Type::String.primitive_byte().unwrap());

        assert_eq!(run(chunk), Value::String("10".to_string()));
    }

    #[test]
    fn test_array_call() {
        let mut chunk = Chunk::new();

        // Push element 1
        let index = chunk.add_constant(Value::U8(10));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        // Push element 2
        let index = chunk.add_constant(Value::U8(20));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        // Create a new array with 2 elements
        chunk.emit_opcode(OpCode::NewArray);
        chunk.write_u16(2);

        // Load the first element
        let index = chunk.add_constant(Value::U16(0));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        chunk.emit_opcode(OpCode::ArrayCall);

        assert_eq!(run(chunk), Value::U8(10));
    }

    #[test]
    fn test_multi_depth_array_call() {
        let mut chunk = Chunk::new();

        let values = vec![
            Value::U8(10),
            Value::U8(20),
            Value::U8(30),
        ].into_iter().map(|v| Rc::new(RefCell::new(v))).collect();

        // Push element 1
        let index = chunk.add_constant(Value::Array(values));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        // Create a new array with 2 elements
        chunk.emit_opcode(OpCode::NewArray);
        chunk.write_u16(1);

        // Load the first element of the first array
        let index = chunk.add_constant(Value::U16(0));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        chunk.emit_opcode(OpCode::ArrayCall);

        // Load the last element
        let index = chunk.add_constant(Value::U16(2));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        chunk.emit_opcode(OpCode::ArrayCall);

        assert_eq!(run(chunk), Value::U8(30));
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
        module.structs.push(new_struct);

        let mut chunk = Chunk::new();
        // Push the first field
        let index = chunk.add_constant(Value::U8(10));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        // Push the second field
        let index = chunk.add_constant(Value::U16(20));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        chunk.emit_opcode(OpCode::NewStruct);
        // struct id
        chunk.write_u16(0);

        chunk.emit_opcode(OpCode::Return);

        module.add_chunk(chunk);

        assert_eq!(super::run(&module, 0).unwrap(), Value::Struct(0, vec![
            Rc::new(RefCell::new(Value::U8(10))),
            Rc::new(RefCell::new(Value::U16(20)))
        ].into()));

        let chunk = module.chunks.get_mut(0).unwrap();
        chunk.pop_instruction();

        // Store the struct in the memory
        chunk.emit_opcode(OpCode::MemorySet);

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

        assert_eq!(super::run(&module, 0).unwrap(), Value::U16(30));
    }

    #[test]
    fn test_function_call() {
        let mut module = Module::new();

        // First function should return "true"
        let mut bool_fn = Chunk::new();
        let index = bool_fn.add_constant(Value::Boolean(true));
        bool_fn.emit_opcode(OpCode::Constant);
        bool_fn.write_u8(index as u8);

        bool_fn.emit_opcode(OpCode::Return);

        // This chunk should call the bool fn
        let mut first = Chunk::new();
        first.emit_opcode(OpCode::InvokeChunk);
        // 0 args
        first.write_u8(0);
        // not on a value
        first.write_bool(false);

        // Call the bool_fn which is at index 1 in chunks list
        first.write_u16(1);
        // return its value
        first.emit_opcode(OpCode::Return);

        module.add_chunk(first);
        module.add_chunk(bool_fn);

        assert_eq!(super::run(&module, 0).unwrap(), Value::Boolean(true));
    }

    #[test]
    fn test_memory() {
        let mut chunk = Chunk::new();

        // Push element 1
        let index = chunk.add_constant(Value::U8(10));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        // Store the value in the memory
        chunk.emit_opcode(OpCode::MemorySet);

        // Load the value from the memory
        chunk.emit_opcode(OpCode::MemoryLoad);
        chunk.write_u16(0);

        assert_eq!(run(chunk), Value::U8(10));
    }

    #[test]
    fn test_for_each() {
        let mut chunk = Chunk::new();

        // Push element 1
        let index = chunk.add_constant(Value::U8(10));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        // Push element 2
        let index = chunk.add_constant(Value::U8(20));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        // Create a new array with 2 elements
        chunk.emit_opcode(OpCode::NewArray);
        chunk.write_u16(2);

        // Store the array in the memory
        chunk.emit_opcode(OpCode::MemorySet);

        let sum_index = chunk.add_constant(Value::U8(0));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(sum_index as u8);

        // Store sum in memory
        chunk.emit_opcode(OpCode::MemorySet);

        let index = chunk.add_constant(Value::U32(0));
        chunk.emit_opcode(OpCode::Constant);
        chunk.write_u8(index as u8);

        // Store in memory
        chunk.emit_opcode(OpCode::MemorySet);

        // Load the array
        let jump_at = chunk.index();
        chunk.emit_opcode(OpCode::MemoryLoad);
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

        assert_eq!(run(chunk), Value::U8(30));
    }
}