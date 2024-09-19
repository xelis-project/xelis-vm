mod chunk;
mod opcode;

use std::{cell::RefCell, collections::VecDeque, rc::Rc};

use chunk::{Chunk, ChunkReader};
use opcode::OpCode;

use crate::{Type, Value};

pub enum CompilerError {
    NotImplemented
}

// Its main purpose is to compile the program into a bytecode version
// that can be executed by the VM
pub struct Compiler;

pub fn run(chunk: Chunk) -> Value {
    let mut stack: Vec<Value> = Vec::new();
    let mut registers: Vec<Value> = Vec::new();

    let mut reader = ChunkReader::new(&chunk);

    while let Some(op_code) = reader.next_op_code() {
        match op_code {
            OpCode::Constant => {
                let constant = chunk.get_constant(reader.read_u8().unwrap() as usize).unwrap();
                stack.push(constant.clone());
            },
            OpCode::Load => {
            },
            OpCode::Register => {
                let value = stack.pop().unwrap();
                registers.push(value);
            },
            OpCode::Cast => {
                let _type = reader.read_type().unwrap();
                let current = stack.pop().unwrap();
                let value = match _type {
                    Type::U8 => Value::U8(current.cast_to_u8().unwrap()),
                    Type::U16 => Value::U16(current.cast_to_u16().unwrap()),
                    Type::U32 => Value::U32(current.cast_to_u32().unwrap()),
                    Type::U64 => Value::U64(current.cast_to_u64().unwrap()),
                    Type::U128 => Value::U128(current.cast_to_u128().unwrap()),
                    Type::String => Value::String(current.cast_to_string().unwrap()),
                    _ => panic!("cast unnkown type")
                };
                stack.push(value);
            },
            OpCode::NewArray => {
                let length = reader.read_u16();
                let mut array = VecDeque::with_capacity(length as usize);
                for _ in 0..length {
                    array.push_front(Rc::new(RefCell::new(stack.pop().unwrap())));
                }

                stack.push(Value::Array(array.into()));
            },
            OpCode::ArrayCall => {
                let index = stack.pop().unwrap().cast_to_u32().unwrap();
                let value = stack.pop().unwrap();
                let array = value.as_sub_vec().unwrap();
                stack.push(array[index as usize].borrow().clone());
            },
            _ => {
                println!("Not implemented");
                break;
            }
        }
    }

    stack.pop().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

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
}