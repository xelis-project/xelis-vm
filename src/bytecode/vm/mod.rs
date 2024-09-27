mod chunk;
mod error;
mod iterator;

use std::{cell::RefCell, collections::VecDeque, rc::Rc};

pub use error::VMError;
pub use chunk::*;
use iterator::PathIterator;

use crate::{Path, types::Struct, Environment, Type, Value};

use super::{opcode::OpCode, Module};


macro_rules! op {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a.as_value(), $b.as_value()) {
            (Value::U8(a), Value::U8(b)) => Value::U8(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::U16(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::U32(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::U64(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::U128(a $op b),
            (a, b) => return Err(VMError::IncompatibleValues(a.clone(), b.clone()))
        }
    }};
}

macro_rules! op_bool {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a.as_value(), $b.as_value()) {
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a $op b),
            (Value::U8(a), Value::U8(b)) => Value::Boolean(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::Boolean(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::Boolean(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::Boolean(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::Boolean(a $op b),
            (a, b) => return Err(VMError::IncompatibleValues(a.clone(), b.clone()))
        }
    }};
}

macro_rules! opcode_op {
    ($self: expr, $macr: tt, $op: tt) => {
        {
            let right = $self.pop_stack()?;
            let left = $self.pop_stack()?;
            // Push the result to the stack, no need to check as we poped 2 values
            $self.push_stack_unchecked(Path::Owned($macr!(left.as_ref(), right.as_ref(), $op)));
        }
    };
}

macro_rules! opcode_op_assign {
    ($self: expr, $macr: tt, $op: tt) => {
        {
            let right = $self.pop_stack()?;
            let mut left = $self.pop_stack()?;
            let result = $macr!(left.as_ref(), right.as_ref(), $op);
            *left.as_mut() = result;
        }
    };
}

// 256 elements maximum in the stack:
// Function Call can have up to 255 arguments and 1 on value
const STACK_SIZE: usize = 256;

// 64 elements maximum in the call stack
const CALL_STACK_SIZE: usize = 64;

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
    // Every chunks to proceed are stored here
    call_stack: Vec<ChunkManager<'a>>,
    // The stack of the VM
    // Every values are stored here
    stack: Vec<Path<'a>>,
}

impl<'a> VM<'a> {
    // Create a new VM
    pub fn new(module: &'a Module, environment: &'a Environment) -> Self {
        VM {
            module,
            environment,
            call_stack: Vec::with_capacity(4),
            stack: Vec::with_capacity(16),
        }
    }

    // Get the stack
    #[inline]
    pub fn get_stack(&self) -> &[Path<'a>] {
        &self.stack
    }

    // Push a value to the stack
    #[inline]
    pub fn push_stack(&mut self, value: Path<'a>) -> Result<(), VMError> {
        if self.stack.len() >= STACK_SIZE {
            return Err(VMError::StackOverflow);
        }

        self.push_stack_unchecked(value);

        Ok(())
    }

    // Push a value to the stack without checking the stack size
    #[inline(always)]
    fn push_stack_unchecked(&mut self, value: Path<'a>) {
        self.stack.push(value);
    }

    // Swap in stack
    #[inline]
    pub fn swap_stack(&mut self, index: usize) -> Result<(), VMError> {
        let len = self.stack.len();
        if len <= index {
            return Err(VMError::StackIndexOutOfBounds);
        }

        self.stack.swap(len - 1, len - 1 - index);
        Ok(())
    }

    // Push multiple values to the stack
    #[inline]
    fn extend_stack<I: IntoIterator<Item = Path<'a>> + ExactSizeIterator>(&mut self, values: I) -> Result<(), VMError> {
        if self.stack.len() + values.len() >= STACK_SIZE {
            return Err(VMError::StackOverflow);
        }

        self.stack.extend(values);
        Ok(())
    }

    // Get the last value from the stack
    #[inline]
    pub fn pop_stack(&mut self) -> Result<Path<'a>, VMError> {
        self.stack.pop().ok_or(VMError::EmptyStack)
    }

    // Get the last value from the stack
    #[inline]
    pub fn last_stack(&self) -> Result<&Path<'a>, VMError> {
        self.stack.last().ok_or(VMError::EmptyStack)
    }

    // Get the last mutable value from the stack
    #[inline]
    pub fn last_mut_stack(&mut self) -> Result<&mut Path<'a>, VMError> {
        self.stack.last_mut().ok_or(VMError::EmptyStack)
    }

    // Get a struct with an id
    // TODO: support env structs
    pub fn get_struct_with_id(&self, id: u16) -> Result<&Struct, VMError> {
        self.module.get_struct_at(id as usize).ok_or(VMError::StructNotFound)
    }

    // Invoke a chunk using its id
    pub fn invoke_chunk_id(&mut self, id: u16) -> Result<(), VMError> {
        if self.call_stack.len() >= CALL_STACK_SIZE {
            return Err(VMError::CallStackOverflow);
        }

        let chunk = self.module.get_chunk_at(id as usize)
            .ok_or(VMError::ChunkNotFound)?;

        let manager = ChunkManager::new(chunk);
        self.call_stack.push(manager);
        Ok(())
    }

    // Invoke a chunk using its id and arguments
    pub fn invoke_chunk_with_args(&mut self, id: u16, args: Vec<Path<'a>>) -> Result<(), VMError> {
        self.extend_stack(args.into_iter())?;
        self.invoke_chunk_id(id)
    }

    // Run the VM
    // It will execute the bytecode
    // First chunk executed should always return a value
    pub fn run(&mut self) -> Result<Value, VMError> {
        'main: while let Some(mut manager) = self.call_stack.pop() {
            while let Ok(op_code) = manager.read_op_code() {
                match op_code {
                    OpCode::Constant => {
                        let index = manager.read_u16()? as usize;
                        let constant = self.module.get_constant_at(index).ok_or(VMError::ConstantNotFound)?;
                        self.push_stack(Path::Borrowed(constant))?;
                    },
                    OpCode::Pop => {
                        self.pop_stack()?;
                    },
                    OpCode::MemoryLoad => {
                        let index = manager.read_u16()?;
                        let value = manager.from_register(index as usize)?
                            .shareable();
                        self.push_stack(value)?;
                    },
                    OpCode::MemorySet => {
                        let index = manager.read_u16()?;
                        let value = self.pop_stack()?;
                        manager.set_register(index as usize, value);
                    },
                    OpCode::Copy => {
                        let value = self.last_stack()?;
                        self.push_stack(value.clone())?;
                    },
                    OpCode::Cast => {
                        let _type = manager.read_type()?;
                        let current = self.pop_stack()?
                            .into_owned();

                        let value = match _type {
                            Type::U8 => Value::U8(current.cast_to_u8()?),
                            Type::U16 => Value::U16(current.cast_to_u16()?),
                            Type::U32 => Value::U32(current.cast_to_u32()?),
                            Type::U64 => Value::U64(current.cast_to_u64()?),
                            Type::U128 => Value::U128(current.cast_to_u128()?),
                            Type::String => Value::String(current.cast_to_string()?),
                            _ => return Err(VMError::UnsupportedCastType)
                        };
                        self.push_stack(Path::Owned(value))?;
                    },
                    OpCode::NewArray => {
                        let length = manager.read_u32()?;
                        let mut array = VecDeque::with_capacity(length as usize);
                        for _ in 0..length {
                            let pop = self.pop_stack()?;
                            array.push_front(Rc::new(RefCell::new(pop.into_owned())));
                        }

                        self.push_stack(Path::Owned(Value::Array(array.into())))?;
                    },
                    OpCode::ArrayCall => {
                        let index = self.pop_stack()?.into_owned().cast_to_u32()?;
                        let value = self.pop_stack()?;
                        let sub = value.get_sub_variable(index as usize)?;
                        self.push_stack_unchecked(sub);
                    },
                    OpCode::InvokeChunk => {
                        let id = manager.read_u16()?;
                        let on_value = manager.read_bool()?;
                        let mut args = manager.read_u8()? as usize;
                        if on_value {
                            args += 1;
                        }

                        // We need to reverse the order of the arguments
                        let len = self.stack.len();
                        if len < args {
                            return Err(VMError::NotEnoughArguments);
                        }

                        self.stack[len - args..].reverse();

                        // Add back our current state to the stack
                        self.call_stack.push(manager);

                        // Invoke the new chunk to the top of the stack
                        self.invoke_chunk_id(id)?;
    
                        continue 'main;
                    },
                    OpCode::SysCall => {
                        let id = manager.read_u16()?;
                        let on_value = manager.read_bool()?;
                        let args = manager.read_u8()?;

                        let mut arguments = VecDeque::with_capacity(args as usize);
                        for _ in 0..args {
                            arguments.push_front(self.pop_stack()?);
                        }

                        let mut on_value = if on_value {
                            Some(self.pop_stack()?)
                        } else {
                            None
                        };

                        let func = self.environment.get_functions().get(id as usize)
                            .ok_or(VMError::UnknownSysCall)?;

                        let mut instance = match on_value.as_mut() {
                            Some(v) => Some(v.as_mut()),
                            None => None,
                        };

                        if let Some(v) = func.call_function(instance.as_deref_mut(), arguments.into())? {
                            self.push_stack(Path::Owned(v))?;
                        }
                    },
                    OpCode::NewStruct => {
                        let id = manager.read_u16()?;
                        let structure = self.get_struct_with_id(id)?;
                        let mut fields = VecDeque::new();
                        for _ in 0..structure.fields.len() {
                            fields.push_front(Rc::new(RefCell::new(self.pop_stack()?.into_owned())));
                        }
    
                        self.push_stack(Path::Owned(Value::Struct(id, fields.into())))?;
                    },
                    OpCode::Add => opcode_op!(self, op, +),
                    OpCode::Sub => opcode_op!(self, op, -),
                    OpCode::Mul => opcode_op!(self, op, *),
                    OpCode::Div => opcode_op!(self, op, /),
                    OpCode::Xor => opcode_op!(self, op, ^),
                    OpCode::Mod => opcode_op!(self, op, %),
                    OpCode::And => opcode_op!(self, op, &),
                    OpCode::Or => opcode_op!(self, op, |),
                    OpCode::Shl => opcode_op!(self, op, <<),
                    OpCode::Shr => opcode_op!(self, op, >>),
                    OpCode::Eq => opcode_op!(self, op_bool, ==),
                    OpCode::Gt => opcode_op!(self, op_bool, >),
                    OpCode::Lt => opcode_op!(self, op_bool, <),
                    OpCode::Gte => opcode_op!(self, op_bool, >=),
                    OpCode::Lte => opcode_op!(self, op_bool, <=),
                    OpCode::Neg => {
                        let value = self.pop_stack()?;
                        self.push_stack_unchecked(Path::Owned(Value::Boolean(!value.as_bool()?)));
                    },
                    OpCode::Assign => {
                        let right = self.pop_stack()?.into_owned();
                        let mut left = self.pop_stack()?;
                        *left.as_mut() = right;
                    },
                    OpCode::AssignAdd => opcode_op_assign!(self, op, +),
                    OpCode::AssignSub => opcode_op_assign!(self, op, -),
                    OpCode::AssignMul => opcode_op_assign!(self, op, *),
                    OpCode::AssignDiv => opcode_op_assign!(self, op, /),
                    OpCode::AssignXor => opcode_op_assign!(self, op, ^),
                    OpCode::AssignMod => opcode_op_assign!(self, op, %),
                    OpCode::AssignAnd => opcode_op_assign!(self, op, &),
                    OpCode::AssignOr => opcode_op_assign!(self, op, |),
                    OpCode::AssignShl => opcode_op_assign!(self, op, <<),
                    OpCode::AssignShr => opcode_op_assign!(self, op, >>),

                    OpCode::SubLoad => {
                        let index = manager.read_u16()?;
                        let path = self.pop_stack()?;
                        let sub = path.get_sub_variable(index as usize)?;
                        self.push_stack_unchecked(sub);
                    },
                    OpCode::Jump => {
                        let index = manager.read_u32()?;
                        manager.set_index(index as usize);
                    },
                    OpCode::JumpIfFalse => {
                        let index = manager.read_u32()?;
                        let value = self.pop_stack()?;
                        if !value.as_bool()? {
                            manager.set_index(index as usize);
                        }
                    },
                    OpCode::IterableLength => {
                        let value = self.pop_stack()?;
                        let len = value.as_ref().as_vec()?.len();
                        self.push_stack_unchecked(Path::Owned(Value::U32(len as u32)));
                    },
                    OpCode::IteratorBegin => {
                        let value = self.pop_stack()?;
                        let iterator = PathIterator::new(value);
                        manager.add_iterator(iterator);
                    },
                    OpCode::IteratorNext => {
                        let addr = manager.read_u32()?;
                        if let Some(value) = manager.next_iterator()? {
                            self.push_stack(value)?;
                        } else {
                            manager.set_index(addr as usize);
                        }
                    },
                    OpCode::IteratorEnd => {
                        manager.pop_iterator()?;
                    },
                    OpCode::Swap => {
                        let index = manager.read_u8()?;
                        self.swap_stack(index as usize)?;
                    }
                    OpCode::Inc => {
                        let v = self.last_mut_stack()?;
                        v.as_mut().increment()?;
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
        }

        Ok(self.pop_stack()?.into_owned())
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
        ].into_iter().map(|v| Rc::new(RefCell::new(v))).collect();

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
            Rc::new(RefCell::new(Value::U8(10))),
            Rc::new(RefCell::new(Value::U16(20)))
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
            Rc::new(RefCell::new(Value::U64(10)))
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
            Rc::new(RefCell::new(Value::U8(10))),
            Rc::new(RefCell::new(Value::U8(20))),
            Rc::new(RefCell::new(Value::U8(30))),
            Rc::new(RefCell::new(Value::U8(40))),
            Rc::new(RefCell::new(Value::U8(50))),
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