use std::ops::{Deref, DerefMut};

use crate::Value;

use super::{Chunk, ChunkReader};

// Manager for a chunk
// It contains the reader and the stack
#[derive(Debug)]
pub struct ChunkManager<'a> {
    reader: ChunkReader<'a>,
    // Stack of values
    stack: Vec<Value>,
    // Registers
    registers: Vec<Value>,
}

impl<'a> ChunkManager<'a> {
    // Create a new chunk manager
    // It will create a reader from the chunk
    // and initialize the stack and registers
    #[inline]
    pub fn new(chunk: &'a Chunk) -> Self {
        ChunkManager {
            reader: ChunkReader::new(chunk),
            stack: Vec::new(),
            registers: Vec::new(),
        }
    }

    // Get the stack
    #[inline]
    pub fn get_stack(&self) -> &Vec<Value> {
        &self.stack
    }

    // Get the registers
    #[inline]
    pub fn get_registers(&self) -> &Vec<Value> {
        &self.registers
    }

    // Push a value to the stack
    #[inline]
    pub fn push_stack(&mut self, value: Value) {
        self.stack.push(value);
    }

    // Get the last value from the stack
    #[inline]
    pub fn pop_stack(&mut self) -> Option<Value> {
        self.stack.pop()
    }

    // Get the last value from the stack
    #[inline]
    pub fn last_stack(&self) -> Option<&Value> {
        self.stack.last()
    }

    // Get the last mutable value from the stack
    #[inline]
    pub fn last_mut_stack(&mut self) -> Option<&mut Value> {
        self.stack.last_mut()
    }

    // Push a new value into the registers
    pub fn push_register(&mut self, value: Value) {
        self.registers.push(value);
    }

    // Get a value from the registers
    #[inline]
    pub fn from_register(&mut self, index: usize) -> Option<&Value> {
        self.registers.get(index)
    }

    // Set a value in the registers
    #[inline]
    pub fn to_register(&mut self, index: usize, value: Value) {
        if index < self.registers.len() {
            self.registers[index] = value;
        }
    }

    // Pop a value from the registers
    #[inline]
    pub fn pop_register(&mut self) -> Option<Value> {
        self.registers.pop()
    }
}

impl<'a> Deref for ChunkManager<'a> {
    type Target = ChunkReader<'a>;

    fn deref(&self) -> &Self::Target {
        &self.reader
    }
}

impl<'a> DerefMut for ChunkManager<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.reader
    }
}
