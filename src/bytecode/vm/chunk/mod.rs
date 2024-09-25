mod reader;

use std::{ops::{Deref, DerefMut}, vec::Drain};

pub use reader::ChunkReader;

use crate::{bytecode::Chunk, Path};

use super::VMError;

// Manager for a chunk
// It contains the reader and the stack
#[derive(Debug)]
pub struct ChunkManager<'a> {
    reader: ChunkReader<'a>,
    // Stack of values
    stack: Vec<Path<'a>>,
    // Registers
    registers: Vec<Path<'a>>,
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
    pub fn get_stack(&self) -> &Vec<Path<'a>> {
        &self.stack
    }

    // Get the registers
    #[inline]
    pub fn get_registers(&self) -> &Vec<Path<'a>> {
        &self.registers
    }

    // Push a value to the stack
    #[inline]
    pub fn push_stack(&mut self, value: Path<'a>) {
        self.stack.push(value);
    }

    // Take multiple values from the stack
    pub fn take_from_stack<'b>(&'b mut self, count: usize) -> Drain<'b, Path<'a>> {
        self.stack.drain(self.stack.len() - count..)
    }

    // Push multiple values to the stack
    #[inline]
    pub fn extend_stack<I: IntoIterator<Item = Path<'a>>>(&mut self, values: I) {
        self.stack.extend(values);
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

    // Push/set a new value into the registers
    #[inline]
    pub fn set_register(&mut self, index: usize, value: Path<'a>) {
        if self.registers.len() <= index {
            self.registers.push(value);
        } else {
            self.registers[index] = value;
        }
    }

    // Get a value from the registers
    #[inline]
    pub fn from_register(&mut self, index: usize) -> Result<&mut Path<'a>, VMError> {
        self.registers.get_mut(index).ok_or(VMError::RegisterNotFound)
    }

    // Pop a value from the registers
    #[inline]
    pub fn pop_register(&mut self) -> Result<Path<'a>, VMError> {
        self.registers.pop().ok_or(VMError::EmptyRegister)
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
