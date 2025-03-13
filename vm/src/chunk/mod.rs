mod reader;

use std::{cmp::Ordering, mem, ops::{Deref, DerefMut}};
use xelis_bytecode::Chunk;
use xelis_types::{StackValue, ValueCell};
use super::{iterator::ValueIterator, VMError};

pub use reader::ChunkReader;

// u16::MAX registers maximum
const REGISTERS_SIZE: usize = u16::MAX as usize;

// Manager for a chunk
// It contains the reader and the stacks
pub struct ChunkManager<'a> {
    reader: ChunkReader<'a>,
    // Registers are temporary and "scoped" per chunk
    registers: Vec<StackValue>,
    // Iterators stack
    iterators: Vec<ValueIterator>,
}

impl<'a> ChunkManager<'a> {
    // Create a new chunk manager
    // It will create a reader from the chunk
    // and initialize the stack and registers
    #[inline]
    pub fn new(chunk: &'a Chunk) -> Self {
        ChunkManager {
            reader: ChunkReader::new(chunk),
            registers: Vec::new(),
            iterators: Vec::new(),
        }
    }

    // Get the registers
    #[inline]
    pub fn get_registers(&self) -> &Vec<StackValue> {
        &self.registers
    }

    // Add an iterator to the stack
    pub fn add_iterator(&mut self, iterator: ValueIterator) {
        self.iterators.push(iterator);
    }

    // Pop an iterator from the stack
    pub fn pop_iterator(&mut self) -> Result<ValueIterator, VMError> {
        self.iterators.pop().ok_or(VMError::EmptyIterator)
    }

    // Get the next value from the iterators stack
    pub fn next_iterator(&mut self) -> Result<Option<StackValue>, VMError> {
        Ok(self.iterators.last_mut()
            .ok_or(VMError::EmptyIterator)?
            .next()?)
    }

    // Push/set a new value into the registers
    #[inline]
    pub fn set_register(&mut self, index: usize, mut value: StackValue) -> Result<Option<(*mut ValueCell, StackValue)>, VMError> {
        if index >= REGISTERS_SIZE {
            return Err(VMError::RegisterMaxSize);
        }

        let cmp = self.registers.len().cmp(&index);
        match cmp {
            Ordering::Equal => {
                self.registers.push(value);
                Ok(None)
            },
            Ordering::Greater => {
                let old_ptr = self.registers[index].ptr();

                // Check if we try to replace our Owned variant by a Pointer
                if value.ptr() == old_ptr {
                    return Ok(None)
                }

                let old_value = mem::replace(&mut self.registers[index], value);
                for register in self.registers.iter_mut() {
                    register.make_owned_if_same_ptr(old_ptr)?;
                }

                Ok(Some((old_ptr, old_value)))
            },
            Ordering::Less => Err(VMError::RegisterOverflow)
        }
    }

    // Get a value from the registers
    #[inline]
    pub fn from_register(&mut self, index: usize) -> Result<&mut StackValue, VMError> {
        self.registers.get_mut(index).ok_or(VMError::RegisterNotFound)
    }

    // Pop a value from the registers
    #[inline]
    pub fn pop_register(&mut self) -> Result<StackValue, VMError> {
        self.registers.pop().ok_or(VMError::EmptyRegister)
    }

    // Make owned the stack value at the registers index
    #[inline]
    pub fn to_owned_register(&mut self, index: usize) -> Result<(), VMError> {
        let value = self.from_register(index)?;
        value.make_owned()?;

        Ok(())
    }

    // Make owned the stack value at the registers index
    #[inline]
    pub fn registers_len(&mut self) -> usize {
        self.registers.len()
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
