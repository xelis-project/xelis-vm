mod reader;

use std::{cmp::Ordering, mem};
use xelis_bytecode::Chunk;
use xelis_types::StackValue;

use super::{Stack, iterator::ValueIterator, VMError};

pub use reader::ChunkReader;

// u16::MAX registers maximum
const REGISTERS_SIZE: usize = u16::MAX as usize;
const ITERATORS_SIZE: usize = 8;

#[derive(Clone, Copy, Debug)]
pub enum ChunkContext {
    // Nothing to do
    None,
    // Must be kept in the callstack
    ShouldKeep,
    // Pending in the callstack, unused
    Pending,
    // has been used, can be removed from
    // the callstack
    Used,
}

// Manager for a chunk
// It contains the reader and the stacks
#[derive(Debug)]
pub struct ChunkManager {
    // Chunk id from which we took the registers
    registers_origin: Option<usize>,
    // Registers are temporary and "scoped" per chunk
    registers: Vec<StackValue>,
    // Iterators stack
    iterators: Vec<ValueIterator>,
    // Chunk id to invoke
    chunk_id: usize,
    // current index in the chunk
    ip: usize,
    // should we keep it despite the end of it?
    context: ChunkContext,
}

impl ChunkManager {
    // Create a new chunk manager
    // It will create a reader from the chunk
    // and initialize the stack and registers
    #[inline(always)]
    pub fn new(chunk_id: usize) -> Self {
        Self::with(chunk_id, None, Vec::new())
    }

    // Create a new chunk manager with a registers origin and a registers list
    #[inline(always)]
    pub fn with(chunk_id: usize, registers_origin: Option<usize>, registers: Vec<StackValue>) -> Self {
        Self {
            registers_origin,
            chunk_id,
            registers,
            iterators: Vec::new(),
            ip: 0,
            context: ChunkContext::None,
        }
    }

    // For which chunk id is it currently configured
    #[inline(always)]
    pub fn chunk_id(&self) -> usize {
        self.chunk_id
    }

    // Retrieve the registers origin chunk id
    #[inline(always)]
    pub fn registers_origin(&self) -> Option<usize> {
        self.registers_origin
    }

    #[inline(always)]
    pub fn set_context(&mut self, value: ChunkContext) {
        self.context = value;
    }

    #[inline(always)]
    pub fn context(&self) -> ChunkContext {
        self.context
    }

    #[inline(always)]
    pub fn set_ip(&mut self, ip: usize) {
        self.ip = ip;
    }

    #[inline(always)]
    pub fn ip(&self) -> usize {
        self.ip
    }

    // Get the registers
    #[inline(always)]
    pub fn get_registers(&self) -> &Vec<StackValue> {
        &self.registers
    }

    // Add an iterator to the stack
    #[inline(always)]
    pub fn add_iterator(&mut self, iterator: ValueIterator) -> Result<(), VMError> {
        if self.iterators.len() >= ITERATORS_SIZE {
            return Err(VMError::IteratorOverflow)
        }

        self.iterators.push(iterator);

        Ok(())
    }

    // Pop an iterator from the stack
    #[inline(always)]
    pub fn pop_iterator(&mut self) -> Result<ValueIterator, VMError> {
        self.iterators.pop().ok_or(VMError::EmptyIterator)
    }

    // Get the next value from the iterators stack
    #[inline(always)]
    pub fn next_iterator(&mut self) -> Result<Option<StackValue>, VMError> {
        self.iterators.last_mut()
            .ok_or(VMError::EmptyIterator)
            .and_then(|v| v.next().map_err(VMError::from))
    }

    // Push/set a new value into the registers
    #[inline]
    pub fn set_register(&mut self, index: usize, mut value: StackValue, stack: &mut Stack) -> Result<(), VMError> {
        if index >= REGISTERS_SIZE {
            return Err(VMError::RegisterMaxSize);
        }

        let cmp = self.registers.len().cmp(&index);
        match cmp {
            Ordering::Equal => {
                self.registers.push(value);
                Ok(())
            },
            Ordering::Greater => {
                let old_ptr = self.registers[index].ptr();

                // Check if we try to replace an element with the same (or sub) pointer
                if let StackValue::Pointer { origin, ptr, .. } = &value {
                    // If we try to store the same, skip it
                    if old_ptr == *ptr {
                        return Ok(())
                    }

                    // If we try to store a sub pointer, check if the origin is the same
                    if *origin == Some(old_ptr) {
                        // We're overwriting from our origin ptr
                        // We need to make the new value owned
                        value.make_owned()?;
                    }
                }

                for register in self.registers.iter_mut() {
                    register.make_owned_if_same_ptr(old_ptr)?;
                }

                stack.verify_pointers(old_ptr)?;

                self.registers[index] = value;

                Ok(())
            },
            Ordering::Less => Err(VMError::RegisterOverflow)
        }
    }

    // Get a value from the registers
    #[inline(always)]
    pub fn from_register(&mut self, index: usize) -> Result<&mut StackValue, VMError> {
        self.registers.get_mut(index).ok_or(VMError::RegisterNotFound)
    }

    // Pop a value from the registers
    #[inline(always)]
    pub fn pop_register(&mut self) -> Result<StackValue, VMError> {
        self.registers.pop().ok_or(VMError::EmptyRegister)
    }

    // Make owned the stack value at the registers index
    #[inline(always)]
    pub fn registers_len(&mut self) -> usize {
        self.registers.len()
    }

    #[inline(always)]
    pub fn set_registers_origin(&mut self, origin: Option<usize>) {
        self.registers_origin = origin;
    }

    // Swap the register with another ChunkManager to allow
    // access from one to another
    #[inline]
    pub fn swap_registers(&mut self, other: &mut Self) {
        mem::swap(&mut self.registers, &mut other.registers);
    }
}