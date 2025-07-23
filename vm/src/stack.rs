use xelis_types::{StackValue, ValueCell};

use crate::debug;
use super::VMError;

// 256 elements maximum in the stack:
// Function Call can have up to 255 arguments and 1 on value
const STACK_SIZE: usize = 256;

pub struct Stack {
    checkpoints: Vec<usize>,
    stack: Vec<StackValue>
}

impl Stack {
    pub fn new() -> Self {
        Self {
            checkpoints: Vec::with_capacity(4),
            stack: Vec::with_capacity(16)
        }
    }

    // Mark as a checkpoint until which len we should
    // clean the pointers
    #[inline]
    pub fn mark_checkpoint(&mut self) {
        debug!("Marking checkpoint at stack len: {}", self.stack.len());
        self.checkpoints.push(self.stack.len());
    }

    // Clean all our stack to prevent any undefined behavior with
    // raw pointers
    // This will transform every value as owned
    #[inline]
    pub fn checkpoint_clean(&mut self) -> Result<(), VMError> {
        let checkpoint = self.checkpoints.pop()
            .ok_or(VMError::NoCheckPoint)?;

        if let Some(values) = self.stack.get_mut(checkpoint..) {
            debug!("Cleaning stack until checkpoint: {}", checkpoint);
            for value in values.iter_mut() {
                value.make_owned()?;
            }
        }

        Ok(())
    }

    // Push a value to the stack
    #[inline]
    pub fn push_stack(&mut self, value: StackValue) -> Result<(), VMError> {
        if self.stack.len() >= STACK_SIZE {
            return Err(VMError::StackOverflow);
        }

        self.push_stack_unchecked(value);

        Ok(())
    }

    pub fn verify_pointers(&mut self, ptr: *mut ValueCell) -> Result<(), VMError> {
        for v in self.stack.iter_mut() {
            v.make_owned_if_same_ptr(ptr)?;
        }

        Ok(())
    }

    // Get the value at a specific index
    #[inline]
    pub fn get_stack_at(&self, index: usize) -> Result<&StackValue, VMError> {
        self.stack.get(index).ok_or(VMError::StackIndexOutOfBounds)
    }

    // Push a value to the stack without checking the stack size
    #[inline(always)]
    pub fn push_stack_unchecked(&mut self, value: StackValue) {
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

    // Swap A and B in stack
    #[inline]
    pub fn swap_stack_both(&mut self, a: usize, b: usize) -> Result<(), VMError> {
        let len = self.stack.len();
        if len <= a || len <= b {
            return Err(VMError::StackIndexOutOfBounds);
        }

        self.stack.swap(len - 1 - a, len - 1 - b);
        Ok(())
    }

    // Push multiple values to the stack
    #[inline]
    pub fn extend_stack<I: IntoIterator<Item = StackValue> + ExactSizeIterator>(&mut self, values: I) -> Result<(), VMError> {
        if self.stack.len() + values.len() >= STACK_SIZE {
            return Err(VMError::StackOverflow);
        }

        self.stack.extend(values);
        Ok(())
    }

    // Get the last value from the stack
    #[inline]
    pub fn pop_stack(&mut self) -> Result<StackValue, VMError> {
        if let Some(checkpoint) = self.checkpoints.last_mut() {
            let len = self.stack.len();
            // If the len is now below the latest checkpoint
            // we should decrement the checkpoint
            // to prevent any undefined behavior
            if *checkpoint > 0 && len <= *checkpoint {
                debug!("Decrementing checkpoint from {}", *checkpoint);
                *checkpoint -= 1;
            }
        }

        self.stack.pop().ok_or(VMError::EmptyStack)
    }

    // Pop last N values
    #[inline]
    pub fn pop_stack_n(&mut self, n: u8) -> Result<(), VMError> {
        let len = self.stack.len();
        if len < n as usize {
            return Err(VMError::StackIndexOutOfBounds);
        }

        self.stack.truncate(len - n as usize);
        Ok(())
    }

    // Get the last value from the stack
    #[inline]
    pub fn last_stack(&self) -> Result<&StackValue, VMError> {
        self.stack.last().ok_or(VMError::EmptyStack)
    }

    // Get the last mutable value from the stack
    #[inline]
    pub fn last_mut_stack(&mut self) -> Result<&mut StackValue, VMError> {
        self.stack.last_mut().ok_or(VMError::EmptyStack)
    }

    // Get the inner stack
    #[inline]
    pub fn get_inner_mut(&mut self) -> &mut Vec<StackValue> {
        &mut self.stack
    }

    // Get the inner stack
    #[inline]
    pub fn get_inner(&self) -> &Vec<StackValue> {
        &self.stack
    }

    // Count the number of elements in the stack
    #[inline]
    pub fn count(&self) -> usize {
        self.stack.len()
    }
}