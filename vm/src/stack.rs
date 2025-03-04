use xelis_types::{StackValue, ValueCell};

use super::VMError;

// 256 elements maximum in the stack:
// Function Call can have up to 255 arguments and 1 on value
const STACK_SIZE: usize = 256;

pub struct Stack {
    stack: Vec<StackValue>,
    // Only elements with sub types 
    dropped_elements: Vec<ValueCell>,
    // Each element is the stack index until which
    // we will need to clean up the pointers
    pub checkpoints: Vec<usize>,
}

impl Stack {
    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(16),
            dropped_elements: Vec::new(),
            checkpoints: Vec::new(),
        }
    }

    // Keep the elements dropped by adding them to the garbage collector
    // This is faster than checking all the possible pointers
    // We should pay anyway based on the memory usage and limit it
    pub fn add_to_garbage_collector(&mut self, element: StackValue) {
        if let StackValue::Owned(v) = element {
            if v.has_sub_values() {
                self.dropped_elements.push(v);
            }
        }
    }

    pub fn checkpoint_commit(&mut self) {
        self.checkpoints.push(self.stack.len());
    }

    pub fn checkpoint_clean(&mut self) -> Result<(), VMError> {
        let index = self.checkpoints.pop()
            .ok_or(VMError::ExpectedCheckPoint)?;
        if let Some(values) = self.stack.get_mut(index..) {
            for value in values {
                value.take_ownership()?;
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
        let checkpoint = self.checkpoints.last()
            .copied()
            .unwrap_or(0);

        let Some(values) = self.stack.get_mut(checkpoint..) else {
            return Ok(())
        };

        for v in values {
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
    pub fn get_inner(&mut self) -> &mut Vec<StackValue> {
        &mut self.stack
    }

    // Count the number of elements in the stack
    #[inline]
    pub fn count(&self) -> usize {
        self.stack.len()
    }
}