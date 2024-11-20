use xelis_types::Path;

use super::VMError;

// 256 elements maximum in the stack:
// Function Call can have up to 255 arguments and 1 on value
const STACK_SIZE: usize = 256;

pub struct Stack<'a> {
    stack: Vec<Path<'a>>,
}

impl<'a> Stack<'a> {
    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(16),
        }
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

    // Get the value at a specific index
    #[inline]
    pub fn get_stack_at(&self, index: usize) -> Result<&Path<'a>, VMError> {
        self.stack.get(index).ok_or(VMError::StackIndexOutOfBounds)
    }

    // Push a value to the stack without checking the stack size
    #[inline(always)]
    pub fn push_stack_unchecked(&mut self, value: Path<'a>) {
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
    pub fn extend_stack<I: IntoIterator<Item = Path<'a>> + ExactSizeIterator>(&mut self, values: I) -> Result<(), VMError> {
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
    pub fn last_stack(&self) -> Result<&Path<'a>, VMError> {
        self.stack.last().ok_or(VMError::EmptyStack)
    }

    // Get the last mutable value from the stack
    #[inline]
    pub fn last_mut_stack(&mut self) -> Result<&mut Path<'a>, VMError> {
        self.stack.last_mut().ok_or(VMError::EmptyStack)
    }

    // Get the inner stack
    #[inline]
    pub fn get_inner(&mut self) -> &mut Vec<Path<'a>> {
        &mut self.stack
    }

    // Count the number of elements in the stack
    #[inline]
    pub fn count(&self) -> usize {
        self.stack.len()
    }
}