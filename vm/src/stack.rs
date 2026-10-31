use silex_types::{StackValue, ValuePointer};

use crate::Callback;

use super::VMError;

// 256 elements maximum in the stack:
// Function Call can have up to 255 arguments and 1 on value
pub(super) const STACK_SIZE: usize = 256;
const STACK_CALLBACKS_SIZE: usize = 16;

pub struct Stack<M> {
    stack: Vec<StackValue>,
    callbacks: Vec<Callback<M>>,
}

impl<M> Stack<M> {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            callbacks: Vec::new(),
        }
    }

    #[inline(always)]
    pub fn get_callback(&mut self) -> Option<Callback<M>> {
        self.callbacks.pop()
    }

    #[inline(always)]
    pub fn push_callback(&mut self, callback: Callback<M>) -> Result<(), VMError> {
        if self.callbacks.len() >= STACK_CALLBACKS_SIZE {
            return Err(VMError::StackOverflow);
        }

        self.callbacks.push(callback);
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

    /// Push a retained handle without passing a full runtime value by value.
    #[inline(always)]
    pub(crate) fn push_pointer(&mut self, ptr: ValuePointer) -> Result<(), VMError> {
        if self.stack.len() >= STACK_SIZE {
            return Err(VMError::StackOverflow);
        }
        self.stack.push(StackValue::Pointer { ptr });
        Ok(())
    }

    // Get the value at a specific index
    #[inline]
    pub fn get_stack_at(&self, index: usize) -> Result<&StackValue, VMError> {
        self.stack.get(index).ok_or_else(|| VMError::StackIndexOutOfBounds)
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
        self.stack.pop().ok_or_else(|| VMError::EmptyStack)
    }

    /// Consume arguments in source order, including the available operands on
    /// underflow, matching sequential pops.
    #[inline]
    pub fn pop_arguments(&mut self, count: usize) -> Result<Vec<StackValue>, VMError> {
        self.pop_arguments_with(count, |_| Ok(()))
    }

    /// Check operands in pop order, then move their contiguous slice once.
    /// Failure consumes exactly the examined operands and preserves drop order.
    #[inline]
    pub fn pop_arguments_with(
        &mut self,
        count: usize,
        mut check: impl FnMut(&StackValue) -> Result<(), VMError>,
    ) -> Result<Vec<StackValue>, VMError> {
        let mut arguments = Vec::with_capacity(count);
        let available = self.stack.len().min(count);
        let start = self.stack.len() - available;
        for index in (start..self.stack.len()).rev() {
            if let Err(error) = check(&self.stack[index]) {
                arguments.extend(self.stack.drain(index..));
                return Err(error);
            }
        }
        arguments.extend(self.stack.drain(start..));
        if available != count {
            return Err(VMError::EmptyStack);
        }
        Ok(arguments)
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
        self.stack.last().ok_or_else(|| VMError::EmptyStack)
    }

    // Get the last mutable value from the stack
    #[inline]
    pub fn last_mut_stack(&mut self) -> Result<&mut StackValue, VMError> {
        self.stack.last_mut().ok_or_else(|| VMError::EmptyStack)
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

    // Clear the stack
    pub fn clear(&mut self) {
        self.stack.clear();
        self.callbacks.clear();
    }
}