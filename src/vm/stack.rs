use crate::IdentifierType;
use super::{VMError, Path};

#[derive(Debug)]
pub struct Stack<'a> {
    // Each scope is a HashMap storing variables
    // This is done to easily push/pop scopes
    stack: Vec<Option<Path<'a>>>,
    // Flag to break a loop
    loop_break: bool,
    // Flag to continue in loop
    loop_continue: bool,
}

impl<'a> Stack<'a> {
    // Create a new stack with a specific capacity to avoid reallocations
    pub fn new(capacity: u16) -> Self {
        Self {
            stack: vec![None; capacity as usize],
            loop_break: false,
            loop_continue: false,
        }
    }

    // Remove a variable from the stack
    #[inline(always)]
    pub fn remove_variable(&mut self, name: &IdentifierType) -> Result<Path<'a>, VMError> {
        self.stack.get_mut(*name as usize)
            .ok_or_else(|| VMError::StackError)
            .and_then(|value| value.take().ok_or_else(|| VMError::VariableNotFound(name.clone())))
    }

    // Get a variable from the stack
    #[inline(always)]
    #[cfg(test)]
    pub fn get_variable<'b>(&'b self, name: &'b IdentifierType) -> Result<&'b Path<'a>, VMError> {
        self.stack.get(*name as usize)
            .ok_or_else(|| VMError::StackError)
            .and_then(|value| value.as_ref().ok_or_else(|| VMError::VariableNotFound(name.clone())))
    }

    // Get a path access to a variable from the stack
    #[inline(always)]
    pub fn get_variable_path<'b>(&'b mut self, name: &'b IdentifierType) -> Result<Path<'a>, VMError> {
        self.stack.get_mut(*name as usize)
            .ok_or_else(|| VMError::StackError)
            .and_then(|value| value.as_mut().ok_or_else(|| VMError::VariableNotFound(name.clone())).map(Path::shareable))
    }

    // Check if a variable exists in the stack
    #[inline(always)]
    #[cfg(test)]
    pub fn has_variable(&self, name: &IdentifierType) -> bool {
        self.get_variable(name).is_ok()
    }

    // Register a variable in the stack
    pub fn register_variable(&mut self, name: IdentifierType, value: Path<'a>) -> Result<(), VMError> {
        *self.stack
            .get_mut(name as usize)
            .ok_or_else(|| VMError::StackError)? = Some(value);

        Ok(())
    }

    // Get the loop break flag
    #[inline(always)]
    pub fn get_loop_break(&self) -> bool {
        self.loop_break
    }

    // Set the loop break flag
    #[inline(always)]
    pub fn set_loop_break(&mut self, value: bool) {
        self.loop_break = value;
    }

    // Get the loop continue flag
    #[inline(always)]
    pub fn get_loop_continue(&self) -> bool {
        self.loop_continue
    }

    // Set the loop continue flag
    #[inline(always)]
    pub fn set_loop_continue(&mut self, value: bool) {
        self.loop_continue = value;
    }
}

#[cfg(test)]
mod tests {
    use crate::values::Value;
    use super::*;

    #[test]
    fn test_variable_exists() {
        let mut stack = Stack::new(2);
        stack.register_variable(0, Path::Owned(Value::U64(42))).unwrap();

        assert!(stack.has_variable(&0));
        assert!(!stack.has_variable(&1));
    }
}