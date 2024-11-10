use super::InterpreterError;
use xelis_types::{Path, IdentifierType};

#[derive(Debug)]
pub struct Stack<'a> {
    // Each scope is a HashMap storing variables
    // This is done to easily push/pop scopes
    stack: Vec<Option<Path<'a>>>
}

impl<'a> Stack<'a> {
    // Create a new stack with a specific capacity to avoid reallocations
    pub fn new(capacity: u16) -> Self {
        Self {
            stack: vec![None; capacity as usize]
        }
    }

    // Remove a variable from the stack
    #[inline(always)]
    pub fn remove_variable(&mut self, name: &IdentifierType) -> Result<Path<'a>, InterpreterError> {
        self.stack.get_mut(*name as usize)
            .ok_or_else(|| InterpreterError::StackError)
            .and_then(|value| value.take().ok_or_else(|| InterpreterError::VariableNotFound(name.clone())))
    }

    // Get a variable from the stack
    #[inline(always)]
    #[cfg(test)]
    pub fn get_variable<'b>(&'b self, name: &'b IdentifierType) -> Result<&'b Path<'a>, InterpreterError> {
        self.stack.get(*name as usize)
            .ok_or_else(|| InterpreterError::StackError)
            .and_then(|value| value.as_ref().ok_or_else(|| InterpreterError::VariableNotFound(name.clone())))
    }

    // Get a path access to a variable from the stack
    #[inline(always)]
    pub fn get_variable_path<'b>(&'b mut self, name: &'b IdentifierType) -> Result<Path<'a>, InterpreterError> {
        self.stack.get_mut(*name as usize)
            .ok_or_else(|| InterpreterError::StackError)
            .and_then(|value| value.as_mut().ok_or_else(|| InterpreterError::VariableNotFound(name.clone())).map(Path::shareable))
    }

    // Check if a variable exists in the stack
    #[inline(always)]
    #[cfg(test)]
    pub fn has_variable(&self, name: &IdentifierType) -> bool {
        self.get_variable(name).is_ok()
    }

    // Register a variable in the stack
    pub fn register_variable(&mut self, name: IdentifierType, value: Path<'a>) -> Result<(), InterpreterError> {
        *self.stack
            .get_mut(name as usize)
            .ok_or_else(|| InterpreterError::StackError)? = Some(value);

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use xelis_types::Value;
    use super::*;

    #[test]
    fn test_variable_exists() {
        let mut stack = Stack::new(2);
        stack.register_variable(0, Path::Owned(Value::U64(42))).unwrap();

        assert!(stack.has_variable(&0));
        assert!(!stack.has_variable(&1));
    }
}