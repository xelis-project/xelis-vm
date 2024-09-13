use crate::{IdentifierType, InterpreterError, NoHashMap, variable::Path};

pub type Scope<'a> = NoHashMap<Path<'a>>;

#[derive(Debug)]
pub struct Context<'a> {
    // Each scope is a HashMap storing variables
    // This is done to easily push/pop scopes
    scopes: Vec<Scope<'a>>,
    // Flags to break a loop
    loop_break: bool,
    // Flags to continue in loop
    loop_continue: bool,
}

impl<'a> Context<'a> {
    // Create a new context
    pub fn new() -> Self {
        Self {
            scopes: Vec::with_capacity(4),
            loop_break: false,
            loop_continue: false,
        }
    }

    // Get the latest scope created
    #[inline(always)]
    fn get_last_scope<'b>(&'b mut self) -> Result<&'b mut Scope<'a>, InterpreterError> {
        self.scopes.last_mut().ok_or(InterpreterError::NoScopeFound)
    }

    // Create a new scope
    #[inline(always)]
    pub fn begin_scope(&mut self) {
        self.scopes.push(Scope::with_capacity_and_hasher(16, Default::default()));
    }

    // Remove the latest scope created
    #[inline(always)]
    fn remove_last_scope(&mut self) -> Result<Scope<'a>, InterpreterError> {
        self.scopes.pop().ok_or(InterpreterError::NoScopeFound)
    }

    // End the latest scope created
    #[inline(always)]
    pub fn end_scope(&mut self) -> Result<(), InterpreterError> {
        self.remove_last_scope()?;
        Ok(())
    }

    // Clear the latest scope created without deleting it
    #[inline(always)]
    pub fn clear_last_scope(&mut self) -> Result<(), InterpreterError> {
        let scope = self.get_last_scope()?;
        scope.clear();
        Ok(())
    }

    // Remove a variable from the context
    #[inline(always)]
    pub fn remove_variable(&mut self, name: &IdentifierType) -> Result<Path<'a>, InterpreterError> {
        self.scopes.iter_mut()
            .rev()
            .find_map(|scope| scope.remove(name))
            .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))
    }

    // Get a variable from the context
    #[inline(always)]
    pub fn get_variable<'b>(&'b self, name: &'b IdentifierType) -> Result<&'b Path<'a>, InterpreterError> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
            .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))
    }

    #[inline(always)]
    pub fn get_variable_reference<'b>(&'b mut self, name: &'b IdentifierType) -> Result<Path<'a>, InterpreterError> {
        self.scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.get_mut(name).map(Path::shareable))
            .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))
    }

    #[inline(always)]
    pub fn has_variable(&self, name: &IdentifierType) -> bool {
        self.get_variable(name).is_ok()
    }

    pub fn register_variable(&mut self, name: IdentifierType, value: Path<'a>) -> Result<(), InterpreterError> {
        if self.has_variable(&name) {
            return Err(InterpreterError::VariableAlreadyExists(name))
        }

        let scope = self.get_last_scope()?;
        scope.insert(name, value);

        Ok(())
    }

    // Get the loop break flag
    pub fn get_loop_break(&self) -> bool {
        self.loop_break
    }

    // Set the loop break flag
    pub fn set_loop_break(&mut self, value: bool) {
        self.loop_break = value;
    }

    // Get the loop continue flag
    pub fn get_loop_continue(&self) -> bool {
        self.loop_continue
    }

    // Set the loop continue flag
    pub fn set_loop_continue(&mut self, value: bool) {
        self.loop_continue = value;
    }
}