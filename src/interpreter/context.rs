use crate::{values::*, IdentifierType, InterpreterError, NoHashMap};
use super::variable::Variable;

pub type Scope<'a> = NoHashMap<Variable<'a>>;

#[derive(Debug)]
pub struct Context<'a> {
    // Each scope is a HashMap storing variables
    // This is done to easily push/pop scopes
    scopes: Vec<Scope<'a>>,
}

impl<'a> Context<'a> {
    // Create a new context
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
        }
    }

    // Get the latest scope created
    fn get_last_scope<'b>(&'b mut self) -> Result<&'b mut Scope<'a>, InterpreterError> {
        self.scopes.last_mut().ok_or(InterpreterError::NoScopeFound)
    }

    // Create a new scope
    pub fn begin_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    // Remove the latest scope created
    #[inline]
    fn remove_last_scope(&mut self) -> Result<Scope<'a>, InterpreterError> {
        self.scopes.pop().ok_or(InterpreterError::NoScopeFound)
    }

    // End the latest scope created
    pub fn end_scope(&mut self) -> Result<(), InterpreterError> {
        self.remove_last_scope()?;
        Ok(())
    }

    // Clear the latest scope created without deleting it
    pub fn clear_last_scope(&mut self) -> Result<(), InterpreterError> {
        let scope = self.get_last_scope()?;
        scope.clear();
        Ok(())
    }

    // Remove a variable from the context
    pub fn remove_variable(&mut self, name: &IdentifierType) -> Result<Variable<'a>, InterpreterError> {
        self.scopes.iter_mut()
            .rev()
            .find_map(|scope| scope.remove(name))
            .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))
    }

    // Get a variable from the context
    pub fn get_variable<'b>(&'b self, name: &'b IdentifierType) -> Result<&'b Variable<'a>, InterpreterError> {
        self.scopes.iter().rev().find_map(|scope| scope.get(name))
            .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))
    }

    // Get a variable from the context as a value
    pub fn get_variable_as_value<'b>(&'b self, name: &'b IdentifierType) -> Result<&'b Value, InterpreterError> {
        self.get_variable(name).map(Variable::as_value)
    }

    pub fn get_variable_as_mut<'b>(&'b mut self, name: &'b IdentifierType) -> Result<&'b mut Value, InterpreterError> {
        self.scopes
            .iter_mut()
            .rev()
            .find_map(|scope| scope.get_mut(name).map(Variable::as_mut))
            .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))
    }

    pub fn has_variable(&self, name: &IdentifierType) -> bool {
        self.get_variable(name).is_ok()
    }

    pub fn register_variable<V: Into<Variable<'a>>>(&mut self, name: IdentifierType, value: V) -> Result<(), InterpreterError> {
        if self.has_variable(&name) {
            return Err(InterpreterError::VariableAlreadyExists(name))
        }

        let scope = self.get_last_scope()?;
        scope.insert(name, value.into());

        Ok(())
    }
}