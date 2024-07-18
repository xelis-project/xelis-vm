use std::collections::HashMap;

use crate::{InterpreterError, IdentifierType};
use super::Value;

pub type Scope = HashMap<IdentifierType, Value>;

#[derive(Debug)]
pub struct Context {
    variables: Vec<Scope>,
    loop_break: bool,
    loop_continue: bool
}

impl Context {
    // Create a new context
    pub fn new() -> Self {
        Self {
            variables: Vec::new(),
            loop_break: false,
            loop_continue: false
        }
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

    // Get the latest scope created
    fn get_last_scope(&mut self) -> Result<&mut Scope, InterpreterError> {
        match self.variables.last_mut() {
            Some(scope) => Ok(scope),
            None => Err(InterpreterError::NoScopeFound)
        }
    }

    // Create a new scope
    pub fn begin_scope(&mut self) {
        self.variables.push(HashMap::new());
    }

    // End the latest scope created
    pub fn end_scope(&mut self) -> Result<(), InterpreterError> {
        self.remove_scope()?;
        Ok(())
    }

    // Remove the latest scope created
    pub fn remove_scope(&mut self) -> Result<Scope, InterpreterError> {
        self.variables.pop().ok_or(InterpreterError::NoScopeFound)
    }

    pub fn get_variable(&self, name: &IdentifierType) -> Result<&Value, InterpreterError> {
        self.variables.iter().rev()
            .find(|v| v.contains_key(name))
            .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))?
            .get(name).ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))
    }

    pub fn get_mut_variable(&mut self, name: &IdentifierType) -> Result<&mut Value, InterpreterError> {
        self.variables.iter_mut().rev()
            .find(|v| v.contains_key(name))
            .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))?
            .get_mut(name).ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))
    }

    pub fn set_variable_value(&mut self, name: &IdentifierType, value: Value) -> Result<(), InterpreterError> {
        let var = self.get_mut_variable(name)?;
        *var = value;
        Ok(())
    }

    pub fn has_variable(&self, name: &IdentifierType) -> bool {
        self.get_variable(name).is_ok()
    }

    pub fn register_variable(&mut self, name: IdentifierType, value: Value) -> Result<(), InterpreterError> {
        if self.has_variable(&name) {
            return Err(InterpreterError::VariableAlreadyExists(name))
        }
        let scope = self.get_last_scope()?;
        scope.insert(name, value);

        Ok(())
    }
}