use std::borrow::Cow;

use crate::{values::*, IdentifierType, InterpreterError, NoHashMap};

#[derive(Debug)]
pub enum VarValue<'a> {
    Owned(Value),
    Borrowed(&'a Value),
    Mut(&'a mut Value)
}

impl<'a> From<Value> for VarValue<'a> {
    fn from(value: Value) -> Self {
        Self::Owned(value)
    }
}

impl<'a> From<&'a Value> for VarValue<'a> {
    fn from(value: &'a Value) -> Self {
        Self::Borrowed(value)
    }
}

impl<'a> From<Cow<'a, Value>> for VarValue<'a> {
    fn from(value: Cow<'a, Value>) -> Self {
        match value {
            Cow::Borrowed(v) => Self::Borrowed(v),
            Cow::Owned(v) => Self::Owned(v)
        }
    }
}

impl<'a> From<&'a mut Value> for VarValue<'a> {
    fn from(value: &'a mut Value) -> Self {
        Self::Mut(value)
    }
}

pub type Scope<'a> = NoHashMap<VarValue<'a>>;

#[derive(Debug)]
pub struct Context<'a> {
    // Each scope is a HashMap storing variables
    // This is done to easily push/pop scopes
    scopes: Vec<Scope<'a>>,
    loop_break: bool,
    loop_continue: bool
}

impl<'a> Context<'a> {
    // Create a new context
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
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
    fn get_last_scope(&mut self) -> Result<&mut Scope<'a>, InterpreterError> {
        match self.scopes.last_mut() {
            Some(scope) => Ok(scope),
            None => Err(InterpreterError::NoScopeFound)
        }
    }

    // Create a new scope
    pub fn begin_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    // End the latest scope created
    pub fn end_scope(&mut self) -> Result<(), InterpreterError> {
        self.remove_scope()?;
        Ok(())
    }

    // Remove the latest scope created
    pub fn remove_scope(&mut self) -> Result<Scope<'a>, InterpreterError> {
        self.scopes.pop().ok_or(InterpreterError::NoScopeFound)
    }

    // Clear the latest scope created
    pub fn clear_last_scope(&mut self) -> Result<(), InterpreterError> {
        let scope = self.get_last_scope()?;
        scope.clear();
        Ok(())
    }

    // Remove a variable from the context
    pub fn remove_variable(&mut self, name: &IdentifierType) -> Result<VarValue<'a>, InterpreterError> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(v) = scope.remove(name) {
                return Ok(v)
            }
        }

        Err(InterpreterError::VariableNotFound(name.clone()))
    }

    pub fn get_variable(&self, name: &IdentifierType) -> Result<&VarValue<'a>, InterpreterError> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Ok(v)
            }
        }

        Err(InterpreterError::VariableNotFound(name.clone()))
    }

    pub fn get_mut_variable(&mut self, name: &IdentifierType) -> Result<&mut VarValue<'a>, InterpreterError> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(v) = scope.get_mut(name) {
                return Ok(v)
            }
        }

        Err(InterpreterError::VariableNotFound(name.clone()))
    }

    pub fn get_sharable_value(&mut self, name: &IdentifierType) -> Result<SharableValue, InterpreterError> {
        self.get_mut_variable(name).map(|v| v.get_sharable())
    }

    pub fn has_variable(&self, name: &IdentifierType) -> bool {
        self.get_variable(name).is_ok()
    }

    pub fn register_variable<V: Into<VarValue<'a>>>(&mut self, name: IdentifierType, value: V) -> Result<(), InterpreterError> {
        if self.has_variable(&name) {
            return Err(InterpreterError::VariableAlreadyExists(name))
        }

        let scope = self.get_last_scope()?;
        scope.insert(name, value.into());

        Ok(())
    }
}