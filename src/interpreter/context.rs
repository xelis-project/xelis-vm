use std::{collections::HashMap, hash::{BuildHasherDefault, Hasher}};

use crate::{InterpreterError, IdentifierType, values::*};

pub type Scope = HashMap<IdentifierType, ValueVariant, BuildHasherDefault<NoOpHasher>>;

// Hasher that does nothing
// Because we have u16 as the key, we don't need to hash it
#[derive(Debug, Default)]
pub struct NoOpHasher(u16);

impl Hasher for NoOpHasher {
    fn write(&mut self, bytes: &[u8]) {
        self.0 = u16::from_le_bytes([bytes[0], bytes[1]]);
    }

    fn write_u64(&mut self, _: u64) {
        unimplemented!("write_u64")
    }

    fn finish(&self) -> u64 {
        self.0 as u64
    }
}

#[derive(Debug)]
pub struct Context {
    // Each scope is a HashMap storing variables
    // This is done to easily push/pop scopes
    scopes: Vec<Scope>,
    loop_break: bool,
    loop_continue: bool
}

impl Context {
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
    fn get_last_scope(&mut self) -> Result<&mut Scope, InterpreterError> {
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
    pub fn remove_scope(&mut self) -> Result<Scope, InterpreterError> {
        self.scopes.pop().ok_or(InterpreterError::NoScopeFound)
    }

    pub fn get_variable(&self, name: &IdentifierType) -> Result<&ValueVariant, InterpreterError> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Ok(v)
            }
        }

        Err(InterpreterError::VariableNotFound(name.clone()))
    }

    pub fn get_mut_variable(&mut self, name: &IdentifierType) -> Result<&mut ValueVariant, InterpreterError> {
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

    pub fn set_variable_value(&mut self, name: &IdentifierType, value: ValueVariant) -> Result<(), InterpreterError> {
        let var = self.get_mut_variable(name)?;
        *var = value;
        Ok(())
    }

    pub fn has_variable(&self, name: &IdentifierType) -> bool {
        self.get_variable(name).is_ok()
    }

    pub fn register_variable<V: Into<ValueVariant>>(&mut self, name: IdentifierType, value: V) -> Result<(), InterpreterError> {
        if self.has_variable(&name) {
            return Err(InterpreterError::VariableAlreadyExists(name))
        }

        let scope = self.get_last_scope()?;
        scope.insert(name, value.into());

        Ok(())
    }
}