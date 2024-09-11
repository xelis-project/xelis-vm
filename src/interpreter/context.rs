use std::cell::{RefCell, Ref, RefMut};
use crate::{values::*, IdentifierType, InterpreterError, NoHashMap};
use super::variable::Variable;

pub type Scope<'a> = NoHashMap<Variable<'a>>;

#[derive(Debug)]
pub struct Context<'a> {
    // Each scope is a HashMap storing variables
    // This is done to easily push/pop scopes
    scopes: RefCell<Vec<Scope<'a>>>,
}

impl<'a> Context<'a> {
    // Create a new context
    pub fn new() -> Self {
        Self {
            scopes: RefCell::new(Vec::new()),
        }
    }

    // Get the latest scope created
    fn get_last_scope<'b>(&'b self) -> Result<RefMut<'b, Scope<'a>>, InterpreterError> {
        let borrow = self.scopes.borrow_mut();  // Create a mutable borrow of the scopes vector
        
        // Use Option::ok_or to handle the None case and return an error
        RefMut::filter_map(borrow, |scopes| scopes.last_mut())
            .map_err(|_| InterpreterError::NoScopeFound)
    }

    // Create a new scope
    pub fn begin_scope(&self) {
        let mut borrow = self.scopes.borrow_mut();
        borrow.push(Scope::default());
    }
    
    // Remove the latest scope created
    #[inline]
    fn remove_last_scope(&self) -> Result<Scope<'a>, InterpreterError> {
        let mut borrow = self.scopes.borrow_mut();
        borrow.pop().ok_or(InterpreterError::NoScopeFound)
    }

    // End the latest scope created
    pub fn end_scope(&self) -> Result<(), InterpreterError> {
        self.remove_last_scope()?;
        Ok(())
    }

    // Clear the latest scope created without deleting it
    pub fn clear_last_scope(&self) -> Result<(), InterpreterError> {
        let mut scope = self.get_last_scope()?;
        scope.clear();
        Ok(())
    }

    // Remove a variable from the context
    pub fn remove_variable(&self, name: &IdentifierType) -> Result<Variable<'a>, InterpreterError> {
        let mut borrow = self.scopes.borrow_mut();
        for scope in borrow.iter_mut().rev() {
            if let Some(v) = scope.remove(name) {
                return Ok(v)
            }
        }

        Err(InterpreterError::VariableNotFound(name.clone()))
    }

    // Get a variable from the context
    pub fn get_variable<'b>(&'b self, name: &'b IdentifierType) -> Result<Ref<'b, Variable<'a>>, InterpreterError> {
        let borrow = self.scopes.borrow();
        Ref::filter_map(borrow, |scopes| {
            for scope in scopes.iter().rev() {
                if let Some(v) = scope.get(name) {
                    return Some(v)
                }
            }

            None
        }).map_err(|_| InterpreterError::VariableNotFound(name.clone()))
    }

    // Get a variable from the context as a value
    pub fn get_variable_as_value<'b>(&'b self, name: &'b IdentifierType) -> Result<Ref<'b, Value>, InterpreterError> {
        self.get_variable(name).map(|variable| Ref::map(variable, |v| v.as_value()))
    }

    pub fn get_variable_as_mut<'b>(&'b self, name: &'b IdentifierType) -> Result<RefMut<'b, Value>, InterpreterError> {
        let borrow = self.scopes.borrow_mut();
        RefMut::filter_map(borrow, |scopes| {
            for scope in scopes.iter_mut().rev() {
                if let Some(v) = scope.get_mut(name) {
                    return Some(v.as_mut())
                }
            }

            None
        }).map_err(|_| InterpreterError::VariableNotFound(name.clone()))
    }

    pub fn has_variable(&self, name: &IdentifierType) -> bool {
        self.get_variable(name).is_ok()
    }

    pub fn register_variable<V: Into<Variable<'a>>>(&self, name: IdentifierType, value: V) -> Result<(), InterpreterError> {
        if self.has_variable(&name) {
            return Err(InterpreterError::VariableAlreadyExists(name))
        }

        let mut scope = self.get_last_scope()?;
        scope.insert(name, value.into());

        Ok(())
    }
}