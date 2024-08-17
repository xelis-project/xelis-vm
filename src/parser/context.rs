use crate::{types::Type, ParserError, IdentifierType};
use super::scope::Scope;

#[derive(Clone)]
pub struct Context {
    // scopes are used to store variables
    scopes: Vec<Scope>,
    // is_in_loop is used to allow the use of the break and continue keywords
    is_in_loop: bool,
}

impl Context {
    // create a new Context with a default scope
    pub fn new() -> Self {
        Self {
            // first is for constants
            scopes: vec![Scope::default()],
            is_in_loop: false,
        }
    }

    // get the value type of a variable registered in scopes using its name
    pub fn get_type_of_variable<'a>(&self, key: &IdentifierType) -> Result<&Type, ParserError<'a>> {
        for vars in self.scopes.iter().rev() {
            if let Some(_type) = vars.get(key) {
                return Ok(_type)
            }
        }

        Err(ParserError::UnexpectedMappedVariableId(key.clone()))
    }

    // returns true if this variable name is registered in scopes
    pub fn has_variable(&self, key: &IdentifierType) -> bool {
        self.get_type_of_variable(key).is_ok()
    }

    // register a variable in the current scope
    pub fn register_variable<'a>(&mut self, key: IdentifierType, var_type: Type) -> Result<(), ParserError<'a>> {
        if self.has_variable(&key) {
            return Err(ParserError::VariableIdAlreadyUsed(key))
        }

        self.scopes.last_mut()
            .ok_or(ParserError::NoScopeFound)?
            .insert(key, var_type);

        Ok(())
    }

    // Add a new scope empty in the Context
    pub fn begin_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    // Delete the latest scope added to the Context
    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    // returns if the Context is in a loop
    pub fn is_in_a_loop(&self) -> bool {
        self.is_in_loop
    }

    // set if the Context is in a loop
    pub fn set_in_a_loop(&mut self, is_in_loop: bool) {
        self.is_in_loop = is_in_loop;
    }
}
