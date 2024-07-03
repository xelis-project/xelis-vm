use crate::{types::Type, Parser, ParserError};
use super::{scope::Scope, VariableId};

#[derive(Clone)]
pub struct Context {
    // scopes are used to store variables
    scopes: Vec<Scope>,
    // is_in_loop is used to allow the use of the break and continue keywords
    is_in_loop: bool,
    current_type: Option<Type>, // used by path walkthrough
}

impl Context {
    // create a new Context with a default scope
    pub fn new() -> Self {
        Self {
            // first is for constants
            scopes: vec![Scope::default()],
            is_in_loop: false,
            current_type: None
        }
    }

    pub fn set_current_type(&mut self, current_type: Type, parser: &Parser) -> Result<(), ParserError> {
        self.remove_current_type(); // prevent any bug

        match &current_type {
            Type::Struct(ref s) => {
                self.begin_scope();
                for (name, _type) in &parser.get_structure(s)?.fields {
                    self.register_variable(name.clone(), _type.clone())?;
                }
            }
            _ => {}
        }
        self.current_type = Some(current_type);
        Ok(())
    }

    pub fn remove_current_type(&mut self) {
        if let Some(t) = self.current_type.take() {
            match t {
                Type::Struct(_) => {
                    self.end_scope();
                }
                _ => {}
            }
        }
    }

    pub fn get_current_type(&self) -> &Option<Type> {
        &self.current_type
    }

    // get the value type of a variable registered in scopes using its name
    pub fn get_type_of_variable(&self, key: &VariableId) -> Result<&Type, ParserError> {
        for vars in self.scopes.iter().rev() {
            if let Some(_type) = vars.get(key) {
                return Ok(_type)
            }
        }

        Err(ParserError::UnexpectedVariable(key.clone()))
    }

    // returns true if this variable name is registered in scopes
    pub fn has_variable(&self, key: &String) -> bool {
        self.get_type_of_variable(key).is_ok()
    }

    // register a variable in the current scope
    pub fn register_variable(&mut self, key: String, var_type: Type) -> Result<(), ParserError> {
        if self.has_variable(&key) {
            return Err(ParserError::VariableNameAlreadyUsed(key))
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

    // returns if the Context has at least one scope
    pub fn has_scope(&self) -> bool {
        !self.scopes.is_empty()
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
