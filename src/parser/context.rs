use crate::{types::Type, Parser, ParserError};
use super::{scope::Scope, VariableId};

#[derive(Clone)]
pub struct Context {
    scopes: Vec<Scope>,
    pub(super) return_type: Option<Type>,
    pub(super) is_in_loop: bool,
    pub(super) current_type: Option<Type>, // used by path walkthrough
}

impl Context {
    pub fn new() -> Self {
        Self {
            // first is for constants
            scopes: vec![Scope::default()],
            return_type: None,
            is_in_loop: false,
            current_type: None
        }
    }

    pub fn set_current_type(&mut self, current_type: Type, parser: &Parser) -> Result<(), ParserError> {
        self.remove_current_type(); // prevent any bug

        match &current_type {
            Type::Struct(ref s) => {
                self.create_new_scope();
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
                    self.remove_last_scope();
                }
                _ => {}
            }
        }
    }

    pub fn get_current_type(&self) -> &Option<Type> {
        &self.current_type
    }

    pub fn get_type_of_variable(&self, key: &VariableId) -> Result<&Type, ParserError> {
        for vars in self.scopes.iter().rev() {
            if let Some(_type) = vars.get(key) {
                return Ok(_type)
            }
        }

        Err(ParserError::UnexpectedVariable(key.clone()))
    }

    pub fn has_variable(&self, key: &String) -> bool {
        self.get_type_of_variable(key).is_ok()
    }

    pub fn register_variable(&mut self, key: String, var_type: Type) -> Result<(), ParserError> {
        if self.has_variable(&key) {
            return Err(ParserError::VariableNameAlreadyUsed(key))
        }

        self.scopes.last_mut()
            .ok_or(ParserError::NoScopeFound)?
            .insert(key, var_type);

        Ok(())
    }

    pub fn create_new_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    pub fn remove_last_scope(&mut self) {
        if self.has_scope() {
            self.scopes.remove(self.scopes.len() - 1);
        }
    }

    pub fn has_scope(&self) -> bool {
        !self.scopes.is_empty()
    }
}
