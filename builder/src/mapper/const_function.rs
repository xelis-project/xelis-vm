use std::collections::HashMap;

use thiserror::Error;
use silex_types::{OwnedValue, Type, ValueError};

use crate::BuilderError;

pub type ConstFnParams = Vec<OwnedValue>;
pub type ConstFnCall = fn(ConstFnParams) -> Result<OwnedValue, ConstFunctionError>;

#[derive(Debug, Error)]
pub enum ConstFunctionError {
    #[error("Invalid parameters for const function")]
    InvalidParameters,
    #[error(transparent)]
    ValueError(#[from] ValueError),
    #[error(transparent)]
    BuilderError(#[from] BuilderError),
}

/// Function structure
/// It contains the name of the function and its parameters
#[derive(Debug)]
pub struct ConstFunction<'a> {
    pub name: &'a str,
    pub parameters: Vec<(&'a str, Type)>,
    pub on_call: ConstFnCall,
    pub return_type: Option<Type>,
    pub comment: Option<&'a str>,
}

impl<'a> ConstFunction<'a> {
    pub fn call(&self, parameters: Vec<(OwnedValue, Type)>) -> Result<OwnedValue, ConstFunctionError> {
        // Check that the params match
        if self.parameters.len() != parameters.len() {
            return Err(BuilderError::InvalidConstFnParametersMismatch.into());
        }

        let mut updated_constants = Vec::with_capacity(parameters.len());
        for ((mut param, param_type), (_, expected_type)) in parameters.into_iter().zip(self.parameters.iter()) {
            if param_type == *expected_type {
                updated_constants.push(param);
            } else if param_type.is_castable_to(expected_type) {
                let cast = param.checked_cast_to_primitive_type(expected_type)?;
                updated_constants.push(cast);
            } else {
                return Err(BuilderError::InvalidConstFnParameters.into());
            }
        }

        // Call the function
        (self.on_call)(updated_constants)
    }
}

/// ConstFunctionMapper is used to store the mapping between function signatures and their identifiers
/// So we can reduce the memory footprint of the VM by using an incremented id
#[derive(Debug)]
pub struct ConstFunctionMapper<'a> {
    mappings: HashMap<Type, HashMap<&'a str, ConstFunction<'a>>>,
}

impl<'a> ConstFunctionMapper<'a> {
    pub fn new() -> Self {
        Self {
            mappings: HashMap::default()
        }
    }

    // Register a function signature
    pub fn register(&mut self, name: &'a str, for_type: Type, parameters: Vec<(&'a str, Type)>, on_call: ConstFnCall) -> Result<(), BuilderError> {
        self.register_with_comment(name, for_type, parameters, on_call, None)
    }

    // Register a constant function with an optional documentation comment
    pub fn register_with_comment(&mut self, name: &'a str, for_type: Type, parameters: Vec<(&'a str, Type)>, on_call: ConstFnCall, comment: Option<&'a str>) -> Result<(), BuilderError> {
        let const_fn = ConstFunction {
            name,
            parameters,
            on_call,
            return_type: None,
            comment
        };

        self.mappings.entry(for_type)
            .or_insert_with(HashMap::new)
            .insert(name, const_fn);

        Ok(())
    }

    pub fn register_with_return_type(
        &mut self,
        name: &'a str,
        for_type: Type,
        parameters: Vec<(&'a str, Type)>,
        return_type: Type,
        on_call: ConstFnCall,
    ) -> Result<(), BuilderError> {
        self.register(name, for_type.clone(), parameters, on_call)?;
        let function = self.mappings.get_mut(&for_type)
            .and_then(|functions| functions.get_mut(name))
            .ok_or(BuilderError::MappingNotFound)?;
        function.return_type = Some(return_type);
        Ok(())
    }

    // Set the documentation comment for a constant function
    pub fn set_comment(&mut self, for_type: &Type, name: &str, comment: &'a str) -> Result<(), BuilderError> {
        let function = self.mappings
            .get_mut(for_type)
            .and_then(|m| m.get_mut(name))
            .ok_or(BuilderError::MappingNotFound)?;
        function.comment = Some(comment);
        Ok(())
    }

    pub fn get_const_fn<'b>(&'b self, for_type: &Type, fn_name: &str) -> Option<&'b ConstFunction<'b>> {
        self.mappings.get(for_type)
            .and_then(|m| m.get(fn_name))
    }

    pub fn get_mappings(&self) -> &HashMap<Type, HashMap<&'a str, ConstFunction<'a>>> {
        &self.mappings
    }
}
