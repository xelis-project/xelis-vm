use std::collections::HashMap;

use xelis_types::{Constant, Type};

use crate::BuilderError;

pub type ConstFnParams = Vec<Constant>;
pub type ConstFnCall = fn(ConstFnParams) -> Result<Constant, anyhow::Error>;

/// Function structure
/// It contains the name of the function and its parameters
#[derive(Debug)]
pub struct ConstFunction<'a> {
    pub name: &'a str,
    pub parameters: Vec<(&'a str, Type)>,
    pub on_call: ConstFnCall
}

impl<'a> ConstFunction<'a> {
    pub fn call(&self, parameters: Vec<Constant>) -> Result<Constant, anyhow::Error> {
        // Check that the params match
        if self.parameters.len() != parameters.len() {
            return Err(BuilderError::InvalidConstFnParameters.into());
        }

        let mut updated_constants = Vec::with_capacity(parameters.len());
        for (param, (_, expected_type)) in parameters.into_iter().zip(self.parameters.iter()) {
            if let Some(param_type) = Type::from_value_type(&param) {
                if param_type != *expected_type {
                    if param_type.is_castable_to(expected_type) {
                        // Try to cast it
                        let cast = param.checked_cast_to_primitive_type(expected_type)?;
                        updated_constants.push(cast);
                        continue;
                    }
                } else {
                    updated_constants.push(param);
                    continue;
                }
            }

            return Err(BuilderError::InvalidConstFnParameters.into());
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
        let const_fn = ConstFunction {
            name,
            parameters,
            on_call
        };

        self.mappings.entry(for_type)
            .or_insert_with(HashMap::new)
            .insert(name, const_fn);

        Ok(())
    }

    pub fn get_const_fn(&self, for_type: &Type, fn_name: &'a str) -> Option<&ConstFunction<'a>> {
        self.mappings.get(for_type)
            .and_then(|m| m.get(fn_name))
    }

    pub fn get_mappings(&self) -> &HashMap<Type, HashMap<&'a str, ConstFunction<'a>>> {
        &self.mappings
    }
}