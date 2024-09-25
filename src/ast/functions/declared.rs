use crate::{
    ast::Statement,
    types::Type,
    IdentifierType
};
use super::Parameter;

#[derive(Debug, PartialEq, Eq)]
pub struct DeclaredFunction {
    for_type: Option<Type>,
    instance_name: Option<IdentifierType>,
    parameters: Vec<Parameter>,
    statements: Vec<Statement>,
    return_type: Option<Type>,
    variables_count: u16,
}

impl DeclaredFunction {
    pub fn new(for_type: Option<Type>, instance_name: Option<IdentifierType>, parameters: Vec<Parameter>, statements: Vec<Statement>, return_type: Option<Type>, variables_count: u16) -> Self {
        DeclaredFunction {
            for_type,
            instance_name,
            parameters,
            statements,
            return_type,
            variables_count
        }
    }

    pub fn get_on_type(&self) -> &Option<Type> {
        &self.for_type
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn get_parameters(&self) -> &Vec<Parameter> {
        &self.parameters
    }

    pub fn get_instance_name(&self) -> Option<&IdentifierType> {
        self.instance_name.as_ref()
    }

    pub fn get_return_type(&self) -> &Option<Type> {
        &self.return_type
    }

    pub fn get_variables_count(&self) -> u16 {
        self.variables_count
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct EntryFunction {
    parameters: Vec<Parameter>,
    statements: Vec<Statement>,
    variables_count: u16,
}

impl EntryFunction {
    // Create a new entry function
    pub fn new(parameters: Vec<Parameter>, statements: Vec<Statement>, variables_count: u16) -> Self {
        EntryFunction {
            parameters,
            statements,
            variables_count
        }
    }

    // Get the parameters of the function
    pub fn get_parameters(&self) -> &Vec<Parameter> {
        &self.parameters
    }

    // Get the statements of the function
    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    // Get the variables count of the function
    pub fn get_variables_count(&self) -> u16 {
        self.variables_count
    }
}