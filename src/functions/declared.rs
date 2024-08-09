use crate::{expressions::{Parameter, Statement}, types::Type, IdentifierType};

#[derive(Debug)]
pub struct DeclaredFunction {
    for_type: Option<Type>,
    instance_name: Option<IdentifierType>,
    parameters: Vec<Parameter>,
    statements: Vec<Statement>,
    return_type: Option<Type>
}

impl DeclaredFunction {
    pub fn new(for_type: Option<Type>, instance_name: Option<IdentifierType>, parameters: Vec<Parameter>, statements: Vec<Statement>, return_type: Option<Type>) -> Self {
        DeclaredFunction {
            for_type,
            instance_name,
            parameters,
            statements,
            return_type
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

    pub fn get_instance_name(&self) -> &Option<IdentifierType> {
        &self.instance_name
    }

    pub fn get_return_type(&self) -> &Option<Type> {
        &self.return_type
    }
}

#[derive(Debug)]
pub struct EntryFunction {
    parameters: Vec<Parameter>,
    statements: Vec<Statement>,
}

impl EntryFunction {
    // Create a new entry function
    pub fn new(parameters: Vec<Parameter>, statements: Vec<Statement>) -> Self {
        EntryFunction {
            parameters,
            statements
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
}