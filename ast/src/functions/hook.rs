use xelis_types::Type;
use crate::Statement;
use super::Parameter;

#[derive(Debug, PartialEq, Eq)]
pub struct HookFunction {
    parameters: Vec<Parameter>,
    statements: Vec<Statement>,
    return_type: Option<Type>,
    variables_count: u16,
    hook_id: u8
}

impl HookFunction {
    pub fn new(parameters: Vec<Parameter>, statements: Vec<Statement>, return_type: Option<Type>, variables_count: u16, hook_id: u8) -> Self {
        Self {
            parameters,
            statements,
            return_type,
            variables_count,
            hook_id
        }
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn get_parameters(&self) -> &Vec<Parameter> {
        &self.parameters
    }

    pub fn get_return_type(&self) -> &Option<Type> {
        &self.return_type
    }

    pub fn get_variables_count(&self) -> u16 {
        self.variables_count
    }

    pub fn set_statements(&mut self, statements: Vec<Statement>) {
        self.statements = statements;
    }

    pub fn set_max_variables_count(&mut self, variables_count: u16) {
        self.variables_count = variables_count;
    }

    pub fn hook_id(&self) -> u8 {
        self.hook_id
    }
}