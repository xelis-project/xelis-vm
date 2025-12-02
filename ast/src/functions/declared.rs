use xelis_types::{Type, IdentifierType};
use crate::Statement;
use super::Parameter;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq)]
pub enum FunctionVisibility {
    // Public function: can be called by any other
    // program
    Public,
    // Private: can only be called from the same program
    #[default]
    Private,
    // Anonymous function (closure)
    Anonymous,
}

impl FunctionVisibility {
    #[inline]
    pub const fn is_public(&self) -> bool {
        matches!(self, FunctionVisibility::Public)
    }

    #[inline]
    pub const fn is_private(&self) -> bool {
        matches!(self, FunctionVisibility::Private)
    }

    #[inline]
    pub const fn is_anonymous(&self) -> bool {
        matches!(self, FunctionVisibility::Anonymous)
    }

}

#[derive(Debug, PartialEq, Eq)]
pub struct DeclaredFunction {
    visibility: FunctionVisibility,
    for_type: Option<Type>,
    instance_name: Option<IdentifierType>,
    parameters: Vec<Parameter>,
    statements: Vec<Statement>,
    return_type: Option<Type>,
    variables_count: u16,
    registers_reserved: u16,
}

impl DeclaredFunction {
    pub fn new(
        visibility: FunctionVisibility,
        for_type: Option<Type>,
        instance_name: Option<IdentifierType>,
        parameters: Vec<Parameter>,
        statements: Vec<Statement>,
        return_type: Option<Type>,
        variables_count: u16,
        registers_reserved: u16
    ) -> Self {
        DeclaredFunction {
            visibility,
            for_type,
            instance_name,
            parameters,
            statements,
            return_type,
            variables_count,
            registers_reserved,
        }
    }

    #[inline]
    pub const fn registers_reserved(&self) -> u16 {
        self.registers_reserved
    }

    #[inline]
    pub const fn visibility(&self) -> FunctionVisibility {
        self.visibility
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

    pub fn set_statements(&mut self, statements: Vec<Statement>) {
        self.statements = statements;
    }

    pub fn set_max_variables_count(&mut self, variables_count: u16) {
        self.variables_count = variables_count;
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

    pub fn set_statements(&mut self, statements: Vec<Statement>) {
        self.statements = statements;
    }

    pub fn set_max_variables_count(&mut self, variables_count: u16) {
        self.variables_count = variables_count;
    }
}