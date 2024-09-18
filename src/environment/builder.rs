use crate::{
    ast::{FunctionType, NativeFunction, OnCallFn, Operator, Signature},
    parser::FunctionMapper,
    types::Type,
    NoHashMap,
};

use super::{std as xstd, Environment};

// EnvironmentBuilder is used to create the environment for the Parser
// it is used to register all the native functions and structures
// and import files by the user
pub struct EnvironmentBuilder<'a> {
    functions_mapper: FunctionMapper<'a>,
    env: Environment
}

impl<'a> EnvironmentBuilder<'a> {
    // Create a new instance of the EnvironmentBuilder
    pub fn new() -> Self {
        Self {
            functions_mapper: FunctionMapper::new(),
            env: Environment::new()
        }
    }

    // Set the cost of an operator
    // Example, setting the cost of the Plus operator to 1
    pub fn register_operator(&mut self, operator: Operator, cost: u64) {
        self.env.operators.insert(operator, cost);
    }

    // Register a native function
    pub fn register_native_function(&mut self, name: &str, for_type: Option<Type>, parameters: Vec<Type>, on_call: OnCallFn, cost: u64, return_type: Option<Type>) {
        let id = self.functions_mapper.register(Signature::new(name.to_owned(), for_type.clone(), parameters.clone())).unwrap();
        self.env.functions.insert(id, FunctionType::Native(NativeFunction::new(for_type, parameters, on_call, cost, return_type)));
    }

    // functions mapper, used to find the function id
    pub fn get_functions_mapper(&self) -> &FunctionMapper {
        &self.functions_mapper
    }

    // all registered functions
    pub fn get_functions(&self) -> &NoHashMap<FunctionType> {
        &self.env.functions
    }

    // Get the environment for the interpreter
    pub fn environment(&self) -> &Environment {
        &self.env
    }

    // Build the environment for the interpreter
    pub fn build(self) -> Environment {
        self.env
    }

    pub fn build_and_take_mapper(self) -> (Environment, FunctionMapper<'a>) {
        (self.env, self.functions_mapper)
    }
}

impl Default for EnvironmentBuilder<'_> {
    fn default() -> Self {
        let mut env = Self::new();

        xstd::register(&mut env);

        env
    }
}