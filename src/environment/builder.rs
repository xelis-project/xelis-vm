use std::collections::HashMap;

use crate::{
    functions::{FnInstance, FnReturnType, FunctionType, NativeFunction, OnCallFn},
    mapper::FunctionMapper,
    types::Type,
    values::Value,
    NoHashMap,
    Signature
};

use super::{std as xstd, Environment};

// EnvironmentBuilder is used to create the environment for the Parser
// it is used to register all the native functions and structures
// and import files by the user
pub struct EnvironmentBuilder<'a> {
    functions_mapper: FunctionMapper<'a>,
    imports: HashMap<&'a str, String>,
    env: Environment
}

impl<'a> EnvironmentBuilder<'a> {
    // Create a new instance of the EnvironmentBuilder
    pub fn new() -> Self {
        Self {
            functions_mapper: FunctionMapper::new(),
            imports: HashMap::new(),
            env: Environment::new()
        }
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

        env.register_native_function("println", None, vec![Type::Any], println, 1, None);

        xstd::register(&mut env);

        env
    }
}

fn println(_: FnInstance, mut parameters: Vec<Value>) -> FnReturnType {
    let param = parameters.remove(0);
    println!("{}", param);

    Ok(None)
}