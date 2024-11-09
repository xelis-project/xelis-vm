pub mod xstd;

use std::borrow::Cow;
use ast::Signature;
use types::Type;
use environment::{Environment, NativeFunction, OnCallFn};
use crate::{StructBuilder, StructManager, FunctionMapper};

// EnvironmentBuilder is used to create an environment
// it is used to register all the native functions and structures
// and import files by the user
pub struct EnvironmentBuilder<'a> {
    functions_mapper: FunctionMapper<'a>,
    struct_manager: StructManager<'a>,
    env: Environment
}

impl<'a> EnvironmentBuilder<'a> {
    // Create a new instance of the EnvironmentBuilder
    pub fn new() -> Self {
        Self {
            functions_mapper: FunctionMapper::new(),
            struct_manager: StructManager::new(),
            env: Environment::new()
        }
    }

    // Register a native function
    // Panic if the function signature is already registered
    pub fn register_native_function(&mut self, name: &str, for_type: Option<Type>, parameters: Vec<Type>, on_call: OnCallFn, cost: u64, return_type: Option<Type>) {
        let _ = self.functions_mapper.register(Signature::new(name.to_owned(), for_type.clone(), parameters.clone())).unwrap();
        self.env.add_function(NativeFunction::new(for_type, parameters, on_call, cost, return_type));
    }

    // Register a structure in the environment
    // Panic if the structure name is already used
    pub fn register_structure(&mut self, name: &'a str, fields: Vec<(&'a str, Type)>) {
        let builder = StructBuilder {
            fields
        };

        let (_, s) = self.struct_manager.build_struct(Cow::Borrowed(name), builder).unwrap();
        self.env.add_structure(s);
    }

    // functions mapper, used to find the function id
    pub fn get_functions_mapper(&self) -> &FunctionMapper {
        &self.functions_mapper
    }

    // struct manager, used to find the struct id
    pub fn get_struct_manager(&self) -> &StructManager {
        &self.struct_manager
    }

    // all registered functions
    pub fn get_functions(&self) -> &Vec<NativeFunction> {
        &self.env.get_functions()
    }

    // Get the environment for the interpreter
    pub fn environment(&self) -> &Environment {
        &self.env
    }

    // Build the environment for the interpreter
    pub fn build(self) -> Environment {
        self.env
    }

    // Finalize the environment builder and return the environment and the function mapper
    pub fn finalize(self) -> (Environment, FunctionMapper<'a>) {
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