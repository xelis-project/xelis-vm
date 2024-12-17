pub mod xstd;

use std::{borrow::Cow, collections::HashMap};
use xelis_ast::Signature;
use xelis_types::{Constant, EnumType, OpaqueType, Opaque, StructType, Type};
use xelis_environment::{Environment, NativeFunction, OnCallFn};
use crate::{EnumManager, EnumVariantBuilder, FunctionMapper, StructManager};

// EnvironmentBuilder is used to create an environment
// it is used to register all the native functions and structures
// and import files by the user
pub struct EnvironmentBuilder<'a> {
    functions_mapper: FunctionMapper<'a>,
    struct_manager: StructManager<'a>,
    enum_manager: EnumManager<'a>,
    opaque_manager: HashMap<&'a str, OpaqueType>,
    constants: HashMap<Type, HashMap<&'a str, Constant>>,
    env: Environment
}

impl<'a> EnvironmentBuilder<'a> {
    // Create a new instance of the EnvironmentBuilder
    pub fn new() -> Self {
        Self {
            functions_mapper: FunctionMapper::new(),
            struct_manager: StructManager::new(),
            enum_manager: EnumManager::new(),
            opaque_manager: HashMap::new(),
            constants: HashMap::new(),
            env: Environment::new(),
        }
    }

    // Register a native function
    // Panic if the function signature is already registered
    pub fn register_native_function(&mut self, name: &'a str, for_type: Option<Type>, parameters: Vec<(&'a str, Type)>, on_call: OnCallFn, cost: u64, return_type: Option<Type>) {
        let params: Vec<_> = parameters.iter().map(|(_, t)| t.clone()).collect();
        let _ = self.functions_mapper.register(name, for_type.clone(), parameters, return_type.clone()).unwrap();
        self.env.add_function(NativeFunction::new(for_type, params, on_call, cost, return_type));
    }

    // Get a function by its signature
    // Panic if the function signature is not found
    pub fn get_mut_function(&mut self, name: &str, on_type: Option<Type>, parameters: Vec<Type>) -> &mut NativeFunction {
        let id = self.functions_mapper.get(&Signature::new(name.to_owned(), on_type.clone(), parameters)).unwrap();
        self.env.get_function_by_id_mut(id as usize).unwrap()

    }

    // Register a structure in the environment
    // Panic if the structure name is already used
    pub fn register_structure(&mut self, name: &'a str, fields: Vec<(&'a str, Type)>) -> StructType {
        let _type = self.struct_manager.build(Cow::Borrowed(name), fields).unwrap();
        self.env.add_structure(_type.clone());
        _type
    }

    // Register an enum in the environment
    // Panic if the enum name is already used
    pub fn register_enum(&mut self, name: &'a str, variants: Vec<(&'a str, EnumVariantBuilder<'a>)>) -> EnumType {
        let _type = self.enum_manager.build(Cow::Borrowed(name), variants).unwrap();
        self.env.add_enum(_type.clone());
        _type
    }

    // Register an opaque type in the environment
    // Panic if the opaque name is already used
    pub fn register_opaque<T: Opaque>(&mut self, name: &'a str) -> OpaqueType {
        let _type = OpaqueType::new::<T>();
        self.opaque_manager.insert(name, _type.clone());
        self.env.add_opaque(_type.clone());
        _type
    }

    // Get an opaque type by name
    pub fn get_opaque_by_name(&self, name: &str) -> Option<&OpaqueType> {
        self.opaque_manager.get(name)
    }

    // Register a constant in the environment
    // Panic if the constant name is already used
    pub fn register_constant(&mut self, _type: Type, name: &'a str, value: Constant) {
        let constants = self.constants.entry(_type.clone()).or_insert_with(HashMap::new);
        constants.insert(name, value.clone());
    }

    // Get a constant by name
    pub fn get_constant_by_name(&self, _type: &Type, name: &str) -> Option<&Constant> {
        self.constants.get(_type).and_then(|v| v.get(name))
    }

    // functions mapper, used to find the function id
    pub fn get_functions_mapper(&self) -> &FunctionMapper {
        &self.functions_mapper
    }

    // struct manager, used to find the struct id
    pub fn get_struct_manager(&self) -> &StructManager {
        &self.struct_manager
    }

    // enum manager, used to find the enum id
    pub fn get_enum_manager(&self) -> &EnumManager {
        &self.enum_manager
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_add_enum() {
        let mut builder = EnvironmentBuilder::new();

        /*
        enum Test {
            A { a: u32 },
            B { b: u64 }
        }
         */
        builder.register_enum("Test", vec![
            ("A", vec![("a", Type::U32)]),
            ("B", vec![("b", Type::U64)])
        ]);

        let env = builder.build();
        assert_eq!(env.get_enums().len(), 1);
    }
}