pub mod xstd;

use std::{any::TypeId, borrow::Cow, collections::HashMap};
use xelis_ast::Signature;
use xelis_types::{Constant, EnumType, EnumVariant, Opaque, OpaqueType, StructType, Type};
use xelis_environment::{Environment, NativeFunction, OnCallFn};
use crate::{
    ConstFnCall,
    ConstFunction,
    ConstFunctionMapper,
    EnumManager,
    FunctionMapper,
    Hook,
    OpaqueManager,
    StructManager
};

// EnvironmentBuilder is used to create an environment
// it is used to register all the native functions and structures
// and import files by the user
pub struct EnvironmentBuilder<'a> {
    functions_mapper: FunctionMapper<'a>,
    struct_manager: StructManager<'a>,
    enum_manager: EnumManager<'a>,
    opaque_manager: OpaqueManager<'a>,
    // All types constants (like u64::MAX)
    types_constants: HashMap<Type, HashMap<&'a str, Constant>>,
    // All types constants functions
    // Example: u64::MAX()
    types_constants_functions: ConstFunctionMapper<'a>,
    // Hooks functions registered in the environment
    hooks: HashMap<&'a str, Hook<'a>>,
    env: Environment
}

impl<'a> EnvironmentBuilder<'a> {
    // Create a new instance of the EnvironmentBuilder
    pub fn new() -> Self {
        Self {
            functions_mapper: FunctionMapper::new(),
            struct_manager: StructManager::new(),
            enum_manager: EnumManager::new(),
            opaque_manager: OpaqueManager::new(),
            types_constants: HashMap::new(),
            types_constants_functions: ConstFunctionMapper::new(),
            hooks: HashMap::new(),
            env: Environment::new(),
        }
    }

    fn register_function_internal(&mut self, name: &'a str, on_type: Option<Type>, require_instance: bool, parameters: Vec<(&'a str, Type)>, on_call: OnCallFn, cost: u64, return_type: Option<Type>) {
        let params: Vec<_> = parameters.iter().map(|(_, t)| t.clone()).collect();
        let _ = self.functions_mapper.register(name, on_type.clone(), require_instance, parameters, return_type.clone()).unwrap();
        self.env.add_function(NativeFunction::new(require_instance, params, on_call, cost, return_type));
    }

    // Register a native function
    // Panic if the function signature is already registered
    pub fn register_native_function(&mut self, name: &'a str, for_type: Option<Type>, parameters: Vec<(&'a str, Type)>, on_call: OnCallFn, cost: u64, return_type: Option<Type>) {
        let instance = for_type.is_some();
        self.register_function_internal(name, for_type, instance, parameters, on_call, cost, return_type);
    }

    // Register a native static function
    // This is function not accessible from an instance but still behind the type
    // Example: u64::from_be_bytes
    // Panic if the function signature is already registered
    pub fn register_static_function(&mut self, name: &'a str, for_type: Type, parameters: Vec<(&'a str, Type)>, on_call: OnCallFn, cost: u64, return_type: Option<Type>) {
        self.register_function_internal(name, Some(for_type), false, parameters, on_call, cost, return_type);
    }

    // Register a constant function
    // This is only available in the builder
    // See this function as a helper
    // Panic if the function signature is already registered
    pub fn register_const_function(&mut self, name: &'a str, for_type: Type, parameters: Vec<(&'a str, Type)>, on_call: ConstFnCall) {
        self.types_constants_functions.register(name, for_type, parameters, on_call).unwrap();
    }

    // Get a native function by its signature
    // Panic if the function signature is not found
    pub fn get_mut_function(&mut self, name: &'a str, on_type: Option<(Type, bool)>, parameters: Vec<Type>) -> &mut NativeFunction {
        let signature = Signature::new(Cow::Borrowed(name), on_type.map(|(t, v)| (Cow::Owned(t), v)), Cow::Owned(parameters));
        let id = self.functions_mapper.get(&signature).unwrap();
        self.env.get_function_by_id_mut(id as usize).unwrap()

    }

    // Register a structure in the environment
    // Panic if the structure name is already used
    pub fn register_structure<const N: usize>(&mut self, name: impl Into<Cow<'static, str>>, fields: [(impl Into<Cow<'static, str>>, Type); N]) -> StructType {
        let data = fields.into_iter()
            .map(|(k, v)| (k.into(), v))
            .collect();
        let _type = self.struct_manager.build(name.into(), data).expect("Failed to build struct");
        self.env.add_structure(_type.clone());
        _type
    }

    // Register an enum in the environment
    // Panic if the enum name is already used
    pub fn register_enum<const N: usize>(&mut self, name: impl Into<Cow<'static, str>>, variants: [(impl Into<Cow<'static, str>>, impl Into<EnumVariant>); N]) -> EnumType {
        let data = variants.into_iter()
            .map(|(k, v)| (k.into(), v.into()))
            .collect();

        let _type = self.enum_manager.build(name, data).expect("Failed to build enum");
        self.env.add_enum(_type.clone());
        _type
    }

    // Register an opaque type in the environment
    // Panic if the opaque name is already used
    pub fn register_opaque<T: Opaque>(&mut self, name: &'a str, allow_external_input: bool) -> OpaqueType {
        let ty = TypeId::of::<T>();
        let opaque = self.opaque_manager.build(name, allow_external_input)
            .expect("Unique opaque name");
        self.env.add_opaque(ty);
        opaque
    }

    // Get an opaque type by name
    pub fn get_opaque_by_name(&self, name: &str) -> Option<&OpaqueType> {
        self.opaque_manager.get_by_name(name)
    }

    // Get the name of an opaque type
    pub fn get_opaque_name(&self, ty: &OpaqueType) -> Option<&str> {
        self.opaque_manager.get_name_of(ty)
    }

    // Register a constant in the environment
    // Panic if the constant name is already used
    pub fn register_constant(&mut self, _type: Type, name: &'a str, value: Constant) {
        let constants = self.types_constants.entry(_type.clone()).or_insert_with(HashMap::new);
        constants.insert(name, value.clone());
    }

    // Get a constant by name
    pub fn get_constant_by_name(&self, _type: &Type, name: &str) -> Option<&Constant> {
        self.types_constants.get(_type).and_then(|v| v.get(name))
    }

    // Get a constant function by its name
    pub fn get_const_fn(&self, for_type: &Type, fn_name: &'a str) -> Option<&ConstFunction> {
        self.types_constants_functions.get_const_fn(for_type, fn_name)
    }

    pub fn get_const_functions_mapper(&self) -> &ConstFunctionMapper<'a> {
        &self.types_constants_functions
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

    // all hooks registered
    pub fn get_hooks(&self) -> &HashMap<&'a str, Hook<'a>> {
        &self.hooks
    }

    // Register a hook
    pub fn register_hook(&mut self, name: &'a str, parameters: Vec<(&'a str, Type)>, return_type: Option<Type>) {
        let hook_id = self.hooks.len() as _;
        self.hooks.insert(name, Hook { parameters, return_type, hook_id });
        self.env.register_hook();
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
        builder.register_enum("Test", [
            ("A", EnumVariant::new(vec![(Cow::Borrowed("a"), Type::U32)])),
            ("B", EnumVariant::new(vec![(Cow::Borrowed("b"), Type::U64)])),
        ]);

        let env = builder.build();
        assert_eq!(env.get_enums().len(), 1);
    }
}