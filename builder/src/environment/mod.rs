pub mod xstd;

use std::{any::TypeId, borrow::Cow, collections::HashMap};
use xelis_types::{Constant, EnumType, EnumVariant, Opaque, OpaqueType, OpaqueTypeTrait, StructType, Type};
use xelis_environment::{Environment, FunctionHandler, NativeFunction};
use crate::{
    BuilderError,
    ConstFnCall,
    ConstFunction,
    ConstFunctionMapper,
    EnumBuilder,
    EnumManager,
    FunctionMapper,
    Hook,
    OpaqueManager,
    StructBuilder,
    StructManager
};

#[derive(Debug, Clone)]
pub struct ConstantInfo<'a> {
    value: (Constant, Type),
    comment: Option<&'a str>,
}

impl<'a> ConstantInfo<'a> {
    pub fn new(value: Constant, value_type: Type, comment: Option<&'a str>) -> Self {
        Self {
            value: (value, value_type),
            comment,
        }
    }

    pub fn value(&self) -> &Constant {
        &self.value.0
    }

    pub fn value_type(&self) -> &Type {
        &self.value.1
    }

    pub fn as_tuple(&self) -> &(Constant, Type) {
        &self.value
    }

    pub fn comment(&self) -> Option<&'a str> {
        self.comment
    }

    pub fn set_comment(&mut self, comment: &'a str) {
        self.comment = Some(comment);
    }
}

// EnvironmentBuilder is used to create an environment
// it is used to register all the native functions and structures
// and import files by the user
#[derive(Debug)]
pub struct EnvironmentBuilder<'a, M> {
    functions_mapper: FunctionMapper<'a>,
    struct_manager: StructManager<'a>,
    enum_manager: EnumManager<'a>,
    opaque_manager: OpaqueManager<'a>,
    // All types constants (like u64::MAX)
    types_constants: HashMap<Type, HashMap<&'a str, ConstantInfo<'a>>>,
    // All types constants functions
    // Example: u64::MAX()
    types_constants_functions: ConstFunctionMapper<'a>,
    // Hooks functions registered in the environment
    hooks: HashMap<&'a str, Hook<'a>>,
    env: Environment<M>
}

impl<'a, M> EnvironmentBuilder<'a, M> {
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

    fn register_function_internal(&mut self, name: &'a str, on_type: Option<Type>, require_instance: bool, parameters: Vec<(&'a str, Type)>, on_call: FunctionHandler<M>, cost: u64, return_type: Option<Type>, comment: Option<&'a str>) {
        let params: Vec<_> = parameters.iter().map(|(_, t)| t.clone()).collect();
        let _ = self.functions_mapper.register_with_comment(name, on_type.clone(), require_instance, parameters, return_type.clone(), cost, comment).unwrap();
        self.env.add_function(NativeFunction::new(on_type, require_instance, params, on_call, cost, return_type));
    }

    // Register a native function
    // Panic if the function signature is already registered
    pub fn register_native_function(&mut self, name: &'a str, for_type: Option<Type>, parameters: Vec<(&'a str, Type)>, on_call: impl Into<FunctionHandler<M>>, cost: u64, return_type: Option<Type>) {
        let instance = for_type.is_some();
        self.register_function_internal(name, for_type, instance, parameters, on_call.into(), cost, return_type, None);
    }

    // Register a native function with a documentation comment
    // Panic if the function signature is already registered
    pub fn register_native_function_with_comment(&mut self, name: &'a str, for_type: Option<Type>, parameters: Vec<(&'a str, Type)>, on_call: impl Into<FunctionHandler<M>>, cost: u64, return_type: Option<Type>, comment: &'a str) {
        let instance = for_type.is_some();
        self.register_function_internal(name, for_type, instance, parameters, on_call.into(), cost, return_type, Some(comment));
    }

    // Register a native static function
    // This is function not accessible from an instance but still behind the type
    // Example: u64::from_be_bytes
    // Panic if the function signature is already registered
    pub fn register_static_function(&mut self, name: &'a str, for_type: Type, parameters: Vec<(&'a str, Type)>, on_call: FunctionHandler<M>, cost: u64, return_type: Option<Type>) {
        self.register_function_internal(name, Some(for_type), false, parameters, on_call, cost, return_type, None);
    }

    // Register a native static function with a documentation comment
    // This is function not accessible from an instance but still behind the type
    // Example: u64::from_be_bytes
    // Panic if the function signature is already registered
    pub fn register_static_function_with_comment(&mut self, name: &'a str, for_type: Type, parameters: Vec<(&'a str, Type)>, on_call: FunctionHandler<M>, cost: u64, return_type: Option<Type>, comment: &'a str) {
        self.register_function_internal(name, Some(for_type), false, parameters, on_call, cost, return_type, Some(comment));
    }

    // Register a constant function
    // This is only available in the builder
    // See this function as a helper
    // Panic if the function signature is already registered
    pub fn register_const_function(&mut self, name: &'a str, for_type: Type, parameters: Vec<(&'a str, Type)>, on_call: ConstFnCall) {
        self.types_constants_functions.register(name, for_type, parameters, on_call).unwrap();
    }

    // Register a constant function with a documentation comment
    // This is only available in the builder
    // See this function as a helper
    // Panic if the function signature is already registered
    pub fn register_const_function_with_comment(&mut self, name: &'a str, for_type: Type, parameters: Vec<(&'a str, Type)>, on_call: ConstFnCall, comment: &'a str) {
        self.types_constants_functions.register_with_comment(name, for_type, parameters, on_call, Some(comment)).unwrap();
    }

    // Set a documentation comment for a registered native or static function
    pub fn set_function_comment(&mut self, name: &str, on_type: Option<Type>, comment: &'a str) -> Result<(), BuilderError> {
        self.functions_mapper.set_comment(name, on_type.as_ref(), comment)
    }

    // Set a documentation comment for a registered constant function
    pub fn set_const_function_comment(&mut self, name: &str, for_type: &Type, comment: &'a str) -> Result<(), BuilderError> {
        self.types_constants_functions.set_comment(for_type, name, comment)
    }

    // Get a native function by its signature
    // Panic if the function signature is not found
    pub fn get_mut_function(&mut self, name: &'a str, on_type: Option<Type>) -> &mut NativeFunction<M> {
        let id = self.functions_mapper.get_by_signature(name, on_type.as_ref())
            .expect("function by signature not found");

        self.env.get_function_by_id_mut(id as usize)
            .expect("native fuction")
    }

    // Register a structure in the environment
    // Panic if the structure name is already used
    pub fn register_structure<const N: usize>(&mut self, name: impl Into<Cow<'static, str>>, fields: [(impl Into<Cow<'static, str>>, Type); N]) -> StructType {
        let fields_data = fields.into_iter()
            .map(|(k, v)| (k.into(), v))
            .collect();
        let data = (Vec::new(), fields_data); // No generics for register_structure
        self.struct_manager.build(name.into(), data)
            .expect("Failed to build struct")
    }

    // Register an enum in the environment
    // Panic if the enum name is already used
    pub fn register_enum<const N: usize>(&mut self, name: impl Into<Cow<'static, str>>, variants: [(impl Into<Cow<'static, str>>, impl Into<EnumVariant>); N]) -> EnumType {
        let variants_data = variants.into_iter()
            .map(|(k, v)| (k.into(), v.into()))
            .collect();
        let data = (Vec::new(), variants_data); // No generics for register_enum

        self.enum_manager.build(name, data)
            .expect("Failed to build enum")
    }

    // Register an opaque type in the environment
    // Panic if the opaque name is already used
    pub fn register_opaque<T: Opaque>(&mut self, name: &'static str, allow_external_input: bool) -> OpaqueType {
        let ty = TypeId::of::<T>();
        let opaque = self.opaque_manager.build::<T>(name, allow_external_input)
            .expect("Unique opaque name");
        self.env.add_opaque(ty, allow_external_input);
        opaque
    }

    // Register a generic opaque type (accepts type parameters, e.g. Iterator<T>)
    // Panic if the opaque name is already used
    pub fn register_generic_opaque<T: Opaque>(&mut self, name: &'static str, allow_external_input: bool, generics_count: u8) -> OpaqueType {
        self.register_generic_opaque_with_traits::<T>(name, allow_external_input, generics_count, &[])
    }

    /// Register a generic opaque type with the given type-level trait markers.
    /// Panic if the opaque name is already used.
    pub fn register_generic_opaque_with_traits<T: Opaque>(
        &mut self,
        name: &'static str,
        allow_external_input: bool,
        generics_count: u8,
        traits: &[OpaqueTypeTrait],
    ) -> OpaqueType {
        let ty = TypeId::of::<T>();
        let opaque = self.opaque_manager
            .build_generic_with_traits::<T>(name, allow_external_input, generics_count, traits)
            .expect("Unique opaque name");
        self.env.add_opaque(ty, allow_external_input);
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
    pub fn register_constant(&mut self, _type: Type, name: &'a str, value: Constant, value_type: Type) {
        self.register_constant_internal(_type, name, value, value_type, None);
    }

    // Register a constant in the environment with a documentation comment
    // Panic if the constant name is already used
    pub fn register_constant_with_comment(&mut self, _type: Type, name: &'a str, value: Constant, value_type: Type, comment: &'a str) {
        self.register_constant_internal(_type, name, value, value_type, Some(comment));
    }

    fn register_constant_internal(&mut self, _type: Type, name: &'a str, value: Constant, value_type: Type, comment: Option<&'a str>) {
        let constants = self.types_constants.entry(_type.clone()).or_insert_with(HashMap::new);
        constants.insert(name, ConstantInfo::new(value, value_type, comment));
    }

    // Get a constant by name
    pub fn get_constant_by_name(&self, _type: &Type, name: &str) -> Option<&(Constant, Type)> {
        self.get_constant_info_by_name(_type, name)
            .map(ConstantInfo::as_tuple)
    }

    // Get full constant metadata by name
    pub fn get_constant_info_by_name(&self, _type: &Type, name: &str) -> Option<&ConstantInfo<'a>> {
        self.types_constants.get(_type).and_then(|v| v.get(name))
    }

    // Get all constants grouped by type
    pub fn get_constants(&self) -> &HashMap<Type, HashMap<&'a str, ConstantInfo<'a>>> {
        &self.types_constants
    }

    // Set a documentation comment for a registered constant
    pub fn set_constant_comment(&mut self, _type: &Type, name: &str, comment: &'a str) -> Result<(), BuilderError> {
        let constant = self.types_constants
            .get_mut(_type)
            .and_then(|v| v.get_mut(name))
            .ok_or(BuilderError::MappingNotFound)?;
        constant.set_comment(comment);
        Ok(())
    }

    // Get a constant function by its name
    pub fn get_const_fn<'b>(&'b self, for_type: &Type, fn_name: &'b str) -> Option<&'b ConstFunction<'b>> {
        self.types_constants_functions.get_const_fn(for_type, fn_name)
    }

    pub fn get_const_functions_mapper(&self) -> &ConstFunctionMapper<'a> {
        &self.types_constants_functions
    }

    // functions mapper, used to find the function id
    pub fn get_functions_mapper(&self) -> &FunctionMapper<'a> {
        &self.functions_mapper
    }

    // struct manager, used to find the struct id
    pub fn get_struct_manager(&self) -> &StructManager<'a> {
        &self.struct_manager
    }

    // all structs registered in this environment
    pub fn get_structs(&self) -> impl Iterator<Item = &StructBuilder> {
        self.struct_manager.iter()
    }

    // enum manager, used to find the enum id
    pub fn get_enum_manager(&self) -> &EnumManager<'a> {
        &self.enum_manager
    }

    // all enums registered in this environment
    pub fn get_enums(&self) -> impl Iterator<Item = &EnumBuilder> {
        self.enum_manager.iter()
    }

    // opaque manager, used to find the opaque id
    pub fn get_opaque_manager(&self) -> &OpaqueManager<'a> {
        &self.opaque_manager
    }

    // all registered functions
    pub fn get_functions(&self) -> &Vec<NativeFunction<M>> {
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
    pub fn environment(&self) -> &Environment<M> {
        &self.env
    }

    // Build the environment for the interpreter
    pub fn build(self) -> Environment<M> {
        self.env
    }

    // Finalize the environment builder and return the environment and the function mapper
    pub fn finalize(self) -> (Environment<M>, FunctionMapper<'a>) {
        (self.env, self.functions_mapper)
    }
}

impl<'a, M: 'static> Default for EnvironmentBuilder<'a, M> {
    fn default() -> Self {
        let mut env = Self::new();

        xstd::register(&mut env);

        env
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use xelis_environment::{FunctionHandler, SysCallResult};
    use xelis_types::{Constant, Primitive};

    #[test]
    pub fn test_add_enum() {
        let mut builder: EnvironmentBuilder<()> = EnvironmentBuilder::new();

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

        let _ = builder.build();
    }

    #[test]
    pub fn test_registered_structs_and_enums_can_be_displayed() {
        let mut builder: EnvironmentBuilder<()> = EnvironmentBuilder::new();

        let person = builder.register_structure("Person", [
            ("name", Type::String),
            ("age", Type::U8),
        ]);
        let foo = builder.register_enum("Foo", [
            ("A", EnumVariant::new(Vec::new())),
            ("B", EnumVariant::new(Vec::new())),
        ]);
        let shape = builder.register_enum("Shape", [
            ("Point", EnumVariant::new_tuple(vec![Type::U32, Type::U32])),
            ("Rect", EnumVariant::new(vec![
                (Cow::Borrowed("width"), Type::U32),
                (Cow::Borrowed("height"), Type::U32),
            ])),
        ]);

        assert_eq!(person.to_string(), "struct Person { name: string, age: u8 }");
        assert_eq!(foo.to_string(), "enum Foo { A, B }");
        assert_eq!(
            shape.to_string(),
            "enum Shape { Point(u32, u32), Rect { width: u32, height: u32 } }"
        );

        assert_eq!(
            builder.get_structs()
                .map(ToString::to_string)
                .collect::<Vec<_>>(),
            vec!["struct Person { name: string, age: u8 }"]
        );
        assert_eq!(
            builder.get_enums()
                .map(ToString::to_string)
                .collect::<Vec<_>>(),
            vec![
                "enum Foo { A, B }",
                "enum Shape { Point(u32, u32), Rect { width: u32, height: u32 } }",
            ]
        );
        assert_eq!(
            builder.get_enum_manager().to_string(),
            "enum Foo { A, B }\nenum Shape { Point(u32, u32), Rect { width: u32, height: u32 } }"
        );
    }

    #[test]
    pub fn test_comments_can_be_registered() {
        let mut builder: EnvironmentBuilder<()> = EnvironmentBuilder::new();

        builder.register_native_function_with_comment(
            "foo",
            None,
            vec![],
            FunctionHandler::Sync(|_, _, _, _| Ok(SysCallResult::None)),
            0,
            None,
            "Runs foo.",
        );

        let id = builder.get_functions_mapper()
            .get_by_signature("foo", None)
            .unwrap();
        let function = builder.get_functions_mapper()
            .get_function(&id)
            .unwrap();
        assert_eq!(function.comment, Some("Runs foo."));

        builder.register_constant_with_comment(
            Type::U8,
            "ANSWER",
            Primitive::U8(42).into(),
            Type::U8,
            "The test answer.",
        );
        assert_eq!(
            builder.get_constant_info_by_name(&Type::U8, "ANSWER")
                .unwrap()
                .comment(),
            Some("The test answer.")
        );

        builder.register_const_function_with_comment(
            "new",
            Type::Bytes,
            vec![],
            |_| Ok(Constant::Bytes(Vec::new())),
            "Creates empty bytes.",
        );
        assert_eq!(
            builder.get_const_fn(&Type::Bytes, "new").unwrap().comment,
            Some("Creates empty bytes.")
        );
    }

    #[test]
    pub fn test_default_environment_items_are_documented() {
        let builder: EnvironmentBuilder<()> = EnvironmentBuilder::default();

        for functions in builder.get_functions_mapper().get_declared_functions().values() {
            for (function, _) in functions {
                assert!(
                    function.comment.map(|v| !v.trim().is_empty()).unwrap_or(false),
                    "missing function comment for {} on {:?}",
                    function.name,
                    function.on_type
                );
            }
        }

        for (ty, constants) in builder.get_constants() {
            for (name, constant) in constants {
                assert!(
                    constant.comment().map(|v| !v.trim().is_empty()).unwrap_or(false),
                    "missing constant comment for {:?}::{}",
                    ty,
                    name
                );
            }
        }

        for (ty, functions) in builder.get_const_functions_mapper().get_mappings() {
            for (name, function) in functions {
                assert!(
                    function.comment.map(|v| !v.trim().is_empty()).unwrap_or(false),
                    "missing const function comment for {:?}::{}",
                    ty,
                    name
                );
            }
        }
    }
}
