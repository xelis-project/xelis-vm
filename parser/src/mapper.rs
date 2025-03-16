use xelis_builder::{EnumManager, EnvironmentBuilder, FunctionMapper, StructManager, NamespaceManager};
use std::borrow::Cow;

#[derive(Debug)]
pub struct GlobalMapper<'a> {
    functions_mapper: FunctionMapper<'a>,
    struct_manager: StructManager<'a>,
    enum_manager: EnumManager<'a>,
    namespace_manager: NamespaceManager<'a>, // for management of namespace definitions at the current path
}

impl<'a> GlobalMapper<'a> {
    pub fn new() -> Self {
        let mut namespace_manager = NamespaceManager::new();
        let _ = namespace_manager.add(Cow::Borrowed(""), &Vec::new(), Vec::new());

        Self {
            functions_mapper: FunctionMapper::new(),
            struct_manager: StructManager::new(),
            enum_manager: EnumManager::new(),
            namespace_manager,
        }
    }

    pub fn with(environment: &'a EnvironmentBuilder) -> Self {
        let mut namespace_manager = NamespaceManager::new();
        let _ = namespace_manager.add(Cow::Borrowed(""), &Vec::new(), Vec::new());

        let mapper = Self {
            functions_mapper: FunctionMapper::with_parent(environment.get_functions_mapper()),
            struct_manager: StructManager::with_parent(environment.get_struct_manager()),
            enum_manager: EnumManager::with_parent(environment.get_enum_manager()),
            namespace_manager,
        };

        mapper
    }

    pub fn functions(&self) -> &FunctionMapper<'a> {
        &self.functions_mapper
    }

    pub fn functions_mut(&mut self) -> &mut FunctionMapper<'a> {
        &mut self.functions_mapper
    }

    pub fn structs(&self) -> &StructManager<'a> {
        &self.struct_manager
    }

    pub fn structs_mut(&mut self) -> &mut StructManager<'a> {
        &mut self.struct_manager
    }

    pub fn enums(&self) -> &EnumManager<'a> {
        &self.enum_manager
    }

    pub fn enums_mut(&mut self) -> &mut EnumManager<'a> {
        &mut self.enum_manager
    }

    pub fn namespaces(&self) -> &NamespaceManager<'a> {
        &self.namespace_manager
    }

    pub fn namespaces_mut(&mut self) -> &mut NamespaceManager<'a> {
        &mut self.namespace_manager
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use xelis_types::Type;

    #[test]
    fn test_function_instance() {
        let mut global_mapper = GlobalMapper::new();

        let functions = global_mapper.functions_mut();
        functions.register("test", Some(Type::Any), true, vec![("name", Type::String)], Some(Type::String)).unwrap();

        let results = functions.get_functions_for_type(Some(&Type::String));
        assert_eq!(results.len(), 1);

        assert_eq!(
            results[0].name,
            "test"
        );
        assert_eq!(
            results[0].parameters,
            vec![("name", Type::String)]
        );
        assert_eq!(
            results[0].return_type,
            Some(Type::String)
        );
    }

    // TODO: refactor this test for the improved namespace handling structure
    // #[test]
    // fn test_namespaced_functions() {
    //     let mut global_mapper = GlobalMapper::new();

    //     // Register a function in the global namespace
    //     global_mapper.functions_mut()
    //         .register("global_fn", None, vec![("param", Type::U64)], Some(Type::U64))
    //         .unwrap();

    //     // Register a function in the "math" namespace
    //     global_mapper.namespace("math");
    //     global_mapper.functions_in_namespace(&["math"])
    //         .register("add", None, vec![("a", Type::U64), ("b", Type::U64)], Some(Type::U64))
    //         .unwrap();

    //     // Register a function in the "math::advanced" namespace
    //     global_mapper.namespace("math").namespace("advanced");
    //     global_mapper.functions_in_namespace(&["math", "advanced"])
    //         .register("complex_op", None, vec![("x", Type::U64)], Some(Type::U64))
    //         .unwrap();

    //     // Verify functions in different namespaces
    //     assert!(global_mapper.functions().get_declared_functions().len() == 1);
    //     assert!(global_mapper.functions_in_namespace(&["math"]).get_declared_functions()
    //         .len() == 1);
    //     assert!(global_mapper.functions_in_namespace(&["math", "advanced"]).get_declared_functions()
    //         .len() == 1);
    // }

  #[test]
    fn test_static_function_on_type() {
        let mut global_mapper = GlobalMapper::new();

        let functions = global_mapper.functions_mut();
        functions.register("test", Some(Type::Any), false, vec![("name", Type::String)], Some(Type::String)).unwrap();

        let results = functions.get_functions_for_type(Some(&Type::Any));
        assert_eq!(results.len(), 1);

        assert_eq!(
            results[0].name,
            "test"
        );
        assert_eq!(
            results[0].parameters,
            vec![("name", Type::String)]
        );
        assert_eq!(
            results[0].return_type,
            Some(Type::String)
        );
    }
}