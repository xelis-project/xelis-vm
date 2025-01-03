use xelis_builder::{EnumManager, EnvironmentBuilder, FunctionMapper, StructManager};
use std::collections::HashMap;
use crate::ParserErrorKind;

#[derive(Debug)]
pub struct GlobalMapper<'a> {
    functions_mapper: FunctionMapper<'a>,
    struct_manager: StructManager<'a>,
    enum_manager: EnumManager<'a>,
    namespaces: HashMap<&'a str, GlobalMapper<'a>>,
}

impl<'a> GlobalMapper<'a> {
    pub fn new() -> Self {
        Self {
            functions_mapper: FunctionMapper::new(),
            struct_manager: StructManager::new(),
            enum_manager: EnumManager::new(),
            namespaces: HashMap::new(),
        }
    }

    pub fn with(environment: &'a EnvironmentBuilder) -> Self {
        let mut mapper = Self {
            functions_mapper: FunctionMapper::with_parent(environment.get_functions_mapper(&[])),
            struct_manager: StructManager::with_parent(environment.get_struct_manager(&[])),
            enum_manager: EnumManager::with_parent(environment.get_enum_manager(&[])),
            namespaces: HashMap::new(),
        };

        for (name, _) in environment.namespaces() {
            mapper.namespaces.insert(name, GlobalMapper::with(&environment.get_namespace(name)));
        }

        mapper
    }

    pub fn namespace(&mut self, name: String) -> &mut GlobalMapper<'a> {
        let name_ref: &'a str = if let Some(&existing) = self.namespaces.keys().find(|&&k| k == name.as_str()) {
            println!("found");
            existing
        } else {
            println!("creating");
            unsafe { std::mem::transmute(name.as_str()) }
        };
        
        self.namespaces.entry(name_ref).or_insert_with(GlobalMapper::new)
    }

    fn with_namespace<T, F>(&mut self, namespace_path: &[&'a str], f: F) -> Result<T, ParserErrorKind>
    where
        F: FnOnce(&mut GlobalMapper<'a>) -> Result<T, ParserErrorKind<'a>>
    {
        if namespace_path.is_empty() {
            f(self)
        } else {
            let (ns, rest) = namespace_path.split_first().unwrap();
            self.namespace(ns.to_string()).with_namespace(rest, f)
        }
    }

    pub fn functions_in_namespace(&mut self, namespace_path: &[&'a str]) -> &mut FunctionMapper<'a> {
        let mut current = self;
        for ns in namespace_path {
            current = current.namespaces.get_mut(&ns as &str).expect("Namespace Not Found");
        }

        &mut current.functions_mapper
    }

    pub fn structs_in_namespace(&mut self, namespace_path: &[&str]) -> &mut StructManager<'a> {
        let mut current = self;
        for ns in namespace_path {
            current = current.namespaces.get_mut(&ns as &str).expect("Namespace Not Found");
        }  
        
        &mut current.struct_manager
    }

    pub fn enums_in_namespace(&mut self, namespace_path: &[&str]) -> &mut EnumManager<'a> {
        let mut current = self;
        for ns in namespace_path {
            current = current.namespaces.get_mut(&ns as &str).expect("Namespace Not Found");
        }    
      
        &mut current.enum_manager
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use xelis_types::Type;

    #[test]
    fn test_functions() {
        let mut global_mapper = GlobalMapper::new();

        let functions = global_mapper.functions_mut();
        functions.register("test", Some(Type::Any), vec![("name", Type::String)], Some(Type::String)).unwrap();

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

    #[test]
    fn test_namespaced_functions() {
        let mut global_mapper = GlobalMapper::new();

        // Register a function in the global namespace
        global_mapper.functions_mut()
            .register("global_fn", None, vec![("param", Type::U64)], Some(Type::U64))
            .unwrap();

        // Register a function in the "math" namespace
        global_mapper.namespace("math");
        global_mapper.functions_in_namespace(&["math"])
            .register("add", None, vec![("a", Type::U64), ("b", Type::U64)], Some(Type::U64))
            .unwrap();

        // Register a function in the "math::advanced" namespace
        global_mapper.namespace("math").namespace("advanced");
        global_mapper.functions_in_namespace(&["math", "advanced"])
            .register("complex_op", None, vec![("x", Type::U64)], Some(Type::U64))
            .unwrap();

        // Verify functions in different namespaces
        assert!(global_mapper.functions().get_declared_functions().len() == 1);
        assert!(global_mapper.functions_in_namespace(&["math"]).get_declared_functions()
            .len() == 1);
        assert!(global_mapper.functions_in_namespace(&["math", "advanced"]).get_declared_functions()
            .len() == 1);
    }
}