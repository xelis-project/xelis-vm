pub mod xstd;

use indexmap::IndexMap;

use std::{borrow::Cow, collections::HashMap};
use xelis_ast::Signature;
use xelis_types::{Constant, EnumType, OpaqueType, Opaque, StructType, Type, IdentifierType};
use xelis_environment::{Environment, NativeFunction, OnCallFn};
use crate::{ConstFnCall, ConstFunction, ConstFunctionMapper, EnumManager, EnumVariantBuilder, FunctionMapper, StructManager};

use crate::BuilderError;

// EnvironmentBuilder is used to create an environment
// it is used to register all the native functions and structures
// and import files by the user
pub struct EnvironmentBuilder<'a> {
    functions_mapper: FunctionMapper<'a>,
    struct_manager: StructManager<'a>,
    enum_manager: EnumManager<'a>,
    opaque_manager: HashMap<&'a str, OpaqueType>,
    // All types constants (like u64::MAX)
    types_constants: HashMap<Type, HashMap<&'a str, Constant>>,
    // All types constants functions
    // Example: u64::MAX()
    types_constants_functions: ConstFunctionMapper<'a>,
    namespaces: IndexMap<String, EnvironmentBuilder<'a>>,
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
            types_constants: HashMap::new(),
            types_constants_functions: ConstFunctionMapper::new(),
            namespaces: IndexMap::new(),
            env: Environment::new(),
        }
    }

    pub fn namespaces(&self) -> &IndexMap<String, EnvironmentBuilder<'a>> {
        &self.namespaces
    }

    // Get or create a new namespace
    pub fn namespace(&mut self, name: String) -> &mut EnvironmentBuilder<'a> {
        self.namespaces.entry(name).or_insert_with(|| EnvironmentBuilder::default())
    }

    pub fn namespace_from_string(&mut self, name: String) -> &mut EnvironmentBuilder<'a> {
        self.namespaces.entry(name).or_insert_with(EnvironmentBuilder::new)
    }

    pub fn get_namespace(&self, namespace_path: &[String]) -> Result<&EnvironmentBuilder<'a>, BuilderError> {
        let mut current = self;
        for ns in namespace_path {
            match current.namespaces.get(ns) {
                Some(child) => current = child,
                None => return Err(BuilderError::NamespaceNotFound),
            }
        }
        Ok(current)
    }

    pub fn namespace_exists(&self, namespace_path: &[String]) -> bool {
        let mut current = self;
        for ns in namespace_path {
            match current.namespaces.get(ns) {
                Some(child) => current = child,
                None => return false,
            }
        }
        true
    }

    pub fn list_namespaces(&mut self, namespace_path: &[String]) -> Result<Vec<String>, BuilderError> {
        self.with_namespace(namespace_path, |namespace| {
            Ok(namespace.namespaces.keys().cloned().collect())
        })
    }

    // Recursive helper function to locate & execute namespace operations
    fn with_namespace<T, F>(&self, namespace_path: &[String], f: F) -> Result<T, BuilderError>
    where
        F: FnOnce(&EnvironmentBuilder<'a>) -> Result<T, BuilderError>,
    {
        if namespace_path.is_empty() {
            f(self)
        } else {
            let (ns, rest) = namespace_path.split_first().unwrap();
            self.namespaces.get(ns)
                .ok_or(BuilderError::NamespaceNotFound)?
                .with_namespace(rest, f)
        }
    }

    // Mutable version
    fn with_namespace_mut<T, F>(&mut self, namespace_path: &[String], f: F) -> Result<T, BuilderError>
    where
        F: FnOnce(&mut EnvironmentBuilder<'a>) -> Result<T, BuilderError>,
    {
        if namespace_path.is_empty() {
            f(self)
        } else {
            let (ns, rest) = namespace_path.split_first().unwrap();
            self.namespace(ns.to_string()).with_namespace_mut(rest, f)
        }
    }

    // Register a native function
    // Panic if the function signature is already registered
    pub fn register_native_function(&mut self, namespace_path: &[String], name: &'a str, for_type: Option<Type>, parameters: Vec<(&'a str, Type)>, on_call: OnCallFn, cost: u64, return_type: Option<Type>) {
        self.with_namespace_mut(namespace_path, |namespace| {  
            let params: Vec<_> = parameters.iter().map(|(_, t)| t.clone()).collect();
            let _ = namespace.functions_mapper.register(name, for_type.clone(), parameters, return_type.clone()).unwrap();
            namespace.env.add_function(NativeFunction::new(for_type, params, on_call, cost, return_type));
            Ok(())
        });
    }

    // Register a constant function
    // This is only available in the builder
    // See this function as a helper
    // Panic if the function signature is already registered
    pub fn register_const_function(&mut self, namespace_path: &[String], name: &'a str, for_type: Type, parameters: Vec<(&'a str, Type)>, on_call: ConstFnCall) {
        self.with_namespace_mut(namespace_path, |mut namespace| {  
            namespace.types_constants_functions.register(name, for_type, parameters, on_call);
            Ok(())
        });
    }

    // Get a function by its signature
    // Panic if the function signature is not found
    pub fn get_mut_function(&mut self, namespace_path: &[&str], name: &str, on_type: Option<Type>, parameters: Vec<Type>) -> Result<&mut NativeFunction, BuilderError> {        
        // Then use the identifier to get a mutable reference
        let mut current = self;
        for ns in namespace_path {
            current = current.namespaces.get_mut(ns.clone())
                .ok_or(BuilderError::NamespaceNotFound)?;
        }
        let signature = Signature::new(name.to_owned(), on_type.clone(), parameters);
        let id = current.functions_mapper.get(&signature)?;
        
        current.env.get_function_by_id_mut(id as usize)
            .ok_or(BuilderError::MappingNotFound)
    }

    // Register a structure in the environment
    // Panic if the structure name is already used
    pub fn register_structure(&mut self, namespace_path: &[String], name: &'a str, fields: Vec<(&'a str, Type)>) -> Result<StructType, BuilderError> {
        self.with_namespace_mut(namespace_path, |namespace| {
            let struct_type = namespace.struct_manager.build(Cow::Borrowed(name), fields)?;
            namespace.env.add_structure(struct_type.clone());
            Ok(struct_type)
        })
    }

    // Register an enum in the environment
    // Panic if the enum name is already used
    pub fn register_enum(&mut self, namespace_path: &[String], name: &'a str, variants: Vec<(&'a str, EnumVariantBuilder<'a>)>) -> Result<EnumType, BuilderError> {
        self.with_namespace_mut(namespace_path, |namespace| {
            let enum_type = namespace.enum_manager.build(Cow::Borrowed(name), variants)?;
            namespace.env.add_enum(enum_type.clone());
            Ok(enum_type)
        })
    }

    // Register an opaque type in the environment
    // Panic if the opaque name is already used
    pub fn register_opaque<T: Opaque>(&mut self, namespace_path: &[String], name: &'a str) -> OpaqueType {
        self.with_namespace_mut(namespace_path, |namespace| {
            let _type = OpaqueType::new::<T>();
            namespace.opaque_manager.insert(name, _type.clone());
            namespace.env.add_opaque(_type.clone());
            Ok(_type)
        }).expect("Unknown Namespace Error")
    }

    // Get an opaque type by name
    pub fn get_opaque_by_name(&self, namespace_path: &[String], name: &'a str) -> Option<&OpaqueType> {          
        let mut current = self;
        for ns in namespace_path {
            current = current.namespaces.get(ns)?;
        }

        current.opaque_manager.get(name)
    }

    // Get the name of an opaque type
    pub fn get_opaque_name(&self, namespace_path: &[String], _type: &OpaqueType) -> Option<&str> {
        self.with_namespace(namespace_path, |namespace| {
            Ok(namespace.opaque_manager.iter().find_map(|(k, v)| if v == _type { Some(*k) } else { None }))
        }).expect("Unknown Namespace Error")
    }

    // Register a constant in the environment
    // Panic if the constant name is already used
    pub fn register_constant(&mut self, namespace_path: &[String], _type: Type, name: &'a str, value: Constant) {
        self.with_namespace_mut(namespace_path, |namespace| {
            let constants = namespace.types_constants.entry(_type.clone()).or_insert_with(HashMap::new);
            constants.insert(name, value.clone());
            Ok(())
        });
    }

    // Get a constant by name
    pub fn get_constant_by_name(&self, namespace_path: &[String], type_: &Type, name: &'a str) -> Option<&Constant> {        
        let mut current = self;
        for ns in namespace_path {
            current = current.namespaces.get(&ns.clone())?;
        }
        
        current.types_constants.get(&type_)?.get(&name)
    }

    // Get a constant function by its name
    pub fn get_const_fn(&self, namespace_path: &[&str], for_type: &Type, fn_name: &'a str) -> Option<&ConstFunction> {        
        let mut current = self;
        for ns in namespace_path {
            current = current.namespaces.get(ns.clone())?;
        }
        
        current.types_constants_functions.get_const_fn(&for_type, fn_name)
    }

    pub fn get_const_functions_mapper(&self, namespace_path: &[String]) -> &ConstFunctionMapper<'a> {
        let mut current = self;
        for ns in namespace_path {
            current = current.namespaces.get(ns).expect("Namespace not found");
        }  

        &current.types_constants_functions
    }

    // functions mapper, used to find the function id
    pub fn get_functions_mapper(&self, namespace_path: &[String]) -> &FunctionMapper {
        let mut current = self;
        for ns in namespace_path {
            current = current.namespaces.get(ns).expect("Namespace not found");
        }  

        &current.functions_mapper
    }

    // struct manager, used to find the struct id
    pub fn get_struct_manager(&self, namespace_path: &[String]) -> &StructManager {
        let mut current = self;
        for ns in namespace_path {
            current = current.namespaces.get(ns).expect("Namespace not found");
        }
        &current.struct_manager
    }

    // enum manager, used to find the enum id
    pub fn get_enum_manager(&self, namespace_path: &[String]) -> &EnumManager {
        let mut current = self;
        for ns in namespace_path {
            current = current.namespaces.get(ns).expect("Namespace not found");
        }
        &current.enum_manager
    }

    // all registered functions
    pub fn get_functions(&self, namespace_path: &[String]) -> &Vec<NativeFunction> {
        let mut current = self;
        for ns in namespace_path {
            current = current.namespaces.get(ns).expect("Namespace not found");
        }
        &current.env.get_functions()
    }

    // Get the environment for the interpreter
    pub fn environment(&self, namespace_path: &[String]) -> &Environment {
        let mut current = self;
        for ns in namespace_path {
            current = current.namespaces.get(ns).expect("Namespace not found");
        }
        &current.env
    }

    // Build the environment for the interpreter
    pub fn build(self) -> Environment {
        self.env
    }
    
    pub fn finalize(self) -> Result<(Environment, FunctionMapper<'a>), BuilderError> {
        Ok((self.env, self.functions_mapper))
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
    use xelis_types::Value;

    #[test]
    pub fn test_add_enum() {
        let mut builder = EnvironmentBuilder::new();
    
        builder.register_enum(&[], "Test", vec![
            ("A", vec![("a", Type::U32)]),
            ("B", vec![("b", Type::U64)])
        ]).expect("Failed to register enum");
    
        let env = builder.build();
        assert_eq!(env.get_enums().len(), 1);
    }

    #[test]
    pub fn test_add_namespaced_enum() {
        let mut builder = EnvironmentBuilder::new();
    
        builder.register_enum(&["math"], "Operation", vec![
            ("Add", vec![]),
            ("Subtract", vec![])
        ]).expect("Failed to register enum");
    
        let env = builder.environment(&["math"]);
        assert_eq!(env.get_enums().len(), 1);
    }

    #[test]
    fn test_namespace_layout() {
        let mut builder = EnvironmentBuilder::new();
    
        // Global items
        builder.register_structure(&[], "GlobalStruct", vec![
            ("field1", Type::U64),
        ]).expect("Failed to register global struct");
    
        // Math namespace
        builder.register_enum(&["math"], "Operation", vec![
            ("Add", vec![]),
            ("Subtract", vec![]),
        ]).expect("Failed to register math enum");
        
        builder.register_constant(&["math"], Type::U64, "PI", Constant::Default(Value::U64(3)));
    
        // Math::Complex namespace
        builder.register_structure(&["math", "complex"], "Complex", vec![
            ("real", Type::U64),
            ("imag", Type::U64),
        ]).expect("Failed to register complex struct");
    
        // String namespace
        builder.register_constant(&["string"], Type::U64, "MAX_LENGTH", Constant::Default(Value::U64(1024)));
        
        // Network namespace
        builder.register_enum(&["network"], "Protocol", vec![
            ("TCP", vec![("port", Type::U16)]),
            ("UDP", vec![("port", Type::U16)]),
        ]).expect("Failed to register network enum");
    
        // Network::HTTP namespace
        builder.register_enum(&["network", "http"], "Method", vec![
            ("GET", vec![]),
            ("POST", vec![]),
        ]).expect("Failed to register http enum");
    
        // Assertions
        
        // Global namespace
        assert_eq!(builder.environment(&[]).get_structures().len(), 1);
        assert!(builder.struct_manager.get_by_name("GlobalStruct").is_ok());
    
        // Math namespace
        assert_eq!(builder.environment(&["math"]).get_enums().len(), 1);
        assert!(builder.get_enum_manager(&["math"]).get_by_name("Operation").is_ok());
        assert_eq!(
            builder.get_constant_by_name(&["math"], &Type::U64, "PI"),
            Some(&Constant::Default(Value::U64(3)))
        );
    
        // Math::Complex namespace
        assert!(builder.get_struct_manager(&["math", "complex"]).get_by_name("Complex").is_ok());
    
        // String namespace
        assert_eq!(
            builder.get_constant_by_name(&["string"], &Type::U64, "MAX_LENGTH"),
            Some(&Constant::Default(Value::U64(1024)))
        );
    
        // Network namespace
        assert!(builder.get_enum_manager(&["network"]).get_by_name("Protocol").is_ok());
    
        // Network::HTTP namespace
        assert!(builder.get_enum_manager(&["network", "http"]).get_by_name("Method").is_ok());
    
        // Check namespace existence
        assert!(builder.namespace_exists(&["math"]));
        assert!(builder.namespace_exists(&["math", "complex"]));
        assert!(builder.namespace_exists(&["string"]));
        assert!(builder.namespace_exists(&["network"]));
        assert!(builder.namespace_exists(&["network", "http"]));
        assert!(!builder.namespace_exists(&["nonexistent"]));
    
        use std::collections::HashSet;

        // Check namespace listings
        let root_namespaces: HashSet<_> = builder.list_namespaces(&[]).unwrap().into_iter().collect();
        assert_eq!(
            root_namespaces,
            ["math", "string", "network"].iter().copied().collect::<HashSet<_>>()
        );
    
        let math_namespaces: HashSet<_> = builder.list_namespaces(&["math"]).unwrap().into_iter().collect();
        assert_eq!(
            math_namespaces,
            ["complex"].iter().copied().collect::<HashSet<_>>()
        );
    
        let network_namespaces: HashSet<_> = builder.list_namespaces(&["network"]).unwrap().into_iter().collect();
        assert_eq!(
            network_namespaces,
            ["http"].iter().copied().collect::<HashSet<_>>()
        );
    
        println!("\nNamespace Layout:");
        print_namespace_layout(&builder, 0);
    }

    fn print_namespace_layout(builder: &EnvironmentBuilder, indent: usize) {
        let indent_str = "  ".repeat(indent);
        
        // Print structures
        for struct_type in builder.environment(&[]).get_structures() {
            if let Ok(name) = builder.struct_manager.get_name_by_ref(struct_type) {
                println!("{}Struct: {}", indent_str, name);
                for (i, field_type) in struct_type.fields().iter().enumerate() {
                    println!("{}  Field {}: {:?}", indent_str, i, field_type);
                }
            }
        }

        // Print enums
        for enum_type in builder.environment(&[]).get_enums() {
            if let Ok(name) = builder.enum_manager.get_name_by_ref(enum_type) {
                println!("{}Enum: {}", indent_str, name);
                for (i, variant) in enum_type.variants().iter().enumerate() {
                    println!("{}  Variant {}: {:?}", indent_str, i, variant);
                }
            }
        }

        // Print constants
        for (type_, constants) in &builder.types_constants {
            for (name, value) in constants {
                println!("{}Constant: {}::{} = {:?}", indent_str, type_, name, value);
            }
        }

        // Print namespaces and their contents
        for (name, namespace) in &builder.namespaces {
            println!("{}Namespace: {}", indent_str, name);
            print_namespace_layout(namespace, indent + 1);
        }
    }
}