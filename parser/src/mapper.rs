use xelis_builder::{EnumManager, EnvironmentBuilder, FunctionMapper, StructManager};

#[derive(Debug)]
pub struct GlobalMapper<'a> {
    functions_mapper: FunctionMapper<'a>,
    struct_manager: StructManager<'a>,
    enum_manager: EnumManager<'a>,
}

impl<'a> GlobalMapper<'a> {
    pub fn new() -> Self {
        Self {
            functions_mapper: FunctionMapper::new(),
            struct_manager: StructManager::new(),
            enum_manager: EnumManager::new(),
        }
    }

    pub fn with(environment: &'a EnvironmentBuilder) -> Self {
        Self {
            functions_mapper: FunctionMapper::with_parent(environment.get_functions_mapper()),
            struct_manager: StructManager::with_parent(environment.get_struct_manager()),
            enum_manager: EnumManager::with_parent(environment.get_enum_manager()),
        }
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
}