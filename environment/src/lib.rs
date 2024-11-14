mod error;
mod function;

pub use error::EnvironmentError;
pub use function::*;

use xelis_types::{EnumType, StructType};

/// Environment is used to store all the registered functions and structures
/// It is used to give a context/std library to the parser / interpreter / VM
pub struct Environment {
    // All functions provided by the Environment
    functions: Vec<NativeFunction>,
    // All structures provided by the Environment
    structures: Vec<StructType>,
    // All enums provided by the Environment
    enums: Vec<EnumType>,
}

impl Default for Environment {
    fn default() -> Self {
        Self {
            functions: Vec::new(),
            structures: Vec::new(),
            enums: Vec::new(),
        }
    }
}

impl Environment {
    // Create a new environment
    pub fn new() -> Self {
        Self::default()
    }

    // Get all the registered functions
    #[inline(always)]
    pub fn get_functions(&self) -> &Vec<NativeFunction> {
        &self.functions
    }

    // Get all the registered structures
    #[inline(always)]
    pub fn get_structures(&self) -> &Vec<StructType> {
        &self.structures
    }

    // Get all the registered enums
    #[inline(always)]
    pub fn get_enums(&self) -> &Vec<EnumType> {
        &self.enums
    }

    // Add a new function to the environment
    #[inline(always)]
    pub fn add_function(&mut self, function: NativeFunction) {
        self.functions.push(function);
    }

    // Add a new structure to the environment
    #[inline(always)]
    pub fn add_structure(&mut self, structure: StructType) {
        self.structures.push(structure);
    }

    // Add a new enum to the environment
    #[inline(always)]
    pub fn add_enum(&mut self, _enum: EnumType) {
        self.enums.push(_enum);
    }

    // Allow to change the cost of a function
    pub fn set_cost_for_function_at_index(&mut self, index: usize, cost: u64) {
        if let Some(function) = self.functions.get_mut(index) {
            function.set_cost(cost);
        }
    }
}