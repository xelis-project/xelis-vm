mod error;
mod function;
mod context;

use indexmap::IndexSet;
use xelis_types::{EnumType, OpaqueType, StructType};

// Also re-export the necessary macro
pub use better_any::tid;
pub use error::EnvironmentError;
pub use function::*;
pub use context::*;


/// Environment is used to store all the registered functions and structures
/// It is used to give a context/std library to the parser / interpreter / VM
#[derive(Debug, Clone)]
pub struct Environment {
    // All functions provided by the Environment
    functions: Vec<NativeFunction>,
    // All structures provided by the Environment
    structures: IndexSet<StructType>,
    // All enums provided by the Environment
    enums: IndexSet<EnumType>,
    // All opaques types provided by the Environment
    opaques: IndexSet<OpaqueType>,
}

tid!(Environment);

impl Default for Environment {
    fn default() -> Self {
        Self {
            functions: Vec::new(),
            structures: IndexSet::new(),
            enums: IndexSet::new(),
            opaques: IndexSet::new(),
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
    pub fn get_structures(&self) -> &IndexSet<StructType> {
        &self.structures
    }

    // Get all the registered enums
    #[inline(always)]
    pub fn get_enums(&self) -> &IndexSet<EnumType> {
        &self.enums
    }

    // Get all the registered opaques
    #[inline(always)]
    pub fn get_opaques(&self) -> &IndexSet<OpaqueType> {
        &self.opaques
    }

    // Add a new function to the environment
    #[inline(always)]
    pub fn add_function(&mut self, function: NativeFunction) {
        self.functions.push(function);
    }

    // Get a mutable native function by its id
    pub fn get_function_by_id_mut(&mut self, id: usize) -> Option<&mut NativeFunction> {
        self.functions.get_mut(id)
    }

    // Add a new structure to the environment
    #[inline(always)]
    pub fn add_structure(&mut self, structure: StructType) {
        self.structures.insert(structure);
    }

    // Add a new enum to the environment
    #[inline(always)]
    pub fn add_enum(&mut self, _enum: EnumType) {
        self.enums.insert(_enum);
    }

    // Add a new opaque type to the environment
    #[inline(always)]
    pub fn add_opaque(&mut self, opaque: OpaqueType) {
        self.opaques.insert(opaque);
    }

    // Allow to change the cost of a function
    pub fn set_cost_for_function_at_index(&mut self, index: usize, cost: u64) {
        if let Some(function) = self.functions.get_mut(index) {
            function.set_cost(cost);
        }
    }
}