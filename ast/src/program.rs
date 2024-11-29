use indexmap::IndexSet;
use xelis_types::{EnumType, StructType};
use crate::ConstantDeclaration;

use super::FunctionType;

#[derive(Debug)]
pub struct Program {
    // All constants declared
    constants: IndexSet<ConstantDeclaration>,
    // All structures declared
    structures: IndexSet<StructType>,
    // All enums declared
    enums: IndexSet<EnumType>,
    // All functions declared
    functions: Vec<FunctionType>
}

impl Program {
    // Create a new program
    pub fn new() -> Self {
        Self {
            constants: IndexSet::new(),
            structures: IndexSet::new(),
            enums: IndexSet::new(),
            functions: Vec::new()
        }
    }

    // Create a new program with constants, structures and functions
    pub fn with(constants: IndexSet<ConstantDeclaration>, structures: IndexSet<StructType>, enums: IndexSet<EnumType>, functions: Vec<FunctionType>) -> Self {
        Program {
            constants,
            structures,
            enums,
            functions
        }
    }

    // Add a constant to the program
    #[inline]
    pub fn add_constant(&mut self, constant: ConstantDeclaration) {
        self.constants.insert(constant);
    }

    // Get the constants declared in the program
    #[inline]
    pub fn constants(&self) -> &IndexSet<ConstantDeclaration> {
        &self.constants
    }

    // Add a structure to the program
    #[inline]
    pub fn add_structure(&mut self, structure: StructType) {
        self.structures.insert(structure);
    }

    // Get the structures declared in the program
    #[inline]
    pub fn structures(&self) -> &IndexSet<StructType> {
        &self.structures
    }

    // Add an enum to the program
    #[inline]
    pub fn add_enum(&mut self, enum_type: EnumType) {
        self.enums.insert(enum_type);
    }

    // Get the enums declared in the program
    #[inline]
    pub fn enums(&self) -> &IndexSet<EnumType> {
        &self.enums
    }

    // Add a function to the program
    #[inline]
    pub fn add_function(&mut self, function: FunctionType) {
        self.functions.push(function);
    }

    // Get the functions declared in the program
    #[inline]
    pub fn functions(&self) -> &[FunctionType] {
        &self.functions
    }
}