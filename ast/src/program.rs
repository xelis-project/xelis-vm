use std::collections::HashSet;
use types::Struct;
use super::{DeclarationStatement, FunctionType};

#[derive(Debug)]
pub struct Program {
    // All constants declared
    constants: HashSet<DeclarationStatement>,
    // All structures declared
    structures: Vec<Struct>,
    // All functions declared
    functions: Vec<FunctionType>
}

impl Program {
    // Create a new program
    pub fn new() -> Self {
        Program {
            constants: HashSet::new(),
            structures: Vec::new(),
            functions: Vec::new()
        }
    }

    // Create a new program with constants, structures and functions
    pub fn with(constants: HashSet<DeclarationStatement>, structures: Vec<Struct>, functions: Vec<FunctionType>) -> Self {
        Program {
            constants,
            structures,
            functions
        }
    }

    // Add a constant to the program
    #[inline]
    pub fn add_constant(&mut self, constant: DeclarationStatement) {
        self.constants.insert(constant);
    }

    // Get the constants declared in the program
    #[inline]
    pub fn constants(&self) -> &HashSet<DeclarationStatement> {
        &self.constants
    }

    // Add a structure to the program
    #[inline]
    pub fn add_structure(&mut self, structure: Struct) {
        self.structures.push(structure);
    }

    // Get the structures declared in the program
    #[inline]
    pub fn structures(&self) -> &[Struct] {
        &self.structures
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