mod chunk;
mod opcode;
pub mod vm;
pub mod compiler;
pub mod assembler;

pub use chunk::Chunk;
pub use opcode::OpCode;

use crate::{types::Struct, Value};

// A module is a collection of declared chunks, constants and structs
// It represents a program compiled in bytecode
pub struct Module {
    // TODO use a IndexSet
    constants: Vec<Value>,
    // Available chunks
    chunks: Vec<Chunk>,
    // registered structs
    structs: Vec<Struct>
}

impl Module {
    // Create a new module
    pub fn new() -> Self {
        Module {
            constants: Vec::new(),
            chunks: Vec::new(),
            structs: Vec::new()
        }
    }

    // Add a constant to the module
    #[inline]
    pub fn add_constant(&mut self, value: Value) -> usize {
        if let Some(index) = self.constants.iter().position(|v| v == &value) {
            index
        } else {
            self.constants.push(value);
            self.constants.len() - 1
        }
    }

    // Get a constant at a specific index
    #[inline]
    pub fn get_constant_at(&self, index: usize) -> Option<&Value> {
        self.constants.get(index)
    }

    // Add a chunk to the module
    #[inline]
    pub fn add_chunk(&mut self, chunk: Chunk) {
        self.chunks.push(chunk);
    }

    // Get a chunk at a specific index
    #[inline]
    pub fn get_chunk_at(&self, index: usize) -> Option<&Chunk> {
        self.chunks.get(index)
    }

    // Add a struct to the module
    #[inline]
    pub fn add_struct(&mut self, structure: Struct) {
        self.structs.push(structure);
    }

    // Get a struct at a specific index
    #[inline]
    pub fn get_struct_at(&self, index: usize) -> Option<&Struct> {
        self.structs.get(index)
    }
}
