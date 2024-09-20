mod chunk;
mod opcode;
pub mod vm;
pub mod compiler;

pub use chunk::Chunk;
pub use opcode::OpCode;

use crate::types::Struct;

// A module is a collection of declared chunks, constants and structs
// It represents a program compiled in bytecode
pub struct Module {
    // TODO: have a central IndexSet of all the constants

    // Available chunks
    chunks: Vec<Chunk>,
    // registered structs
    structs: Vec<Struct>
}

impl Module {
    // Create a new module
    pub fn new() -> Self {
        Module {
            chunks: Vec::new(),
            structs: Vec::new()
        }
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
