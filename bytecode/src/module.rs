
use std::collections::HashSet;
use indexmap::IndexSet;
use xelis_types::{EnumType, StructType, Value};

use super::Chunk;

// A module is a collection of declared chunks, constants and types
// It represents a program compiled in bytecode
pub struct Module {
    // Set of constants used by the program
    constants: IndexSet<Value>,
    // Available chunks
    chunks: Vec<Chunk>,
    // Chunks callable from external programs
    entry_chunk_ids: HashSet<usize>,
    // registered structs
    structs: IndexSet<StructType>,
    // registered enums
    enums: IndexSet<EnumType>
}

impl Module {
    // Create a new module
    pub fn new() -> Self {
        Self {
            constants: IndexSet::new(),
            chunks: Vec::new(),
            entry_chunk_ids: HashSet::new(),
            structs: IndexSet::new(),
            enums: IndexSet::new()
        }
    }

    // Validate the module
    pub fn validate(&self) -> bool {
        let max = u16::MAX as usize;

        self.constants.len() < max
        && self.chunks.len() < max
        && self.structs.len() < max
        && self.enums.len() < max
    }

    // Get the constants declared in the module
    #[inline]
    pub fn constants(&self) -> &IndexSet<Value> {
        &self.constants
    }

    // Add a constant to the module
    #[inline]
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.insert_full(value).0
    }

    // Get a constant at a specific index
    #[inline]
    pub fn get_constant_at(&self, index: usize) -> Option<&Value> {
        self.constants.get_index(index)
    }

    // Get the chunks declared in the module
    #[inline]
    pub fn chunks(&self) -> &[Chunk] {
        &self.chunks
    }

    // Get the chunks ids callable from externals
    pub fn chunks_entry_ids(&self) -> &HashSet<usize> {
        &self.entry_chunk_ids
    }

    // Add a chunk to the module
    #[inline]
    pub fn add_chunk(&mut self, chunk: Chunk) {
        self.chunks.push(chunk);
    }

    // Add a chunk to the module
    // and mark it as callable from externals (entry)
    #[inline]
    pub fn add_entry_chunk(&mut self, chunk: Chunk) {
        let index = self.chunks.len();
        self.chunks.push(chunk);
        self.entry_chunk_ids.insert(index);
    }

    // Is chunk callable from externals
    #[inline]
    pub fn is_entry_chunk(&self, index: usize) -> bool {
        self.entry_chunk_ids.contains(&index)
    }

    // Get a chunk at a specific index
    #[inline]
    pub fn get_chunk_at(&self, index: usize) -> Option<&Chunk> {
        self.chunks.get(index)
    }

    // Get a mutable chunk at a specific index
    #[inline]
    pub fn get_chunk_at_mut(&mut self, index: usize) -> Option<&mut Chunk> {
        self.chunks.get_mut(index)
    }

    // Get all the structs declared in the module
    #[inline]
    pub fn structs(&self) -> &IndexSet<StructType> {
        &self.structs
    }

    // Add a struct to the module
    #[inline]
    pub fn add_struct(&mut self, structure: StructType) -> bool {
        self.structs.insert(structure)
    }

    // Get a struct at a specific index
    #[inline]
    pub fn get_struct_at(&self, index: usize) -> Option<&StructType> {
        self.structs.get_index(index)
    }

    // Get all the enums declared in the module
    #[inline]
    pub fn enums(&self) -> &IndexSet<EnumType> {
        &self.enums
    }

    // Add an enum to the module
    #[inline]
    pub fn add_enum(&mut self, enumeration: EnumType) -> bool {
        self.enums.insert(enumeration)
    }

    // Get an enum at a specific index
    #[inline]
    pub fn get_enum_at(&self, index: usize) -> Option<&EnumType> {
        self.enums.get_index(index)
    }
}
