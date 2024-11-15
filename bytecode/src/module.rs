
use std::collections::HashSet;
use xelis_types::{EnumType, StructType, Value};

use super::Chunk;

// A module is a collection of declared chunks, constants and structs
// It represents a program compiled in bytecode
pub struct Module {
    // TODO use a IndexSet
    constants: Vec<Value>,
    // Available chunks
    chunks: Vec<Chunk>,
    // Chunks callable from external programs
    entry_chunk_ids: HashSet<usize>,
    // registered structs
    structs: Vec<StructType>,
    // registered enums
    enums: Vec<EnumType>
}

impl Module {
    // Create a new module
    pub fn new() -> Self {
        Self {
            constants: Vec::new(),
            chunks: Vec::new(),
            entry_chunk_ids: HashSet::new(),
            structs: Vec::new(),
            enums: Vec::new()
        }
    }

    // Get the constants declared in the module
    #[inline]
    pub fn constants(&self) -> &[Value] {
        &self.constants
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
    pub fn structs(&self) -> &[StructType] {
        &self.structs
    }

    // Add a struct to the module
    #[inline]
    pub fn add_struct(&mut self, structure: StructType) {
        self.structs.push(structure);
    }

    // Get a struct at a specific index
    #[inline]
    pub fn get_struct_at(&self, index: usize) -> Option<&StructType> {
        self.structs.get(index)
    }

    // Get all the enums declared in the module
    #[inline]
    pub fn enums(&self) -> &[EnumType] {
        &self.enums
    }

    // Add an enum to the module
    #[inline]
    pub fn add_enum(&mut self, enumeration: EnumType) {
        self.enums.push(enumeration);
    }

    // Get an enum at a specific index
    #[inline]
    pub fn get_enum_at(&self, index: usize) -> Option<&EnumType> {
        self.enums.get(index)
    }
}
