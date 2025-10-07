use indexmap::{IndexMap, IndexSet};
use serde::{Deserialize, Serialize};
use xelis_types::ValueCell;

use super::{Chunk, Access};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleChunk {
    #[serde(flatten)]
    pub chunk: Chunk,
    #[serde(flatten)]
    pub access: Access,
}

// A module is a collection of declared chunks, constants and types
// It represents a program compiled in bytecode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Module {
    // Set of constants used by the program
    #[serde(default)]
    constants: IndexSet<ValueCell>,
    // Available chunks
    #[serde(default)]
    chunks: Vec<ModuleChunk>,
    // Hook id => chunk id
    #[serde(default)]
    hook_chunk_ids: IndexMap<u8, usize>,
}

impl Module {
    // Create a new module
    pub fn new() -> Self {
        Self {
            constants: IndexSet::new(),
            chunks: Vec::new(),
            hook_chunk_ids: IndexMap::new()
        }
    }

    // Create a new module with all needed data
    pub fn with(
        constants: IndexSet<ValueCell>,
        chunks: Vec<ModuleChunk>,
        hook_chunk_ids: IndexMap<u8, usize>
    ) -> Self {
        Self {
            constants,
            chunks,
            hook_chunk_ids,
        }
    }

    // Get the constants declared in the module
    #[inline]
    pub fn constants(&self) -> &IndexSet<ValueCell> {
        &self.constants
    }

    // Add a constant to the module
    #[inline]
    pub fn add_constant(&mut self, value: impl Into<ValueCell>) -> usize {
        self.constants.insert_full(value.into()).0
    }

    // Get a constant at a specific index
    #[inline]
    pub fn get_constant_at(&self, index: usize) -> Option<&ValueCell> {
        self.constants.get_index(index)
    }

    // Get the chunks declared in the module
    #[inline]
    pub fn chunks(&self) -> &[ModuleChunk] {
        &self.chunks
    }

    // Add a publicly callable chunk to the module
    #[inline]
    pub fn add_public_chunk(&mut self, chunk: Chunk) {
        self.chunks.push(ModuleChunk {
            chunk,
            access: Access::All
        });
    }

    // Add a entry callable chunk to the module
    // Can only be called as a program main function
    #[inline]
    pub fn add_entry_chunk(&mut self, chunk: Chunk) {
        self.chunks.push(ModuleChunk {
            chunk,
            access: Access::Entry
        });
    }

    // Add an internal chunk to the module
    // Only callable by the program itself
    #[inline]
    pub fn add_internal_chunk(&mut self, chunk: Chunk) {
        self.chunks.push(ModuleChunk {
            chunk,
            access: Access::Internal
        });
    }

    // Is chunk callable by a user as an entrypoint
    #[inline]
    pub fn is_entry_chunk(&self, index: usize) -> bool {
        self.chunks.get(index)
            .map_or(false, |c| matches!(c.access, Access::Entry))
    }

    // Is chunk callable (not an entry or hook)
    #[inline]
    pub fn is_callable_chunk(&self, index: usize) -> bool {
        self.chunks.get(index)
            .map_or(false, |c| matches!(c.access, Access::All | Access::Internal))
    }

    // Is chunk callable as a public function
    #[inline]
    pub fn is_public_chunk(&self, index: usize) -> bool {
        self.chunks.get(index)
            .map_or(false, |c| matches!(c.access, Access::All))
    }

    // Get a chunk at a specific index
    #[inline(always)]
    pub fn get_chunk_at(&self, index: usize) -> Option<&Chunk> {
        self.get_chunk_access_at(index)
            .map(|c| &c.chunk)
    }

    // Get a chunk at a specific index
    #[inline(always)]
    pub fn get_chunk_access_at(&self, index: usize) -> Option<&ModuleChunk> {
        self.chunks.get(index)
    }

    // Get a mutable chunk at a specific index
    #[inline]
    pub fn get_chunk_at_mut(&mut self, index: usize) -> Option<&mut Chunk> {
        self.chunks.get_mut(index)
            .map(|c| &mut c.chunk)
    }

    // Get the hook chunks ids called during an event
    pub fn hook_chunk_ids(&self) -> &IndexMap<u8, usize> {
        &self.hook_chunk_ids
    }

    // Get the chunk id linked for the hook
    pub fn get_chunk_id_of_hook(&self, id: u8) -> Option<usize> {
        self.hook_chunk_ids.get(&id).copied()
    }


    // Add a chunk to the module
    // and mark it as the requested hook id
    #[inline]
    pub fn add_hook_chunk(&mut self, id: u8, chunk: Chunk) -> Option<usize> {
        let index = self.chunks.len();
        self.chunks.push(ModuleChunk {
            chunk,
            access: Access::Hook { id }
        });
        self.hook_chunk_ids.insert(id, index)
    }
}