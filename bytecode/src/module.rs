use indexmap::{IndexMap, IndexSet};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize, ser::SerializeSeq};
use xelis_types::ValueCell;

use super::{Chunk, Access};

#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ModuleChunk {
    #[serde(flatten)]
    pub chunk: Chunk,
    #[serde(flatten)]
    pub access: Access,
}

fn serialize_map<S>(
    map: &IndexMap<u8, usize>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    let mut seq = serializer.serialize_seq(Some(map.len()))?;
    for (k, v) in map {
        seq.serialize_element(&(k, v))?;
    }
    seq.end()
}

fn deserialize_map<'de, D>(
    deserializer: D,
) -> Result<IndexMap<u8, usize>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    struct MapVecVisitor;

    impl<'de> serde::de::Visitor<'de> for MapVecVisitor {
        type Value = IndexMap<u8, usize>;

        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("a list of key-value pairs")
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::SeqAccess<'de>,
        {
            let mut map = IndexMap::new();
            while let Some((k, v)) = seq.next_element::<(u8, usize)>()? {
                if map.insert(k, v).is_some() {
                    return Err(serde::de::Error::custom(format!("duplicate key found: {}", k)));
                }
            }

            Ok(map)
        }
    }

    deserializer.deserialize_seq(MapVecVisitor)
}

// A module is a collection of declared chunks, constants and types
// It represents a program compiled in bytecode
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct Module {
    // Set of constants used by the program
    #[serde(default)]
    constants: IndexSet<ValueCell>,
    // Available chunks
    #[serde(default)]
    chunks: Vec<ModuleChunk>,
    // Hook id => chunk id
    // Serde it as a vec of tuples
    #[serde(
        default,
        serialize_with = "serialize_map",
        deserialize_with = "deserialize_map"
    )]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_serde_module_with_hooks() {
        let mut module = Module::new();
        let chunk = Chunk::new();
        module.add_public_chunk(chunk.clone());
        module.add_entry_chunk(chunk.clone());
        module.add_internal_chunk(chunk.clone());
        module.add_hook_chunk(1, chunk.clone());
        module.add_hook_chunk(2, chunk.clone());

        let serialized = serde_json::to_string_pretty(&module).unwrap();
        let deserialized: Module = serde_json::from_str(&serialized).unwrap();

        assert_eq!(module.constants().len(), deserialized.constants().len());
        assert_eq!(module.chunks().len(), deserialized.chunks().len());
        assert_eq!(module.hook_chunk_ids().len(), deserialized.hook_chunk_ids().len());
    }
}