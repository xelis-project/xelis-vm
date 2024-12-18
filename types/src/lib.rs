mod types;
mod values;
mod u256;

use std::{
    collections::HashMap,
    hash::{
        BuildHasherDefault,
        Hasher
    }
};

pub use types::*;
pub use values::*;
pub use types::opaque::*;
pub use u256::U256;

// Variable identifier used in the parser and interpreter
// This is used to optimize the memory usage by using a smaller type
// to represent an identifier
// A mapper is done to map a string name into an identifier
pub type IdentifierType = u16;


// Hasher that does nothing
// Because we have u16 as the key, we don't need to hash it
#[derive(Debug, Default)]
pub struct NoOpHasher(u16);

impl Hasher for NoOpHasher {
    fn write(&mut self, bytes: &[u8]) {
        self.0 = u16::from_le_bytes([bytes[0], bytes[1]]);
    }

    fn write_u64(&mut self, _: u64) {
        unimplemented!("write_u64")
    }

    fn finish(&self) -> u64 {
        self.0 as u64
    }
}

pub type NoHashMap<V> = HashMap<IdentifierType, V, BuildHasherDefault<NoOpHasher>>;