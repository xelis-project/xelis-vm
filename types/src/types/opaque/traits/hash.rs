use std::hash::{Hash, Hasher};

/// Object-safe hashing for opaque values, with a blanket adapter for `Hash` types.
pub trait DynHash {
    fn dyn_hash(&self, state: &mut dyn Hasher);
}

impl<H: Hash + ?Sized> DynHash for H {
    fn dyn_hash(&self, mut state: &mut dyn Hasher) {
        self.hash(&mut state);
    }
}
