use std::hash::{Hash, Hasher};

pub trait DynHash {
    fn dyn_hash(&self, state: &mut dyn Hasher);
    fn can_hash(&self) -> bool;
}

impl<H: Hash + ?Sized> DynHash for H {
    fn dyn_hash(&self, mut state: &mut dyn Hasher) {
        self.hash(&mut state);
    }

    fn can_hash(&self) -> bool {
        true
    }
}
