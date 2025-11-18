use std::hash::{Hasher, BuildHasher};

#[derive(Default)]
pub struct IdentityHasher {
    hash: u64,
}

impl Hasher for IdentityHasher {
    #[inline]
    fn finish(&self) -> u64 {
        self.hash
    }

    #[inline]
    fn write_u64(&mut self, i: u64) {
        self.hash = i;
    }

    #[inline]
    fn write_usize(&mut self, i: usize) {
        self.hash = i as u64;
    }

    // Required by trait, but should not be used
    #[inline]
    fn write(&mut self, _: &[u8]) {
        unimplemented!("IdentityHasher does not support write with byte slices");
    }
}

#[derive(Default)]
pub struct IdentityBuildHasher;

impl BuildHasher for IdentityBuildHasher {
    type Hasher = IdentityHasher;

    #[inline]
    fn build_hasher(&self) -> IdentityHasher {
        IdentityHasher::default()
    }
}