use std::hash::{Hash, Hasher};

use crate::{GasMeter, ValueError};

/// Hashing and optional prepaid costs for VM opaque values.
/// `Hash` types receive a blanket adapter with no additional gas charge.
/// Implement directly on types without `Hash` to charge for custom work.
pub trait DynHash {
    /// Called by metered hashing immediately before `dyn_hash`.
    /// Default adapters add no charge; custom types can price their own work.
    fn pre_hash_cost(&self, _: &mut GasMeter) -> Result<(), ValueError> {
        Ok(())
    }

    fn dyn_hash(&self, state: &mut dyn Hasher);
}

impl<H: Hash + ?Sized> DynHash for H {
    fn dyn_hash(&self, mut state: &mut dyn Hasher) {
        self.hash(&mut state);
    }
}
