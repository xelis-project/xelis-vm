use super::AsAny;
use crate::{GasMeter, ValueError};

/// Equality and optional prepaid costs for VM opaque values.
/// `PartialEq` types receive a blanket adapter with no additional gas charge.
/// Implement directly on types without `PartialEq` to charge for custom work.
pub trait DynEq: AsAny {
    /// Called by metered equality immediately before `is_equal`, including each
    /// map collision comparison. Default adapters add no charge.
    fn pre_eq_cost(&self, _: &mut GasMeter) -> Result<(), ValueError> {
        Ok(())
    }

    fn is_equal(&self, other: &dyn DynEq) -> bool;
}

impl<T: PartialEq + AsAny> DynEq for T {
    fn is_equal(&self, other: &dyn DynEq) -> bool {
        if let Some(other) = other.as_any().downcast_ref::<T>() {
            self.eq(other)
        } else {
            false
        }
    }
}
