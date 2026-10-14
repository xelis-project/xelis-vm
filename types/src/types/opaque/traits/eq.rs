use super::AsAny;

pub trait DynEq: AsAny {
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
