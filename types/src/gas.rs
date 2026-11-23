use thiserror::Error;

#[derive(Debug, Error, PartialEq, Eq)]
pub enum GasError {
    #[error("Not enough gas: limit {limit}, actual {actual}")]
    NotEnoughGas { limit: u64, actual: u64 },
    #[error("Gas overflow")]
    Overflow,
}

/// Execution-local accounting shared by the VM and opaque value operations.
/// Passing this concrete type adds no dynamic dispatch to gas charges.
#[derive(Debug, Clone)]
pub struct GasMeter {
    limit: u64,
    used: u64,
}

impl Default for GasMeter {
    fn default() -> Self {
        Self::new(u64::MAX)
    }
}

impl GasMeter {
    pub fn new(limit: u64) -> Self {
        Self { limit, used: 0 }
    }

    /// Reserve gas before performing work. Exhaustion consumes the remaining
    /// budget; arithmetic overflow leaves the previous usage unchanged.
    #[inline]
    pub fn charge(&mut self, gas: u64) -> Result<(), GasError> {
        let actual = self.used.checked_add(gas).ok_or(GasError::Overflow)?;
        if actual > self.limit {
            self.used = self.limit;
            return Err(GasError::NotEnoughGas {
                limit: self.limit,
                actual,
            });
        }
        self.used = actual;
        Ok(())
    }

    #[inline]
    pub fn limit(&self) -> u64 {
        self.limit
    }

    #[inline]
    pub fn used(&self) -> u64 {
        self.used
    }

    #[inline]
    pub fn left(&self) -> u64 {
        self.limit.saturating_sub(self.used)
    }

    #[inline]
    pub fn set_limit(&mut self, limit: u64) {
        self.limit = limit;
    }

    #[inline]
    pub fn increase_limit(&mut self, gas: u64) -> Result<(), GasError> {
        self.limit = self.limit.checked_add(gas).ok_or(GasError::Overflow)?;
        Ok(())
    }

    #[inline]
    pub fn reset(&mut self) {
        self.used = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn charges_preserve_budget_overflow_and_reset_semantics() {
        let mut gas = GasMeter::new(10);
        gas.charge(10).unwrap();
        gas.charge(0).unwrap();
        assert_eq!(gas.left(), 0);
        assert_eq!(
            gas.charge(1),
            Err(GasError::NotEnoughGas {
                limit: 10,
                actual: 11
            })
        );
        assert_eq!(gas.used(), 10);
        gas.increase_limit(5).unwrap();
        assert_eq!(gas.left(), 5);
        gas.reset();
        assert_eq!(gas.used(), 0);
        assert_eq!(gas.limit(), 15);

        gas.set_limit(u64::MAX);
        gas.charge(u64::MAX).unwrap();
        assert_eq!(gas.charge(1), Err(GasError::Overflow));
        assert_eq!(gas.used(), u64::MAX);
        assert_eq!(gas.increase_limit(1), Err(GasError::Overflow));
        assert_eq!(gas.limit(), u64::MAX);
    }
}
