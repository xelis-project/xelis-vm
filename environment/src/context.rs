use std::ops::{Deref, DerefMut};
use runtime_context::Context;
use crate::EnvironmentError;

pub struct VMContext<'ty, 'r> {
    // main Context
    context: Context<'ty, 'r>,
    // Configurable gas limit for an execution
    // By default, set to u64::MAX because
    // no program should be able to run indefinitely
    max_gas: u64,
    // Price per byte of memory
    memory_price_per_byte: u64,
    // Max value depth allowed
    // This is used to prevent stack overflow attacks
    max_value_depth: usize,
    // Max memory usage allowed
    max_memory_usage: usize,
    // Current gas used in the execution
    current_gas: u64,
    // Current memory used in the execution
    current_memory: usize,
    // Peak memory used in the execution (Ethereum-style)
    // Only pay gas when growing beyond this peak
    peak_memory: usize,
}

impl Default for VMContext<'_, '_> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<'ty, 'r> VMContext<'ty, 'r> {
    // Create a new VMContext with default values
    #[inline]
    pub fn new() -> Self {
        Self::with_context(Context::new())
    }

    // Create a new VMContext with a specific Context
    #[inline]
    pub fn with_context(context: Context<'ty, 'r>) -> Self {
        Self {
            context,
            max_gas: u64::MAX,
            current_gas: 0,
            memory_price_per_byte: 1,
            max_value_depth: 16,
            max_memory_usage: 1024 * 1024 * 128, // 128 MB
            current_memory: 0,
            peak_memory: 0,
        }
    }

    // Set a gas limit for the Context
    #[inline(always)]
    pub fn set_gas_limit(&mut self, gas: u64) {
        self.max_gas = gas;
    }

    // Increase the gas limit by N
    #[inline(always)]
    pub fn increase_gas_limit(&mut self, gas: u64) -> Result<(), EnvironmentError> {
         self.max_gas = self.max_gas.checked_add(gas)
            .ok_or(EnvironmentError::GasOverflow)?;

        Ok(())
    }

    // Get current gas limit
    #[inline(always)]
    pub fn get_gas_limit(&self) -> u64 {
        self.max_gas
    }

    // Get gas left
    #[inline(always)]
    pub fn get_gas_left(&self) -> u64 {
        self.max_gas - self.current_gas
    }

    // Set the price per byte of memory
    #[inline(always)]
    pub fn set_memory_price_per_byte(&mut self, price: u64) {
        self.memory_price_per_byte = price;
    }

    // Get the price per byte of memory
    #[inline(always)]
    pub fn memory_price_per_byte(&self) -> u64 {
        self.memory_price_per_byte
    }

    // Get the current gas usage
    #[inline(always)]
    pub fn current_gas_usage(&self) -> u64 {
        self.current_gas
    }

    // Get the max value depth allowed
    #[inline(always)]
    pub fn max_value_depth(&self) -> usize {
        self.max_value_depth
    }

    // Set the max value depth allowed
    #[inline(always)]
    pub fn set_max_value_depth(&mut self, depth: usize) {
        assert!(depth > 0, "Max value depth must be greater than 0");
        self.max_value_depth = depth;
    }

    // Increase the gas usage by a specific amount
    #[inline]
    pub fn increase_gas_usage(&mut self, gas: u64) -> Result<(), EnvironmentError> {
        let new_gas = self.current_gas.checked_add(gas)
            .ok_or(EnvironmentError::GasOverflow)?;

        if new_gas > self.max_gas {
            // used gas exceeds limit
            // cap it to the max_gas value
            self.current_gas = self.max_gas;
            return Err(EnvironmentError::NotEnoughGas { limit: self.max_gas, actual: new_gas });
        }

        self.current_gas = new_gas;

        Ok(())
    }

    // Get the current memory usage
    #[inline(always)]
    pub fn current_memory_usage(&self) -> usize {
        self.current_memory
    }

    // Get the max memory usage allowed
    #[inline(always)]
    pub fn max_memory_usage(&self) -> usize {
        self.max_memory_usage
    }

    // Get the memory left
    #[inline(always)]
    pub fn memory_left(&self) -> usize {
        self.max_memory_usage.saturating_sub(self.current_memory)
    }

    // Get the peak memory usage
    #[inline(always)]
    pub fn peak_memory_usage(&self) -> usize {
        self.peak_memory
    }

    #[inline]
    fn handle_peak_memory(&mut self) -> Result<(), EnvironmentError> {
        if let Some(growth) = self.current_memory.checked_sub(self.peak_memory) {
            self.increase_gas_usage((growth as u64) * self.memory_price_per_byte)?;
            self.peak_memory = self.current_memory;
        }

        Ok(())
    }

    // Increase the memory usage by a specific amount
    // Ethereum-style: Only pay gas when growing beyond peak memory
    #[inline]
    pub fn increase_memory_usage(&mut self, memory: usize) -> Result<(), EnvironmentError> {
        self.current_memory = self.current_memory.checked_add(memory)
            .ok_or(EnvironmentError::OutOfMemory)?;

        if self.current_memory > self.max_memory_usage {
            return Err(EnvironmentError::OutOfMemory);
        }

        self.handle_peak_memory()
    }

    // Increase the memory usage by a specific amount
    // The memory added is unchecked but we still check for the gas price of the memory
    // Ethereum-style: Only pay gas when growing beyond peak memory
    #[inline]
    pub fn increase_memory_usage_unchecked(&mut self, memory: usize) -> Result<(), EnvironmentError> {
        self.current_memory += memory;
        self.handle_peak_memory()
    }

    // Decrease the memory usage by a specific amount
    #[inline]
    pub fn decrease_memory_usage(&mut self, memory: usize) {
        self.current_memory = self.current_memory.saturating_sub(memory);
    }

    // Reset the gas & memory usage
    #[inline]
    pub fn reset_usage(&mut self) {
        self.current_gas = 0;
        self.current_memory = 0;
        self.peak_memory = 0;
    }
}

impl<'ty, 'r> Deref for VMContext<'ty, 'r> {
    type Target = Context<'ty, 'r>;

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.context
    }
}

impl<'ty, 'r> AsRef<Context<'ty, 'r>> for VMContext<'ty, 'r> {
    #[inline]
    fn as_ref(&self) -> &Context<'ty, 'r> {
        &self.context
    }
}

impl<'ty, 'r> AsMut<Context<'ty, 'r>> for VMContext<'ty, 'r> {
    #[inline]
    fn as_mut(&mut self) -> &mut Context<'ty, 'r> {
        &mut self.context
    }
}

impl<'ty, 'r> DerefMut for VMContext<'ty, 'r> {
    #[inline]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.context
    }
}