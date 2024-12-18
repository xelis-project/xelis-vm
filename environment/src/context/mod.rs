mod data;

use std::{
    any::TypeId,
    collections::HashMap,
    hash::{BuildHasherDefault, Hasher}
};
use crate::EnvironmentError;

use better_any::Tid;
pub use data::Data;

// A hasher for `TypeId`s that takes advantage of its known characteristics.
#[derive(Debug, Default)]
struct NoOpHasher(u64);

impl Hasher for NoOpHasher {
    fn write(&mut self, _: &[u8]) {
        unimplemented!("This NoOpHasher can only handle u64s")
    }

    fn write_u64(&mut self, i: u64) {
        self.0 = i;
    }

    fn finish(&self) -> u64 {
        self.0
    }
}

// Context is a simple data store that allows for storing and retrieving values of different types.
pub struct Context<'a> {
    data: HashMap<TypeId, Data<'a>, BuildHasherDefault<NoOpHasher>>,
    // Configurable gas limit for an execution
    // By default, set to u64::MAX because
    // no program should be able to run indefinitely
    max_gas: u64,
    // Price per byte of memory
    memory_price_per_byte: u64,
    // Max value depth allowed
    // This is used to prevent stack overflow attacks
    max_value_depth: usize,
    // Current gas used in the execution
    current_gas: u64,
}

impl Default for Context<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Context<'a> {
    // Create a new Context
    pub fn new() -> Self {
        Self {
            data: HashMap::default(),
            max_gas: u64::MAX,
            current_gas: 0,
            memory_price_per_byte: 0,
            max_value_depth: 16,
        }
    }

    // Set a gas limit for the Context
    #[inline(always)]
    pub fn set_gas_limit(&mut self, gas: u64) {
        self.max_gas = gas;
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
        self.current_gas = self.current_gas.checked_add(gas)
            .ok_or(EnvironmentError::GasOverflow)?;

        if self.current_gas > self.max_gas {
            return Err(EnvironmentError::NotEnoughGas { limit: self.max_gas, actual: self.current_gas });
        }

        Ok(())
    }

    // Insert a value into the Context without checking the type
    #[inline]
    pub fn insert_unchecked(&mut self, key: TypeId, data: Data<'a>) {
        self.data.insert(key, data);
    }

    // Insert a borrowed value into the Context
    #[inline]
    pub fn insert_ref<T: Tid<'a>>(&mut self, value: &'a T) {
        self.data.insert(T::id(), Data::Borrowed(value));
    }

    // Insert a mutable value into the Context
    #[inline]
    pub fn insert_mut<T: Tid<'a>>(&mut self, value: &'a mut T) {
        self.data.insert(T::id(), Data::Mut(value));
    }

    // Insert an owned value into the Context
    #[inline]
    pub fn insert<T: Tid<'a>>(&mut self, value: T) {
        self.data.insert(T::id(), Data::Owned(Box::new(value)));
    }

    // Get a borrowed value from the Context
    #[inline]
    pub fn get<'b, T: Tid<'a>>(&'b self) -> Option<&'b T> {
        self.data.get(&T::id()).map(|v| v.downcast_ref()).flatten()
    }

    // Get a mutable value from the Context
    #[inline]
    pub fn get_mut<'b, T: Tid<'a>>(&'b mut self) -> Option<&'b mut T> {
        self.data.get_mut(&T::id()).map(|v| v.downcast_mut()).flatten()
    }

    // Get an owned value from the Context
    #[inline]
    pub fn take<T: Tid<'a>>(&mut self) -> Option<T> {
        let id = T::id();
        match self.data.remove(&id) {
            Some(data) => data.try_take_owned::<T>().ok(),
            None => None,
        }
    }

    // remove a value from the Context and returns it.
    #[inline]
    pub fn remove<T: Tid<'a>>(&mut self) -> Option<Data<'a>> {
        self.data.remove(&T::id())
    }

    // Check if the Context contains a value of a specific type.
    #[inline]
    pub fn contains<T: Tid<'a>>(&self) -> bool {
        self.data.contains_key(&T::id())
    }

    // Clear the Context
    #[inline]
    pub fn clear(&mut self) {
        self.data.clear();
    }

    // Reset the gas usage
    #[inline]
    pub fn reset_gas_usage(&mut self) {
        self.current_gas = 0;
    }
}

#[cfg(test)]
mod tests {
    use better_any::tid;

    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct Dummy<'a>(&'a str);
    tid!(Dummy<'_>);

    #[test]
    fn test_context_owned() {
        let dummy = Dummy("Hello, World!");
        let mut context = Context::new();

        context.insert(dummy);
        assert!(matches!(context.get::<Dummy>(), Some(_)));
        assert!(matches!(context.get_mut::<Dummy>(), Some(_)));
        assert_eq!(context.contains::<Dummy>(), true);
    }

    #[test]
    fn test_context_ref() {
        let dummy = Dummy("Hello, World!");
        let mut context = Context::new();

        context.insert_ref(&dummy);
        assert_eq!(context.get::<Dummy>(), Some(&dummy));
        assert_eq!(context.get_mut::<Dummy>(), None);
        assert_eq!(context.contains::<Dummy>(), true);
    }

    #[test]
    fn test_context_mut() {
        let mut dummy = Dummy("Hello, World!");
        let mut context = Context::new();

        context.insert_mut(&mut dummy);
        assert!(matches!(context.get::<Dummy>(), Some(_)));
        assert!(matches!(context.get_mut::<Dummy>(), Some(_)));
        assert_eq!(context.contains::<Dummy>(), true);
    }
}