use std::{any::{Any, TypeId}, collections::HashMap, hash::{BuildHasherDefault, Hasher}};

use crate::EnvironmentError;

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

// Data is a wrapper around Any that allows for borrowed and mutable references.
pub enum Data<'a> {
    Owned(Box<dyn Any>),
    Borrowed(&'a dyn Any),
    Mut(&'a mut dyn Any),
}

impl<'a> Data<'a> {
    // downcast allows for immutable access to the underlying value.
    pub fn downcast_ref<'b, T: 'static>(&'b self) -> Option<&'b T> {
        match self {
            Data::Owned(value) => value.downcast_ref(),
            Data::Borrowed(value) => value.downcast_ref(),
            Data::Mut(value) => value.downcast_ref(),
        }
    }

    // downcast_mut allows for mutable access to the underlying value.
    pub fn downcast_mut<'b, T: 'static>(&'b mut self) -> Option<&'b mut T> {
        match self {
            Data::Owned(value) => value.downcast_mut(),
            Data::Mut(value) => value.downcast_mut(),
            _ => None,
        }
    }

    // into_owned consumes the Data and returns the underlying value or clone it if it's borrowed.
    pub fn into_owned<T: Clone + 'static>(self) -> T {
        match self {
            Data::Owned(value) => *value.downcast::<T>().unwrap(),
            Data::Borrowed(value) => value.downcast_ref::<T>().unwrap().clone(),
            Data::Mut(value) => value.downcast_ref::<T>().unwrap().clone(),
        }
    }

    // take consumes the Data and returns the underlying value if it's owned.
    pub fn take<T: 'static>(self) -> Option<T> {
        match self {
            Data::Owned(value) => match value.downcast::<T>() {
                Ok(value) => Some(*value),
                Err(_) => None,
            },
            _ => None,
        }
    }
}

// Context is a simple data store that allows for storing and retrieving values of different types.
pub struct Context<'a> {
    data: HashMap<TypeId, Data<'a>, BuildHasherDefault<NoOpHasher>>,
    // Configurable gas limit for an execution
    max_gas: Option<u64>,
    // Price per byte of memory
    memory_price_per_byte: u64,
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
            max_gas: None,
            current_gas: 0,
            memory_price_per_byte: 0,
        }
    }

    // Set a gas limit for the Context
    #[inline(always)]
    pub fn set_gas_limit(&mut self, gas: Option<u64>) {
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

    // Increase the gas usage by a specific amount
    #[inline]
    pub fn increase_gas_usage(&mut self, gas: u64) -> Result<(), EnvironmentError> {
        if let Some(max_gas) = self.max_gas {
            self.current_gas = self.current_gas.checked_add(gas)
                .ok_or(EnvironmentError::NotEnoughGas { limit: max_gas, actual: self.current_gas })?;

            if self.current_gas > max_gas {
                return Err(EnvironmentError::NotEnoughGas { limit: max_gas, actual: self.current_gas });
            }
        }

        Ok(())
    }

    // Insert a borrowed value into the Context
    pub fn insert_ref<T: 'static>(&mut self, value: &'a T) {
        self.data.insert(TypeId::of::<T>(), Data::Borrowed(value));
    }

    // Insert a mutable value into the Context
    pub fn insert_mut<T: 'static>(&mut self, value: &'a mut T) {
        self.data.insert(TypeId::of::<T>(), Data::Mut(value));
    }

    // Insert an owned value into the Context
    pub fn insert<T: 'static>(&mut self, value: T) {
        self.data.insert(TypeId::of::<T>(), Data::Owned(Box::new(value)));
    }

    // Get a borrowed value from the Context
    pub fn get<'b, T: 'static>(&'b self) -> Option<&'b T> {
        self.data.get(&TypeId::of::<T>()).map(|v| v.downcast_ref()).flatten()
    }

    // Get a mutable value from the Context
    pub fn get_mut<'b, T: 'static>(&'b mut self) -> Option<&'b mut T> {
        self.data.get_mut(&TypeId::of::<T>()).map(|v| v.downcast_mut()).flatten()
    }

    // Get an owned value from the Context
    pub fn take<T: 'static>(&mut self) -> Option<T> {
        self.data.remove(&TypeId::of::<T>()).and_then(|v| v.take())
    }

    // remove a value from the Context and returns it.
    pub fn remove<T: 'static>(&mut self) -> Option<Data<'a>> {
        self.data.remove(&TypeId::of::<T>())
    }

    // Check if the Context contains a value of a specific type.
    pub fn contains<T: 'static>(&self) -> bool {
        self.data.contains_key(&TypeId::of::<T>())
    }

    // Clear the Context
    pub fn clear(&mut self) {
        self.data.clear();
    }

    // Reset the gas usage
    pub fn reset_gas_usage(&mut self) {
        self.current_gas = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_context_ref() {
        let mut context = Context::new();
        context.insert_ref(&1);

        assert_eq!(context.get::<i32>(), Some(&1));
        assert_eq!(context.remove::<i32>().unwrap().into_owned::<i32>(), 1i32);
        assert_eq!(context.get::<i32>(), None);
        assert!(context.remove::<i32>().is_none());
        assert_eq!(context.contains::<i32>(), false);

        context.insert_ref(&2);
        context.clear();
        assert_eq!(context.get::<i32>(), None);
        assert_eq!(context.contains::<i32>(), false);
    }

    #[test]
    fn test_context_owned() {
        let mut context = Context::new();
        context.insert(1);

        assert_eq!(context.get::<i32>(), Some(&1));
        assert_eq!(context.remove::<i32>().unwrap().into_owned::<i32>(), 1i32);
        assert_eq!(context.take::<i32>(), None);
        assert!(context.remove::<i32>().is_none());
        assert_eq!(context.contains::<i32>(), false);

        context.insert(2);
        context.clear();
        assert_eq!(context.get::<i32>(), None);
        assert_eq!(context.contains::<i32>(), false);
    }

    #[test]
    fn test_context_mut() {
        struct Dummy(i32);

        let mut context = Context::new();
        let mut dummy = Dummy(1);
        context.insert_mut(&mut dummy);

        assert_eq!(context.get_mut::<Dummy>().unwrap().0, 1);
        context.get_mut::<Dummy>().unwrap().0 = 2;
        assert_eq!(context.get_mut::<Dummy>().unwrap().0, 2);
    }
}