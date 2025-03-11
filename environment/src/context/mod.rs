mod data;

use std::{
    any::TypeId,
    hash::{BuildHasherDefault, Hasher}
};
use crate::EnvironmentError;

use better_any::Tid;
pub use data::Data;
use hashbrown::HashMap;

// A hasher for `TypeId`s that takes advantage of its known characteristics.
#[derive(Debug, Default)]
struct TypeIdHasher(u64);

impl Hasher for TypeIdHasher {
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
pub struct Context<'ty, 'r> {
    data: HashMap<TypeId, Data<'ty, 'r>, BuildHasherDefault<TypeIdHasher>>,
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
}

impl Default for Context<'_, '_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'ty, 'r> Context<'ty, 'r> {
    // Create a new Context
    pub fn new() -> Self {
        Self {
            data: HashMap::default(),
            max_gas: u64::MAX,
            current_gas: 0,
            memory_price_per_byte: 1,
            max_value_depth: 16,
            max_memory_usage: 1024 * 1024 * 128, // 128 MB
            current_memory: 0,
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

    // Increase the memory usage by a specific amount
    #[inline]
    pub fn increase_memory_usage(&mut self, memory: usize) -> Result<(), EnvironmentError> {
        self.current_memory = self.current_memory.checked_add(memory)
            .ok_or(EnvironmentError::OutOfMemory)?;

        if self.current_memory > self.max_memory_usage {
            return Err(EnvironmentError::OutOfMemory);
        }

        Ok(())
    }


    // Increase the memory usage by a specific amount
    #[inline]
    pub fn increase_memory_usage_unchecked(&mut self, memory: usize) {
        self.current_memory += memory;
    }

    // Decrease the memory usage by a specific amount
    #[inline]
    pub fn decrease_memory_usage(&mut self, memory: usize) {
        self.current_memory = self.current_memory.saturating_sub(memory);
    }

    // Insert a value into the Context without checking the type
    #[inline]
    pub fn insert_unchecked(&mut self, key: TypeId, data: Data<'ty, 'r>) {
        self.data.insert(key, data);
    }

    // Insert a borrowed value into the Context
    #[inline]
    pub fn insert_ref<T: Tid<'ty>>(&mut self, value: &'r T) {
        self.data.insert(T::id(), Data::Borrowed(value));
    }

    // Insert a mutable value into the Context
    #[inline]
    pub fn insert_mut<T: Tid<'ty>>(&mut self, value: &'r mut T) {
        self.data.insert(T::id(), Data::Mut(value));
    }

    // Insert an owned value into the Context
    #[inline]
    pub fn insert<T: Tid<'ty>>(&mut self, value: T) {
        self.data.insert(T::id(), Data::Owned(Box::new(value)));
    }

    // Insert a value into the Context
    // Going through the Any trait to allow for downcasting to a generic type
    // and skip the lifetime constraints
    #[inline]
    pub fn insert_any_ref<T: Tid<'ty> + 'static>(&mut self, any: &'r T) {
        let r: &'r dyn Tid<'ty> = any.into();
        self.data.insert(T::id(), Data::Borrowed(r));
    }

    // Insert a value into the Context
    // Going through the Any trait to allow for downcasting to a generic type
    // and skip the lifetime constraints
    #[inline]
    pub fn insert_any_mut<T: Tid<'ty> + 'static>(&mut self, any: &'r mut T) {
        let r: &'r mut dyn Tid<'ty> = any.into();
        self.data.insert(T::id(), Data::Mut(r));
    }

    // Get a borrowed value from the Context
    #[inline]
    pub fn get<'b, T: Tid<'ty>>(&'b self) -> Option<&'b T> {
        self.data.get(&T::id()).map(|v| v.downcast_ref()).flatten()
    }

    // Get a mutable value from the Context
    #[inline]
    pub fn get_mut<'b, T: Tid<'ty>>(&'b mut self) -> Option<&'b mut T> {
        self.data.get_mut(&T::id()).map(|v| v.downcast_mut()).flatten()
    }

    // Get a borrowed value from the Context by TypeId
    #[inline]
    pub fn get_data<'b>(&'b self, id: &TypeId) -> Option<&'b Data<'ty, 'r>> {
        self.data.get(id)
    }

    // Get a borrowed value from the Context by TypeId
    #[inline]
    pub fn get_data_mut<'b>(&'b mut self, id: &TypeId) -> Option<&'b mut Data<'ty, 'r>> {
        self.data.get_mut(id)
    }

    // Get many mutable values from the Context
    #[inline]
    pub fn get_many_mut<'b, const N: usize>(&'b mut self, keys: [&TypeId; N]) -> [Option<&'b mut Data<'ty, 'r>>; N] {
        self.data.get_many_mut(keys)
    }

    // Get an owned value from the Context
    #[inline]
    pub fn take<T: Tid<'ty>>(&mut self) -> Option<T> {
        let id = T::id();
        match self.data.remove(&id) {
            Some(data) => data.try_take_owned::<T>().ok(),
            None => None,
        }
    }

    // remove a value from the Context and returns it.
    #[inline]
    pub fn remove<T: Tid<'ty>>(&mut self) -> Option<Data<'ty, 'r>> {
        self.data.remove(&T::id())
    }

    // Check if the Context contains a value of a specific type.
    #[inline]
    pub fn contains<T: Tid<'ty>>(&self) -> bool {
        self.data.contains_key(&T::id())
    }

    // Clear the Context
    #[inline]
    pub fn clear(&mut self) {
        self.data.clear();
    }

    // Reset the gas & memory usage
    #[inline]
    pub fn reset_usage(&mut self) {
        self.current_gas = 0;
        self.current_memory = 0;
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

    #[test]
    fn test_context_no_immutable_err() {
        let mut dummy = Dummy("Hello, World!");
        {
            let mut context = Context::new();
            context.insert_mut(&mut dummy);
        }

        assert_eq!(dummy.0, "Hello, World!");
    }

    #[test]
    fn test_downcast_to_trait_any() {
        trait Foo {
            fn foo(&self) -> &str;
        }

        impl Foo for Dummy<'_> {
            fn foo(&self) -> &str {
                self.0
            }
        }

        let mut dummy = Dummy("Hello, World!");
        let mut context = Context::new();
        context.insert_any_mut(&mut dummy);

        fn inner_ref_fn<T: Foo + 'static>(context: &Context) {
            let data = context.get_data(&TypeId::of::<T>()).unwrap();
            data.downcast_ref_any::<T>()
                .unwrap()
                .foo();
        }

        inner_ref_fn::<Dummy>(&context);

        fn inner_mut_fn<T: Foo + 'static>(context: &mut Context) {
            let data = context.get_data_mut(&TypeId::of::<T>()).unwrap();
            data.downcast_mut_any::<T>()
                .unwrap()
                .foo();
        }

        inner_mut_fn::<Dummy>(&mut context);
    }

    #[test]
    fn test_downcast_to_trait() {
        trait Foo {
            fn foo(&self) -> &str;
        }

        impl Foo for Dummy<'_> {
            fn foo(&self) -> &str {
                self.0
            }
        }

        struct FooWrapper<'a, T: Foo + 'static>(&'a mut T);
        tid! { impl<'a, T: 'static> TidAble<'a> for FooWrapper<'a, T> where T: Foo }

        let mut dummy = Dummy("Hello, World!");
        let mut context = Context::new();
        context.insert(FooWrapper(&mut dummy));

        fn inner_ref_fn<T: Foo + 'static>(context: &Context) {
            let data = context.get_data(&FooWrapper::<T>::id())
                .expect("Data not found");
            data.downcast_ref::<FooWrapper<T>>()
                .expect("Downcast failed")
                .0
                .foo();
        }

        inner_ref_fn::<Dummy>(&context);

        fn inner_mut_fn<T: Foo + 'static>(context: &mut Context) {
            let data = context.get_data_mut(&FooWrapper::<T>::id())
                .expect("Data not found");
            data.downcast_mut::<FooWrapper<T>>()
                .expect("Downcast failed")
                .0
                .foo();
        }

        inner_mut_fn::<Dummy>(&mut context);
    }
}