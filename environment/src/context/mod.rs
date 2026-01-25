mod data;
mod vm;

use std::{
    any::TypeId,
    hash::{BuildHasherDefault, Hasher}
};
use hashbrown::HashMap;

pub use data::*;
pub use vm::*;

/// A hasher for `TypeId`s that takes advantage of its known characteristics.
#[derive(Debug, Default)]
pub struct TypeIdHasher(u64);

impl Hasher for TypeIdHasher {
    fn write(&mut self, _: &[u8]) {
        unimplemented!("This TypeIdHasher can only handle u64s")
    }

    fn write_u64(&mut self, i: u64) {
        self.0 = i;
    }

    fn finish(&self) -> u64 {
        self.0
    }
}
    
/// Context is a simple data store that allows for storing and retrieving values of different types.
pub struct Context<'ty, 'r> {
    data: HashMap<TypeId, Data<'ty, 'r>, BuildHasherDefault<TypeIdHasher>>,

}

impl Default for Context<'_, '_> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl<'ty, 'r> Context<'ty, 'r> {
    // Create a new Context
    #[inline]
    pub fn new() -> Self {
        Self {
            data: HashMap::default(),
        }
    }

    // Insert a value into the Context without checking the type
    #[inline]
    pub fn insert_unchecked(&mut self, key: TypeId, data: Data<'ty, 'r>) {
        self.data.insert(key, data);
    }

    // Insert a borrowed value into the Context
    #[inline]
    pub fn insert_ref<T: ShareableTid<'ty>>(&mut self, value: &'r T) {
        self.data.insert(T::id(), Data::Borrowed(value));
    }

    // Insert a mutable value into the Context
    #[inline]
    pub fn insert_mut<T: ShareableTid<'ty>>(&mut self, value: &'r mut T) {
        self.data.insert(T::id(), Data::Mut(value));
    }

    // Insert an owned value into the Context
    #[inline]
    pub fn insert<T: ShareableTid<'ty>>(&mut self, value: T) {
        self.data.insert(T::id(), Data::Owned(Box::new(value)));
    }

    // Get a borrowed value from the Context
    #[inline]
    pub fn get<'b, T: ShareableTid<'ty>>(&'b self) -> Option<&'b T> {
        self.data.get(&T::id()).map(|v| v.downcast_ref()).flatten()
    }

    // Get a mutable value from the Context
    #[inline]
    pub fn get_mut<'b, T: ShareableTid<'ty>>(&'b mut self) -> Option<&'b mut T> {
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
    pub fn take<T: ShareableTid<'ty>>(&mut self) -> Option<T> {
        let id = T::id();
        match self.data.remove(&id) {
            Some(data) => data.try_take_owned::<T>().ok(),
            None => None,
        }
    }

    // remove a value from the Context and returns it.
    #[inline]
    pub fn remove<T: ShareableTid<'ty>>(&mut self) -> Option<Data<'ty, 'r>> {
        self.data.remove(&T::id())
    }

    // Check if the Context contains a value of a specific type.
    #[inline]
    pub fn contains<T: ShareableTid<'ty>>(&self) -> bool {
        self.data.contains_key(&T::id())
    }

    // Clear the Context
    #[inline]
    pub fn clear(&mut self) {
        self.data.clear();
    }
}

#[cfg(test)]
mod tests {
    use better_any::{tid, Tid};

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