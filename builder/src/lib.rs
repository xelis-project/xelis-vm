mod environment;
mod manager;
mod mapper;
mod hook;

use thiserror::Error;
use xelis_types::ValueError;

pub use environment::*;
pub use manager::*;
pub use mapper::*;
pub use hook::Hook;

#[derive(Debug, Error)]
pub enum BuilderError {
    #[error("Struct name already used")]
    StructNameAlreadyUsed,
    #[error("Type not found")]
    TypeNotFound,
    #[error("mapping not found")]
    MappingNotFound,
    #[error("mapping already exists")]
    MappingExists,
    #[error("Signature already registered")]
    SignatureAlreadyRegistered,
    #[error("Invalid signature")]
    InvalidSignature,
    #[error(transparent)]
    ValueError(#[from] ValueError),
    #[error("Invalid const fn parameters")]
    InvalidConstFnParameters,
    #[error("Invalid const fn parameters: mismatch")]
    InvalidConstFnParametersMismatch,
    #[error("Function instance mismatch")]
    FunctionInstanceMismatch,
    #[error(transparent)]
    Any(#[from] anyhow::Error)
}

#[cfg(test)]
mod tests {
    use xelis_environment::{tid, Context, FnInstance, FnParams, FnReturnType, FunctionHandler, SysCallResult};
    use crate::EnvironmentBuilder;

    trait Foo {}

    struct FooImpl;
    tid!(FooImpl);

    impl Foo for FooImpl {}

    struct FooWrapper<F: Foo>(pub F);

    tid! { impl<'a, F: 'static> TidAble<'a> for FooWrapper<F> where F: Foo }

    fn bar<'a, 'ty, 'r, M, F: Foo + 'static>(_: FnInstance<'a>, _: FnParams, context: &'a mut Context<'ty, 'r>) -> FnReturnType<M> {
        let _: &FooWrapper<F> = context.get().unwrap();
        Ok(SysCallResult::None)
    }

    fn build_env<'a, F: Foo + 'static>() -> EnvironmentBuilder<'a, ()> {
        let mut env = EnvironmentBuilder::default();
        env.register_native_function(
            "bar",
            None,
            vec![],
            FunctionHandler::Sync(bar::<(), F>),
            1000,
            None
        );

        env
    }

    #[test]
    fn test_context_lifetime<'a>() {
        let env = build_env::<FooImpl>();
        let f = env.get_functions()
            .last()
            .unwrap();

        let mut context = Context::new();
        context.insert(FooWrapper(FooImpl));

        assert!(futures::executor::block_on(f.call_function(None, vec![], &mut context)).unwrap().is_none());
    }
}