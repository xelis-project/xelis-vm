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
    use xelis_environment::{tid, Tid, Context, FnInstance, FnParams, FnReturnType};
    use crate::EnvironmentBuilder;

    trait Foo {}

    struct FooImpl;
    tid!(FooImpl);

    impl Foo for FooImpl {}

    fn bar<'ty, F: Foo + Tid<'ty>>(_: FnInstance, _: FnParams, context: &mut Context<'ty, '_>) -> FnReturnType {
        let _: &F = context.get().unwrap();
        Ok(None)
    }

    fn build_env<'a, F: Foo + Tid<'a>>() -> EnvironmentBuilder<'a> {
        let mut env = EnvironmentBuilder::default();
        env.register_native_function(
            "bar",
            None,
            vec![],
            bar::<F>,
            1000,
            None
        );

        env
    }

    #[test]
    fn test_context_lifetime<'a>() {
        build_env::<'a, FooImpl>();
    }
}