mod environment;
mod manager;
mod mapper;
mod hook;

use thiserror::Error;
use xelis_types::{Type, ValueError};

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
    #[error("Function instance mismatch for {name} on type {on_type:?} with parameters {parameter_types:?}")]
    FunctionInstanceMismatch {
        name: String,
        on_type: Option<Type>,
        parameter_types: Vec<Option<Type>>,
    },
    #[error("No matching function found for {name} on type {on_type:?} with parameters {parameter_types:?}")]
    NoMatchingFunction {
        name: String,
        on_type: Option<Type>,
        // a type can be None in case we can't determine it (eg. generic type)
        parameter_types: Vec<Option<Type>>,
    },
    #[error(transparent)]
    Any(#[from] anyhow::Error)
}

#[cfg(test)]
mod tests {
    use std::collections::VecDeque;

    use xelis_bytecode::Module;
    use xelis_environment::{Context, FnInstance, FnParams, FnReturnType, FunctionHandler, ModuleMetadata, SysCallResult, tid};
    use xelis_types::Reference;
    use crate::EnvironmentBuilder;

    trait Foo: Send + Sync {}

    struct FooImpl;
    tid!(FooImpl);

    impl Foo for FooImpl {}

    struct FooWrapper<F: Foo>(pub F);

    tid! { impl<'a, F: 'static> TidAble<'a> for FooWrapper<F> where F: Foo }

    fn bar<'a, 'ty, 'r, M, F: Foo + 'static>(_: FnInstance<'a>, _: FnParams, _: &ModuleMetadata<'_, M>, context: &'a mut Context<'ty, 'r>) -> FnReturnType<M> {
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
    fn test_context_lifetime() {
        let env = build_env::<FooImpl>();
        let f = env.get_functions()
            .last()
            .unwrap();

        let mut context = Context::new();
        context.insert(FooWrapper(FooImpl));

        assert!((f.call_function(VecDeque::new(), &ModuleMetadata {
            module: Reference::Borrowed(&Module::new()),
            metadata: Reference::Borrowed(&()),
            environment: Reference::Borrowed(env.environment()),
        }, &mut context)).unwrap().is_none());
    }
}