mod array;
mod optional;
mod string;
mod integer;
mod range;

use xelis_types::Type;
use xelis_environment::{EnvironmentError, FnInstance, FnParams, FnReturnType};
use super::EnvironmentBuilder;

pub fn register(env: &mut EnvironmentBuilder) {
    array::register(env);
    optional::register(env);
    string::register(env);
    integer::register(env);
    range::register(env);

    env.register_native_function("println", None, vec![Type::Any], println, 1, None);
    env.register_native_function("panic", None, vec![Type::Any], panic, 1, Some(Type::Any));
}

fn println(_: FnInstance, parameters: FnParams) -> FnReturnType {
    let param = &parameters[0];
    println!("{}", param.as_ref().as_value());

    Ok(None)
}

fn panic(_: FnInstance, mut parameters: FnParams) -> FnReturnType {
    let param = parameters.remove(0);
    let value = param.into_owned();

    Err(EnvironmentError::Panic(value))
}