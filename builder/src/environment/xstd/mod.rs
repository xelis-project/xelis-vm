mod array;
mod optional;
mod string;
mod integer;
mod range;
mod map;

use xelis_types::Type;
use xelis_environment::{
    EnvironmentError,
    FnInstance,
    FnParams,
    FnReturnType,
    Context,
};
use super::EnvironmentBuilder;

pub fn register(env: &mut EnvironmentBuilder) {
    array::register(env);
    optional::register(env);
    string::register(env);
    integer::register(env);
    range::register(env);
    map::register(env);

    env.register_native_function("println", None, vec![Type::Any], println, 1, None);
    env.register_native_function("debug", None, vec![Type::Any], debug, 1, None);

    env.register_native_function("panic", None, vec![Type::Any], panic, 1, Some(Type::Any));
}

fn println(_: FnInstance, parameters: FnParams, _: &mut Context) -> FnReturnType {
    let param = &parameters[0];
    println!("{}", param.as_ref().as_value());

    Ok(None)
}

fn debug(_: FnInstance, parameters: FnParams, _: &mut Context) -> FnReturnType {
    let param = &parameters[0];
    println!("{:?}", param.as_ref().as_value());

    Ok(None)
}

fn panic(_: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let param = parameters.remove(0);
    let value = param.into_owned();

    Err(EnvironmentError::Panic(value))
}