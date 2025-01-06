mod array;
mod optional;
mod string;
mod integer;
mod range;
mod map;

use xelis_types::{Type, Value};
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

    env.register_native_function("println", None, vec![("value", Type::Any)], println, 1, None);
    env.register_native_function("debug", None, vec![("value", Type::Any)], debug, 1, None);
    env.register_native_function("panic", None, vec![("value", Type::Any)], panic, 1, Some(Type::Any));
    env.register_native_function("require", None, vec![("value", Type::Any), ("value", Type::Any)], require, 1, Some(Type::Any));
    env.register_native_function("assert", None, vec![("value", Type::Bool)], assert, 1, None);
    env.register_native_function("is_same_ptr", None, vec![("left", Type::Any), ("right", Type::Any)], is_same_ptr, 5, Some(Type::Bool));
    env.register_native_function("require", None, vec![("value", Type::Bool)], require, 1, None);
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

    Err(EnvironmentError::Panic(format!("{:#}", value)))
}

fn assert(_: FnInstance, parameters: FnParams, _: &mut Context) -> FnReturnType {
    let param = &parameters[0];
    let value = param.as_ref().as_bool()?;

    if value {
        Ok(None)
    } else {
        Err(EnvironmentError::AssertionFailed)
    }
}

fn require(_: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let param = &parameters[0];
    let value = param.as_ref().as_bool()?;

    let msg_param = parameters.remove(1);
    let msg = msg_param.into_owned();

    if value {
        Ok(None)
    } else {
        Err(EnvironmentError::Panic(format!("{:#}", msg)))
    }
}

fn is_same_ptr(_: FnInstance, parameters: FnParams, _: &mut Context) -> FnReturnType {
    let same = parameters[0].is_same_ptr(&parameters[1]);
    Ok(Some(Value::Boolean(same).into()))
}

fn require(_: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let msg = parameters.remove(1)
        .into_owned()
        .to_string()?;

    if !msg.chars().all(|c| c.is_alphanumeric() || c == ' ') {
        return Err(EnvironmentError::InvalidExpect);
    }

    let param = &parameters[0];
    let value = param.as_ref().as_bool()?;

    if value {
        Ok(None)
    } else {
        Err(EnvironmentError::Expect(msg))
    }
}