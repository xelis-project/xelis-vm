mod array;
mod optional;
mod string;
mod integer;
mod range;
mod map;
mod bytes;
mod math;

use std::ptr;

use xelis_types::{Primitive, Type};
use xelis_environment::{
    Context,
    EnvironmentError,
    FnInstance,
    FnParams,
    FnReturnType,
    FunctionHandler,
    SysCallResult
};
use super::EnvironmentBuilder;

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    array::register(env);
    bytes::register(env);
    optional::register(env);
    string::register(env);
    integer::register(env);
    range::register(env);
    map::register(env);
    math::register(env);

    env.register_native_function("println", None, vec![("value", Type::Any)], FunctionHandler::Sync(println), 1, None);
    env.register_native_function("debug", None, vec![("value", Type::Any)], FunctionHandler::Sync(debug), 1, None);
    env.register_native_function("panic", None, vec![("value", Type::Any)], FunctionHandler::Sync(panic), 1, Some(Type::Any));
    env.register_native_function("assert", None, vec![("value", Type::Bool)], FunctionHandler::Sync(assert), 1, None);
    env.register_native_function("is_same_ptr", None, vec![("left", Type::Any), ("right", Type::Any)], FunctionHandler::Sync(is_same_ptr), 5, Some(Type::Bool));
    env.register_native_function("require", None, vec![("condition", Type::Bool), ("msg", Type::String)], FunctionHandler::Sync(require), 1, None);
    env.register_native_function("clone", Some(Type::T(None)), vec![], FunctionHandler::Sync(clone), 5, Some(Type::T(None)));
}

fn println<M>(_: FnInstance, parameters: FnParams, _: &mut Context) -> FnReturnType<M> {
    let param = &parameters[0];
    println!("{}", param.as_ref()?);

    Ok(SysCallResult::None)
}

fn debug<M>(_: FnInstance, parameters: FnParams, _: &mut Context) -> FnReturnType<M> {
    let param = &parameters[0];
    println!("{:?}", param);

    Ok(SysCallResult::None)
}

fn panic<M>(_: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType<M> {
    let param = parameters.remove(0);
    let value = param.into_owned()?;

    Err(EnvironmentError::Panic(format!("{:#}", value)))
}

fn assert<M>(_: FnInstance, parameters: FnParams, _: &mut Context) -> FnReturnType<M> {
    let param = &parameters[0];
    let value = param.as_bool()?;

    if value {
        Ok(SysCallResult::None)
    } else {
        Err(EnvironmentError::AssertionFailed)
    }
}

fn is_same_ptr<M>(_: FnInstance, parameters: FnParams, _: &mut Context) -> FnReturnType<M> {
    let left = parameters[0].as_ref()?;
    let right = parameters[1].as_ref()?;
    let same = ptr::from_ref(left) == ptr::from_ref(right);

    Ok(SysCallResult::Return(Primitive::Boolean(same).into()))
}

fn require<M>(_: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType<M> {
    let msg = parameters.remove(1)
        .into_owned()?
        .into_string()?;

    if !msg.chars().all(|c| c.is_alphanumeric() || c == ' ') {
        return Err(EnvironmentError::InvalidExpect);
    }

    let param = &parameters[0];
    let value = param.as_bool()?;

    if value {
        Ok(SysCallResult::None)
    } else {
        Err(EnvironmentError::Expect(msg))
    }
}

fn clone<M>(zelf: FnInstance, _: FnParams, context: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;

    let memory = zelf.calculate_memory_usage(context.memory_left())?;
    // Double cost: computation for cloning and memory allocation?
    context.increase_memory_usage_unchecked(memory)?;

    Ok(SysCallResult::Return(zelf.deep_clone().into()))
}