use xelis_types::{Type, Primitive, ValueError};
use xelis_environment::{
    Context,
    EnvironmentError,
    FnInstance,
    FnParams,
    FnReturnType,
    SysCallResult
};
use super::EnvironmentBuilder;

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    env.register_native_function("is_none", Some(Type::Optional(Box::new(Type::T(Some(0))))), vec![], is_none, 1, Some(Type::Bool));
    env.register_native_function("is_some", Some(Type::Optional(Box::new(Type::T(Some(0))))), vec![], is_some, 1, Some(Type::Bool));
    env.register_native_function("unwrap", Some(Type::Optional(Box::new(Type::T(Some(0))))), vec![], unwrap, 1, Some(Type::T(Some(0))));
    env.register_native_function("unwrap_or", Some(Type::Optional(Box::new(Type::T(Some(0))))), vec![("default", Type::T(Some(0)))], unwrap_or, 1, Some(Type::T(Some(0))));
    env.register_native_function("expect", Some(Type::Optional(Box::new(Type::T(Some(0))))), vec![("msg", Type::String)], expect, 15, Some(Type::T(Some(0))));
}

fn is_none<M>(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType<M> {
    Ok(SysCallResult::Return(Primitive::Boolean(zelf?.is_null()).into()))
}

fn is_some<M>(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType<M> {
    Ok(SysCallResult::Return(Primitive::Boolean(!zelf?.is_null()).into()))
}

fn unwrap<M>(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType<M> {
    let opt = zelf?.take_as_optional()?.ok_or(ValueError::OptionalIsNull)?;
    Ok(SysCallResult::Return(opt))
}

fn unwrap_or<M>(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType<M> {
    let default = parameters.remove(0);
    let optional = zelf?.take_as_optional()?;
    match optional {
        Some(value) => Ok(SysCallResult::Return(value)),
        None => Ok(SysCallResult::Return(default.into_owned()?))
    }
}

fn expect<M>(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType<M> {
    let mut param = parameters.remove(0)
        .into_owned()?;
    let msg = param.into_string()?;

    if !msg.chars().all(|c| c.is_alphanumeric() || c == ' ') {
        return Err(EnvironmentError::InvalidExpect);
    }

    let opt = zelf?.take_as_optional()?
        .ok_or(EnvironmentError::Expect(msg))?;

    Ok(SysCallResult::Return(opt))
}