use xelis_types::{Type, Primitive, ValueError};
use xelis_environment::{
    Context,
    EnvironmentError,
    FnInstance,
    FnParams,
    FnReturnType
};
use super::EnvironmentBuilder;

pub fn register(env: &mut EnvironmentBuilder) {
    env.register_native_function("is_none", Some(Type::Optional(Box::new(Type::T(0)))), vec![], is_none, 1, Some(Type::Bool));
    env.register_native_function("is_some", Some(Type::Optional(Box::new(Type::T(0)))), vec![], is_some, 1, Some(Type::Bool));
    env.register_native_function("unwrap", Some(Type::Optional(Box::new(Type::T(0)))), vec![], unwrap, 1, Some(Type::T(0)));
    env.register_native_function("unwrap_or", Some(Type::Optional(Box::new(Type::T(0)))), vec![("default", Type::T(0))], unwrap_or, 1, Some(Type::T(0)));
    env.register_native_function("expect", Some(Type::Optional(Box::new(Type::T(0)))), vec![("msg", Type::String)], expect, 15, Some(Type::T(0)));
}

fn is_none(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    Ok(Some(Primitive::Boolean(zelf?.is_null()).into()))
}

fn is_some(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    Ok(Some(Primitive::Boolean(!zelf?.is_null()).into()))
}

fn unwrap(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let opt = zelf?.take_as_optional()?.ok_or(ValueError::OptionalIsNull)?;
    Ok(Some(opt))
}

fn unwrap_or(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let default = parameters.remove(0);
    let optional = zelf?.take_as_optional()?;
    match optional {
        Some(value) => Ok(Some(value)),
        None => Ok(Some(default.into_owned()?))
    }
}

fn expect(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let mut param = parameters.remove(0)
        .into_owned()?;
    let msg = param.into_string()?;

    if !msg.chars().all(|c| c.is_alphanumeric() || c == ' ') {
        return Err(EnvironmentError::InvalidExpect);
    }

    let opt = zelf?.take_as_optional()?
        .ok_or(EnvironmentError::Expect(msg))?;

    Ok(Some(opt))
}