use xelis_environment::{Context, EnvironmentError, FnInstance, FnParams, FnReturnType};
use xelis_types::{Type, Value, ValueCell};
use paste::paste;

use crate::EnvironmentBuilder;

macro_rules! contains {
    ($t: ident, $start: expr, $end: expr, $value: expr) => {
        paste! {
            {
                let start = $start.[<as_ $t>]()?;
                let end = $end.[<as_ $t>]()?;
                let value = $value.[<as_ $t>]()?;
                Value::Boolean((start..end).contains(&value)).into()
            }
        }
    };
}

macro_rules! collect {
    ($t: ident, $start: expr, $end: expr, $type: ident, $context: expr) => {
        paste! {
            {
                let start = $start.[<as_ $type>]()?;
                let end = $end.[<as_ $type>]()?;

                if start >= end {
                    ValueCell::Array(Vec::new())
                } else {
                    let diff = end - start;
                    if diff > u32::MAX as _ {
                        return Err(EnvironmentError::RangeTooLarge);
                    }

                    $context.increase_gas_usage(diff as u64 * 8)?;

                    let vec = (start..end).map(|i| Value::$t(i).into()).collect();
                    ValueCell::Array(vec)
                }
            }
        }
    };
}

macro_rules! count {
    ($t: ident, $start: expr, $end: expr, $type: ident) => {
        paste! {
            {
                let start = $start.[<as_ $type>]()?;
                let end = $end.[<as_ $type>]()?;
                let count = end.checked_sub(start).unwrap_or(Default::default());
                Value::$t(count).into()
            }
        }
    };
}

pub fn register(env: &mut EnvironmentBuilder) {
    let _type = Type::Range(Box::new(Type::T(0)));
    env.register_native_function("contains", Some(_type.clone()), vec![("value", Type::T(0))], contains, 5, Some(Type::Bool));
    env.register_native_function("collect", Some(_type.clone()), vec![], collect, 20, Some(Type::Array(Box::new(Type::T(0)))));
    env.register_native_function("max", Some(_type.clone()), vec![], max, 1, Some(Type::T(0)));
    env.register_native_function("min", Some(_type.clone()), vec![], min, 1, Some(Type::T(0)));
    env.register_native_function("count", Some(_type.clone()), vec![], count, 5, Some(Type::T(0)));
}

fn contains(zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
    let value = parameters.remove(0);
    let zelf = zelf?;
    let (start, end, _type) = zelf.as_range()?;

    let value = value.as_ref();
    Ok(Some(match _type {
        Type::U8 => contains!(u8, start, end, value),
        Type::U16 => contains!(u16, start, end, value),
        Type::U32 => contains!(u32, start, end, value),
        Type::U64 => contains!(u64, start, end, value),
        Type::U128 => contains!(u128, start, end, value),
        Type::U256 => contains!(u256, start, end, value),
        _ => return Err(EnvironmentError::InvalidType)
    }))
}

fn collect(zelf: FnInstance, _: FnParams, context: &mut Context) -> FnReturnType {
    let zelf = zelf?;
    let (start, end, _type) = zelf.as_range()?;
    Ok(Some(match _type {
        Type::U8 => collect!(U8, start, end, u8, context),
        Type::U16 => collect!(U16, start, end, u16, context),
        Type::U32 => collect!(U32, start, end, u32, context),
        Type::U64 => collect!(U64, start, end, u64, context),
        Type::U128 => collect!(U128, start, end, u128, context),
        Type::U256 => {
            let start = start.as_u256()?;
            let end = end.as_u256()?;
            let (diff, overflow) = end.overflowing_sub(start);

            let mut vec = Vec::new();
            if !overflow {
                let diff = diff.as_u64().ok_or(EnvironmentError::InvalidParameter)?;
                if diff > u32::MAX as u64 {
                    return Err(EnvironmentError::RangeTooLarge);
                }

                context.increase_gas_usage(diff as u64 * 8)?;

                for i in 0..diff {
                    vec.push(Value::U256(i.into()).into());
                }
            }

            ValueCell::Array(vec)
        }
        _ => return Err(EnvironmentError::InvalidType)
    }))
}

fn max(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let zelf = zelf?;
    let (_, end, _) = zelf.as_range()?;
    Ok(Some(end.clone().into()))
}

fn min(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let zelf = zelf?;
    let (start, _, _) = zelf.as_range()?;
    Ok(Some(start.clone().into()))
}

fn count(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let zelf = zelf?;
    let (start, end, _type) = zelf.as_range()?;

    Ok(Some(match _type {
        Type::U8 => count!(U8, start, end, u8),
        Type::U16 => count!(U16, start, end, u16),
        Type::U32 => count!(U32, start, end, u32),
        Type::U64 => count!(U64, start, end, u64),
        Type::U128 => count!(U128, start, end, u128),
        Type::U256 => count!(U256, start, end, u256),
        _ => return Err(EnvironmentError::InvalidType)
    }))
}