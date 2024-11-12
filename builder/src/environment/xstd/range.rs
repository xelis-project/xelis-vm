use xelis_environment::{EnvironmentError, FnInstance, FnParams, FnReturnType};
use xelis_types::{Type, Value, ValueOwnable};
use paste::paste;

use crate::EnvironmentBuilder;

macro_rules! contains {
    ($t: ident, $start: expr, $end: expr, $value: expr) => {
        paste! {
            {
                let start = $start.[<as_ $t>]()?;
                let end = $end.[<as_ $t>]()?;
                let value = $value.[<as_ $t>]()?;
                Value::Boolean((start..end).contains(&value))
            }
        }
    };
}

macro_rules! collect {
    ($t: ident, $start: expr, $end: expr, $type: ident) => {
        paste! {
            {
                let start = $start.[<as_ $type>]()?;
                let end = $end.[<as_ $type>]()?;
                let vec = (start..end).map(|i| ValueOwnable::Owned(Box::new(Value::$t(i)))).collect();
                Value::Array(vec)
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
                Value::$t(count)
            }
        }
    };
}

pub fn register(env: &mut EnvironmentBuilder) {
    env.register_native_function("contains", Some(Type::Range(Box::new(Type::T))), vec![Type::T], contains, 5, Some(Type::Bool));
    env.register_native_function("collect", Some(Type::Range(Box::new(Type::T))), vec![], collect, 500, Some(Type::Array(Box::new(Type::T))));
    env.register_native_function("max", Some(Type::Range(Box::new(Type::T))), vec![], max, 1, Some(Type::T));
    env.register_native_function("min", Some(Type::Range(Box::new(Type::T))), vec![], min, 1, Some(Type::T));
    env.register_native_function("count", Some(Type::Range(Box::new(Type::T))), vec![], count, 5, Some(Type::T));
}

fn contains(zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
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
        _ => return Err(EnvironmentError::InvalidType(zelf.clone()))
    }))
}

fn collect(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let zelf = zelf?;
    let (start, end, _type) = zelf.as_range()?;
    Ok(Some(match _type {
        Type::U8 => collect!(U8, start, end, u8),
        Type::U16 => collect!(U16, start, end, u16),
        Type::U32 => collect!(U32, start, end, u32),
        Type::U64 => collect!(U64, start, end, u64),
        Type::U128 => collect!(U128, start, end, u128),
        Type::U256 => {
            let start = start.as_u256()?;
            let end = end.as_u256()?;
            let mut vec = Vec::new();
            let (diff, overflow) = end.overflowing_sub(start);
            if !overflow {
                if diff.low_u64() > u32::MAX as u64 {
                    return Err(EnvironmentError::RangeTooLarge);
                }

                let max: u32 = diff.into();
                for i in 0..max {
                    vec.push(ValueOwnable::Owned(Box::new(Value::U256(i.into()))));
                }
            }

            Value::Array(vec)
        }
        _ => return Err(EnvironmentError::InvalidType(zelf.clone()))
    }))
}

fn max(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let zelf = zelf?;
    let (_, end, _) = zelf.as_range()?;
    Ok(Some(end.clone()))
}

fn min(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let zelf = zelf?;
    let (start, _, _) = zelf.as_range()?;
    Ok(Some(start.clone()))
}

fn count(zelf: FnInstance, _: FnParams) -> FnReturnType {
    let zelf = zelf?;
    let (start, end, _type) = zelf.as_range()?;

    Ok(Some(match _type {
        Type::U8 => count!(U8, start, end, u8),
        Type::U16 => count!(U16, start, end, u16),
        Type::U32 => count!(U32, start, end, u32),
        Type::U64 => count!(U64, start, end, u64),
        Type::U128 => count!(U128, start, end, u128),
        Type::U256 => count!(U256, start, end, u256),
        _ => return Err(EnvironmentError::InvalidType(zelf.clone()))
    }))
}