use xelis_environment::{Context, EnvironmentError, FnInstance, FnParams, FnReturnType};
use xelis_types::{Type, Value, ValueCell};
use paste::paste;

use crate::EnvironmentBuilder;

macro_rules! contains {
    ($t: ident, $start: expr, $end: expr, $value: expr) => {
        paste! {
            {
                let value = $value.[<as_ $t>]()?;
                Value::Boolean((*$start..*$end).contains(&value)).into()
            }
        }
    };
}

macro_rules! collect {
    ($t: ident, $start: expr, $end: expr, $type: ident, $context: expr) => {
        paste! {
            {
                if $start >= $end {
                    ValueCell::Array(Vec::new())
                } else {
                    let diff = $end - $start;
                    if diff > u32::MAX as _ {
                        return Err(EnvironmentError::RangeTooLarge);
                    }

                    $context.increase_gas_usage(diff as u64 * 8)?;

                    let vec = (*$start..*$end).map(|i| Value::$t(i).into()).collect();
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
                let count = $end.checked_sub(*$start).unwrap_or(Default::default());
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
    let (start, end) = zelf.as_range()?;

    let value = value.as_ref();
    Ok(Some(match (start, end) {
        (Value::U8(start), Value::U8(end)) => contains!(u8, start, end, value),
        (Value::U16(start), Value::U16(end)) => contains!(u16, start, end, value),
        (Value::U32(start), Value::U32(end)) => contains!(u32, start, end, value),
        (Value::U64(start), Value::U64(end)) => contains!(u64, start, end, value),
        (Value::U128(start), Value::U128(end)) => contains!(u128, start, end, value),
        (Value::U256(start), Value::U256(end)) => contains!(u256, start, end, value),
        _ => return Err(EnvironmentError::InvalidType)
    }))
}

fn collect(zelf: FnInstance, _: FnParams, context: &mut Context) -> FnReturnType {
    let zelf = zelf?;
    let (start, end) = zelf.as_range()?;
    Ok(Some(match (start, end) {
        (Value::U8(start), Value::U8(end)) => collect!(U8, start, end, u8, context),
        (Value::U16(start), Value::U16(end)) => collect!(U16, start, end, u16, context),
        (Value::U32(start), Value::U32(end)) => collect!(U32, start, end, u32, context),
        (Value::U64(start), Value::U64(end)) => collect!(U64, start, end, u64, context),
        (Value::U128(start), Value::U128(end)) => collect!(U128, start, end, u128, context),
        (Value::U256(start), Value::U256(end)) => {
            let (diff, overflow) = end.overflowing_sub(*start);

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
    let (_, end) = zelf.as_range()?;
    Ok(Some(end.clone().into()))
}

fn min(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let zelf = zelf?;
    let (start, _) = zelf.as_range()?;
    Ok(Some(start.clone().into()))
}

fn count(zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
    let zelf = zelf?;
    let (start, end) = zelf.as_range()?;

    Ok(Some(match (start, end) {
        (Value::U8(start), Value::U8(end)) => count!(U8, start, end, u8),
        (Value::U16(start), Value::U16(end)) => count!(U16, start, end, u16),
        (Value::U32(start), Value::U32(end)) => count!(U32, start, end, u32),
        (Value::U64(start), Value::U64(end)) => count!(U64, start, end, u64),
        (Value::U128(start), Value::U128(end)) => count!(U128, start, end, u128),
        (Value::U256(start), Value::U256(end)) => count!(U256, start, end, u256),
        _ => return Err(EnvironmentError::InvalidType)
    }))
}