use xelis_bytecode::ModuleMetadata;
use xelis_environment::{
    Context,
    EnvironmentError,
    FnInstance,
    FnParams,
    FnReturnType,
    FunctionHandler,
    SysCallResult
};
use xelis_types::{Type, Primitive, ValueCell};
use paste::paste;

use crate::EnvironmentBuilder;

macro_rules! contains {
    ($t: ident, $start: expr, $end: expr, $value: expr) => {
        paste! {
            {
                let value = $value.[<as_ $t>]()?;
                Primitive::Boolean((*$start..*$end).contains(&value)).into()
            }
        }
    };
}

macro_rules! collect {
    ($t: ident, $start: expr, $end: expr, $type: ident, $context: expr) => {
        paste! {
            {
                if $start >= $end {
                    ValueCell::Object(Vec::new())
                } else {
                    let diff = $end - $start;
                    if diff > u32::MAX as _ {
                        return Err(EnvironmentError::RangeTooLarge);
                    }

                    $context.increase_gas_usage(diff as u64 * 8)?;

                    let vec = (*$start..*$end).map(|i| Primitive::$t(i).into()).collect();
                    ValueCell::Object(vec)
                }.into()
            }
        }
    };
}

macro_rules! count {
    ($t: ident, $start: expr, $end: expr, $type: ident) => {
        paste! {
            {
                let count = $end.checked_sub(*$start).unwrap_or(Default::default());
                Primitive::$t(count).into()
            }
        }
    };
}

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    let _type = Type::Range(Box::new(Type::T(Some(0))));
    env.register_native_function("contains", Some(_type.clone()), vec![("value", Type::T(Some(0)))], FunctionHandler::Sync(contains), 5, Some(Type::Bool));
    env.register_native_function("collect", Some(_type.clone()), vec![], FunctionHandler::Sync(collect), 20, Some(Type::Array(Box::new(Type::T(Some(0))))));
    env.register_native_function("max", Some(_type.clone()), vec![], FunctionHandler::Sync(max), 1, Some(Type::T(Some(0))));
    env.register_native_function("min", Some(_type.clone()), vec![], FunctionHandler::Sync(min), 1, Some(Type::T(Some(0))));
    env.register_native_function("count", Some(_type.clone()), vec![], FunctionHandler::Sync(count), 5, Some(Type::T(Some(0))));
}

fn contains<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let value = parameters.remove(0);
    let zelf = zelf?;
    let (start, end) = zelf.as_range()?;

    let value = value.as_ref();
    Ok(SysCallResult::Return(match (start, end) {
        (Primitive::U8(start), Primitive::U8(end)) => contains!(u8, start, end, value),
        (Primitive::U16(start), Primitive::U16(end)) => contains!(u16, start, end, value),
        (Primitive::U32(start), Primitive::U32(end)) => contains!(u32, start, end, value),
        (Primitive::U64(start), Primitive::U64(end)) => contains!(u64, start, end, value),
        (Primitive::U128(start), Primitive::U128(end)) => contains!(u128, start, end, value),
        (Primitive::U256(start), Primitive::U256(end)) => contains!(u256, start, end, value),
        _ => return Err(EnvironmentError::InvalidType)
    }))
}

fn collect<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let (start, end) = zelf.as_range()?;
    Ok(SysCallResult::Return(match (start, end) {
        (Primitive::U8(start), Primitive::U8(end)) => collect!(U8, start, end, u8, context),
        (Primitive::U16(start), Primitive::U16(end)) => collect!(U16, start, end, u16, context),
        (Primitive::U32(start), Primitive::U32(end)) => collect!(U32, start, end, u32, context),
        (Primitive::U64(start), Primitive::U64(end)) => collect!(U64, start, end, u64, context),
        (Primitive::U128(start), Primitive::U128(end)) => collect!(U128, start, end, u128, context),
        (Primitive::U256(start), Primitive::U256(end)) => {
            let (diff, overflow) = end.overflowing_sub(*start);

            let mut vec = Vec::new();
            if !overflow {
                let diff = diff.as_u64().ok_or(EnvironmentError::InvalidParameter)?;
                if diff > u32::MAX as u64 {
                    return Err(EnvironmentError::RangeTooLarge);
                }

                context.increase_gas_usage(diff as u64 * 8)?;

                for i in 0..diff {
                    vec.push(Primitive::U256(i.into()).into());
                }
            }

            ValueCell::Object(vec).into()
        }
        _ => return Err(EnvironmentError::InvalidType)
    }))
}

fn max<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let (_, end) = zelf.as_range()?;
    Ok(SysCallResult::Return(end.clone().into()))
}

fn min<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let (start, _) = zelf.as_range()?;
    Ok(SysCallResult::Return(start.clone().into()))
}

fn count<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
    let zelf = zelf?;
    let (start, end) = zelf.as_range()?;

    Ok(SysCallResult::Return(match (start, end) {
        (Primitive::U8(start), Primitive::U8(end)) => count!(U8, start, end, u8),
        (Primitive::U16(start), Primitive::U16(end)) => count!(U16, start, end, u16),
        (Primitive::U32(start), Primitive::U32(end)) => count!(U32, start, end, u32),
        (Primitive::U64(start), Primitive::U64(end)) => count!(U64, start, end, u64),
        (Primitive::U128(start), Primitive::U128(end)) => count!(U128, start, end, u128),
        (Primitive::U256(start), Primitive::U256(end)) => count!(U256, start, end, u256),
        _ => return Err(EnvironmentError::InvalidType)
    }))
}