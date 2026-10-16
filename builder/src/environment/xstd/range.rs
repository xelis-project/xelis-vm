use silex_environment::{
    VMContext,
    EnvironmentError,
    FnInstance,
    FnParams,
    FnReturnType,
    FunctionHandler,
    SysCallResult,
    ModuleMetadata
};
use silex_types::{PrimitiveInteger, PrimitiveRange, Type, Primitive, ValueCell};
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

                    let vec = (*$start..*$end).map(|i| Primitive::Integer(PrimitiveInteger::$t(i)).into()).collect();
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
                Primitive::Integer(PrimitiveInteger::$t(count)).into()
            }
        }
    };
}

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    let _type = Type::Range(Box::new(Type::T(Some(0))));
    env.register_native_function_with_comment("contains", Some(_type.clone()), vec![("element", Type::T(Some(0)))], FunctionHandler::Sync(contains), 5, Some(Type::Bool), "Returns true when the range contains the element.");
    env.register_native_function_with_comment("collect", Some(_type.clone()), vec![], FunctionHandler::Sync(collect), 20, Some(Type::Array(Box::new(Type::T(Some(0))))), "Collects all values in the range into an array.");
    env.register_native_function_with_comment("max", Some(_type.clone()), vec![], FunctionHandler::Sync(max), 1, Some(Type::T(Some(0))), "Returns the range upper bound.");
    env.register_native_function_with_comment("min", Some(_type.clone()), vec![], FunctionHandler::Sync(min), 1, Some(Type::T(Some(0))), "Returns the range lower bound.");
    env.register_native_function_with_comment("count", Some(_type.clone()), vec![], FunctionHandler::Sync(count), 5, Some(Type::T(Some(0))), "Returns the number of values in the range.");
}

fn contains<M>(zelf: FnInstance, mut parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let value = parameters.remove(0);
    let zelf = zelf?;
    let range = zelf.as_range()?;

    let value = value.as_ref();
    Ok(SysCallResult::Return(match range {
        PrimitiveRange::U8(range) => {
            let (start, end) = (&range.start, &range.end);
            contains!(u8, start, end, value)
        },
        PrimitiveRange::U16(range) => {
            let (start, end) = (&range.start, &range.end);
            contains!(u16, start, end, value)
        },
        PrimitiveRange::U32(range) => {
            let (start, end) = (&range.start, &range.end);
            contains!(u32, start, end, value)
        },
        PrimitiveRange::U64(range) => {
            let (start, end) = (&range.start, &range.end);
            contains!(u64, start, end, value)
        },
        PrimitiveRange::U128(range) => {
            let (start, end) = (&range.start, &range.end);
            contains!(u128, start, end, value)
        },
        PrimitiveRange::U256(range) => {
            let (start, end) = (&range.start, &range.end);
            contains!(u256, start, end, value)
        },
    }))
}

fn collect<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext) -> FnReturnType<M> {
    let zelf = zelf?;
    let range = zelf.as_range()?;
    Ok(SysCallResult::Return(match range {
        PrimitiveRange::U8(range) => {
            let (start, end) = (&range.start, &range.end);
            collect!(U8, start, end, u8, context)
        },
        PrimitiveRange::U16(range) => {
            let (start, end) = (&range.start, &range.end);
            collect!(U16, start, end, u16, context)
        },
        PrimitiveRange::U32(range) => {
            let (start, end) = (&range.start, &range.end);
            collect!(U32, start, end, u32, context)
        },
        PrimitiveRange::U64(range) => {
            let (start, end) = (&range.start, &range.end);
            collect!(U64, start, end, u64, context)
        },
        PrimitiveRange::U128(range) => {
            let (start, end) = (&range.start, &range.end);
            collect!(U128, start, end, u128, context)
        },
        PrimitiveRange::U256(range) => {
            let (start, end) = (&range.start, &range.end);
            let (diff, overflow) = end.overflowing_sub(*start);

            let mut vec = Vec::new();
            if !overflow {
                let diff = diff.as_u64().ok_or(EnvironmentError::InvalidParameter)?;
                if diff > u32::MAX as u64 {
                    return Err(EnvironmentError::RangeTooLarge);
                }

                context.increase_gas_usage(diff as u64 * 8)?;

                for i in 0..diff {
                    vec.push(Primitive::Integer(PrimitiveInteger::U256(i.into())).into());
                }
            }

            ValueCell::Object(vec).into()
        }
    }))
}

fn max<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let zelf = zelf?;
    let end = zelf.as_range()?.end();
    Ok(SysCallResult::Return(end.into()))
}

fn min<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let zelf = zelf?;
    let start = zelf.as_range()?.start();
    Ok(SysCallResult::Return(start.into()))
}

fn count<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut VMContext) -> FnReturnType<M> {
    let zelf = zelf?;
    let range = zelf.as_range()?;

    Ok(SysCallResult::Return(match range {
        PrimitiveRange::U8(range) => {
            let (start, end) = (&range.start, &range.end);
            count!(U8, start, end, u8)
        },
        PrimitiveRange::U16(range) => {
            let (start, end) = (&range.start, &range.end);
            count!(U16, start, end, u16)
        },
        PrimitiveRange::U32(range) => {
            let (start, end) = (&range.start, &range.end);
            count!(U32, start, end, u32)
        },
        PrimitiveRange::U64(range) => {
            let (start, end) = (&range.start, &range.end);
            count!(U64, start, end, u64)
        },
        PrimitiveRange::U128(range) => {
            let (start, end) = (&range.start, &range.end);
            count!(U128, start, end, u128)
        },
        PrimitiveRange::U256(range) => {
            let (start, end) = (&range.start, &range.end);
            count!(U256, start, end, u256)
        },
    }))
}
