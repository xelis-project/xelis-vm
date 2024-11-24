use xelis_environment::{
    FnInstance,
    FnParams,
    FnReturnType,
    Context,
};
use xelis_types::{Type, Value, ValueCell, Constant, U256 as u256};
use paste::paste;

use crate::EnvironmentBuilder;

macro_rules! overflow_fn {
    ($env: expr, $op: ident, $t: ident, $f: ident) => {
        paste! {
            fn [<overflowing_ $op _ $f>](zelf: FnInstance, mut parameters: FnParams, _: &mut Context) -> FnReturnType {
                // Extract and convert parameters
                let other = parameters.remove(0).into_owned().[<as_ $f>]()?;
                let value = zelf?.[<as_ $f>]()?;
                
                // Perform the operation with `overflowing_$op` as a method name
                let (result, overflow) = value.[<overflowing_ $op>](other);
                
                Ok(Some(if overflow {
                    ValueCell::Optional(None)
                } else {
                    let inner = Value::$t(result).into();
                    ValueCell::Optional(Some(inner))
                }))
            }

            // Registering the generated function in the environment
            $env.register_native_function(
                // Function name as a string
                stringify!([<overflowing_ $op>]),
                Some(Type::$t),
                vec![("other", Type::$t)],
                // The function identifier
                [<overflowing_ $op _ $f>],
                2,
                Some(Type::Optional(Box::new(Type::$t)))
            );
        }
    };
}

// macro to register multiple operations for a specific type
macro_rules! register_overflows {
    ($env: expr, $t: ident, $f: ident) => {
        {
            overflow_fn!($env, add, $t, $f);
            overflow_fn!($env, sub, $t, $f);
            overflow_fn!($env, mul, $t, $f);
            overflow_fn!($env, div, $t, $f);
            overflow_fn!($env, rem, $t, $f);
        }
    };
}

macro_rules! to_endian_bytes {
    ($env: expr, $t: ident, $f: ident, $endian: ident) => {
        paste! {
            fn [<to_ $endian _bytes_ $f>](zelf: FnInstance, _: FnParams, _: &mut Context) -> FnReturnType {
                let value = zelf?.[<as_ $f>]()?;
                let bytes = value.[<to_ $endian _bytes>]();
                let vec = bytes.iter().map(|b| Value::U8(*b).into()).collect();
                Ok(Some(ValueCell::Array(vec)))
            }

            $env.register_native_function(
                stringify!([<to_ $endian _bytes>]),
                Some(Type::$t),
                vec![],
                [<to_ $endian _bytes_ $f>],
                15,
                Some(Type::Optional(Box::new(Type::Array(Box::new(Type::U8)))))
            );
        }
    };
}

macro_rules! register_to_endian_bytes {
    ($env: expr, $t: ident, $f: ident) => {
        to_endian_bytes!($env, $t, $f, be);
        to_endian_bytes!($env, $t, $f, le);
    };
}

macro_rules! register_constants_min_max {
    ($env: expr, $t: ident, $f: ident) => {
        let min = $f::MIN;
        let max = $f::MAX;

        let min_inner = Constant::Default(Value::$t(min));
        let max_inner = Constant::Default(Value::$t(max));

        $env.register_constant(Type::$t, "MIN", Constant::Optional(Some(Box::new(min_inner))));
        $env.register_constant(Type::$t, "MAX", Constant::Optional(Some(Box::new(max_inner))));
    };
}

pub fn register(env: &mut EnvironmentBuilder) {
    // Register all operations with overflow checking
    register_overflows!(env, U8, u8);
    register_overflows!(env, U16, u16);
    register_overflows!(env, U32, u32);
    register_overflows!(env, U64, u64);
    register_overflows!(env, U128, u128);
    register_overflows!(env, U256, u256);

    // Register min/max functions for all types
    register_constants_min_max!(env, U8, u8);
    register_constants_min_max!(env, U16, u16);
    register_constants_min_max!(env, U32, u32);
    register_constants_min_max!(env, U64, u64);
    register_constants_min_max!(env, U128, u128);
    register_constants_min_max!(env, U256, u256);

    // Register all 'to endian bytes' (be/le) functions for all types
    register_to_endian_bytes!(env, U16, u16);
    register_to_endian_bytes!(env, U32, u32);
    register_to_endian_bytes!(env, U64, u64);
    register_to_endian_bytes!(env, U128, u128);
    register_to_endian_bytes!(env, U256, u256);
}