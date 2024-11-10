use xelis_environment::{
    FnInstance,
    FnParams,
    FnReturnType
};
use std::{
    rc::Rc,
    cell::RefCell
};
use xelis_types::{Type, Value, ValueOwnable, U256 as u256};
use paste::paste;

use crate::EnvironmentBuilder;

macro_rules! overflow_fn {
    ($env: expr, $op: ident, $t: ident, $f: ident) => {
        paste! {
            fn [<overflowing_ $op _ $f>](zelf: FnInstance, mut parameters: FnParams) -> FnReturnType {
                // Extract and convert parameters
                let other = parameters.remove(0).into_owned().[<as_ $f>]()?;
                let value = zelf?.[<as_ $f>]()?;
                
                // Perform the operation with `overflowing_$op` as a method name
                let (result, overflow) = value.[<overflowing_ $op>](other);
                
                Ok(Some(if overflow {
                    Value::Optional(None)
                } else {
                    let inner = ValueOwnable::Owned(Box::new(Value::$t(result)));
                    Value::Optional(Some(inner))
                }))
            }

            // Registering the generated function in the environment
            $env.register_native_function(
                // Function name as a string
                stringify!([<overflowing_ $op>]),
                Some(Type::$t),
                vec![Type::$t],
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

// macro to generate min and max functions for a specific type
// Allow to get the lowest or highest value of a type
macro_rules! min_max_fns {
    ($env: expr, $t: ident, $f: ident) => {
        paste! {
            fn [<min_ $f>](_: FnInstance, _: FnParams) -> FnReturnType {
                let min = $f::MIN;
                let inner = ValueOwnable::Owned(Box::new(Value::$t(min)));
                Ok(Some(Value::Optional(Some(inner))))
            }

            fn [<max_ $f>](_: FnInstance, _: FnParams) -> FnReturnType {
                let max = $f::MAX;
                let inner = ValueOwnable::Owned(Box::new(Value::$t(max)));
                Ok(Some(Value::Optional(Some(inner))))
            }

            $env.register_native_function(
                stringify!([<min_ $f>]),
                Some(Type::$t),
                vec![],
                [<min_ $f>],
                1,
                Some(Type::Optional(Box::new(Type::$t)))
            );

            $env.register_native_function(
                stringify!([<max_ $f>]),
                Some(Type::$t),
                vec![],
                [<max_ $f>],
                1,
                Some(Type::Optional(Box::new(Type::$t)))
            );
        }
    };
}

macro_rules! to_endian_bytes {
    ($env: expr, $t: ident, $f: ident, $endian: ident) => {
        paste! {
            fn [<to_ $endian _bytes_ $f>](zelf: FnInstance, _: FnParams) -> FnReturnType {
                let value = zelf?.[<as_ $f>]()?;
                let bytes = value.[<to_ $endian _bytes>]();
                let vec = bytes.iter().map(|b| Rc::new(RefCell::new(Value::U8(*b)))).collect();
                Ok(Some(Value::Array(vec)))
            }

            $env.register_native_function(
                stringify!([<to_ $endian _bytes>]),
                Some(Type::$t),
                vec![],
                [<to_ $endian _bytes_ $f>],
                10,
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

pub fn register(env: &mut EnvironmentBuilder) {
    // Register all operations with overflow checking
    register_overflows!(env, U8, u8);
    register_overflows!(env, U16, u16);
    register_overflows!(env, U32, u32);
    register_overflows!(env, U64, u64);
    register_overflows!(env, U128, u128);
    register_overflows!(env, U256, u256);

    // Register min/max functions for all types
    // TODO: don't create types functions, but constants like u256::MAX etc.
    min_max_fns!(env, U8, u8);
    min_max_fns!(env, U16, u16);
    min_max_fns!(env, U32, u32);
    min_max_fns!(env, U64, u64);
    min_max_fns!(env, U128, u128);
    min_max_fns!(env, U256, u256);

    // Register all 'to endian bytes' (be/le) functions for all types
    register_to_endian_bytes!(env, U16, u16);
    register_to_endian_bytes!(env, U32, u32);
    register_to_endian_bytes!(env, U64, u64);
    register_to_endian_bytes!(env, U128, u128);
    register_to_endian_bytes!(env, U256, u256);
}