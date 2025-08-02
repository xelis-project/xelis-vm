use xelis_environment::{
    SysCallResult,
    FunctionHandler,
    FnInstance,
    FnParams,
    FnReturnType,
    Context,
};
use xelis_types::{Type, Primitive, ValueCell, Constant, U256 as u256};
use paste::paste;

use crate::EnvironmentBuilder;

macro_rules! checked_fn {
    ($env: expr, $op: ident, $t: ident, $f: ident) => {
        paste! {
            fn [<checked_ $op _ $f>]<M>(zelf: FnInstance, mut parameters: FnParams, _: &M, _: &mut Context) -> FnReturnType<M> {
                // Extract and convert parameters
                let other = parameters.remove(0).into_owned().[<as_ $f>]()?;
                let value = zelf?.[<as_ $f>]()?;
                
                // Perform the operation with `checked_$op` as a method name
                let result = value.[<checked_ $op>](other);
                
                Ok(SysCallResult::Return(
                    result.map(|v| Primitive::$t(v))
                        .unwrap_or_default()
                        .into()
                ))
            }

            // Registering the generated function in the environment
            $env.register_native_function(
                // Function name as a string
                stringify!([<checked_ $op>]),
                Some(Type::$t),
                vec![("other", Type::$t)],
                // The function identifier
                FunctionHandler::Sync([<checked_ $op _ $f>]),
                2,
                Some(Type::Optional(Box::new(Type::$t)))
            );
        }
    };
}

// macro to register multiple operations for a specific type
macro_rules! register_checked_fns {
    ($env: expr, $t: ident, $f: ident) => {
        {
            checked_fn!($env, add, $t, $f);
            checked_fn!($env, sub, $t, $f);
            checked_fn!($env, mul, $t, $f);
            checked_fn!($env, div, $t, $f);
            checked_fn!($env, rem, $t, $f);
        }
    };
}

macro_rules! to_endian_array {
    ($env: expr, $t: ident, $f: ident, $endian: ident) => {
        paste! {
            fn [<to_ $endian _array_ $f>]<M>(zelf: FnInstance, _: FnParams, _: &M, _: &mut Context) -> FnReturnType<M> {
                let value = zelf?.[<as_ $f>]()?;
                let bytes = value.[<to_ $endian _bytes>]();
                let vec = bytes.iter().map(|b| Primitive::U8(*b).into()).collect();
                Ok(SysCallResult::Return(ValueCell::Object(vec).into()))
            }

            $env.register_native_function(
                stringify!([<to_ $endian _array>]),
                Some(Type::$t),
                vec![],
                FunctionHandler::Sync([<to_ $endian _array_ $f>]),
                15,
                Some(Type::Array(Box::new(Type::U8)))
            );
        }
    };
}

macro_rules! register_to_endian_array {
    ($env: expr, $t: ident, $f: ident) => {
        to_endian_array!($env, $t, $f, be);
        to_endian_array!($env, $t, $f, le);
    };
}

macro_rules! to_endian_bytes {
    ($env: expr, $t: ident, $f: ident, $endian: ident) => {
        paste! {
            fn [<to_ $endian _bytes_ $f>]<M>(zelf: FnInstance, _: FnParams, _: &M, _: &mut Context) -> FnReturnType<M> {
                let value = zelf?.[<as_ $f>]()?;
                let bytes = value.[<to_ $endian _bytes>]();
                Ok(SysCallResult::Return(ValueCell::Bytes(bytes.to_vec()).into()))
            }

            $env.register_native_function(
                stringify!([<to_ $endian _bytes>]),
                Some(Type::$t),
                vec![],
                FunctionHandler::Sync([<to_ $endian _bytes_ $f>]),
                10,
                Some(Type::Bytes)
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

macro_rules! min {
    ($env: expr, $t: ident, $f: ident, $endian: ident) => {
        paste! {
            fn [<min_ $f>]<M>(zelf: FnInstance, params: FnParams, _: &M, _: &mut Context) -> FnReturnType<M> {
                let value = zelf?.[<as_ $f>]()?;
                let other = params[0].as_ref().[<as_ $f>]()?;

                let min = value.min(other);
                Ok(SysCallResult::Return(Primitive::$t(min).into()))
            }

            $env.register_native_function(
                "min",
                Some(Type::$t),
                vec![],
                FunctionHandler::Sync([<min_ $f>]),
                1,
                Some(Type::Array(Box::new(Type::U8)))
            );
        }
    };
}


macro_rules! max {
    ($env: expr, $t: ident, $f: ident, $endian: ident) => {
        paste! {
            fn [<max_ $f>]<M>(zelf: FnInstance, params: FnParams, _: &M, _: &mut Context) -> FnReturnType<M> {
                let value = zelf?.[<as_ $f>]()?;
                let other = params[0].as_ref().[<as_ $f>]()?;

                let min = value.max(other);
                Ok(SysCallResult::Return(Primitive::$t(min).into()))
            }

            $env.register_native_function(
                "max",
                Some(Type::$t),
                vec![],
                FunctionHandler::Sync([<max_ $f>]),
                1,
                Some(Type::Array(Box::new(Type::U8)))
            );
        }
    };
}

macro_rules! register_min_max {
    ($env: expr, $t: ident, $f: ident) => {
        min!($env, $t, $f, be);
        max!($env, $t, $f, be);
    };
}

macro_rules! register_constants_min_max {
    ($env: expr, $t: ident, $f: ident) => {
        let min = $f::MIN;
        let max = $f::MAX;

        let min_inner = Constant::Default(Primitive::$t(min));
        let max_inner = Constant::Default(Primitive::$t(max));

        $env.register_constant(Type::$t, "MIN", min_inner);
        $env.register_constant(Type::$t, "MAX", max_inner);
    };
}

pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    // Register all operations with overflow checking
    register_checked_fns!(env, U8, u8);
    register_checked_fns!(env, U16, u16);
    register_checked_fns!(env, U32, u32);
    register_checked_fns!(env, U64, u64);
    register_checked_fns!(env, U128, u128);
    register_checked_fns!(env, U256, u256);

    // Register min/max functions for all types
    register_constants_min_max!(env, U8, u8);
    register_constants_min_max!(env, U16, u16);
    register_constants_min_max!(env, U32, u32);
    register_constants_min_max!(env, U64, u64);
    register_constants_min_max!(env, U128, u128);
    register_constants_min_max!(env, U256, u256);

    // Register all 'to endian bytes' (be/le) functions for all types
    // Returns a Type::Array(T)
    register_to_endian_array!(env, U16, u16);
    register_to_endian_array!(env, U32, u32);
    register_to_endian_array!(env, U64, u64);
    register_to_endian_array!(env, U128, u128);
    register_to_endian_array!(env, U256, u256);

    // Register all 'to endian bytes' (be/le) functions for all types
    // Returns a Bytes type
    register_to_endian_bytes!(env, U16, u16);
    register_to_endian_bytes!(env, U32, u32);
    register_to_endian_bytes!(env, U64, u64);
    register_to_endian_bytes!(env, U128, u128);
    register_to_endian_bytes!(env, U256, u256);

    register_min_max!(env, U8, u8);
    register_min_max!(env, U16, u16);
    register_min_max!(env, U32, u32);
    register_min_max!(env, U64, u64);
    register_min_max!(env, U128, u128);
    register_min_max!(env, U256, u256);
}