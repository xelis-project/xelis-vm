use anyhow::Context as _;
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
    ($env: expr, $op: ident, $t: ident, $f: ident, $param: ident, $cost: expr) => {
        paste! {
            fn [<checked_ $op _ $f>]<M>(zelf: FnInstance, parameters: FnParams, _: &M, _: &mut Context) -> FnReturnType<M> {
                // Extract and convert parameters
                let other = parameters[0]
                    .as_ref()
                    .[<as_ $param>]()?;
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
                $cost,
                Some(Type::Optional(Box::new(Type::$t)))
            );
        }
    };
}

// macro to register multiple operations for a specific type
macro_rules! register_checked_fns {
    ($env: expr, $t: ident, $f: ident) => {
        {
            checked_fn!($env, add, $t, $f, $f, 1);
            checked_fn!($env, sub, $t, $f, $f, 1);
            checked_fn!($env, mul, $t, $f, $f, 3);
            checked_fn!($env, div, $t, $f, $f, 8);
            checked_fn!($env, rem, $t, $f, $f, 8);
            checked_fn!($env, pow, $t, $f, u32, 50);
            checked_fn!($env, shr, $t, $f, u32, 5);
            checked_fn!($env, shl, $t, $f, u32, 5);
        }
    };
}

macro_rules! integer_param_fn {
    ($env: expr, $op: ident, $t: ident, $f: ident, $param: ident, $param_ty: ident, $cost: expr) => {
        paste! {
            fn [<$op _ $f>]<M>(
                zelf: FnInstance,
                parameters: FnParams,
                _: &M,
                _: &mut Context
            ) -> FnReturnType<M> {
                // Extract and convert parameters
                let other = parameters[0].as_ref().[<as_ $param>]()?;
                let value = zelf?.[<as_ $f>]()?;

                // Perform the operation with `$op`
                let result = value.[<$op>](other);

                Ok(SysCallResult::Return(Primitive::$t(result).into()))
            }

            // Registering the generated function in the environment
            $env.register_native_function(
                stringify!($op),
                Some(Type::$t),
                // $param with uppercase first letter
                vec![("other", Type::$param_ty)],
                FunctionHandler::Sync([<$op _ $f>]),
                $cost,
                Some(Type::$t)
            );
        }
    };
}

macro_rules! register_saturating_fns {
    ($env: expr, $t: ident, $f: ident) => {
        {
            integer_param_fn!($env, saturating_add, $t, $f, $f, $t, 1);
            integer_param_fn!($env, saturating_sub, $t, $f, $f, $t, 1);
            integer_param_fn!($env, saturating_mul, $t, $f, $f, $t, 3);
            integer_param_fn!($env, saturating_div, $t, $f, $f, $t, 3);
            integer_param_fn!($env, saturating_pow, $t, $f, u32, U32, 50);
        }
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

macro_rules! from_endian_bytes {
    ($env: expr, $t: ident, $f: ident, $endian: ident) => {
        paste! {
            fn [<from_ $endian _bytes_ $f>]<M>(_: FnInstance, params: FnParams, _: &M, _: &mut Context) -> FnReturnType<M> {
                let value = params[0].as_bytes()?;
                let slice: &[u8] = &value;
                let v = [<$f>]::[<from_ $endian _bytes>](slice.try_into().context("invalid bytes size")?);
                Ok(SysCallResult::Return(Primitive::$t(v).into()))
            }

            $env.register_static_function(
                stringify!([<from_ $endian _bytes>]),
                Type::$t,
                vec![("bytes", Type::Bytes)],
                FunctionHandler::Sync([<from_ $endian _bytes_ $f>]),
                10,
                Some(Type::$t)
            );
        }
    };
}

macro_rules! integer_no_param_fn {
    ($env: expr, $t: ident, $f: ident, $func: ident) => {
        paste! {
            fn [<$f _ $func>]<M>(zelf: FnInstance, _: FnParams, _: &M, _: &mut Context) -> FnReturnType<M> {
                let value = zelf?.[<as_ $f>]()?;
                Ok(SysCallResult::Return(Primitive::U32(value.[<$func>]()).into()))
            }

            $env.register_native_function(
                stringify!($func),
                Some(Type::$t),
                vec![],
                FunctionHandler::Sync([<$f _ $func>]),
                3,
                Some(Type::U32)
            );
        }
    };
}

macro_rules! integer_numbers {
    ($env: expr, $t: ident, $f: ident) => {
        integer_no_param_fn!($env, $t, $f, leading_ones);
        integer_no_param_fn!($env, $t, $f, trailing_ones);
        integer_no_param_fn!($env, $t, $f, count_ones);

        integer_no_param_fn!($env, $t, $f, leading_zeros);
        integer_no_param_fn!($env, $t, $f, trailing_zeros);
        integer_no_param_fn!($env, $t, $f, count_zeros);

        integer_param_fn!($env, rotate_left, $t, $f, u32, U32, 20);
        integer_param_fn!($env, rotate_right, $t, $f, u32, U32, 20);
        integer_param_fn!($env, pow, $t, $f, u32, U32, 50);
    };
}

macro_rules! register_reverse_bits {
    ($env: expr, $t: ident, $f: ident) => {
        paste! {
            fn [<reverse_bits_ $f>]<M>(zelf: FnInstance, _: FnParams, _: &M, _: &mut Context) -> FnReturnType<M> {
                let value = zelf?.[<as_ $f>]()?;
                Ok(SysCallResult::Return(Primitive::$t(value.reverse_bits()).into()))
            }

            $env.register_native_function(
                "reverse_bits",
                Some(Type::$t),
                vec![],
                FunctionHandler::Sync([<reverse_bits_ $f>]),
                10,
                Some(Type::$t)
            );
        }
    };
}

macro_rules! register_endian_bytes {
    ($env: expr, $t: ident, $f: ident) => {
        to_endian_bytes!($env, $t, $f, be);
        to_endian_bytes!($env, $t, $f, le);

        from_endian_bytes!($env, $t, $f, be);
        from_endian_bytes!($env, $t, $f, le);
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

        let min_inner = Constant::Primitive(Primitive::$t(min));
        let max_inner = Constant::Primitive(Primitive::$t(max));

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

    // Saturating functions
    register_saturating_fns!(env, U8, u8);
    register_saturating_fns!(env, U16, u16);
    register_saturating_fns!(env, U32, u32);
    register_saturating_fns!(env, U64, u64);
    register_saturating_fns!(env, U128, u128);
    register_saturating_fns!(env, U256, u256);

    // Register min/max functions for all types
    register_constants_min_max!(env, U8, u8);
    register_constants_min_max!(env, U16, u16);
    register_constants_min_max!(env, U32, u32);
    register_constants_min_max!(env, U64, u64);
    register_constants_min_max!(env, U128, u128);
    register_constants_min_max!(env, U256, u256);

    // Register all 'to endian bytes' (be/le) functions for all types
    // Returns a Bytes type
    register_endian_bytes!(env, U16, u16);
    register_endian_bytes!(env, U32, u32);
    register_endian_bytes!(env, U64, u64);
    register_endian_bytes!(env, U128, u128);
    register_endian_bytes!(env, U256, u256);

    // Register zeros/ones functions
    integer_numbers!(env, U8, u8);
    integer_numbers!(env, U16, u16);
    integer_numbers!(env, U32, u32);
    integer_numbers!(env, U64, u64);
    integer_numbers!(env, U128, u128);
    integer_numbers!(env, U256, u256);

    // Register reverse bits functions for all types
    register_reverse_bits!(env, U8, u8);
    register_reverse_bits!(env, U16, u16);
    register_reverse_bits!(env, U32, u32);
    register_reverse_bits!(env, U64, u64);
    register_reverse_bits!(env, U128, u128);
    register_reverse_bits!(env, U256, u256);

    register_min_max!(env, U8, u8);
    register_min_max!(env, U16, u16);
    register_min_max!(env, U32, u32);
    register_min_max!(env, U64, u64);
    register_min_max!(env, U128, u128);
    register_min_max!(env, U256, u256);
}