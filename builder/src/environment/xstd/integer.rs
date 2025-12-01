use anyhow::Context as _;
use xelis_environment::{
    SysCallResult,
    FunctionHandler,
    FnInstance,
    FnParams,
    FnReturnType,
    ModuleMetadata,
    Context,
    EnvironmentError,
};
use xelis_types::{Type, Primitive, ValueCell, Constant, U256 as u256};
use paste::paste;

use crate::EnvironmentBuilder;

macro_rules! checked_fn {
    // Special cases for div and rem with primitive types to check for division by zero
    ($env: expr, div, $t: ident, $f: ident, $param: ident, $param_ty: ident, $cost: expr) => {
        paste! {
            fn [<checked_div_ $f>]<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
                let other = parameters[0].as_ref().[<as_ $param>]()?;
                let value = zelf?.[<as_ $f>]()?;
                
                // Check for division by zero
                if other == 0 {
                    return Err(EnvironmentError::DivisionByZero);
                }
                
                let result = value.checked_div(other);
                Ok(SysCallResult::Return(
                    result.map(|v| Primitive::$t(v))
                        .unwrap_or_default()
                        .into()
                ))
            }

            $env.register_native_function(
                "checked_div",
                Some(Type::$t),
                vec![("other", Type::$param_ty)],
                FunctionHandler::Sync([<checked_div_ $f>]),
                $cost,
                Some(Type::Optional(Box::new(Type::$t)))
            );
        }
    };
    ($env: expr, rem, $t: ident, $f: ident, $param: ident, $param_ty: ident, $cost: expr) => {
        paste! {
            fn [<checked_rem_ $f>]<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
                let other = parameters[0].as_ref().[<as_ $param>]()?;
                let value = zelf?.[<as_ $f>]()?;
                
                // Check for division by zero
                if other == 0 {
                    return Err(EnvironmentError::DivisionByZero);
                }
                
                let result = value.checked_rem(other);
                Ok(SysCallResult::Return(
                    result.map(|v| Primitive::$t(v))
                        .unwrap_or_default()
                        .into()
                ))
            }

            $env.register_native_function(
                "checked_rem",
                Some(Type::$t),
                vec![("other", Type::$param_ty)],
                FunctionHandler::Sync([<checked_rem_ $f>]),
                $cost,
                Some(Type::Optional(Box::new(Type::$t)))
            );
        }
    };
    // General case for all other operations
    ($env: expr, $op: ident, $t: ident, $f: ident, $param: ident, $param_ty: ident, $cost: expr) => {
        paste! {
            fn [<checked_ $op _ $f>]<M>(zelf: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
                let other = parameters[0]
                    .as_ref()
                    .[<as_ $param>]()?;
                let value = zelf?.[<as_ $f>]()?;
                
                let result = value.[<checked_ $op>](other);
                Ok(SysCallResult::Return(
                    result.map(|v| Primitive::$t(v))
                        .unwrap_or_default()
                        .into()
                ))
            }

            $env.register_native_function(
                stringify!([<checked_ $op>]),
                Some(Type::$t),
                vec![("other", Type::$param_ty)],
                FunctionHandler::Sync([<checked_ $op _ $f>]),
                $cost,
                Some(Type::Optional(Box::new(Type::$t)))
            );
        }
    };
    ($env: expr, $op: ident, $t: ident, $f: ident, $cost: expr) => {
        checked_fn!($env, $op, $t, $f, $f, $t, $cost);
    }
}

// macro to register multiple operations for a specific type
macro_rules! register_checked_fns {
    ($env: expr, $t: ident, $f: ident) => {
        {
            checked_fn!($env, add, $t, $f, 1);
            checked_fn!($env, sub, $t, $f, 1);
            checked_fn!($env, mul, $t, $f, 3);
            checked_fn!($env, div, $t, $f, 8);
            checked_fn!($env, rem, $t, $f, 8);
            checked_fn!($env, pow, $t, $f, u32, U32, 50);
            checked_fn!($env, shr, $t, $f, u32, U32, 5);
            checked_fn!($env, shl, $t, $f, u32, U32, 5);
        }
    };
}

macro_rules! wrapping_fn {
    ($env: expr, wrapping_div, $t: ident, $f: ident, $param: ident, $param_ty: ident, $cost: expr) => {
        paste! {
            fn [<wrapping_div _ $f>]<M>(
                zelf: FnInstance,
                parameters: FnParams,
                _: &ModuleMetadata<'_, M>,
                _: &mut Context
            ) -> FnReturnType<M> {
                let other = parameters[0].as_ref().[<as_ $param>]()?;
                let value = zelf?.[<as_ $f>]()?;

                // Check for division by zero (for primitive types)
                if other == 0 {
                    return Err(EnvironmentError::DivisionByZero);
                }

                let result = value.wrapping_div(other);
                Ok(SysCallResult::Return(Primitive::$t(result).into()))
            }

            $env.register_native_function(
                "wrapping_div",
                Some(Type::$t),
                vec![("other", Type::$param_ty)],
                FunctionHandler::Sync([<wrapping_div _ $f>]),
                $cost,
                Some(Type::$t)
            );
        }
    };
    ($env: expr, wrapping_rem, $t: ident, $f: ident, $param: ident, $param_ty: ident, $cost: expr) => {
        paste! {
            fn [<wrapping_rem _ $f>]<M>(
                zelf: FnInstance,
                parameters: FnParams,
                _: &ModuleMetadata<'_, M>,
                _: &mut Context
            ) -> FnReturnType<M> {
                let other = parameters[0].as_ref().[<as_ $param>]()?;
                let value = zelf?.[<as_ $f>]()?;

                // Check for division by zero (for primitive types)
                if other == 0 {
                    return Err(EnvironmentError::DivisionByZero);
                }

                let result = value.wrapping_rem(other);
                Ok(SysCallResult::Return(Primitive::$t(result).into()))
            }

            $env.register_native_function(
                "wrapping_rem",
                Some(Type::$t),
                vec![("other", Type::$param_ty)],
                FunctionHandler::Sync([<wrapping_rem _ $f>]),
                $cost,
                Some(Type::$t)
            );
        }
    };
    ($env: expr, $op: ident, $t: ident, $f: ident, $param: ident, $param_ty: ident, $cost: expr) => {
        paste! {
            fn [<$op _ $f>]<M>(
                zelf: FnInstance,
                parameters: FnParams,
                _: &ModuleMetadata<'_, M>,
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
    ($env: expr, $op: ident, $t: ident, $f: ident, $cost: expr) => {
        wrapping_fn!($env, $op, $t, $f, $f, $t, $cost);
    };
}

macro_rules! register_wrapping_fns {
    ($env: expr, $t: ident, $f: ident) => {
        {
            wrapping_fn!($env, wrapping_add, $t, $f, 1);
            wrapping_fn!($env, wrapping_sub, $t, $f, 1);
            wrapping_fn!($env, wrapping_mul, $t, $f, 3);
            wrapping_fn!($env, wrapping_div, $t, $f, 3);
            wrapping_fn!($env, wrapping_rem, $t, $f, 3);
            wrapping_fn!($env, wrapping_pow, $t, $f, u32, U32, 50);
        }
    };
}

macro_rules! integer_param_fn {
    // Special case for saturating_div to check for division by zero
    ($env: expr, saturating_div, $t: ident, $f: ident, $param: ident, $param_ty: ident, $cost: expr) => {
        paste! {
            fn [<saturating_div _ $f>]<M>(
                zelf: FnInstance,
                parameters: FnParams,
                _: &ModuleMetadata<'_, M>,
                _: &mut Context
            ) -> FnReturnType<M> {
                let other = parameters[0].as_ref().[<as_ $param>]()?;
                let value = zelf?.[<as_ $f>]()?;

                // Check for division by zero
                if other == 0 {
                    return Err(EnvironmentError::DivisionByZero);
                }

                let result = value.saturating_div(other);
                Ok(SysCallResult::Return(Primitive::$t(result).into()))
            }

            $env.register_native_function(
                "saturating_div",
                Some(Type::$t),
                vec![("other", Type::$param_ty)],
                FunctionHandler::Sync([<saturating_div _ $f>]),
                $cost,
                Some(Type::$t)
            );
        }
    };
    // General case for all other operations
    ($env: expr, $op: ident, $t: ident, $f: ident, $param: ident, $param_ty: ident, $cost: expr) => {
        paste! {
            fn [<$op _ $f>]<M>(
                zelf: FnInstance,
                parameters: FnParams,
                _: &ModuleMetadata<'_, M>,
                _: &mut Context
            ) -> FnReturnType<M> {
                let other = parameters[0].as_ref().[<as_ $param>]()?;
                let value = zelf?.[<as_ $f>]()?;

                let result = value.[<$op>](other);
                Ok(SysCallResult::Return(Primitive::$t(result).into()))
            }

            $env.register_native_function(
                stringify!($op),
                Some(Type::$t),
                vec![("other", Type::$param_ty)],
                FunctionHandler::Sync([<$op _ $f>]),
                $cost,
                Some(Type::$t)
            );
        }
    };
}

macro_rules! register_saturating_fns {
    // General variant for primitive types (without saturating_rem)
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
            fn [<to_ $endian _bytes_ $f>]<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
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
            fn [<from_ $endian _bytes_ $f>]<M>(_: FnInstance, params: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
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
            fn [<$f _ $func>]<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
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
            fn [<reverse_bits_ $f>]<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
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
            fn [<min_ $f>]<M>(zelf: FnInstance, params: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
                let value = zelf?.[<as_ $f>]()?;
                let other = params[0].as_ref().[<as_ $f>]()?;

                let min = value.min(other);
                Ok(SysCallResult::Return(Primitive::$t(min).into()))
            }

            $env.register_native_function(
                "min",
                Some(Type::$t),
                vec![("other", Type::$t)],
                FunctionHandler::Sync([<min_ $f>]),
                1,
                Some(Type::$t)
            );
        }
    };
}

macro_rules! max {
    ($env: expr, $t: ident, $f: ident, $endian: ident) => {
        paste! {
            fn [<max_ $f>]<M>(zelf: FnInstance, params: FnParams, _: &ModuleMetadata<'_, M>, _: &mut Context) -> FnReturnType<M> {
                let value = zelf?.[<as_ $f>]()?;
                let other = params[0].as_ref().[<as_ $f>]()?;

                let min = value.max(other);
                Ok(SysCallResult::Return(Primitive::$t(min).into()))
            }

            $env.register_native_function(
                "max",
                Some(Type::$t),
                vec![("other", Type::$t)],
                FunctionHandler::Sync([<max_ $f>]),
                1,
                Some(Type::$t)
            );
        }
    };
}

// to_string(base: u32) -> string
// Converts an integer to a string with the specified base (radix)
// Supported bases: 2
macro_rules! to_string {
    ($env: expr, U256, u256) => {
        paste! {
            fn to_string_u256<M>(
                zelf: FnInstance,
                parameters: FnParams,
                _: &ModuleMetadata<'_, M>,
                context: &mut Context
            ) -> FnReturnType<M> {
                let value = zelf?.as_u256()?;
                let base = parameters[0].as_ref().as_u32()?;
                
                // Validate base range
                if base < 2 || base > 36 {
                    return Err(anyhow::anyhow!("Base must be between 2 and 36, got {}", base).into());
                }
                
                // For U256, use custom implementation for all bases
                let mut num = value;
                if num.is_zero() {
                    return Ok(SysCallResult::Return(Primitive::String("0".to_string()).into()));
                }

                let mut result = String::new();
                let digits = b"0123456789abcdefghijklmnopqrstuvwxyz";
                let base_u256 = u256::from(base);

                // Calculate gas based on number of digits
                // Each iteration: modulo, division, string insert (costly operations)
                // Use approximate max digits: 256 bits / log2(base)
                // Precomputed ceiling values for common bases to avoid floating point
                let baseline_256 = match base {
                    2 => 256,
                    3 => 162,
                    4 => 128,
                    5 => 110,
                    6 => 99,
                    7 => 91,
                    8 => 86,
                    9 => 81,
                    10 => 78,
                    11 => 75,
                    12 => 72,
                    13 => 69,
                    14 => 67,
                    15 => 66,
                    16 => 64,
                    _ => 256 / base.ilog2() + 1, // conservative for other bases
                };

                // Scale linearly by bit_width/256, with ceil division
                let max_digits = ((baseline_256 as u64) * (256 as u64) + 255) / 256;

                context.increase_gas_usage(max_digits as u64)?;

                while !num.is_zero() {
                    let remainder_u256 = num % base_u256;
                    // Convert remainder to u32 using Into trait (safe because base <= 36)
                    let remainder: u32 = remainder_u256.into();
                    result.insert(0, digits[remainder as usize] as char);
                    num /= base_u256;
                }
                
                Ok(SysCallResult::Return(Primitive::String(result).into()))
            }

            $env.register_native_function(
                "to_string",
                Some(Type::U256),
                vec![("base", Type::U32)],
                FunctionHandler::Sync(to_string_u256),
                3,
                Some(Type::String)
            );
        }
    };
    ($env: expr, $t: ident, $f: ident) => {
        paste! {
            fn [<to_string_ $f>]<M>(
                zelf: FnInstance,
                parameters: FnParams,
                _: &ModuleMetadata<'_, M>,
                context: &mut Context
            ) -> FnReturnType<M> {
                let value = zelf?.[<as_ $f>]()?;
                let base = parameters[0].as_ref().as_u32()?;

                // Validate base
                if base < 2 || base > 36 {
                    // replace this with your own error type/handling
                    return Err(EnvironmentError::from("base must be >= 2 and <= 36"));
                }

                // Calculate gas based on number of digits
                // Use integer arithmetic: bit_width / log2(base), rounded up, plus margin for non-powers of two
                let bit_width: u32 = $f::BITS;
                let base_log2: u32 = base.ilog2();

                let max_digits: u32 = if base.is_power_of_two() {
                    // ceil(bit_width / log2(base))
                    (bit_width + base_log2 - 1) / base_log2
                } else {
                    // Conservative upper bound for non-power-of-two bases
                    (bit_width + base_log2 - 1) / base_log2 + 1
                };

                context.increase_gas_usage(max_digits as u64)?;

                // Convert to string with the given base
                let result = match base {
                    2 => format!("{:b}", value),
                    8 => format!("{:o}", value),
                    10 => value.to_string(),
                    16 => format!("{:x}", value),
                    _ => {
                        // For other bases, use custom implementation
                        let mut num = value;
                        if num == 0 {
                            return Ok(SysCallResult::Return(Primitive::String("0".to_owned()).into()));
                        }

                        let mut result = String::new();
                        let digits = b"0123456789abcdefghijklmnopqrstuvwxyz";
                        
                        while num > 0 {
                            let remainder = (num % (base as $f)) as usize;
                            result.insert(0, digits[remainder] as char);
                            num /= base as $f;
                        }
                        result
                    }
                };
                
                Ok(SysCallResult::Return(Primitive::String(result).into()))
            }

            $env.register_native_function(
                "to_string",
                Some(Type::$t),
                vec![("base", Type::U32)],
                FunctionHandler::Sync([<to_string_ $f>]),
                1,
                Some(Type::String)
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

macro_rules! register_to_string {
    ($env: expr, $t: ident, $f: ident) => {
        to_string!($env, $t, $f);
    };
}

macro_rules! register_constants_min_max {
    ($env: expr, $t: ident, $f: ident) => {
        let min = $f::MIN;
        let max = $f::MAX;

        let min_inner = Constant::Primitive(Primitive::$t(min));
        let max_inner = Constant::Primitive(Primitive::$t(max));

        $env.register_constant(Type::$t, "MIN", min_inner, Type::$t);
        $env.register_constant(Type::$t, "MAX", max_inner, Type::$t);
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

    // Register all wrapping operations
    register_wrapping_fns!(env, U8, u8);
    register_wrapping_fns!(env, U16, u16);
    register_wrapping_fns!(env, U32, u32);
    register_wrapping_fns!(env, U64, u64);
    register_wrapping_fns!(env, U128, u128);
    register_wrapping_fns!(env, U256, u256);

    // Register min/max functions for all types
    register_constants_min_max!(env, U8, u8);
    register_constants_min_max!(env, U16, u16);
    register_constants_min_max!(env, U32, u32);
    register_constants_min_max!(env, U64, u64);
    register_constants_min_max!(env, U128, u128);
    register_constants_min_max!(env, U256, u256);

    // Register all 'to endian bytes' (be/le) functions for all types
    // Returns a Bytes type
    register_endian_bytes!(env, U8, u8);
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

    // Register to_string functions for all types
    register_to_string!(env, U8, u8);
    register_to_string!(env, U16, u16);
    register_to_string!(env, U32, u32);
    register_to_string!(env, U64, u64);
    register_to_string!(env, U128, u128);
    register_to_string!(env, U256, u256);
}