use environment::{FnInstance, FnParams, FnReturnType};
use types::{Type, Value, ValueOwnable};
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
                1,
                Some(Type::Optional(Box::new(Type::$t)))
            );
        }
    };
}

// New macro to register multiple operations for a specific type
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

pub fn register(env: &mut EnvironmentBuilder) {
    // Register all operations with overflow checking
    register_overflows!(env, U8, u8);
    register_overflows!(env, U16, u16);
    register_overflows!(env, U32, u32);
    register_overflows!(env, U64, u64);
    register_overflows!(env, U128, u128);
    register_overflows!(env, U256, u256);
}