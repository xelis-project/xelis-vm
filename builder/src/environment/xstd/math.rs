use xelis_environment::{
    FnInstance,
    FnParams,
    FnReturnType,
    Context,
    SysCallResult,
    FunctionHandler,
};
use xelis_types::{Type, Primitive, U256 as u256};
use paste::paste;
use crate::EnvironmentBuilder;

// Macro to implement sqrt for different integer types
macro_rules! sqrt_fn {
    ($env: expr, $t: ident, $f: ident) => {
        paste! {
            // Square root implementation using a simple binary search method
            fn [<sqrt_ $f>]<M>(zelf: FnInstance, _: FnParams, _: &M, _: &mut Context) -> FnReturnType<M> {
                let n = zelf?.[<as_ $f>]()?;
                let one = $f::from(1u8);

                // Binary search for the square root
                let mut left = $f::from(0u8);
                let mut right = (n >> 1) + one;

                while left <= right {
                    let mid = (left + right) >> 1;

                    // Check if mid²
                    if let Some(sq) = mid.checked_mul(mid) {
                        if sq > n {
                            // mid is too large
                            right = mid - one;
                        } else {
                            // mid is either correct or too small
                            if let Some(new_left) = mid.checked_add(one) {
                                left = new_left;
                            } else {
                                // If mid + 1 would overflow, mid must be the maximum possible value
                                // which means it's our answer
                                return Ok(SysCallResult::Return(Primitive::$t(mid).into()));
                            }
                        }
                    } else {
                        // mid² overflows, so mid is definitely too large
                        right = mid - one;
                    }
                }
                
                // When the loop terminates, right is the floor(sqrt(n))
                Ok(SysCallResult::Return(Primitive::$t(right).into()))
            }

            // Register the sqrt function for this type
            $env.register_native_function(
                "sqrt",
                Some(Type::$t),
                vec![],
                FunctionHandler::Sync([<sqrt_ $f>]),
                10,
                Some(Type::$t)
            );
        }
    };
}

// Register sqrt function for numeric types
macro_rules! register_sqrt_fns {
    ($env: expr) => {
        {
            sqrt_fn!($env, U8, u8);
            sqrt_fn!($env, U16, u16);
            sqrt_fn!($env, U32, u32);
            sqrt_fn!($env, U64, u64);
            sqrt_fn!($env, U128, u128);
            sqrt_fn!($env, U256, u256);
        }
    };
}

// Register all math functions
pub fn register<M>(env: &mut EnvironmentBuilder<M>) {
    // Register square root functions for all integer types
    register_sqrt_fns!(env);
}
