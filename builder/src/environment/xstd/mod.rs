use silex_types::Heap;
// All modules are public
// So anyone can re-use them in their own environment if they want to
pub mod array;
pub mod iterator;
pub mod optional;
pub mod string;
pub mod integer;
pub mod range;
pub mod map;
pub mod bytes;
pub mod math;

use std::ptr;

use silex_types::{Primitive, Type};
use silex_environment::{
    VMContext,
    EnvironmentError,
    FnInstance,
    FnParams,
    FnReturnType,
    FunctionHandler,
    OnCallSyncFn,
    SysCallResult,
    ModuleMetadata,
};
use super::EnvironmentBuilder;

// This module registers all the default functions and types available in the environment, like arrays, strings, integers, etc.
pub fn register<M: 'static>(env: &mut EnvironmentBuilder<M>) {
    array::register(env);
    bytes::register(env);
    optional::register(env);
    string::register(env);
    integer::register(env);
    range::register(env);
    map::register(env);
    math::register(env);
    iterator::register(env);

    register_defaults(env);
}

// Register default functions available in the environment, like println, debug, panic, assert, etc.
pub fn register_defaults<M>(env: &mut EnvironmentBuilder<M>) {
    env.register_native_function_with_comment("println", None, vec![("message", Type::Any)], native(println), 1, None, "Prints a value to standard output using its display representation.");
    env.register_native_function_with_comment("debug", None, vec![("message", Type::Any)], native(debug), 1, None, "Prints a value to standard output using its debug representation.");
    env.register_native_function_with_comment("panic", None, vec![("message", Type::Any)], native(panic), 1, Some(Type::Any), "Stops execution with the provided panic message.");
    env.register_native_function_with_comment("assert", None, vec![("condition", Type::Bool)], native(assert), 1, None, "Fails execution if the condition is false.");
    env.register_native_function_with_comment("is_same_ptr", None, vec![("left", Type::Any), ("right", Type::Any)], native(is_same_ptr), 5, Some(Type::Bool), "Returns true when both values point to the same underlying value.");
    env.register_native_function_with_comment("require", None, vec![("condition", Type::Bool), ("message", Type::String)], native(require), 1, None, "Fails execution with the message if the condition is false.");
    env.register_native_function_with_comment("clone", Some(Type::T(None)), vec![], native(clone), 5, Some(Type::T(None)), "Returns a deep clone of the value.");
}

fn println<M>(_: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let param = &parameters[0];
    println!("{}", param.as_ref(heap).display(heap));

    Ok(SysCallResult::None)
}

fn debug<M>(_: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, _heap: &mut Heap) -> FnReturnType<M> {
    let param = &parameters[0];
    println!("{:?}", param);

    Ok(SysCallResult::None)
}

fn panic<M>(_: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let param = parameters.remove(0);
    let value = param.as_ref(heap);

    Err(EnvironmentError::Panic(format!("{:#}", value.display(heap))))
}

fn assert<M>(_: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let param = &parameters[0];
    let value = param.as_bool(heap)?;

    if value {
        Ok(SysCallResult::None)
    } else {
        Err(EnvironmentError::AssertionFailed)
    }
}

fn is_same_ptr<M>(_: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let left = parameters[0].as_ref(heap);
    let right = parameters[1].as_ref(heap);
    let same = ptr::from_ref(left) == ptr::from_ref(right);

    Ok(SysCallResult::Return(Primitive::Boolean(same).into()))
}

fn require<M>(_: FnInstance, parameters: FnParams, _: &ModuleMetadata<'_, M>, _context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let msg = parameters.remove(1)
        .into_owned(heap)?
        .into_string()?;

    let param = &parameters[0];
    let value = param.as_bool(heap)?;

    if value {
        Ok(SysCallResult::None)
    } else {
        Err(EnvironmentError::Expect(msg))
    }
}

fn clone<M>(zelf: FnInstance, _: FnParams, _: &ModuleMetadata<'_, M>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<M> {
    let zelf = zelf?;

    let memory = zelf.as_ref(heap).calculate_memory_usage(heap, context.memory_left())?;
    // Double cost: computation for cloning and memory allocation?
    context.increase_memory_usage_unchecked(memory)?;

    Ok(SysCallResult::Return(zelf.to_owned(heap)?))
}

// This private constructor is used only by the standard library below this
// module. Its handlers retain runtime owners exclusively in engine-managed
// values and callback state; no host callback receives those owners.
fn native<M>(handler: OnCallSyncFn<M>) -> FunctionHandler<M> {
    unsafe { FunctionHandler::sync_unchecked(handler) }
}
