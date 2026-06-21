mod iter;
mod terminal;
mod fns;

pub use iter::{BaseSource, XIterator};

use xelis_environment::FunctionHandler;
use xelis_types::{ClosureType, OpaqueTypeTrait, Type};
use xelis_types::opaque::OpaqueType;

use crate::EnvironmentBuilder;

use fns::*;

pub fn register<M: 'static>(env: &mut EnvironmentBuilder<M>) -> OpaqueType {
    let iter_template = env.register_generic_opaque_with_traits::<XIterator>("Iterator", false, 1, &[OpaqueTypeTrait::Iterable]);
    let iter_t0 = || Type::Opaque(iter_template.with_generics(vec![Type::T(Some(0))]));

    env.register_static_function_with_comment(
        "once",
        Type::Opaque(iter_template.clone()),
        vec![("value", Type::T(Some(0)))],
        FunctionHandler::Sync(iter_once),
        2,
        Some(iter_t0()),
        "Creates an iterator that yields one value."
    );

    env.register_static_function_with_comment(
        "empty",
        Type::Opaque(iter_template.clone()),
        vec![],
        FunctionHandler::Sync(iter_empty),
        1,
        Some(iter_t0()),
        "Creates an iterator that yields no values."
    );

    let unfold_closure = Type::Closure(ClosureType::new(
        vec![Type::T(Some(0))],
        Some(Type::Optional(Box::new(Type::Tuples(vec![Type::T(Some(1)), Type::T(Some(0))])))),
    ));
    env.register_static_function_with_comment(
        "unfold",
        Type::Opaque(iter_template.clone()),
        vec![("seed", Type::T(Some(0))), ("f", unfold_closure)],
        FunctionHandler::Sync(iter_unfold),
        10,
        Some(Type::Opaque(iter_template.with_generics(vec![Type::T(Some(1))]))),
        "Creates an iterator by repeatedly calling a closure with evolving state."
    );

    env.register_native_function_with_comment(
        "iter",
        Some(Type::Array(Box::new(Type::T(Some(0))))),
        vec![],
        FunctionHandler::Sync(array_iter),
        5,
        Some(iter_t0()),
        "Creates an iterator over the array elements."
    );

    env.register_native_function_with_comment(
        "next",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_next),
        2,
        Some(Type::Optional(Box::new(Type::T(Some(0))))),
        "Advances the iterator and returns the next value, or null when exhausted."
    );

    env.register_native_function_with_comment(
        "count",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_count),
        1,
        Some(Type::U32),
        "Consumes the iterator and returns how many values it yields."
    );

    env.register_native_function_with_comment(
        "skip",
        Some(iter_t0()),
        vec![("n", Type::U32)],
        FunctionHandler::Sync(iter_skip),
        2,
        Some(iter_t0()),
        "Returns an iterator that skips the first n values."
    );

    env.register_native_function_with_comment(
        "take",
        Some(iter_t0()),
        vec![("n", Type::U32)],
        FunctionHandler::Sync(iter_take),
        2,
        Some(iter_t0()),
        "Returns an iterator that yields at most n values."
    );

    env.register_native_function_with_comment(
        "chain",
        Some(iter_t0()),
        vec![("other", iter_t0())],
        FunctionHandler::Sync(iter_chain),
        5,
        Some(iter_t0()),
        "Returns an iterator that yields this iterator followed by another."
    );

    env.register_native_function_with_comment(
        "enumerate",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_enumerate),
        5,
        Some(Type::Opaque(iter_template.with_generics(vec![Type::Tuples(vec![Type::U32, Type::T(Some(0))])]))),
        "Returns an iterator of index-value pairs."
    );

    env.register_native_function_with_comment(
        "rev",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_rev),
        5,
        Some(iter_t0()),
        "Returns an iterator that yields values in reverse order when supported."
    );

    env.register_native_function_with_comment(
        "collect",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_collect),
        5,
        Some(Type::Array(Box::new(Type::T(Some(0))))),
        "Consumes the iterator and returns all yielded values as an array."
    );

    env.register_native_function_with_comment(
        "zip",
        Some(iter_t0()),
        vec![("other", Type::Opaque(iter_template.with_generics(vec![Type::T(Some(1))])))],
        FunctionHandler::Sync(iter_zip),
        5,
        Some(Type::Opaque(iter_template.with_generics(vec![Type::Tuples(vec![Type::T(Some(0)), Type::T(Some(1))])]))),
        "Returns an iterator that pairs values from two iterators until either is exhausted."
    );

    env.register_native_function_with_comment(
        "flatten",
        Some(Type::Opaque(iter_template.with_generics(vec![iter_t0()]))),
        vec![],
        FunctionHandler::Sync(iter_flatten),
        10,
        Some(iter_t0()),
        "Flattens an iterator of iterators into a single iterator."
    );

    env.register_native_function_with_comment(
        "sum",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_sum),
        10,
        Some(Type::T(Some(0))),
        "Consumes the iterator and returns the sum of its values."
    );

    let mapper_closure = Type::Closure(ClosureType::new(vec![Type::T(Some(0))], Some(Type::T(Some(1)))));
    env.register_native_function_with_comment(
        "map",
        Some(iter_t0()),
        vec![("mapper", mapper_closure)],
        FunctionHandler::Sync(iter_map),
        10,
        Some(Type::Opaque(iter_template.with_generics(vec![Type::T(Some(1))]))),
        "Returns a lazy iterator that applies the mapper closure to each value."
    );

    let predicate_closure = Type::Closure(ClosureType::new(vec![Type::T(Some(0))], Some(Type::Bool)));
    env.register_native_function_with_comment(
        "filter",
        Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_filter),
        10,
        Some(iter_t0()),
        "Returns a lazy iterator that keeps values accepted by the predicate."
    );

    let consumer_closure = Type::Closure(ClosureType::new(vec![Type::T(Some(0))], None));
    env.register_native_function_with_comment(
        "for_each",
        Some(iter_t0()),
        vec![("f", consumer_closure)],
        FunctionHandler::Sync(iter_for_each),
        10,
        None,
        "Consumes the iterator and calls the closure for each value."
    );

    env.register_native_function_with_comment(
        "find",
        Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_find),
        10,
        Some(Type::Optional(Box::new(Type::T(Some(0))))),
        "Consumes the iterator until a value matches the predicate, returning it or null."
    );

    env.register_native_function_with_comment(
        "any",
        Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_any),
        10,
        Some(Type::Bool),
        "Returns true when any yielded value matches the predicate."
    );

    env.register_native_function_with_comment(
        "all",
        Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_all),
        10,
        Some(Type::Bool),
        "Returns true when every yielded value matches the predicate."
    );

    let fold_closure = Type::Closure(ClosureType::new(
        vec![Type::T(Some(0)), Type::T(Some(0))],
        Some(Type::T(Some(0)))
    ));
    env.register_native_function_with_comment(
        "fold",
        Some(iter_t0()),
        vec![("init", Type::T(Some(0))), ("f", fold_closure)],
        FunctionHandler::Sync(iter_fold),
        10,
        Some(Type::T(Some(0))),
        "Consumes the iterator and combines values with an accumulator closure."
    );

    env.register_native_function_with_comment(
        "position",
        Some(iter_t0()),
        vec![("predicate", predicate_closure)],
        FunctionHandler::Sync(iter_position),
        10,
        Some(Type::Optional(Box::new(Type::U32))),
        "Returns the index of the first value matching the predicate, or null."
    );

    iter_template
}
