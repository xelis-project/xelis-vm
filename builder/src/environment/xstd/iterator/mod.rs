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

    env.register_static_function(
        "once",
        Type::Opaque(iter_template.clone()),
        vec![("value", Type::T(Some(0)))],
        FunctionHandler::Sync(iter_once),
        2,
        Some(iter_t0())
    );

    env.register_static_function(
        "empty",
        Type::Opaque(iter_template.clone()),
        vec![],
        FunctionHandler::Sync(iter_empty),
        1,
        Some(iter_t0())
    );

    let unfold_closure = Type::Closure(ClosureType::new(
        vec![Type::T(Some(0))],
        Some(Type::Optional(Box::new(Type::Tuples(vec![Type::T(Some(1)), Type::T(Some(0))])))),
    ));
    env.register_static_function(
        "unfold",
        Type::Opaque(iter_template.clone()),
        vec![("seed", Type::T(Some(0))), ("f", unfold_closure)],
        FunctionHandler::Sync(iter_unfold),
        10,
        Some(Type::Opaque(iter_template.with_generics(vec![Type::T(Some(1))])))
    );

    env.register_native_function(
        "iter",
        Some(Type::Array(Box::new(Type::T(Some(0))))),
        vec![],
        FunctionHandler::Sync(array_iter),
        5,
        Some(iter_t0())
    );

    env.register_native_function(
        "next",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_next),
        2,
        Some(Type::Optional(Box::new(Type::T(Some(0)))))
    );

    env.register_native_function(
        "count",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_count),
        1,
        Some(Type::U32)
    );

    env.register_native_function(
        "skip",
        Some(iter_t0()),
        vec![("n", Type::U32)],
        FunctionHandler::Sync(iter_skip),
        2,
        Some(iter_t0())
    );

    env.register_native_function(
        "take",
        Some(iter_t0()),
        vec![("n", Type::U32)],
        FunctionHandler::Sync(iter_take),
        2,
        Some(iter_t0())
    );

    env.register_native_function(
        "chain",
        Some(iter_t0()),
        vec![("other", iter_t0())],
        FunctionHandler::Sync(iter_chain),
        5,
        Some(iter_t0())
    );

    env.register_native_function(
        "enumerate",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_enumerate),
        5,
        Some(Type::Opaque(iter_template.with_generics(vec![Type::Tuples(vec![Type::U32, Type::T(Some(0))])])))
    );

    env.register_native_function(
        "rev",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_rev),
        5,
        Some(iter_t0())
    );

    env.register_native_function(
        "collect",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_collect),
        5,
        Some(Type::Array(Box::new(Type::T(Some(0)))))
    );

    env.register_native_function(
        "zip",
        Some(iter_t0()),
        vec![("other", Type::Opaque(iter_template.with_generics(vec![Type::T(Some(1))])))],
        FunctionHandler::Sync(iter_zip),
        5,
        Some(Type::Opaque(iter_template.with_generics(vec![Type::Tuples(vec![Type::T(Some(0)), Type::T(Some(1))])])))
    );

    env.register_native_function(
        "flatten",
        Some(Type::Opaque(iter_template.with_generics(vec![iter_t0()]))),
        vec![],
        FunctionHandler::Sync(iter_flatten),
        10,
        Some(iter_t0())
    );

    env.register_native_function(
        "sum",
        Some(iter_t0()),
        vec![],
        FunctionHandler::Sync(iter_sum),
        10,
        Some(Type::T(Some(0)))
    );

    let mapper_closure = Type::Closure(ClosureType::new(vec![Type::T(Some(0))], Some(Type::T(Some(1)))));
    env.register_native_function(
        "map",
        Some(iter_t0()),
        vec![("mapper", mapper_closure)],
        FunctionHandler::Sync(iter_map),
        10,
        Some(Type::Opaque(iter_template.with_generics(vec![Type::T(Some(1))])))
    );

    let predicate_closure = Type::Closure(ClosureType::new(vec![Type::T(Some(0))], Some(Type::Bool)));
    env.register_native_function(
        "filter",
        Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_filter),
        10,
        Some(iter_t0())
    );

    let consumer_closure = Type::Closure(ClosureType::new(vec![Type::T(Some(0))], None));
    env.register_native_function(
        "for_each",
        Some(iter_t0()),
        vec![("f", consumer_closure)],
        FunctionHandler::Sync(iter_for_each),
        10,
        None
    );

    env.register_native_function(
        "find",
        Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_find),
        10,
        Some(Type::Optional(Box::new(Type::T(Some(0)))))
    );

    env.register_native_function(
        "any",
        Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_any),
        10,
        Some(Type::Bool)
    );

    env.register_native_function(
        "all",
        Some(iter_t0()),
        vec![("predicate", predicate_closure.clone())],
        FunctionHandler::Sync(iter_all),
        10,
        Some(Type::Bool)
    );

    let fold_closure = Type::Closure(ClosureType::new(
        vec![Type::T(Some(0)), Type::T(Some(0))],
        Some(Type::T(Some(0)))
    ));
    env.register_native_function(
        "fold",
        Some(iter_t0()),
        vec![("init", Type::T(Some(0))), ("f", fold_closure)],
        FunctionHandler::Sync(iter_fold),
        10,
        Some(Type::T(Some(0)))
    );

    env.register_native_function(
        "position",
        Some(iter_t0()),
        vec![("predicate", predicate_closure)],
        FunctionHandler::Sync(iter_position),
        10,
        Some(Type::Optional(Box::new(Type::U32)))
    );

    iter_template
}
