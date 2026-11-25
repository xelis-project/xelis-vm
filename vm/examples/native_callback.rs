//! Run with: cargo run -p xelis-vm --example native_callback

use silex_builder::EnvironmentBuilder;
use silex_compiler::Compiler;
use silex_lexer::Lexer;
use silex_parser::Parser;
use xelis_vm::{
    ClosureType,
    EnvironmentError,
    FunctionHandler,
    ModuleMetadata,
    ModuleValidator,
    NativeScope,
    NativeState,
    OwnedValue,
    Type,
    VMContext,
    VM,
};

fn resume<'call, 'ty, 'r>(
    _: NativeState,
    scope: &mut NativeScope<'call, ()>,
    context: &mut VMContext<'ty, 'r>,
) -> Result<(), EnvironmentError> {
    // The VM creates a fresh scope: argument 0 is the closure's return value.
    let result = scope.as_u64(scope.argument(0)?)?;
    let array = scope.retained(0)?;
    let element = scope.array_pop(array)?;
    let original = scope.as_u64(element)?;

    // Portable results use the active meter when importing them rebuilds maps.
    // This scalar result needs no hashing or equality charges.
    scope.return_owned(OwnedValue::from(result + original), context.gas_meter())
}

fn invoke<'call, 'metadata, 'ty, 'r>(
    scope: &mut NativeScope<'call, ()>,
    _: &ModuleMetadata<'metadata, ()>,
    _: &mut VMContext<'ty, 'r>,
) -> Result<(), EnvironmentError> {
    let function = scope.argument(0)?;
    let array = scope.argument(1)?;
    let element = scope.array_get(array, 0)?;

    // This schedules the closure; it does not execute it inside this Rust call.
    // Retained VM values stay inside the engine, without a portable snapshot.
    // The views belong to this call and cannot be stored in NativeState.
    scope.call_and_resume(
        function,
        &[element],
        &[array],
        Box::new(()),
        1,
        resume,
    );
    Ok(())
}

fn main() {
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_native_function(
        "invoke",
        None,
        vec![
            ("function", Type::Closure(ClosureType::new(vec![Type::U64], Some(Type::U64)))),
            ("array", Type::Array(Box::new(Type::U64))),
        ],
        FunctionHandler::Sync(invoke),
        0,
        Some(Type::U64),
    );

    let code = r#"
        entry main() {
            return invoke(|x: u64| { return x + 1 }, [7u64])
        }
    "#;
    let tokens = Lexer::new(code)
        .into_iter()
        .collect::<Result<Vec<_>, _>>()
        .expect("tokenize example");
    let (program, _) = Parser::with(tokens.into_iter(), &builder)
        .parse()
        .expect("parse example");
    let environment = builder.build();
    let module = Compiler::new(&program, &environment)
        .compile()
        .expect("compile example");
    ModuleValidator::new(&module, &environment)
        .verify()
        .expect("validate example");

    let mut vm = VM::default();
    vm.append_module(ModuleMetadata {
        module: (&module).into(),
        environment: (&environment).into(),
        metadata: (&()).into(),
    })
    .expect("append example module");
    vm.invoke_entry_chunk(0).expect("invoke main");
    let result = vm.run_blocking().expect("run example");
    assert_eq!(result.as_u64().expect("u64 result"), 15);
    println!("closure: 7 + 1 = 8; resumed native call: 8 + 7 = 15");
}
