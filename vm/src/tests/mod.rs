use matriochka::ResultExt as _;
use silex_builder::EnvironmentBuilder;
use silex_bytecode::Module;
use crate::*;
use silex_types::OwnedValue;

/// This module contains all the tests for the VM
/// Where each test is crafted by hand using OpCodes
mod op_code;
mod arena;
mod confinement;

/// This module contains all the tests using the suite available
/// (Lexer, Parser, Compiler) to produce a valid Module.
mod full;

/// This module contains comprehensive tests for all standard library functions
mod stdlib_tests;

/// This module contains tests for the ModuleValidator
mod validator;

#[track_caller]
fn run_internal<'a>(module: Module, environment: &'a Environment<()>, id: u16) -> Result<Primitive, VMError> {
    // Verify the module using validator
    let validator = ModuleValidator::new(&module, environment);
    validator.verify()
        .context("validating compiled module")?;

    let mut vm = VM::default();
    vm.append_module(ModuleMetadata {
        module: (&module).into(),
        environment: environment.into(),
        metadata: (&()).into(),
    }).expect("module");
    vm.context_mut().set_gas_limit(10u64.pow(8u32));
    vm.invoke_chunk_id_unchecked(id as _).expect("valid entry chunk");
    let result = vm.run_blocking().map(|mut v| v.into_value().expect("primitive"));
    if result.is_ok() { assert!(vm.is_idle()); }
    result
}

#[track_caller]
fn run(module: Module) -> Primitive {
    let environment = EnvironmentBuilder::default().build();
    run_internal(module, &environment, 0).unwrap()
}

fn assert_send<T: Send>(_: T) {}

#[test]
fn assert_thread_safe() {
    assert_send(OwnedValue::default());

    let mut vm = VM::<()>::default();
    assert_send(&vm);
    assert_send(vm.run());
    assert_send(vm.run_blocking());
}