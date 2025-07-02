use xelis_builder::EnvironmentBuilder;
use xelis_bytecode::Module;
use crate::*;

/// This module contains all the tests for the VM
/// Where each test is crafted by hand using OpCodes
mod op_code;

/// This module contains all the tests using the suite available
/// (Lexer, Parser, Compiler) to produce a valid Module.
mod full;


#[track_caller]
fn run_internal<'a>(module: Module, environment: &'a Environment<()>, id: u16) -> Result<Primitive, VMError> {
    // Verify the module using validator
    let validator = ModuleValidator::new(&module, environment);
    validator.verify().unwrap();

    let mut vm = VM::new(environment);
    vm.context_mut().set_gas_limit(10u64.pow(8u32));
    vm.append_module(&module, &()).expect("module");
    vm.invoke_chunk_id(id as _).expect("valid entry chunk");
    vm.run().map(|mut v| v.into_value().expect("primitive"))
}

#[track_caller]
fn run(module: Module) -> Primitive {
    let environment = EnvironmentBuilder::default().build();
    run_internal(module, &environment, 0).unwrap()
}
