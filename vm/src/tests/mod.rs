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
fn run_internal(module: Module, environment: &Environment, id: u16) -> Result<Primitive, VMError> {
    let mut vm = VM::new(&module, environment);
    vm.context_mut().set_gas_limit(10u64.pow(8u32));
    vm.invoke_chunk_id(id).unwrap();
    vm.run().map(|v| v.into_value().unwrap())
}

#[track_caller]
fn run(module: Module) -> Primitive {
    let environment = EnvironmentBuilder::default().build();
    run_internal(module, &environment, 0).unwrap()
}
