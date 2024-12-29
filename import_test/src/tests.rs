use crate::*;
use std::env;
use std::fs;

use xelis_vm::VMError;

#[track_caller]
fn try_run_code(silex: &Silex, module: &Module, id: u16) -> Result<Value, VMError> {
    let mut vm = VM::new(module, silex.environment.environment());
    vm.invoke_entry_chunk(1).unwrap();
    vm.run().map(|v| v.into_value().unwrap())
}

#[track_caller]
fn run_code_id(silex: &Silex, module: &Module, id: u16) -> Value {
    try_run_code(silex, module, id).unwrap()
}

#[track_caller]
fn run_code(silex: &Silex, module: &Module, ) -> Value {
    run_code_id(silex, module, 0)
}

#[test]
fn test_compile_silex_program() {
    // Determine the absolute path to the test file
    let base_dir = env::current_dir().unwrap();
    let test_file_path = base_dir.join("src").join("silex").join("main.slx");

    let p2 = &base_dir.join("src").join("silex");

    println!("Resolved test file path: {:?}", test_file_path);

    // Read the contents of the test file
    let code = fs::read_to_string(&test_file_path)
        .expect(&format!("Failed to read slx file: {:?}", test_file_path));

    // Create a new instance of Silex
    let silex = Silex::new();

    // Compile the code
    match silex.compile(&code, test_file_path.to_str().expect("Invaid utf-8")) {
        Ok(program) => {
            println!("Compilation successful!");
            println!("Entries: {:?}", program.entries());

            let res = run_code_id(&silex, &program.module, 1);
            println!("result: {:?}", res);
        }
        Err(err) => {
            panic!("Compilation failed: {}", err);
        }
    }
}