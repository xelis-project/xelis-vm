use crate::*;
use std::env;
use std::fs;

use xelis_vm::VMError;

#[track_caller]
fn try_run_code(silex: &Silex, module: &Module, id: u16) -> Result<Value, VMError> {
    let mut vm = VM::new(module, silex.environment.environment());
    vm.invoke_entry_chunk(id).unwrap();
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
fn test_import_basic() {
    let base_dir = env::current_dir().unwrap();
    let test_file_path = base_dir.join("src").join("silex/basic").join("main.slx");

    let mut silex = Silex::new();

    match silex.compile(test_file_path.to_str().expect("Invaid utf-8")) {
        Ok(program) => {
            let res = run_code_id(&silex, &program.module, 1);
            assert_eq!(res, Value::U64(255));
        }
        Err(err) => {
            panic!("Compilation failed: {}", err);
        }
    }
}

#[test]
fn test_import_main() {
    let base_dir = env::current_dir().unwrap();
    let test_file_path = base_dir.join("src").join("silex/import_main").join("main.slx");

    let mut silex = Silex::new();

    match silex.compile(test_file_path.to_str().expect("Invaid utf-8")) {
        Ok(program) => {
            let res = run_code(&silex, &program.module);
            assert_eq!(res, Value::U64(0));
        }
        Err(err) => {
            panic!("Compilation failed: {}", err);
        }
    }
}

#[test]
fn test_import_duplicate() {
    let base_dir = env::current_dir().unwrap();
    let test_file_path = base_dir.join("src").join("silex/duplicate_import").join("main.slx");

    let mut silex = Silex::new();

    match silex.compile(test_file_path.to_str().expect("Invaid utf-8")) {
        Ok(program) => {
            let res = run_code_id(&silex, &program.module, 2);
            assert_eq!(res, Value::U64(u64::MAX));
        }
        Err(err) => {
            panic!("Compilation failed: {}", err);
        }
    }
}

#[test]
fn test_import_implied() {
    let base_dir = env::current_dir().unwrap();
    let test_file_path = base_dir.join("src").join("silex/implied_import").join("main.slx");

    let mut silex = Silex::new();

    match silex.compile(test_file_path.to_str().expect("Invaid utf-8")) {
        Ok(program) => {
            let res = run_code_id(&silex, &program.module, 2);
            assert_eq!(res, Value::U64(u64::MAX));
        }
        Err(err) => {
            panic!("Compilation failed: {}", err);
        }
    }
}

#[test]
fn test_import_common() {
    let base_dir = env::current_dir().unwrap();
    let test_file_path = base_dir.join("src").join("silex/common_import").join("main.slx");

    let mut silex = Silex::new();

    match silex.compile(test_file_path.to_str().expect("Invaid utf-8")) {
        Ok(program) => {
            println!("Generated ABI:\n{}", program.abi());
            let res = run_code_id(&silex, &program.module, 3);
            assert_eq!(res, Value::U64(0));
        }
        Err(err) => {
            panic!("Compilation failed: {}", err);
        }
    }
}

#[test]
fn test_import_as() {
    let base_dir = env::current_dir().unwrap();
    let test_file_path = base_dir.join("src").join("silex/import_as/main.slx");

    let mut silex = Silex::new();

    match silex.compile(test_file_path.to_str().expect("Invaid utf-8")) {
        Ok(program) => {
            println!("Generated ABI:\n{}", program.abi());
            let res = run_code_id(&silex, &program.module, 6);
            assert_eq!(res, Value::U64(25));
        }
        Err(err) => {
            panic!("Compilation failed: {}", err);
        }
    }
}

#[test]
fn test_circular_dependency() {
    let base_dir = env::current_dir().unwrap();
    let test_file_path = base_dir.join("src").join("silex/circular").join("main.slx");

    let mut silex = Silex::new();

    match silex.compile(test_file_path.to_str().expect("Invaid utf-8")) {
        Ok(program) => {
            panic!("Circular Dependency Undetected");
        },
        Err(err) => {},
    }
}