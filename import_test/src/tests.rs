use crate::*;
use std::env;
use std::fs;

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
        }
        Err(err) => {
            panic!("Compilation failed: {}", err);
        }
    }
}