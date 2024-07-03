use xelis_vm::Lexer;
use xelis_vm::Parser;
use xelis_vm::Environment;
use xelis_vm::Interpreter;
use std::time::Instant;
use std::{fs, env};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = match args.get(1) {
        Some(v) => v,
        None => {
            panic!("Error, expected a file path as argument.");
        }
    };

    let code = fs::read_to_string(file).expect("Something went wrong reading the file");
    let chars: Vec<char> = code.chars().collect();
    let mut start = Instant::now();
    match Lexer::new(chars).get() {
        Ok(result) => {
            println!("Lexer: {} microseconds\n", start.elapsed().as_micros());
            start = Instant::now();
            let environment = Environment::default();
            match Parser::new(result, &environment).parse() {
                Ok(result) => {
                    println!("Parser: {} microseconds\n", start.elapsed().as_micros());
                    // println!("Parser:\n{:?}\n", result);
                    match Interpreter::new(&result, 50000, 1000, &environment) {
                        Ok(interpreter) => match interpreter.call_entry_function(&"main".to_owned(), vec![]) {
                            Ok(value) => println!("Exit code: {} | Expressions executed: {}", value, interpreter.get_count_expr()),
                            Err(e) => println!("Error: {:?}", e)
                        },
                        Err(e) => println!("Interpreter error: {:?}", e)
                    };
                }
                Err(e) => println!("Parser error: {:?}", e)
            };
        }
        Err(e) => println!("Lexer error: {:?}", e)
    };
}