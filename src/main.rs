use xelis_vm::{
    Lexer,
    Parser,
    Environment,
    Interpreter,
    State
};
use std::{
    time::Instant,
    {fs, env}
};

fn main() {
    let args: Vec<String> = env::args().collect();
    let file = match args.get(1) {
        Some(v) => v,
        None => {
            panic!("Error, expected a file path as argument.");
        }
    };

    let code = fs::read_to_string(file).expect("Something went wrong reading the file");
    let mut start = Instant::now();

    // Apply the lexer to the code
    let tokens = Lexer::new(code.as_str()).get().unwrap();
    println!("Lexer: {:?}", start.elapsed());

    // Create the default environment
    let (environment, mut mapper) = Environment::new();

    // Build the program
    start = Instant::now();
    let program = Parser::new(tokens, &environment).parse(&mut mapper).unwrap();
    println!("Parser: {:?}", start.elapsed());

    // Create the VM instance
    let vm = Interpreter::new(&program, &environment).unwrap();
    let mut state = State::new(1000, 100);
    start = Instant::now();
    let exit = vm.call_entry_function(&mapper.get(&"main".to_owned()).unwrap(), vec![], &mut state).unwrap();
    println!("Exit code: {} | {} expressions executed in {:?}", exit, state.get_expressions_executed(), start.elapsed());
}