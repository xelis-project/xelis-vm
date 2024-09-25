use xelis_vm::{
    ast::Signature,
    bytecode::{compiler::Compiler, vm::VM},
    EnvironmentBuilder,
    Interpreter,
    Lexer,
    Parser,
    State
};
use std::{
    env,
    fs,
    time::Instant
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
    let builder = EnvironmentBuilder::default();

    // Build the program
    start = Instant::now();
    // let path = Path::new(file).parent().map(|p| p.to_str()).flatten();
    let (program, mapper) = Parser::new(tokens, &builder).parse().unwrap();
    println!("Parser: {:?}", start.elapsed());

    // Create the Interpreter instance
    {
        let mut interpreter = Interpreter::new(&program, builder.environment()).unwrap();
        let mut state = State::new(None, Some(100), None);
        interpreter.compute_constants(&mut state).unwrap();
    
        start = Instant::now();
        let signature = Signature::new("main".to_owned(), None, Vec::new());
        let exit = interpreter.call_entry_function(&mapper.get(&signature).unwrap(), Vec::new(), &mut state).unwrap();
        println!("interpreter | Exit code: {} | gas: {} | {} expressions executed in {:?}", exit, state.get_gas_usage(), state.get_expressions_executed(), start.elapsed());
    }

    // VM
    {
        let compiler = Compiler::new(&program, builder.environment());
        let module = compiler.compile().unwrap();
        let mut vm = VM::new(&module, builder.environment());
        start = Instant::now();
        vm.invoke_chunk_id(1).unwrap();
        let value = vm.run().unwrap();
        println!("VM | Value: {:?} | {:?}", value, start.elapsed());
    }
}