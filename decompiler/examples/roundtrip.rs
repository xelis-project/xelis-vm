use silex_builder::EnvironmentBuilder;
use silex_compiler::Compiler;
use silex_decompiler::Decompiler;
use silex_lexer::Lexer;
use silex_parser::Parser;

fn main() {
    let environment = EnvironmentBuilder::<()>::default();
    let input = r#"
        pub fn sum(limit: u64) -> u64 {
            let total: u64 = 0
            foreach value in 0..limit {
                total += value
            }
            return total
        }

        entry main() {
            println("Reconstructed contract")
            return sum(10)
        }
    "#;
    let tokens = Lexer::new(input).get().expect("lex source");
    let (program, _) = Parser::new(tokens, &environment)
        .parse()
        .expect("parse source");
    let module = Compiler::new(&program, environment.environment())
        .compile()
        .expect("compile source");
    let source = Decompiler::new(&module, &environment)
        .decompile()
        .expect("decompile module");
    println!("{source}");
}
