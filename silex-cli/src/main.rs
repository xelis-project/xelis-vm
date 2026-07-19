use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::{bail, Context, Result};
use clap::{Args, Parser as ClapParser, Subcommand};
use silex_assembler::{Assembler, Disassembler};
use silex_builder::EnvironmentBuilder;
use silex_bytecode::Module;
use silex_compiler::Compiler;
use silex_environment::{Environment, ModuleMetadata};
use silex_lexer::Lexer;
use silex_parser::Parser;
use silex_types::{Primitive, ValueCell};
use xelis_vm::{ModuleValidator, VM};

#[derive(Debug, ClapParser)]
#[command(
    name = "silex",
    about = "Compile, inspect, assemble, and run Silex programs"
)]
struct Config {
    #[command(subcommand)]
    command: SubCommands,
}

#[derive(Debug, Subcommand)]
enum SubCommands {
    /// Compile a Silex source program into a JSON bytecode module.
    Compile(CompileConfig),
    /// Compile and run an entry chunk from a Silex source program.
    Run(RunConfig),
    /// Print a JSON bytecode module as assembly.
    Disasm(DisasmConfig),
    /// Assemble textual bytecode into a JSON bytecode module.
    Asm(AsmConfig),
}

#[derive(Debug, Args)]
struct CompileConfig {
    #[arg(value_name = "INPUT")]
    input: PathBuf,
    #[arg(
        short,
        long,
        value_name = "OUTPUT",
        help = "Output JSON module path (defaults to INPUT with a .json extension)"
    )]
    output: Option<PathBuf>,
}

#[derive(Debug, Args)]
struct RunConfig {
    #[arg(value_name = "INPUT")]
    input: PathBuf,
    #[arg(
        short,
        long,
        value_name = "ID",
        help = "Entry chunk ID to invoke (defaults to the first entry chunk)"
    )]
    entry: Option<u16>,
    #[arg(
        long,
        value_name = "GAS",
        help = "Maximum gas available to the program"
    )]
    gas_limit: Option<u64>,
    #[arg(
        value_name = "ARG",
        trailing_var_arg = true,
        allow_hyphen_values = true,
        help = "Arguments: null, bool, unsigned integer, string, or a JSON ValueCell"
    )]
    arguments: Vec<String>,
}

#[derive(Debug, Args)]
struct DisasmConfig {
    #[arg(value_name = "INPUT")]
    input: PathBuf,
}

#[derive(Debug, Args)]
struct AsmConfig {
    #[arg(value_name = "INPUT")]
    input: PathBuf,
    #[arg(
        short,
        long,
        value_name = "OUTPUT",
        help = "Output JSON module path (defaults to INPUT with a .json extension)"
    )]
    output: Option<PathBuf>,
}

fn read_file(path: &Path) -> Result<String> {
    fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))
}

fn output_path(input: &Path, output: Option<&PathBuf>) -> PathBuf {
    output
        .cloned()
        .unwrap_or_else(|| input.with_extension("json"))
}

fn write_module(module: &Module, path: &Path) -> Result<()> {
    let json =
        serde_json::to_string_pretty(module).context("failed to serialize bytecode module")?;
    fs::write(path, format!("{json}\n"))
        .with_context(|| format!("failed to write {}", path.display()))
}

fn read_module(path: &Path) -> Result<Module> {
    let source = read_file(path)?;
    serde_json::from_str(&source)
        .with_context(|| format!("failed to parse JSON bytecode module {}", path.display()))
}

fn compile_source(source: &str) -> Result<(Module, Environment<()>)> {
    let environment = EnvironmentBuilder::<()>::default();
    let tokens = Lexer::new(source)
        .collect::<std::result::Result<Vec<_>, _>>()
        .map_err(|error| anyhow::anyhow!("failed to lex Silex source: {error}"))?;
    let (program, _) = Parser::with(tokens.into_iter(), &environment)
        .parse()
        .map_err(|error| anyhow::anyhow!("failed to parse Silex source: {error}"))?;
    let module = Compiler::new(&program, environment.environment())
        .compile()
        .context("failed to compile Silex source")?;

    Ok((module, environment.build()))
}

fn parse_argument(argument: &str) -> Result<ValueCell> {
    if let Ok(value) = serde_json::from_str(argument) {
        return Ok(value);
    }

    let primitive = match argument {
        "null" => Primitive::Null,
        "true" => Primitive::Boolean(true),
        "false" => Primitive::Boolean(false),
        _ => match argument.parse() {
            Ok(value) => Primitive::U64(value),
            Err(_) => Primitive::String(argument.to_owned()),
        },
    };

    Ok(primitive.into())
}

fn main() -> Result<()> {
    let config = Config::parse();

    match config.command {
        SubCommands::Compile(config) => {
            let source = read_file(&config.input)?;
            let (module, _) = compile_source(&source)?;
            let output = output_path(&config.input, config.output.as_ref());
            write_module(&module, &output)?;
        }
        SubCommands::Asm(config) => {
            let source = read_file(&config.input)?;
            let module = Assembler::new(&source)
                .assemble()
                .map_err(|error| anyhow::anyhow!("failed to assemble source: {error}"))?;
            let output = output_path(&config.input, config.output.as_ref());
            write_module(&module, &output)?;
        }
        SubCommands::Disasm(config) => {
            let module = read_module(&config.input)?;
            let dump = Disassembler::new(&module)
                .disasemble()
                .context("failed to disassemble bytecode module")?;
            println!("{dump}");
        }
        SubCommands::Run(config) => {
            let source = read_file(&config.input)?;
            let (module, environment) = compile_source(&source)?;
            ModuleValidator::new(&module, &environment)
                .verify()
                .context("compiled module failed validation")?;

            let entry = config
                .entry
                .or_else(|| {
                    module
                        .chunks()
                        .iter()
                        .position(|chunk| {
                            matches!(chunk.access, silex_bytecode::Access::Entry { .. })
                        })
                        .and_then(|id| u16::try_from(id).ok())
                })
                .context("program does not define an entry chunk")?;
            if !module.is_entry_chunk(entry as usize) {
                bail!("chunk {entry} is not an entry chunk");
            }

            let arguments = config
                .arguments
                .iter()
                .map(|argument| parse_argument(argument))
                .collect::<Result<Vec<_>>>()?;

            let mut vm = VM::<()>::default();
            vm.append_module(ModuleMetadata {
                module: (&module).into(),
                metadata: (&()).into(),
                environment: (&environment).into(),
            })?;
            if let Some(gas_limit) = config.gas_limit {
                vm.context_mut().set_gas_limit(gas_limit);
            }
            vm.invoke_chunk_with_args(entry, arguments.into_iter())?;
            println!("{}", vm.run_blocking()?);
        }
    }

    Ok(())
}
