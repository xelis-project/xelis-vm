use std::{
  collections::HashMap,
  sync::{
      atomic::{AtomicBool, Ordering},
      mpsc,
  },
};

use humantime::format_duration;
use indexmap::IndexMap;
// use storage::MockStorage;
use tokio;
use xelis_builder::EnvironmentBuilder;
use xelis_bytecode::Module;
use xelis_common::{
  block::{Block, BlockHeader, BlockVersion},
  contract::{ChainState, DeterministicRandom},
  crypto::{elgamal::CompressedPublicKey, Hash},
  utils::format_xelis,
};
use xelis_compiler::Compiler;
use xelis_lexer::Lexer;
use xelis_parser::Parser;
use xelis_types::{Type, Primitive};
use xelis_vm::VM;
use xelis_abi::abi_from_parse;

pub struct Silex {
  pub flattener: xelis_lexer::Flattener,
  pub environment: EnvironmentBuilder<'static>,
  pub logs_receiver: mpsc::Receiver<String>,
  pub is_running: AtomicBool,
}

#[derive(Clone)]
pub struct Program {
  pub module: Module,
  pub entries: Vec<Entry>,
  pub abi: String,
}

impl Program {
  pub fn entries(&self) -> Vec<Entry> {
      self.entries.clone()
  }

  pub fn abi(&self) -> &str {
    &self.abi
  }

  // pub fn to_bytes(&self) -> Vec<u8> {
  //     self.module.to_bytes()
  // }

  // pub fn to_hex(&self) -> String {
  //     self.module.to_hex()
  // }
}

#[derive(Debug, Clone)]
pub struct Parameter {
  name: String,
  _type: Type,
}

impl Parameter {
  pub fn name(&self) -> String {
      self.name.clone()
  }

  pub fn _type(&self) -> String {
      self._type.to_string()
  }
}

#[derive(Debug, Clone)]
pub struct Entry {
  id: usize,
  chunk_id: u16,
  name: String,
  parameters: Vec<Parameter>,
}

impl Entry {
  pub fn id(&self) -> usize {
      self.id
  }

  pub fn name(&self) -> String {
      self.name.clone()
  }

  pub fn parameters(&self) -> Vec<Parameter> {
      self.parameters.clone()
  }
}

pub struct StorageEntry {
  key: String,
  value: String,
}

impl StorageEntry {
  pub fn key(&self) -> String {
      self.key.clone()
  }

  pub fn value(&self) -> String {
      self.value.clone()
  }
}

pub struct ExecutionResult {
  value: String,
  logs: Vec<String>,
  elapsed_time: String,
  used_gas: u64,
  // storage: MockStorage,
}

impl ExecutionResult {
  pub fn value(&self) -> String {
      self.value.clone()
  }

  pub fn logs(&self) -> Vec<String> {
      self.logs.clone()
  }

  pub fn elapsed_time(&self) -> String {
      self.elapsed_time.clone()
  }

  pub fn used_gas(&self) -> u64 {
      self.used_gas
  }

  pub fn used_gas_formatted(&self) -> String {
      format_xelis(self.used_gas)
  }

  // pub fn storage(&self) -> Vec<StorageEntry> {
  //     self.storage
  //         .data
  //         .iter()
  //         .map(|(k, v)| StorageEntry {
  //             key: format!("{}", k),
  //             value: format!("{}", v),
  //         })
  //         .collect()
  // }
}

#[derive(Debug)]
pub struct Func {
  name: String,
  on_type: Option<String>,
  return_type: Option<String>,
  params: Vec<String>,
}

impl Func {
  pub fn name(&self) -> String {
      self.name.clone()
  }

  pub fn on_type(&self) -> Option<String> {
      self.on_type.clone()
  }

  pub fn return_type(&self) -> Option<String> {
      self.return_type.clone()
  }

  pub fn params(&self) -> Vec<String> {
      self.params.clone()
  }
}

impl Silex {
  pub fn new() -> Self {
      let mut environment = EnvironmentBuilder::default();
      let (_sender, receiver) = mpsc::channel();

      environment
          .get_mut_function("println", None, vec![Type::Any])
          .set_on_call(move |_, args, _| -> _ {
              let param = &args[0];
              println!("{:?}", param.as_ref()?.as_value());
              Ok(None)
          });

      Self {
          flattener: xelis_lexer::Flattener::new(),
          environment: environment,
          logs_receiver: receiver,
          is_running: AtomicBool::new(false),
      }
  }


  fn compile_internal(&self, code: &str) -> anyhow::Result<Program> {
      let tokens = Lexer::new(code)
          .into_iter()
          .collect::<Result<Vec<_>, _>>()?;

      let parser = Parser::with(tokens.into_iter(), &self.environment);
      let (program, mapper) = parser.parse().map_err(|err| anyhow::anyhow!("{:#}", err))?;

      let abi_json = abi_from_parse(program.clone(), &mapper, &self.environment)?.clone();

      // Collect all the available entry functions
      let mut entries = Vec::new();
      for (i, func) in program.functions().iter().enumerate() {
          let env_offset = self.environment.get_functions().len() as u16;

          if func.is_entry() {
              let mapping = mapper
                  .functions()
                  .get_function(&(i as u16 + env_offset))
                  .unwrap();

              let parameters: Vec<Parameter> = mapping
                  .parameters
                  .iter()
                  .map(|(name, _type)| Parameter {
                      name: name.to_string(),
                      _type: _type.clone(),
                  })
                  .collect();

              entries.push(Entry {
                  id: entries.len(),
                  chunk_id: i as u16,
                  name: mapping.name.to_owned(),
                  parameters,
              });
          }
      }

      let compiler = Compiler::new(&program, self.environment.environment());

      Ok(Program {
          module: compiler.compile()?,
          entries,
          abi: abi_json,
      })
  }

  // Compile the code
  pub fn compile(&mut self, path: &str) -> Result<Program, String> {
      let code = self.flattener.flatten(path)?;

      match self.compile_internal(&code) {
          Ok(program) => Ok(program),
          Err(err) => Err(format!("{:#}", err)),
      }
  }

  // Check if a program is running
  pub fn has_program_running(&self) -> bool {
      self.is_running.load(Ordering::Relaxed)
  }

  pub fn get_env_functions(&self) -> Vec<Func> {
      let mapper = self.environment.get_functions_mapper();
      fn type_to_string(env: &EnvironmentBuilder, ty: &Type) -> String {
          match ty {
              Type::Opaque(opaque) => env.get_opaque_name(opaque).unwrap().to_string(),
              Type::Struct(ty) => env.get_struct_manager().get_name_by_ref(ty).unwrap().0.to_string(),
              Type::Enum(ty) => env.get_enum_manager().get_name_by_ref(ty).unwrap().0.to_string(),
              Type::Array(ty) => format!("{}[]", type_to_string(env, ty)),
              Type::Optional(ty) => format!("optional<{}>", type_to_string(env, ty)),
              _ => ty.to_string(),
          }
      }

      let mut funcs = Vec::new();
      for (_t, list) in mapper.get_declared_functions().iter() {
          for f in list.iter() {
              let params: Vec<String> = f.parameters.iter().map(|(name, ty)| 
                  format!("{}: {}", name, type_to_string(&self.environment, &ty))
              ).collect();

              funcs.push(Func {
                  name: f.name.to_string(),
                  on_type: f.on_type.as_ref().map(|v| type_to_string(&self.environment, v)),
                  return_type: f.return_type.as_ref().map(|v| type_to_string(&self.environment, v)),
                  params: params,
              });
          }
      }

      funcs
  }

  // Execute the program
  pub async fn execute_program(
      &self,
      program: Program,
      entry_id: usize,
      max_gas: Option<u64>,
      params: Vec<String>,
  ) -> Result<ExecutionResult, String> {
      if self.has_program_running() {
          return Err("A program is already running".to_string());
      }

      let entry = program
          .entries
          .get(entry_id)
          .ok_or_else(|| "Invalid entry point")?;

      if entry.parameters.len() != params.len() {
          return Err("Invalid number of parameters".to_string());
      }

      let mut values = Vec::with_capacity(params.len());
      for (value, param) in params.into_iter().zip(entry.parameters.iter()) {
          let v = match param._type {
              Type::U8 => Primitive::U8(
                  value
                      .parse::<u8>()
                      .map_err(|_| format!("Expected a valid u8 type, got '{}'", value))?,
              ),
              Type::U16 => Primitive::U16(
                  value
                      .parse::<u16>()
                      .map_err(|_| format!("Expected a valid u16 type, got '{}'", value))?,
              ),
              Type::U32 => Primitive::U32(
                  value
                      .parse::<u32>()
                      .map_err(|_| format!("Expected a valid u32 type, got '{}'", value))?,
              ),
              Type::U64 => Primitive::U64(
                  value
                      .parse::<u64>()
                      .map_err(|_| format!("Expected a valid u64 type, got '{}'", value))?,
              ),
              Type::U128 => Primitive::U128(
                  value
                      .parse::<u128>()
                      .map_err(|_| format!("Expected a valid u128 type, got '{}'", value))?,
              ),
              Type::U256 => Primitive::U256(
                  value
                      .parse::<u128>() // Use u128 to parse, then convert to U256
                      .map(|v| v.into())
                      .map_err(|_| format!("Expected a valid u256 type, got '{}'", value))?,
              ),
              Type::String => Primitive::String(value.clone()), // Strings are directly used
              _ => {
                  return Err(format!(
                      "Unsupported parameter type: {}",
                      param._type
                  ));
              }
          };
      
          values.push(v);
    }

      // Mark it as running
      self.is_running.store(true, Ordering::Relaxed);

      let chunk_id = entry.chunk_id;
      let environment = self.environment.environment().clone();
      let res = tokio::task::spawn_blocking(move || {
          // Fake storage
          // TODO: allow user to configure data in it before running the program
          // TODO: configurable
          let deposits = IndexMap::new();
          // TODO: configurable
          let header = BlockHeader::new(BlockVersion::V0, 0, 0, Default::default(), Default::default(), CompressedPublicKey::new(Default::default()), Default::default());
          let block = Block::with(header, Vec::new());
          // TODO: configurable
          let random = DeterministicRandom::new(&Hash::zero(), &Hash::zero(), &Hash::max());

          let zero_hash = Hash::zero();

          let mut chain_state = ChainState {
              debug_mode: true,
              mainnet: false,
              random,
              block: &block,
              contract: &zero_hash,
              block_hash: &zero_hash,
              topoheight: 0,
              tx_hash: &zero_hash,
              deposits: &deposits,
              transfers: Vec::new(),
              storage: HashMap::new(),
          };

          let (res, elapsed_time, used_gas) = {
              // Create the VM, this will initialize the context also
              let mut vm = VM::new(&program.module, &environment);

              let context = vm.context_mut();
              context.insert_mut(&mut chain_state);

              if let Some(max_gas) = max_gas {
                  context.set_gas_limit(max_gas);
              }
              context.set_memory_price_per_byte(1);

              vm.invoke_entry_chunk_with_args(chunk_id, values.into_iter().rev())
                  .map_err(|err| format!("{:#}", err))?;

              let start = web_time::Instant::now();
              let res = vm.run();
              let elapsed_time = start.elapsed();
              let used_gas = vm.context().current_gas_usage();

              (res, elapsed_time, used_gas)
          };

          match res {
              Ok(value) => Ok(ExecutionResult {
                  value: format!("{}", value),
                  logs: Vec::new(),
                  elapsed_time: format_duration(elapsed_time).to_string(),
                  used_gas,
              }),
              Err(err) => Err(format!("{:#}", err)),
          }
      })
      .await;

      // Mark it as not running
      self.is_running.store(false, Ordering::Relaxed);

      let handle = res.map_err(|err| format!("{:#}", err))?;

      // collect all logs
      let logs: Vec<String> = self.logs_receiver.try_iter().collect();

      match handle {
          Ok(mut result) => {
              result.logs = logs;
              Ok(result)
          }
          Err(err) => Err(err),
      }
  }
}