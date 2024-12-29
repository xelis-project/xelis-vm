use std::{collections::HashMap, sync::{
  atomic::{AtomicBool, Ordering},
  mpsc, Mutex,
}};

use xelis_builder::EnvironmentBuilder;
use xelis_bytecode::Module;
use xelis_common::{
    block::{Block, BlockHeader, BlockVersion},
    contract::{build_environment, ChainState, DeterministicRandom, StorageWrapper},
    crypto::{elgamal::CompressedPublicKey, Hash},
    serializer::Serializer,
    utils::format_xelis
};
use xelis_compiler::Compiler;
use xelis_lexer::Lexer;
use xelis_parser::Parser;
use xelis_types::{Type, Value};
use xelis_vm::VM;

pub struct Silex {
    environment: EnvironmentBuilder<'static>,
    logs_receiver: mpsc::Receiver<String>,
    is_running: AtomicBool,
}

pub struct Program {
    module: Module,
    entries: Vec<Entry>,
}

impl Program {
  // Get the entries of the program
  pub fn entries(&self) -> Vec<Entry> {
    self.entries.clone()
  }

  pub fn to_bytes(&self) -> Vec<u8> {
    self.module.to_bytes()
  }

  pub fn to_hex(&self) -> String {
    self.module.to_hex()
  }
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
