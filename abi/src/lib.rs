use std::{
  collections::HashMap,
  sync::{
      atomic::{AtomicBool, Ordering},
      mpsc, Mutex,
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
  contract::{ChainState, DeterministicRandom, StorageWrapper},
  crypto::{elgamal::CompressedPublicKey, Hash},
  serializer::Serializer,
  utils::format_xelis,
};
use xelis_compiler::Compiler;
use xelis_lexer::Lexer;
use xelis_parser::Parser;
use xelis_types::{Type, Value};
use xelis_vm::VM;
use xelis_ast::*;

use xelis_parser::mapper::GlobalMapper;
use xelis_builder::Builder;

pub fn abi_from_silex(code: &str, path: &str) -> anyhow::Result<String> {
    let tokens = Lexer::new(code)
        .with_path(path.to_string())
        .into_iter()
        .collect::<Result<Vec<_>, _>>()?;

    let environment = EnvironmentBuilder::default();

    let parser = Parser::with(tokens.into_iter(), &environment);
    let (program, mapper) = parser.parse().map_err(|err| anyhow::anyhow!("{:#}", err))?;

    abi_from_parse(program, &mapper, &environment)
}

pub fn abi_from_parse(program: Program, mapper: &GlobalMapper, environment: &EnvironmentBuilder) -> anyhow::Result<String> {
    // Collect all the available entry functions
    let mut abi_functions: Vec<serde_json::Value> = Vec::new(); // Collect ABI data here
    for (i, func) in program.functions().iter().enumerate() {
        let env_offset = environment.get_functions().len() as u16;

        if func.is_entry() {
            let mapping = mapper
                .functions()
                .get_function(&(i as u16 + env_offset))
                .unwrap();

            let mut flattened_params = Vec::new(); // Flattened parameters will be stored here

            for (name, _type) in &mapping.parameters {
                match _type {
                    // Type::Struct(struct_type) => {
                    //     // Flatten the struct fields
                    //     let struct_fields: Vec<(String, Type)> = struct_type.fields().iter().enumerate().map(|(i, field_type)| {
                    //         (format!("field{}", i), field_type.clone())
                    //     }).collect();

                    //     let builder = mapper
                    //         .structs_in_namespace(path);

                    //     let struct_field_names: Vec<&str> = builder
                    //         .get_by_ref(&struct_type)?
                    //         .names()
                    //         .clone();

                    //     let prefix = if path.is_empty() {
                    //         "".to_string()
                    //     } else {
                    //         path.join("::") + "::"
                    //     };

                    //     for (field_index, field_type) in struct_fields.iter().enumerate() {
                    //         flattened_params.push(serde_json::json!({
                    //             "name": format!("{}.{}", name, struct_field_names[field_index]),
                    //             "type": field_type.1.to_string(),
                    //             "internal_struct": 
                    //                 format!("{}{}", prefix, builder.get_name_by_ref(&struct_type)?),  
                    //         }));
                    //     }
                    // },
                    Type::Enum(enum_type) => {
                        // Represent enums as uint8 in ABI and include metadata
                        flattened_params.push(serde_json::json!({
                            "name": name.to_string(),
                            "type": format!("uint8"),
                            "internalType": format!("Enum {}", enum_type.id())
                        }));
                    },
                    _ => {
                        // Add non-struct parameter as is
                        flattened_params.push(serde_json::json!({
                            "name": name.to_string(),
                            "type": _type.to_string(),
                        }));
                    }
                }
            }
            
            let abi_entry = serde_json::json!({
                "name": mapping.name.to_owned(),
                "type": "entry",
                "chunk_id": i as u16,
                "params": flattened_params,
                "outputs": func.return_type().clone().map(|rt| rt.to_string()).unwrap_or_else(|| "void".to_string()),
            });
            abi_functions.push(abi_entry);
        }
    }

    Ok(serde_json::to_string_pretty(&abi_functions)?)
}