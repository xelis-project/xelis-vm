// use storage::MockStorage;
use xelis_builder::EnvironmentBuilder;
// use xelis_common::{
//   block::{Block, BlockHeader, BlockVersion},
//   contract::{ChainState, DeterministicRandom, StorageWrapper},
//   crypto::{elgamal::CompressedPublicKey, Hash},
//   serializer::Serializer,
//   utils::format_xelis,
// };
use xelis_lexer::Lexer;
use xelis_parser::Parser;
use xelis_types::Type;
use xelis_ast::*;

use xelis_parser::mapper::GlobalMapper;
use xelis_builder::Builder;

pub fn abi_from_silex(code: &str) -> anyhow::Result<String> {
    let tokens = Lexer::new(code)
        .into_iter()
        .collect::<Result<Vec<_>, _>>()?;

    let environment = EnvironmentBuilder::default();

    let parser = Parser::with(tokens.into_iter(), &environment);
    let (program, mapper) = parser.parse().map_err(|err| anyhow::anyhow!("{:#}", err))?;

    abi_from_parse(program, &mapper, &environment)
}

pub fn abi_from_parse(program: Program, mapper: &GlobalMapper, environment: &EnvironmentBuilder) -> anyhow::Result<String> {
    // Collect all the available entry functions
    let mut abi_functions: Vec<serde_json::Value> = Vec::new();
    for (i, func) in program.functions().iter().enumerate() {
        let env_offset = environment.get_functions().len() as u16;

        if func.is_entry() {
            let function_builder = mapper.functions();
            let mapping = function_builder
                .get_function(&(i as u16 + env_offset))
                .unwrap();

            let mut flattened_params = Vec::new();

            for (name, _type) in &mapping.parameters {
                match _type {
                    Type::Struct(struct_type) => {
                        let struct_fields: Vec<(String, Type)> = struct_type.fields().iter().enumerate().map(|(i, field_type)| {
                            (format!("field{}", i), field_type.clone())
                        }).collect();

                        let builder = mapper
                            .structs();

                        let struct_field_names: Vec<&str> = builder
                            .get_by_ref(&struct_type)?
                            .names()
                            .clone();


                        let name_info = builder.get_name_by_ref(&struct_type)?;
                        let namespace = &name_info.1;
                        let prefix = if namespace.is_empty() {
                            "".to_string()
                        } else {
                            namespace.join("::") + "::"
                        };

                        let mut struct_data = Vec::new();
                        for (field_index, field_type) in struct_fields.iter().enumerate() {
                            struct_data.push(serde_json::json!({
                                "name": struct_field_names[field_index],
                                "type": field_type.1.to_string(),
                            }));
                        }

                        flattened_params.push(
                            serde_json::json!({
                                "name": name,
                                "type": "struct",
                                "internalType": format!("{}{}", prefix, name_info.0),
                                "fields": struct_data
                            })
                        )
                    },
                    Type::Enum(enum_type) => {

                        let builder = mapper
                            .enums();

                        let name_info = builder.get_name_by_ref(&enum_type)?;
                        let namespace = &name_info.1;
                        let prefix = if namespace.is_empty() {
                            "".to_string()
                        } else {
                            namespace.join("::") + "::"
                        };

                        flattened_params.push(serde_json::json!({
                            "name": name.to_string(),
                            "type": format!("enum"),
                            "internalType": format!("{}{}", prefix, name_info.0)
                        }));
                    },
                    _ => {
                        flattened_params.push(serde_json::json!({
                            "name": name.to_string(),
                            "type": _type.to_string(),
                        }));
                    }
                }
            }

            let namespace = &mapping.namespace;
            let is_root = namespace.is_empty() || (
                namespace.len() == 1 && namespace[0] == ""
            );

            let prefix = if is_root {
                "".to_string()
            } else {
                namespace.join("::") + "::"
            };

            let abi_entry = serde_json::json!({
                "name": format!("{}{}", prefix, mapping.name.to_owned()),
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