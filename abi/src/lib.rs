use xelis_builder::EnvironmentBuilder;
use xelis_lexer::Lexer;
use xelis_parser::Parser;
use xelis_types::Type;
use xelis_ast::*;

use xelis_parser::mapper::GlobalMapper;
use xelis_builder::Builder;

#[warn(unused_extern_crates)]

const ABI_VERSION: &str = "1.1.0";

pub fn abi_from_silex<M>(code: &str, env: EnvironmentBuilder<'_, M>) -> anyhow::Result<String> {
    let tokens = Lexer::new(code)
        .into_iter()
        .collect::<Result<Vec<_>, _>>()?;

    let parser = Parser::with(tokens.into_iter(), &env);
    let (program, mapper) = parser.parse().map_err(|err| anyhow::anyhow!("{:#}", err))?;

    abi_from_parse(&program, &mapper, &env)
}

fn register_type(
    _type: &Type,
    mapper: &GlobalMapper,
    internal_types: &mut Vec<serde_json::Value>,
    seen_types: &mut std::collections::HashSet<String>
) -> anyhow::Result<()> {
    match _type {
        // Recursively handle containers first
        Type::Array(inner) => {
            register_type(inner.as_ref(), mapper, internal_types, seen_types)?;
        },
        Type::Optional(inner) => {
            register_type(inner.as_ref(), mapper, internal_types, seen_types)?;
        },
        Type::Range(inner) => {
            register_type(inner.as_ref(), mapper, internal_types, seen_types)?;
        },
        Type::Map(key, value) => {
            register_type(key.as_ref(), mapper, internal_types, seen_types)?;
            register_type(value.as_ref(), mapper, internal_types, seen_types)?;
        },
        Type::Tuples(types) => {
            for ty in types {
                register_type(ty, mapper, internal_types, seen_types)?;
            }
        },
        
        // Then handle struct/enum (existing code)
        Type::Struct(struct_type) => {
            let builder = mapper.structs();
            let name_info = builder.get_name_by_ref(&struct_type)?;

            if seen_types.insert(name_info.to_string()) {
                // Recursively register nested types in fields
                for (_field_name, field_type) in struct_type.fields() {
                    register_type(field_type, mapper, internal_types, seen_types)?;
                }

                let struct_field_names: Vec<&str> = builder
                    .get_by_ref(&struct_type)?
                    .names()
                    .collect();

                let fields: Vec<serde_json::Value> = struct_type
                    .fields()
                    .iter()
                    .enumerate()
                    .map(|(field_index, (_field_name, field_type))| {
                        serde_json::json!({
                            "name": struct_field_names[field_index],
                            "type": field_type.to_string()
                        })
                    })
                    .collect();

                internal_types.push(serde_json::json!({
                    "name": name_info,
                    "kind": "struct",
                    "fields": fields
                }));
            }
        },
        Type::Enum(enum_type) => {
            let builder = mapper.enums();
            let name_info = builder.get_name_by_ref(&enum_type)?;

            if seen_types.insert(name_info.to_string()) {
                // Recursively register nested types in variant fields
                for (_variant_name, variant_fields) in enum_type.variants() {
                    for (_field_name, field_type) in variant_fields.fields() {
                        register_type(field_type, mapper, internal_types, seen_types)?;
                    }
                }

                let variants: Vec<serde_json::Value> = enum_type
                    .variants()
                    .iter()
                    .map(|(variant_name, variant_fields)| {
                        let fields: Vec<serde_json::Value> = variant_fields
                            .fields()
                            .iter()
                            .map(|(field_name, field_type)| {
                                serde_json::json!({
                                    "name": field_name.as_ref(),
                                    "type": field_type.to_string()
                                })
                            })
                            .collect();

                        serde_json::json!({
                            "name": variant_name.as_ref(),
                            "fields": fields
                        })
                    })
                    .collect();

                internal_types.push(serde_json::json!({
                    "name": name_info,
                    "kind": "enum",
                    "variants": variants
                }));
            }
        },
        _ => {
            // Primitives, Any, T, Opaque, Function, Bytes - nothing to register
        }
    }
    
    Ok(())
}

pub fn abi_from_parse<M>(program: &Program, mapper: &GlobalMapper, environment: &EnvironmentBuilder<M>) -> anyhow::Result<String> {
    let mut abi_functions: Vec<serde_json::Value> = Vec::new();
    let mut internal_types: Vec<serde_json::Value> = Vec::new();
    let mut seen_types: std::collections::HashSet<String> = std::collections::HashSet::new();

    for (i, func) in program.functions().iter().enumerate() {
        let env_offset = environment.get_functions().len() as u16;

        if func.is_entry() {
            let function_builder = mapper.functions();
            let mapping = function_builder
                .get_function(&(i as u16 + env_offset))
                .unwrap();

            let mut flattened_params = Vec::new();

            for (name, _type) in &mapping.parameters {
                register_type(_type, mapper, &mut internal_types, &mut seen_types)?;
                
                match _type {
                    Type::Struct(struct_type) => {
                        let builder = mapper.structs();
                        let name_info = builder.get_name_by_ref(&struct_type)?;
                        
                        flattened_params.push(serde_json::json!({
                            "name": name,
                            "type": "struct",
                            "internal_type": name_info
                        }));
                    },
                    Type::Enum(enum_type) => {
                        let builder = mapper.enums();
                        let name_info = builder.get_name_by_ref(&enum_type)?;
                        
                        flattened_params.push(serde_json::json!({
                            "name": name.to_string(),
                            "type": "enum",
                            "internal_type": name_info
                        }));
                    },
                    _ => {
                        // Primitives, arrays, maps, etc.
                        flattened_params.push(serde_json::json!({
                            "name": name.to_string(),
                            "type": _type.to_string(),
                        }));
                    }
                }
            }

            if let Some(return_type) = func.return_type() {
                register_type(return_type, mapper, &mut internal_types, &mut seen_types)?;
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

    let abi_root = serde_json::json!({
        "version": ABI_VERSION,
        "internal_types": internal_types,
        "data": abi_functions
    });

    Ok(serde_json::to_string_pretty(&abi_root)?)
}

// TODO: re-add import support and switch to something like below
// Below is the original version from v1 of ABI/Imports. 

// pub fn abi_from_parse(program: Program, mapper: &GlobalMapper, environment: &EnvironmentBuilder) -> anyhow::Result<String> {
//     // Collect all the available entry functions
//     let mut abi_functions: Vec<serde_json::Value> = Vec::new();
//     for (i, func) in program.functions().iter().enumerate() {
//         let env_offset = environment.get_functions().len() as u16;

//         if func.is_entry() {
//             let function_builder = mapper.functions();
//             let mapping = function_builder
//                 .get_function(&(i as u16 + env_offset))
//                 .unwrap();

//             let mut flattened_params = Vec::new();

//             for (name, _type) in &mapping.parameters {
//                 match _type {
//                     Type::Struct(struct_type) => {
//                         let struct_fields: Vec<(String, Type)> = struct_type.fields().iter().enumerate().map(|(i, field_type)| {
//                             (format!("field{}", i), field_type.clone())
//                         }).collect();

//                         let builder = mapper
//                             .structs();

//                         let struct_field_names: Vec<&str> = builder
//                             .get_by_ref(&struct_type)?
//                             .names()
//                             .clone();


//                         let name_info = builder.get_name_by_ref(&struct_type)?;
//                         let namespace = &name_info.1;
//                         let prefix = if namespace.is_empty() {
//                             "".to_string()
//                         } else {
//                             namespace.join("::") + "::"
//                         };

//                         let mut struct_data = Vec::new();
//                         for (field_index, field_type) in struct_fields.iter().enumerate() {
//                             struct_data.push(serde_json::json!({
//                                 "name": struct_field_names[field_index],
//                                 "type": field_type.1.to_string(),
//                             }));
//                         }

//                         flattened_params.push(
//                             serde_json::json!({
//                                 "name": name,
//                                 "type": "struct",
//                                 "internal_type": format!("{}{}", prefix, name_info.0),
//                                 "fields": struct_data
//                             })
//                         )
//                     },
//                     Type::Enum(enum_type) => {

//                         let builder = mapper
//                             .enums();

//                         let name_info = builder.get_name_by_ref(&enum_type)?;
//                         let namespace = &name_info.1;
//                         let prefix = if namespace.is_empty() {
//                             "".to_string()
//                         } else {
//                             namespace.join("::") + "::"
//                         };

//                         flattened_params.push(serde_json::json!({
//                             "name": name.to_string(),
//                             "type": format!("enum"),
//                             "internal_type": format!("{}{}", prefix, name_info.0)
//                         }));
//                     },
//                     _ => {
//                         flattened_params.push(serde_json::json!({
//                             "name": name.to_string(),
//                             "type": _type.to_string(),
//                         }));
//                     }
//                 }
//             }

//             let namespace = &mapping.namespace;
//             let is_root = namespace.is_empty() || (
//                 namespace.len() == 1 && namespace[0] == ""
//             );

//             let prefix = if is_root {
//                 "".to_string()
//             } else {
//                 namespace.join("::") + "::"
//             };

//             let abi_entry = serde_json::json!({
//                 "name": format!("{}{}", prefix, mapping.name.to_owned()),
//                 "type": "entry",
//                 "chunk_id": i as u16,
//                 "params": flattened_params,
//                 "outputs": func.return_type().clone().map(|rt| rt.to_string()).unwrap_or_else(|| "void".to_string()),
//             });
//             abi_functions.push(abi_entry);
//         }
//     }

//     Ok(serde_json::to_string_pretty(&abi_functions)?)
// }

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    use xelis_builder::{EnvironmentBuilder};
    use xelis_types::{Opaque, traits::{
      DynType, DynEq, DynHash, JSONHelper, Serializable
    }};

    macro_rules! impl_dummy_opaque {
        ($name:ident) => {
            #[derive(Debug)]
            pub struct $name;

            impl DynType for $name {
                fn get_type_name(&self) -> &'static str {
                    stringify!($name)
                }
                fn get_type(&self) -> std::any::TypeId {
                    std::any::TypeId::of::<Self>()
                }
            }

            impl DynEq for $name {
                fn as_eq(&self) -> &(dyn DynEq + 'static) {
                    self
                }
                fn is_equal(&self, _other: &(dyn DynEq + 'static)) -> bool {
                    false
                }
            }

            impl DynHash for $name {
                fn dyn_hash(&self, _: &mut dyn std::hash::Hasher) {}
            }

            impl JSONHelper for $name {
                fn serialize_json(&self) -> anyhow::Result<serde_json::Value> {
                    Ok(serde_json::json!("dummy"))
                }

                fn is_json_supported(&self) -> bool {
                    true
                }
            }

            impl Serializable for $name {
                fn get_size(&self) -> usize {
                    8
                }

                fn serialize(&self, _: &mut Vec<u8>) -> usize {
                    0
                }

                fn is_serializable(&self) -> bool {
                    false
                }
            }

            impl Opaque for $name {
                fn clone_box(&self) -> Box<dyn Opaque> {
                    Box::new($name)
                }
            }
        };
    }

    // Dummy type definitions to register opaque types
    impl_dummy_opaque!(Address);

    #[test]
    fn test_abi_from_example_slx() {
        let slx_path = "./silex/example.slx";
        let abi_path = "./silex/example.abi.json";

        let code = fs::read_to_string(slx_path)
            .expect("Failed to read example.slx");
        let expected_abi = fs::read_to_string(abi_path)
            .expect("Failed to read example.abi.json");

        let mut env: EnvironmentBuilder<'_, ()> = EnvironmentBuilder::default();
        let _ = env.register_opaque::<Address>("Address", true);

        match abi_from_silex::<()>(&code, env) {
            Ok(generated_abi) => {
                let generated_json: serde_json::Value = serde_json::from_str(&generated_abi).unwrap();
                let expected_json: serde_json::Value = serde_json::from_str(&expected_abi).unwrap();
                assert_eq!(
                    generated_json, expected_json,
                    "Generated ABI does not match expected ABI JSON"
                );
            }
            Err(err) => panic!("Failed to generate ABI: {:?}", err),
        }
    }
}