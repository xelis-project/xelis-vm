use matriochka::{Error, OptionExt, ResultExt};
use serde_json::json;
use silex_builder::EnvironmentBuilder;
use silex_lexer::Lexer;
use silex_parser::Parser;
use silex_types::Type;
use silex_ast::*;

use silex_parser::mapper::GlobalMapper;
use silex_builder::Builder;

#[warn(unused_extern_crates)]

const ABI_VERSION: &str = "1.1.1";

pub fn abi_from_silex<M>(code: &str, env: EnvironmentBuilder<'_, M>) -> Result<String, Error> {
    let tokens = Lexer::new(code)
        .into_iter()
        .collect::<Result<Vec<_>, _>>()
        .context("tokenizing Silex source for ABI generation")?;

    let parser = Parser::with(tokens.into_iter(), &env);
    let (program, mapper) = parser.parse()
        // Parser errors borrow the source and cannot be stored in an owned error.
        .map_err(|err| Error::new(std::io::Error::other(err.to_string())))
        .context("parsing Silex source for ABI generation")?;

    abi_from_parse(&program, &mapper, &env)
}

fn register_type(
    _type: &Type,
    mapper: &GlobalMapper,
    internal_types: &mut Vec<serde_json::Value>,
    seen_types: &mut std::collections::HashSet<String>
) -> Result<(), Error> {
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
            let name_info = builder.get_name_by_ref(&struct_type)
                .with_context(|| format!("resolving ABI struct {struct_type}"))?;

            if seen_types.insert(name_info.to_string()) {
                // Recursively register nested types in fields
                for (field_name, field_type) in struct_type.fields() {
                    register_type(field_type, mapper, internal_types, seen_types)
                        .with_context(|| format!("registering ABI field {name_info}.{field_name}"))?;
                }

                let struct_field_names: Vec<&str> = builder
                    .get_by_ref(&struct_type)
                    .with_context(|| format!("resolving ABI fields for struct {name_info}"))?
                    .names()
                    .collect();

                let fields: Vec<serde_json::Value> = struct_type
                    .fields()
                    .iter()
                    .enumerate()
                    .map(|(field_index, (_field_name, field_type))| {
                        json!({
                            "name": struct_field_names[field_index],
                            "type": field_type.to_string()
                        })
                    })
                    .collect();

                internal_types.push(json!({
                    "name": name_info,
                    "kind": "struct",
                    "fields": fields
                }));
            }
        },
        Type::Enum(enum_type) => {
            let builder = mapper.enums();
            let name_info = builder.get_name_by_ref(&enum_type)
                .with_context(|| format!("resolving ABI enum {enum_type}"))?;

            if seen_types.insert(name_info.to_string()) {
                // Recursively register nested types in variant fields
                for (variant_name, variant_fields) in enum_type.variants() {
                    for (field_name, field_type) in variant_fields.fields() {
                        register_type(field_type, mapper, internal_types, seen_types)
                            .with_context(|| format!("registering ABI field {name_info}::{variant_name}.{field_name}"))?;
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
                                json!({
                                    "name": field_name.as_ref(),
                                    "type": field_type.to_string()
                                })
                            })
                            .collect();

                        json!({
                            "name": variant_name.as_ref(),
                            "fields": fields
                        })
                    })
                    .collect();

                internal_types.push(json!({
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

pub fn abi_from_parse<M>(program: &Program, mapper: &GlobalMapper, environment: &EnvironmentBuilder<M>) -> Result<String, Error> {
    let mut abi_functions: Vec<serde_json::Value> = Vec::new();
    let mut internal_types: Vec<serde_json::Value> = Vec::new();
    let mut seen_types: std::collections::HashSet<String> = std::collections::HashSet::new();

    for (i, func) in program.functions().iter().enumerate() {
        let env_offset = environment.get_functions().len() as u16;

        if func.is_entry() {
            let function_builder = mapper.functions();
            let mapping = function_builder
                .get_function(&(i as u16 + env_offset))
                .with_context(|| format!("resolving ABI entry function at index {i}"))?;

            let mut flattened_params = Vec::new();

            for (name, _type) in &mapping.parameters {
                register_type(_type, mapper, &mut internal_types, &mut seen_types)
                    .with_context(|| format!("registering ABI parameter {name} of entry {}", mapping.name))?;
                
                match _type {
                    Type::Struct(struct_type) => {
                        let builder = mapper.structs();
                        let name_info = builder.get_name_by_ref(&struct_type)
                            .with_context(|| format!("resolving ABI struct for parameter {name} of entry {}", mapping.name))?;
                        
                        flattened_params.push(json!({
                            "name": name,
                            "type": "struct",
                            "internal_type": name_info
                        }));
                    },
                    Type::Enum(enum_type) => {
                        let builder = mapper.enums();
                        let name_info = builder.get_name_by_ref(&enum_type)
                            .with_context(|| format!("resolving ABI enum for parameter {name} of entry {}", mapping.name))?;
                        
                        flattened_params.push(json!({
                            "name": name.to_string(),
                            "type": "enum",
                            "internal_type": name_info
                        }));
                    },
                    _ => {
                        // Primitives, arrays, maps, etc.
                        flattened_params.push(json!({
                            "name": name.to_string(),
                            "type": _type.to_string(),
                        }));
                    }
                }
            }

            if let Some(return_type) = func.return_type() {
                register_type(return_type, mapper, &mut internal_types, &mut seen_types)
                    .with_context(|| format!("registering ABI return type of entry {}", mapping.name))?;
            }

            let abi_entry = json!({
                "name": mapping.name.to_owned(),
                "type": "entry",
                "entry_id": i as u16,
                "params": flattened_params,
                "outputs": func.return_type().clone().map(|rt| rt.to_string()).unwrap_or_else(|| "void".to_string()),
            });
            abi_functions.push(abi_entry);
        }
    }

    let abi_root = json!({
        "version": ABI_VERSION,
        "internal_types": internal_types,
        "data": abi_functions
    });

    serde_json::to_string_pretty(&abi_root)
        .context("serializing ABI to JSON")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    use silex_builder::{EnvironmentBuilder};
    use silex_types::{Opaque, StructType, traits::{
      DynType, JSONHelper
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

            impl JSONHelper for $name {
                fn serialize_json(&self) -> Result<serde_json::Value, Error> {
                    Ok(json!("dummy"))
                }

                fn is_json_supported(&self) -> bool {
                    true
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
    fn abi_errors_include_the_source_stage() {
        let error = abi_from_silex::<()>("@", EnvironmentBuilder::default()).unwrap_err();
        assert!(error.to_string().contains("tokenizing"));
        assert!(error.downcast_ref::<silex_lexer::LexerError>().is_some());

        let error = abi_from_silex::<()>("entry", EnvironmentBuilder::default()).unwrap_err();
        assert!(error.to_string().contains("parsing"));
        assert!(format!("{error:#}").contains("error at line"));
    }

    #[test]
    fn missing_entry_mapping_returns_a_contextual_error() {
        let mut program = Program::new();
        program.add_function(FunctionType::Entry(EntryFunction::new(vec![], vec![], 0)));

        let error = abi_from_parse::<()>(
            &program,
            &GlobalMapper::new(),
            &EnvironmentBuilder::default(),
        ).unwrap_err();
        assert!(error.to_string().contains("entry function at index 0"));
    }

    #[test]
    fn missing_abi_types_preserve_the_builder_error() {
        let error = register_type(
            &Type::Struct(StructType::new(0, "Missing", vec![])),
            &GlobalMapper::new(),
            &mut Vec::new(),
            &mut std::collections::HashSet::new(),
        ).unwrap_err();

        assert!(error.to_string().contains("Missing"));
        assert!(matches!(
            error.downcast_ref::<silex_builder::BuilderError>(),
            Some(silex_builder::BuilderError::TypeNotFound),
        ));
    }

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