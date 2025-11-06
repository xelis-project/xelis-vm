use std::{borrow::Cow, collections::HashMap};

use xelis_ast::{Expression, Signature, SignatureId};
use xelis_types::{IdentifierType, NoHashMap, Type};
use log::trace;

use crate::BuilderError;

use super::Mapper;

/// Function structure
/// It contains the name of the function and its parameters
#[derive(Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub on_type: Option<Type>,
    pub parameters: Vec<(&'a str, Type)>,
    pub require_instance: bool,
    pub return_type: Option<Type>,
    pub cost: u64,
}

/// FunctionMapper is used to store the mapping between function signatures and their identifiers
/// So we can reduce the memory footprint of the VM by using an incremented id
#[derive(Debug)]
pub struct FunctionMapper<'a> {
    mapper: Mapper<'a, Signature<'a>>,
    parent: Option<&'a FunctionMapper<'a>>,
    mappings: NoHashMap<Function<'a>>,
    closure_id: usize,
}

impl<'a> FunctionMapper<'a> {
    pub fn new() -> Self {
        Self {
            mapper: Mapper::new(),
            parent: None,
            mappings: NoHashMap::default(),
            closure_id: 0,
        }
    }

    // Create a new Function Mapper
    pub fn with_parent(parent: &'a Self) -> Self {
        Self {
            mapper: Mapper::with_parent(&parent.mapper),
            closure_id: parent.closure_id,
            parent: Some(parent),
            mappings: NoHashMap::default(),
        }
    }

    fn next_closure_id(&mut self) -> usize {
        let id = self.closure_id;
        self.closure_id += 1;
        id
    }

    // Get the identifier of a variable name
    pub fn get(&self, key: &Signature) -> Result<IdentifierType, BuilderError> {
        self.mapper.get(key)
    }

    // Register a function signature
    pub fn register(&mut self, name: &'a str, on_type: Option<Type>, require_instance: bool, parameters: Vec<(&'a str, Type)>, return_type: Option<Type>, cost: u64) -> Result<IdentifierType, BuilderError> {
        if on_type.as_ref().map_or(false, |v| v.is_closure()) {
            return Err(BuilderError::InvalidSignature)
        }

        let signature = Signature::new(SignatureId::Function(Cow::Borrowed(name)), on_type.clone().map(Cow::Owned));

        if self.mapper.has_variable(&signature) {
            trace!("{:?}", signature);
            return Err(BuilderError::SignatureAlreadyRegistered);
        }

        // Register the signature
        let id = self.mapper.register(signature)?;
        // Register the mappings
        self.mappings.insert(id.clone(), Function {
            name,
            on_type,
            parameters,
            require_instance,
            return_type,
            cost
        });

        Ok(id)
    }

    pub fn register_closure(&mut self) -> Result<IdentifierType, BuilderError> {
        let id = self.next_closure_id();
        let signature = Signature::new(SignatureId::Closure(id), None);

        let id = self.mapper.register(signature)?;
        Ok(id)
    }

    // Get a function mapp
    pub fn get_function(&self, id: &IdentifierType) -> Option<&Function<'a>> {
        if let Some(f) = self.parent.and_then(|parent| parent.get_function(id)) {
            return Some(f)
        }

        self.mappings.get(id)
    }

    pub fn get_by_signature(&self, name: &str, on_type: Option<&Type>) -> Result<IdentifierType, BuilderError> {
        self.mapper.get(&Signature::new(SignatureId::Function(Cow::Borrowed(name)), on_type.map(Cow::Borrowed)))
    }

    // This look for the exact signature and for a signature on type T(0)
    pub fn get_by_compatible_signature<'b>(&'b self, name: &'b str, on_type: Option<&'b Type>, instance: bool) -> Option<&'b Function<'a>> {
        if let Ok(id) = self.get_by_signature(name, on_type) {
            return self.get_function(&id)
        }

        let signature_id = SignatureId::Function(Cow::Borrowed(name));
        // Lets find a compatible signature
        for (signature, id) in self.mapper.mappings.iter()
            .filter(|(s, _)|
                *s.get_id() == signature_id
            ) {
            trace!("checking {:?}", signature);
            let is_on_type = match (signature.get_on_type(), on_type) {
                (Some(s), Some(k)) => s.is_compatible_with(k),
                (None, None) => true,
                _ => false
            };

            if !is_on_type {
                continue;
            }

            let function = self.get_function(id)?;

            if function.require_instance != instance {
                trace!("invalid instance");
                continue;
            }

            return Some(function)
        }

        if let Some(parent) = self.parent {
            return parent.get_by_compatible_signature(name, on_type, instance);
        }

        None
    }

    pub fn has_compatible_params<'b>(
        &'b self,
        on_type: Option<&Type>,
        params: impl Iterator<Item = &'b Type>,
        detected_types: impl Iterator<Item = &'b Option<Type>>,
        expressions: &mut [Expression]
    ) -> bool {
        let mut updated_expressions = Vec::new();
        for (i, (expected_param_type, current_param_type)) in params.zip(detected_types).enumerate() {
            trace!("Checking parameter {} with types {:?} and {:?}", i, expected_param_type, current_param_type);
            let Some(current_param_type) = current_param_type else {
                // We don't know the type of the parameter, we can't cast it
                if !expected_param_type.allow_null() {
                    trace!("Parameter {} is not nullable", i);
                    return false;
                } else {
                    continue;
                }
            };

            if let Some(instance) = on_type.filter(|v| v.contains_sub_type() && expected_param_type.is_generic()) {
                if !expected_param_type.is_generic_compatible_with(instance, current_param_type) {
                    trace!("Parameter {} is not generic compatible with instance {} and {}", expected_param_type, instance, current_param_type);
                    return false;
                }
            }

            let mut inner_type = on_type
                .map(Type::get_inner_type)
                .filter(|t| expected_param_type.is_generic() && current_param_type.is_castable_to(t));

            if inner_type.is_none() && !expected_param_type.is_compatible_with(current_param_type) {
                // If our parameter is castable to the signature parameter, cast it
                if current_param_type.is_castable_to(expected_param_type) {
                    inner_type = Some(expected_param_type);
                } else if expected_param_type != current_param_type {
                    // If the parameter is optional and its the exact same type inside
                    // we allow to pass it
                    if let Type::Optional(inner) = expected_param_type {
                        if !inner.is_compatible_with(current_param_type) {
                            trace!("inner {} is not compatible with {}", inner, current_param_type);
                            return false;
                        }
                    } else if expected_param_type.map_generic_type(on_type) != *current_param_type {
                        trace!("not optional, invalid");
                        return false;
                    }
                }
            }

            // If cast is needed, cast it, if we fail, we continue to the next signature
            if let Some(a) = inner_type {
                // We can only cast hardcoded values
                if let Expression::Constant(value) = &expressions[i] {
                    let cloned = value.clone();
                    match cloned.checked_cast_to_primitive_type(a) {
                        Ok(v) => {
                            updated_expressions.push((i, Expression::Constant(v)));
                        },
                        Err(e) => {
                            trace!("Parameter {} failed to cast: {:?}", i, e);
                            return false;
                        }
                    };
                } else {
                    trace!("Parameter {} is not a constant", i);
                    return false;
                }
            }
        }

        for (i, expr) in updated_expressions {
            expressions[i] = expr;
        }

        true
    }

    // Only real functions are available
    // Closure are not selected
    pub fn get_compatible(&self, name: &str, on_type: Option<&Type>, instance: bool, types: &Vec<Option<Type>>, expressions: &mut [Expression]) -> Result<IdentifierType, BuilderError> {
        // First check if we have the exact signature  
        if let Ok(id) = self.get_by_signature(name, on_type) {
            trace!("found id {} from signature", id);
            let function = self.get_function(&id)
                .ok_or(BuilderError::MappingNotFound)?;

            if function.require_instance != instance {
                return Err(BuilderError::FunctionInstanceMismatch)
            }

            if self.has_compatible_params(on_type, function.parameters.iter().map(|(_, v)| v), types.iter(), expressions) {
                return Ok(id);
            }

            trace!("id {} not compatible!", id);
        }

        let signature_id = SignatureId::Function(Cow::Borrowed(name));
        // Lets find a compatible signature
        for (signature, id) in self.mapper.mappings.iter()
            .filter(|(s, _)|
                *s.get_id() == signature_id
            ) {
            trace!("checking {:?}", signature);
            let is_on_type = match (signature.get_on_type(), on_type) {
                (Some(s), Some(k)) => s.is_compatible_with(k),
                (None, None) => true,
                _ => false
            };

            if !is_on_type {
                trace!("not on same type");
                continue;
            }

            let function = self.get_function(id)
                .ok_or(BuilderError::MappingNotFound)?;

            if function.require_instance != instance {
                trace!("invalid instance");
                continue;
            }

            if self.has_compatible_params(on_type, function.parameters.iter().map(|(_, v)| v), types.iter(), expressions) {
                return Ok(id.clone());
            }
        }

        if let Some(parent) = self.parent {
            return parent.get_compatible(name, on_type, instance, types, expressions);
        }

        trace!("no function found for '{}' on {:?} (instance = {}) with {:?}", name, on_type, instance, types);

        Err(BuilderError::MappingNotFound)
    }

    // Find all functions declared for a specific type
    // Example: "hello world".to_uppercase()
    // Calling this function with the type String will return the to_uppercase function
    pub fn get_functions_for_type(&self, on_type: Option<&Type>) -> Vec<&Function<'a>> {
        let mut functions = Vec::new();
        if let Some(parent) = self.parent {
            functions.extend(parent.get_functions_for_type(on_type));
        }

        // We need to find all functions that are compatible with the provided type
        if let Some(on_type) = on_type {
            for (id, function) in self.mappings.iter() {
                if let Some(signature) = self.mapper.get_by_id(*id) {
                    if let Some(signature_on_type) = signature.get_on_type() {
                        if on_type.is_compatible_with(signature_on_type) {
                            functions.push(function);
                        }
                    }
                }
            }
        } else {
            functions.extend(self.mappings.values());
        }

        functions
    }

    // Find all functions declared grouped per type
    // Value contains also the syscall id
    pub fn get_declared_functions(&'a self) -> HashMap<Option<&'a Type>, Vec<(&'a Function<'a>, u16)>> {
        let mut functions = HashMap::new();
        if let Some(parent) = self.parent {
            functions.extend(parent.get_declared_functions());
        }

        for (id, function) in self.mappings.iter() {
            if let Some(signature) = self.mapper.get_by_id(*id) {
                let on_type = signature.get_on_type()
                    .map(|t| t.as_ref());
                functions.entry(on_type)
                    .or_insert_with(Vec::new)
                    .push((function, *id));
            }
        }

        functions
    }
}