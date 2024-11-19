use xelis_ast::{Expression, Signature};
use xelis_types::{IdentifierType, Type};

use crate::BuilderError;

use super::Mapper;

pub type FunctionMapper<'a> = Mapper<'a, Signature>;

impl<'a> FunctionMapper<'a> {
    pub fn get_compatible(&self, key: Signature, expressions: &mut [Expression]) -> Result<IdentifierType, BuilderError> {
        // First check if we have the exact signature
        if let Ok(id) = self.get(&key) {
            return Ok(id);
        }

        // Lets find a compatible signature
        'main: for (signature, id) in self.mappings.iter().filter(|(s, _)| s.get_name() == key.get_name() && s.get_parameters().len() == key.get_parameters().len()) {            
            let on_type = match (signature.get_on_type(), key.get_on_type()) {
                (Some(s), Some(k)) => s.is_compatible_with(k),
                (None, None) => true,
                _ => false
            };

            if !on_type {
                continue;
            }

            let mut updated_expressions = Vec::new();
            for (i, (a, b)) in signature.get_parameters().iter().zip(key.get_parameters()).enumerate() {
                let mut cast_to_type = key.get_on_type()
                    .as_ref()
                    .map(Type::get_inner_type)
                    .filter(|t| b.is_castable_to(t));

                if cast_to_type.is_none() && !a.is_compatible_with(b) {
                    // If our parameter is castable to the signature parameter, cast it
                    if b.is_castable_to(a) {
                        cast_to_type = Some(a);
                    } else {
                        continue 'main;
                    }
                }

                // If cast is needed, cast it, if we fail, we continue to the next signature
                if let Some(a) = cast_to_type {
                    // We can only cast hardcoded values
                    if let Expression::Value(value) = &expressions[i] {
                        let cloned = value.clone();
                        let v = cloned.checked_cast_to_primitive_type(a)?;
                        updated_expressions.push(Expression::Value(v));
                        continue;
                    } else {
                        continue 'main;
                    }
                }
            }

            for (i, expr) in updated_expressions.into_iter().enumerate() {
                expressions[i] = expr;
            }

            return Ok(id.clone());
        }

        if let Some(parent) = self.parent {
            return parent.get_compatible(key, expressions);
        }

        Err(BuilderError::MappingNotFound)
    }
}