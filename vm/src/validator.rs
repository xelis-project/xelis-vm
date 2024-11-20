use xelis_environment::Environment;
use xelis_types::{EnumType, EnumVariant, StructType, Type, Value};
use xelis_bytecode::Module;

pub enum ValidatorError<'a> {
    TooManyConstants,
    TooManyChunks,
    TooManyTypes,

    TooManyStructs,
    TooManyStructFields(&'a StructType),
    RecursiveStruct(&'a StructType),

    TooManyEnums,
    TooManyEnumsVariants,
    TooManyEnumsVariantsFields(&'a EnumVariant),
    RecursiveEnum(&'a EnumType),

    IncorrectFields,
    IncorrectVariant,
}

pub struct ModuleValidator<'a> {
    module: &'a Module,
    environment: &'a Environment,
}

impl<'a> ModuleValidator<'a> {
    pub fn new(module: &'a Module, environment: &'a Environment) -> Self {
        Self { module, environment }
    }

    fn verify_constants(&self) -> Result<(), ValidatorError<'a>> {
        for c in self.module.constants() {
            match c {
                Value::Struct(fields, t) => {
                    if fields.len() != t.fields().len() {
                        return Err(ValidatorError::IncorrectFields);
                    }
        
                    if !self.module.structs().contains(t) && !self.environment.get_structures().contains(t) {
                        return Err(ValidatorError::TooManyStructs);
                    }
                },
                Value::Enum(fields, t) => {
                    let variant = t.enum_type()
                        .variants()
                        .get(t.variant_id() as usize)
                        .ok_or(ValidatorError::IncorrectVariant)?;

                    if fields.len() != variant.fields().len() {
                        return Err(ValidatorError::IncorrectFields);
                    }

                    if !self.module.enums().contains(t.enum_type()) && !self.environment.get_enums().contains(t.enum_type()) {
                        return Err(ValidatorError::TooManyEnums);
                    }
                },
                Value::Array(elements) => {
                    if elements.len() > u32::MAX as usize {
                        return Err(ValidatorError::TooManyConstants);
                    }
                },
                Value::Map(map) => {
                    if map.len() > u32::MAX as usize {
                        return Err(ValidatorError::TooManyConstants);
                    }
                },
                _ => {}
            };
        }

        Ok(())
    }

    // Verify all the declared chunks in the module
    fn verify_chunks(&self) -> Result<(), ValidatorError<'a>> {
        for _ in self.module.chunks() {

        }

        Ok(())
    }

    // Verify the enums integrity
    fn verify_enums(&self) -> Result<(), ValidatorError<'a>> {
        // No need to check the ids, they are already checked by the Module
        for e in self.module.enums() {
            // Verify the variants
            if e.variants().len() > u8::MAX as usize {
                return Err(ValidatorError::TooManyEnumsVariants);
            }

            for variant in e.variants() {
                if variant.fields().len() > u8::MAX as usize {
                    return Err(ValidatorError::TooManyEnumsVariantsFields(variant));
                }

                for field in variant.fields() {
                    if let Type::Enum(inner) = field {
                        if e == inner {
                            return Err(ValidatorError::RecursiveEnum(e));
                        }
                    }
                }
            }
        }

        Ok(())
    }

    // Verify the structs integrity
    fn verify_structs(&self) -> Result<(), ValidatorError<'a>> {
        // No need to check the ids, they are already checked by the Module
        for s in self.module.structs() {
            if s.fields().len() > u8::MAX as usize {
                return Err(ValidatorError::TooManyStructFields(s));
            }

            for field in s.fields() {
                if let Type::Struct(inner) = field {
                    if s == inner {
                        return Err(ValidatorError::RecursiveStruct(s));
                    }
                }
            }
        }
        Ok(())
    }

    // Verify the module integrity and return an error if it's invalid
    pub fn verify(&self) -> Result<(), ValidatorError<'a>> {
        let max = u16::MAX as usize;

        // We support max of 65535 constants, chunks, structs and enums
        if self.module.constants().len() >= max {
            return Err(ValidatorError::TooManyConstants);
        }

        if self.module.chunks().len() >= max {
            return Err(ValidatorError::TooManyChunks);
        }

        if self.module.structs().len() >= max {
            return Err(ValidatorError::TooManyStructs);
        }

        if self.module.enums().len() >= max {
            return Err(ValidatorError::TooManyEnums);
        }

        // Maximum of 65535 types
        let max_types = self.module.structs().len() + self.module.enums().len();
        if max_types >= max {
            return Err(ValidatorError::TooManyTypes);
        }

        self.verify_enums()?;
        self.verify_structs()?;
        self.verify_constants()?;
        self.verify_chunks()?;

        Ok(())
    }
}