use thiserror::Error;
use xelis_environment::Environment;
use xelis_types::{EnumType, EnumVariant, Primitive, StructType, ValueCell, ValueError};
use xelis_bytecode::{Module, OpCode};

use crate::ChunkReader;

#[derive(Debug, Error)]
pub enum ValidatorError<'a> {
    #[error("too much memory usage in constants")]
    TooMuchMemoryUsage,
    #[error("too many constants")]
    TooManyConstants,
    #[error("constant too deep")]
    ConstantTooDeep,
    #[error("too many chunks")]
    TooManyChunks,
    #[error("too many types")]
    TooManyTypes,
    #[error("invalid opaque")]
    InvalidOpaque,

    #[error("too many structs")]
    TooManyStructs,
    #[error("too many struct fields")]
    TooManyStructFields(&'a StructType),
    #[error("recursive struct")]
    RecursiveStruct(&'a StructType),

    #[error("too many enums")]
    TooManyEnums,
    #[error("too many enums variants")]
    TooManyEnumsVariants,
    #[error("too many enums variants fields")]
    TooManyEnumsVariantsFields(&'a EnumVariant),
    #[error("recursive enum")]
    RecursiveEnum(&'a EnumType),

    #[error("incorrect fields")]
    IncorrectFields,
    #[error("incorrect variant")]
    IncorrectVariant,
    #[error("invalid op code")]
    InvalidOpCode,
    #[error("invalid opcode arguments")]
    InvalidOpCodeArguments,
    #[error("invalid range")]
    InvalidRange,
    #[error("invalid range type")]
    InvalidRangeType,
    #[error("reference not allowed")]
    ReferenceNotAllowed,
    #[error("map as key not allowed")]
    MapAsKeyNotAllowed,
    #[error("empty module")]
    EmptyModule,
    #[error("invalid entry id {0}")]
    InvalidEntryId(usize),
    #[error(transparent)]
    ValueError(#[from] ValueError),
    #[error("unknown struct")]
    UnknownStruct,
    #[error("unknown enum")]
    UnknownEnum,
    #[error("string is too big")]
    StringTooBig,
}

pub struct ModuleValidator<'a> {
    module: &'a Module,
    environment: &'a Environment,
    constant_max_depth: usize,
    constant_max_memory: usize
}

impl<'a> ModuleValidator<'a> {
    pub fn new(module: &'a Module, environment: &'a Environment) -> Self {
        Self { module, environment, constant_max_depth: 16, constant_max_memory: 1024 }
    }

    // Verify a constant and return the memory usage
    pub fn verify_constant(&self, constant: &ValueCell) -> Result<usize, ValidatorError<'a>> {
        let mut stack = vec![(constant, 0)];
        let mut memory_usage = 0;

        while let Some((value, depth)) = stack.pop() {
            if depth > self.constant_max_depth {
                return Err(ValidatorError::ConstantTooDeep);
            }

            // Increase by one for the byte type of the value
            memory_usage += 1;
            if memory_usage > self.constant_max_memory {
                return Err(ValidatorError::TooMuchMemoryUsage);
            }

            match value {
                ValueCell::Array(elements) => {
                    if elements.len() > u32::MAX as usize {
                        return Err(ValidatorError::TooManyConstants);
                    }

                    for element in elements {
                        stack.push((element, depth + 1));
                    }
                    memory_usage += 4;
                },
                ValueCell::Bytes(values) => {
                    if values.len() > u32::MAX as usize {
                        return Err(ValidatorError::TooManyConstants);
                    }

                    memory_usage += values.len();
                }
                ValueCell::Map(map) => {
                    if map.len() > u32::MAX as usize {
                        return Err(ValidatorError::TooManyConstants);
                    }

                    for (key, value) in map.iter() {
                        if key.is_map() {
                            return Err(ValidatorError::MapAsKeyNotAllowed);
                        }

                        stack.push((key, depth + 1));
                        stack.push((value, depth + 1));
                    }
                    memory_usage += 16;
                },
                ValueCell::Default(v) => match v {
                    Primitive::Range(range) => {
                        if !range.0.is_number() || !range.1.is_number() {
                            return Err(ValidatorError::InvalidRange);
                        }
    
                        let left_type = range.0.get_type()?;
                        if left_type != range.1.get_type()? {
                            return Err(ValidatorError::InvalidRange);
                        }

                        memory_usage += 8;
                    },
                    Primitive::Null => memory_usage += 1,
                    Primitive::Boolean(_) => memory_usage += 1,
                    Primitive::String(str) => {
                        if str.len() > u32::MAX as usize {
                            return Err(ValidatorError::StringTooBig);
                        }

                        memory_usage += 8 + str.len();
                    },
                    Primitive::U8(_) => memory_usage += 1,
                    Primitive::U16(_) => memory_usage += 2,
                    Primitive::U32(_) => memory_usage += 4,
                    Primitive::U64(_) => memory_usage += 8,
                    Primitive::U128(_) => memory_usage += 16,
                    Primitive::U256(_) => memory_usage += 32,
                    Primitive::Opaque(opaque) => {
                        if !self.environment.get_opaques()
                            .contains(&opaque.get_type_id()) {
                            return Err(ValidatorError::InvalidOpaque);
                        }

                        memory_usage += opaque.get_size();
                    }
                }
            }
        }

        Ok(memory_usage)
    }

    // Verify all the declared constants in the module
    pub fn verify_constants<'b, I: Iterator<Item = &'b ValueCell>>(&self, constants: I) -> Result<(), ValidatorError<'a>> {
        let mut memory_usage = 0;
        for c in constants {
            memory_usage += self.verify_constant(&c)?;

            if memory_usage > self.constant_max_memory {
                return Err(ValidatorError::TooManyConstants);
            }
        }

        Ok(())
    }

    // Verify all the declared chunks in the module
    // We verify that the opcodes are valid and that the count of arguments are correct
    fn verify_chunks(&self) -> Result<(), ValidatorError<'a>> {
        let len = self.module.chunks().len();
        if len == 0 {
            return Err(ValidatorError::EmptyModule);
        }

        // Verify all the chunks
        for chunk in self.module.chunks() {
            let mut reader = ChunkReader::new(chunk);
            while let Some(instruction) = reader.next_u8() {
                let op = OpCode::from_byte(instruction)
                    .ok_or(ValidatorError::InvalidOpCode)?;

                reader.advance(op.arguments_bytes())
                    .map_err(|_| ValidatorError::InvalidOpCodeArguments)?;
            }
        }

        // Verify that the entry ids are valid
        for entry_id in self.module.chunks_entry_ids() {
            if *entry_id >= len {
                return Err(ValidatorError::InvalidEntryId(*entry_id));
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

        self.verify_constants(self.module.constants().iter().map(|v| &v.0))?;
        self.verify_chunks()?;

        Ok(())
    }
}