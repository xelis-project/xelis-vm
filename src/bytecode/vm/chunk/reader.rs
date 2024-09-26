use crate::{
    bytecode::{opcode::OpCode, vm::VMError},
    Type,
};

use super::Chunk;

// Read instructions from a chunk
#[derive(Debug)]
pub struct ChunkReader<'a> {
    chunk: &'a Chunk,
    ip: usize
}

impl<'a> ChunkReader<'a> {
    #[inline]
    pub fn new(chunk: &'a Chunk) -> Self {
        ChunkReader {
            chunk,
            ip: 0
        }
    }

    // Get the current index in our reader
    #[inline]
    pub fn index(&self) -> usize {
        self.ip
    }

    // Set the current index in our reader
    #[inline]
    pub fn set_index(&mut self, index: usize) {
        self.ip = index;
    }

    // Get the op code at the given index
    #[inline]
    pub fn read_op_code(&mut self) -> Result<OpCode, VMError> {
        let byte = self.read_u8()?;
        OpCode::from_byte(byte).ok_or(VMError::InvalidOpCode)
    }

    // Read a bool from the instructions
    #[inline]
    pub fn read_bool(&mut self) -> Result<bool, VMError> {
        self.read_u8().map(|v| v == 1)
    }

    // Read a u8 from the instructions
    #[inline]
    pub fn read_u8(&mut self) -> Result<u8, VMError> {
        let byte = self.chunk.get_instruction_at(self.ip)
            .copied()
            .ok_or(VMError::MissingInstruction)?;

        self.ip += 1;

        Ok(byte)
    }

    // Read a &[u8] from the instructions
    #[inline]
    pub fn read_bytes(&mut self, length: usize) -> Result<&[u8], VMError> {
        let bytes = self.chunk.get_instructions_at(self.ip, length)
            .ok_or(VMError::MissingInstruction)?;
        self.ip += length;
        Ok(bytes)
    }

    // Read a u16 from the instructions
    #[inline]
    pub fn read_u16(&mut self) -> Result<u16, VMError> {
        let bytes = self.read_bytes(2)?;
        Ok(u16::from_le_bytes([bytes[0], bytes[1]]))
    }

    // Read a u32 from the instructions
    #[inline]
    pub fn read_u32(&mut self) -> Result<u32, VMError> {
        let bytes = self.read_bytes(4)?;
        Ok(u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    // Read a primitive type from the instructions
    #[inline]
    pub fn read_type(&mut self) -> Result<Type, VMError> {
        let type_id = self.read_u8()?;
        Type::primitive_type_from_byte(type_id).ok_or(VMError::InvalidPrimitiveType)
    }
}