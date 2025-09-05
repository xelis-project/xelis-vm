use thiserror::Error;
use xelis_types::Type;
use crate::OpCode;

use super::Chunk;

// Read instructions from a chunk
#[derive(Debug)]
pub struct ChunkReader<'a> {
    chunk: &'a Chunk,
    ip: usize
}

#[derive(Debug, Clone, Copy, Error)]
pub enum ChunkReaderError {
    #[error("invalid opcode")]
    InvalidOpCode,
    #[error("unexpected end of chunk")]
    EndOfChunk,
    #[error("missing instruction at requested index")]
    MissingInstruction,
    #[error("invalid primitive type from byte")]
    InvalidPrimitiveType,
}

impl<'a> ChunkReader<'a> {
    #[inline(always)]
    pub fn new(chunk: &'a Chunk, ip: usize) -> Self {
        Self {
            chunk,
            ip
        }
    }

    // Get the current index in our reader
    #[inline(always)]
    pub fn index(&self) -> usize {
        self.ip
    }

    // Set the current index in our reader
    #[inline]
    pub fn set_index(&mut self, index: usize) -> Result<(), ChunkReaderError> {
        if self.chunk.get_instructions().len() < index {
            return Err(ChunkReaderError::EndOfChunk);
        }

        self.ip = index;
        Ok(())
    }

    // Advance the index by a given amount
    // Check that we don't go out of bounds
    #[inline]
    pub fn advance(&mut self, amount: usize) -> Result<(), ChunkReaderError> {
        if self.ip + amount > self.chunk.get_instructions().len() {
            return Err(ChunkReaderError::EndOfChunk);
        }

        self.ip += amount;
        Ok(())
    }

    // Get the op code at the given index
    #[inline]
    pub fn read_op_code(&mut self) -> Result<OpCode, ChunkReaderError> {
        let byte = self.read_u8()?;
        OpCode::from_byte(byte).ok_or(ChunkReaderError::InvalidOpCode)
    }

    // Read a bool from the instructions
    #[inline]
    pub fn read_bool(&mut self) -> Result<bool, ChunkReaderError> {
        self.read_u8().map(|v| v == 1)
    }

    #[inline(always)]
    pub fn next_u8(&mut self) -> Option<u8> {
        match self.chunk.get_instruction_at(self.ip) {
            Some(byte) => {
                self.ip += 1;
                Some(*byte)
            },
            None => None
        }
    }

    pub fn has_next(&self) -> bool {
        self.ip <= self.chunk.last_index()
    }

    // Read a u8 from the instructions
    #[inline]
    pub fn read_u8(&mut self) -> Result<u8, ChunkReaderError> {
        let byte = self.chunk.get_instruction_at(self.ip)
            .copied()
            .ok_or(ChunkReaderError::MissingInstruction)?;

        self.ip += 1;

        Ok(byte)
    }

    // Read a &[u8] from the instructions
    #[inline]
    pub fn read_bytes(&mut self, length: usize) -> Result<&[u8], ChunkReaderError> {
        let bytes = self.chunk.get_instructions_at(self.ip, length)
            .ok_or(ChunkReaderError::MissingInstruction)?;
        self.ip += length;
        Ok(bytes)
    }

    // Read a u16 from the instructions
    #[inline]
    pub fn read_u16(&mut self) -> Result<u16, ChunkReaderError> {
        let bytes = self.read_bytes(2)?;
        Ok(u16::from_le_bytes([bytes[0], bytes[1]]))
    }

    // Read a u32 from the instructions
    #[inline]
    pub fn read_u32(&mut self) -> Result<u32, ChunkReaderError> {
        let bytes = self.read_bytes(4)?;
        Ok(u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    // Read a primitive type from the instructions
    #[inline]
    pub fn read_type(&mut self) -> Result<Type, ChunkReaderError> {
        let type_id = self.read_u8()?;
        Type::primitive_type_from_byte(type_id).ok_or(ChunkReaderError::InvalidPrimitiveType)
    }

    // Check if we have another instruction to execute
    // Return OpCode is exempted
    // This function is used for the call stack optimization
    #[inline]
    pub fn has_next_instruction(&self) -> bool {
        match self.chunk.get_instruction_at(self.ip) {
            Some(byte) => *byte != OpCode::Return.as_byte(),
            None => false
        }
    }
}