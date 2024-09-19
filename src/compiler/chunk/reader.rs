use crate::{
    compiler::opcode::OpCode,
    Type,
    Value
};

use super::Chunk;

// Read opcodes and constants from a chunk
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

    // Get the constant at the given index
    #[inline]
    pub fn get_constant(&self, index: usize) -> Option<&Value> {
        self.chunk.get_constant(index)
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
    pub fn read_op_code(&mut self) -> Option<OpCode> {
        self.read_u8().map(OpCode::from_byte).flatten()
    }

    // Read a bool from the instructions
    #[inline]
    pub fn read_bool(&mut self) -> Option<bool> {
        self.read_u8().map(|v| v == 1)
    }

    // Read a u8 from the instructions
    #[inline]
    pub fn read_u8(&mut self) -> Option<u8> {
        let byte = self.chunk.instructions.get(self.ip).copied()?;
        self.ip += 1;
        Some(byte)
    }

    // Read a u16 from the instructions
    #[inline]
    pub fn read_u16(&mut self) -> u16 {
        let bytes = &self.chunk.instructions[self.ip..self.ip + 2];
        self.ip += 2;
        u16::from_be_bytes([bytes[0], bytes[1]])
    }

    // Read a u32 from the instructions
    #[inline]
    pub fn read_u32(&mut self) -> u32 {
        let bytes = &self.chunk.instructions[self.ip..self.ip + 4];
        self.ip += 4;
        u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]])
    }

    // Read a &[u8] from the instructions
    #[inline]
    pub fn read_bytes(&mut self, length: usize) -> &[u8] {
        let bytes = &self.chunk.instructions[self.ip..self.ip + length];
        self.ip += length;
        bytes
    }

    // Read a primitive type from the instructions
    #[inline]
    pub fn read_type(&mut self) -> Option<Type> {
        self.read_u8()
            .map(Type::primitive_type_from_byte)
            .flatten()
    }
}