use crate::{Type, Value};

use super::OpCode;

pub struct Chunk {
    // All the constants used in the chunk
    constants: Vec<Value>,
    // All the opcodes defined in the chunk
    op_codes: Vec<u8>
}

// Read opcodes and constants from a chunk
pub struct ChunkReader<'a> {
    chunk: &'a Chunk,
    ip: usize
}

impl Chunk {
    // Create a new chunk
    #[inline]
    pub fn new() -> Self {
        Chunk {
            constants: Vec::new(),
            op_codes: Vec::new()
        }
    }

    // Get the constant at the given index
    #[inline]
    pub fn get_constant(&self, index: usize) -> Option<&Value> {
        self.constants.get(index)
    }

    // Get the opcodes length
    #[inline]
    pub fn index(&self) -> usize {
        self.op_codes.len()
    }

    // Add a constant and retrieve its index
    #[inline]
    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(value);
        self.constants.len() - 1
    }

    // Emit an opcode
    #[inline]
    pub fn emit_opcode(&mut self, op_code: OpCode) {
        self.op_codes.push(op_code.as_byte());
    }

    #[inline]
    pub fn write_u8(&mut self, byte: u8) {
        self.op_codes.push(byte);
    }

    #[inline]
    pub fn write_u16(&mut self, byte: u16) {
        let bytes = byte.to_be_bytes();
        self.op_codes.extend_from_slice(&bytes);
    }

    #[inline]
    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.op_codes.extend_from_slice(bytes);
    }
}

impl<'a> ChunkReader<'a> {
    #[inline]
    pub fn new(chunk: &'a Chunk) -> Self {
        ChunkReader {
            chunk,
            ip: 0
        }
    }

    #[inline]
    pub fn get_constant(&self, index: usize) -> Option<&Value> {
        self.chunk.get_constant(index)
    }

    pub fn index(&self) -> usize {
        self.ip
    }

    pub fn set_index(&mut self, index: usize) {
        self.ip = index;
    }

    // Get the op code at the given index
    #[inline]
    pub fn next_op_code(&mut self) -> Option<OpCode> {
        self.read_u8().map(OpCode::from_byte).flatten()
    }

    #[inline]
    pub fn read_u8(&mut self) -> Option<u8> {
        let byte = self.chunk.op_codes.get(self.ip).copied()?;
        self.ip += 1;
        Some(byte)
    }

    #[inline]
    pub fn read_u16(&mut self) -> u16 {
        let bytes = &self.chunk.op_codes[self.ip..self.ip + 2];
        self.ip += 2;
        u16::from_be_bytes([bytes[0], bytes[1]])
    }

    #[inline]
    pub fn read_bytes(&mut self, length: usize) -> &[u8] {
        let bytes = &self.chunk.op_codes[self.ip..self.ip + length];
        self.ip += length;
        bytes
    }

    pub fn read_type(&mut self) -> Option<Type> {
        self.read_u8()
            .map(Type::primitive_type_from_byte)
            .flatten()
    }
}