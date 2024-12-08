use serde::{Deserialize, Serialize};

use super::OpCode;

// Each chunk is a collection of opcodes and constants
// It represent a function or a block of code
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Chunk {
    // All the opcodes defined in the chunk
    instructions: Vec<u8>
}

impl Chunk {
    // Create a new chunk
    #[inline]
    pub fn new() -> Self {
        Self {
            instructions: Vec::new()
        }
    }

    // Create a new chunk with all needed data
    #[inline]
    pub fn from_instructions(instructions: Vec<u8>) -> Self {
        Self {
            instructions
        }
    }

    // Get the opcodes length
    #[inline]
    pub fn index(&self) -> usize {
        self.instructions.len()
    }

    // Get the index of the last instruction
    #[inline]
    pub fn last_index(&self) -> usize {
        self.instructions.len() - 1
    }

    // Pop the latest instruction
    #[inline]
    pub fn pop_instruction(&mut self) -> Option<u8> {
        self.instructions.pop()
    }

    // Last instruction present in the chunk
    #[inline]
    pub fn last_instruction(&self) -> Option<&u8> {
        self.instructions.last()
    }

    // Get the instructions
    #[inline]
    pub fn get_instructions(&self) -> &[u8] {
        &self.instructions
    }

    // Get the instruction at a specific index
    #[inline(always)]
    pub fn get_instruction_at(&self, index: usize) -> Option<&u8> {
        self.instructions.get(index)
    }

    // Get the instructions at a specific index
    #[inline]
    pub fn get_instructions_at(&self, index: usize, size: usize) -> Option<&[u8]> {
        self.instructions.get(index..index + size)
    }

    // Emit an opcode
    #[inline]
    pub fn emit_opcode(&mut self, op_code: OpCode) {
        self.instructions.push(op_code.as_byte());
    }

    // Inject an opcode at a specific index
    #[inline]
    pub fn inject_opcode_at(&mut self, op_code: OpCode, index: usize) {
        self.instructions.insert(index, op_code.as_byte());
    }

    // Patch a jump instruction
    #[inline]
    pub fn patch_jump(&mut self, index: usize, addr: u32) {
        let bytes = addr.to_le_bytes();
        // addr are u32, so we need to copy 4 bytes
        // index is the position of the latest byte written in instructions
        self.instructions[index - 3..=index].copy_from_slice(&bytes);
    }

    // Write a byte in the instructions
    #[inline]
    pub fn write_u8(&mut self, instruction: u8) {
        self.instructions.push(instruction);
    }

    // Write a bool in the instructions
    // A bool is represented as a byte
    #[inline]
    pub fn write_bool(&mut self, instruction: bool) {
        self.instructions.push(instruction as u8);
    }

    // Write a u16 in the instructions
    #[inline]
    pub fn write_u16(&mut self, instruction: u16) {
        let bytes = instruction.to_le_bytes();
        self.write_bytes(&bytes);
    }

    // Write a u32 in the instructions
    #[inline]
    pub fn write_u32(&mut self, instruction: u32) {
        let bytes = instruction.to_le_bytes();
        self.write_bytes(&bytes);
    }

    // Write bytes in the instructions
    #[inline]
    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.instructions.extend_from_slice(bytes);
    }
}
