use crate::DecompilerError;
use silex_bytecode::{Chunk, OpCode};

#[derive(Clone, Debug)]
pub(crate) struct Instruction {
    pub offset: usize,
    pub op: OpCode,
    pub arg: usize,
    pub extra: usize,
}

/// Ordinary stack operations consume inputs and leave their outputs on top.
/// Control flow and instructions with type-dependent arity are handled separately.
#[derive(Clone, Copy)]
pub(crate) struct StackEffect {
    pub inputs: usize,
    pub outputs: usize,
}

impl StackEffect {
    pub fn new(inputs: usize, outputs: usize) -> Self {
        Self { inputs, outputs }
    }
}

impl Instruction {
    pub fn stack_effect(&self) -> Option<StackEffect> {
        use OpCode::*;
        let (inputs, outputs) = match self.op {
            Constant | MemoryLoad | MemoryPop | MemoryLen => (0, 1),
            MemorySet | Pop | IteratorBegin => (1, 0),
            PopN => (self.arg, 0),
            MemoryToOwned | IteratorEnd | CaptureContext => (0, 0),
            SubLoad | Cast | Neg | IterableLength => (1, 1),
            // These read/modify the top value in place, without changing height.
            ToOwned | Inc | Dec => (1, 1),
            NewObject => (self.arg, 1),
            NewMap => (self.arg * 2, 1),
            NewRange | ArrayCall => (2, 1),
            Add | Sub | Mul | Div | Mod | Pow | And | Or | BitwiseAnd | BitwiseOr | BitwiseXor
            | BitwiseShl | BitwiseShr | Eq | Gt | Lt | Gte | Lte => (2, 1),
            Assign | AssignAdd | AssignSub | AssignMul | AssignDiv | AssignMod | AssignPow
            | AssignBitwiseAnd | AssignBitwiseOr | AssignBitwiseXor | AssignBitwiseShl
            | AssignBitwiseShr => (2, 0),
            Copy | CopyN | Swap | Swap2 | Jump | JumpIfFalse | IteratorNext | Return
            | InvokeChunk | SysCall | DynamicCall | Flatten | Match => return None,
        };
        Some(StackEffect::new(inputs, outputs))
    }
}

pub(crate) fn decode(chunk: &Chunk, id: usize) -> Result<Vec<Instruction>, DecompilerError> {
    let bytes = chunk.get_instructions();
    let mut result = Vec::new();
    let mut offset = 0;
    while offset < bytes.len() {
        let error = |message| DecompilerError::new(id, offset, message);
        let op = OpCode::from_byte(bytes[offset]).ok_or_else(|| error("invalid opcode"))?;
        let end = offset + 1 + op.arguments_bytes();
        let args = bytes
            .get(offset + 1..end)
            .ok_or_else(|| error("truncated instruction"))?;
        let (arg, extra) = match args.len() {
            0 => (0, 0),
            1 => (args[0] as usize, 0),
            2 if matches!(op, OpCode::Swap2) => (args[0] as usize, args[1] as usize),
            2 => (u16::from_le_bytes([args[0], args[1]]) as usize, 0),
            3 => (
                u16::from_le_bytes([args[0], args[1]]) as usize,
                args[2] as usize,
            ),
            4 => (u32::from_le_bytes(args.try_into().unwrap()) as usize, 0),
            5 => (
                u32::from_le_bytes(args[1..].try_into().unwrap()) as usize,
                args[0] as usize,
            ),
            _ => unreachable!(),
        };
        result.push(Instruction {
            offset,
            op,
            arg,
            extra,
        });
        offset = end;
    }
    for instruction in &result {
        if matches!(
            instruction.op,
            OpCode::Jump | OpCode::JumpIfFalse | OpCode::IteratorNext | OpCode::Match
        ) && instruction.arg != bytes.len()
            && result
                .binary_search_by_key(&instruction.arg, |i| i.offset)
                .is_err()
        {
            return Err(DecompilerError::new(
                id,
                instruction.offset,
                "jump target is not an instruction boundary",
            ));
        }
    }
    Ok(result)
}
