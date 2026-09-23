use crate::DecompilerError;
use silex_bytecode::{Chunk, OpCode};

#[derive(Clone, Debug)]
pub(crate) struct Instruction {
    pub offset: usize,
    pub op: OpCode,
    pub arg: usize,
    pub extra: usize,
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
