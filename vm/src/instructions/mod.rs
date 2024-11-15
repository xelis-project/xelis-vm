mod operator;
mod r#impl;
mod iterator;
mod constructor;

use operator::*;
use r#impl::*;
use iterator::*;
use constructor::*;

use xelis_bytecode::OpCode;

use super::{stack::Stack, Backend, ChunkManager, VMError};

#[derive(Debug)]
pub enum InstructionResult {
    Nothing,
    Break,
    InvokeChunk(u16),
}

pub type Handler<'a> = fn(&Backend<'a>, &mut Stack<'a>, &mut ChunkManager<'a>) -> Result<InstructionResult, VMError>;

pub struct InstructionTable<'a> {
    instructions: [Handler<'a>; 256],
}

impl<'a> InstructionTable<'a> {
    pub const fn new() -> Self {
        let mut instructions: [Handler; 256] = [unimplemented; 256];

        instructions[OpCode::Constant.as_usize()] = constant;
        instructions[OpCode::MemoryLoad.as_usize()] = memory_load;
        instructions[OpCode::MemorySet.as_usize()] = memory_set;
        instructions[OpCode::SubLoad.as_usize()] = subload;
        instructions[OpCode::Pop.as_usize()] = pop;
        instructions[OpCode::PopN.as_usize()] = pop_n;
        instructions[OpCode::Copy.as_usize()] = copy;
        instructions[OpCode::Swap.as_usize()] = swap;
        instructions[OpCode::ArrayCall.as_usize()] = array_call;
        instructions[OpCode::Cast.as_usize()] = cast;
        instructions[OpCode::InvokeChunk.as_usize()] = invoke_chunk;
        instructions[OpCode::SysCall.as_usize()] = syscall;
        instructions[OpCode::NewArray.as_usize()] = new_array;
        instructions[OpCode::NewStruct.as_usize()] = new_struct;
        instructions[OpCode::NewRange.as_usize()] = new_range;
        instructions[OpCode::NewMap.as_usize()] = new_map;
        instructions[OpCode::NewEnum.as_usize()] = new_enum;

        instructions[OpCode::Jump.as_usize()] = jump;
        instructions[OpCode::JumpIfFalse.as_usize()] = jump_if_false;

        instructions[OpCode::IterableLength.as_usize()] = iterable_length;
        instructions[OpCode::IteratorBegin.as_usize()] = iterator_begin;
        instructions[OpCode::IteratorNext.as_usize()] = iterator_next;
        instructions[OpCode::IteratorEnd.as_usize()] = iterator_end;

        instructions[OpCode::Return.as_usize()] = return_fn;

        instructions[OpCode::Add.as_usize()] = add;
        instructions[OpCode::Sub.as_usize()] = sub;
        instructions[OpCode::Mul.as_usize()] = mul;
        instructions[OpCode::Div.as_usize()] = div;
        instructions[OpCode::Mod.as_usize()] = rem;
        instructions[OpCode::Pow.as_usize()] = pow;
        instructions[OpCode::And.as_usize()] = and;
        instructions[OpCode::Or.as_usize()] = or;
        instructions[OpCode::Xor.as_usize()] = xor;
        instructions[OpCode::Shl.as_usize()] = shl;
        instructions[OpCode::Shr.as_usize()] = shr;

        instructions[OpCode::Eq.as_usize()] = eq;
        instructions[OpCode::Neg.as_usize()] = neg;
        instructions[OpCode::Gt.as_usize()] = gt;
        instructions[OpCode::Lt.as_usize()] = lt;
        instructions[OpCode::Gte.as_usize()] = gte;
        instructions[OpCode::Lte.as_usize()] = lte;

        instructions[OpCode::Assign.as_usize()] = assign;
        instructions[OpCode::AssignAdd.as_usize()] = add_assign;
        instructions[OpCode::AssignSub.as_usize()] = sub_assign;
        instructions[OpCode::AssignMul.as_usize()] = mul_assign;
        instructions[OpCode::AssignDiv.as_usize()] = div_assign;
        instructions[OpCode::AssignMod.as_usize()] = rem_assign;
        instructions[OpCode::AssignXor.as_usize()] = xor_assign;
        instructions[OpCode::AssignShl.as_usize()] = shl_assign;
        instructions[OpCode::AssignShr.as_usize()] = shr_assign;

        instructions[OpCode::Inc.as_usize()] = increment;
        instructions[OpCode::Dec.as_usize()] = decrement;

        Self { instructions }
    }

    // Execute an instruction
    pub fn execute(&self, opcode: u8, backend: &Backend<'a>, stack: &mut Stack<'a>, chunk_manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
        let instruction = self.instructions[opcode as usize];
        instruction(backend, stack, chunk_manager)
    }
}

fn unimplemented<'a>(_: &Backend<'a>, _: &mut Stack<'a>, _: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    Err(VMError::InvalidOpCode)
}

fn return_fn<'a>(_: &Backend<'a>, _: &mut Stack<'a>, _: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    Ok(InstructionResult::Break)
}

fn jump<'a>(_: &Backend<'a>, _: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let addr = manager.read_u32()?;
    manager.set_index(addr as usize);
    Ok(InstructionResult::Nothing)
}

fn jump_if_false<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>) -> Result<InstructionResult, VMError> {
    let addr = manager.read_u32()?;
    let value = stack.pop_stack()?;
    if !value.as_bool()? {
        manager.set_index(addr as usize);
    }
    Ok(InstructionResult::Nothing)
}