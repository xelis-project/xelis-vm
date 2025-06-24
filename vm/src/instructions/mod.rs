mod operator;
mod r#impl;
mod iterator;
mod constructor;
mod memory;

use operator::*;
use r#impl::*;
use iterator::*;
use constructor::*;
use memory::*;

use xelis_bytecode::OpCode;
use xelis_types::{Primitive, ValueCell};

use crate::Context;

use super::{stack::Stack, Backend, ChunkManager, VMError};

/// A macro to log debug messages
/// It will only log if the feature "logging" is enabled
#[macro_export]
macro_rules! debug {
    ($($arg:tt)*) => {
        #[cfg(feature = "logging")]
        log::debug!($($arg)*);
    };
}

/// A macro to log info messages
/// It will only log if the feature "logging" is enabled
macro_rules! trace {
    ($($arg:tt)*) => {
        #[cfg(feature = "logging")]
        log::trace!($($arg)*);
    };
}


#[derive(Debug)]
pub enum InstructionResult {
    Nothing,
    Break,
    InvokeChunk(u16),
}

// A handler is a function pointer to an instruction
// With its associated cost
pub type Handler<'a> = (fn(&Backend<'a>, &mut Stack, &mut ChunkManager<'a>, &mut Context<'a, '_>) -> Result<InstructionResult, VMError>, u64);

// Table of instructions
// It contains all the instructions that the VM can execute
// It is a fixed size array of 256 elements
// Each element is a function pointer to the instruction
pub struct InstructionTable<'a> {
    instructions: [Handler<'a>; 256],
}

impl Default for InstructionTable<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> InstructionTable<'a> {
    // Create a new instruction table with all the instructions
    pub const fn new() -> Self {
        let mut instructions: [Handler; 256] = [(unimplemented, 0); 256];

        instructions[OpCode::Constant.as_usize()] = (constant, 1);
        instructions[OpCode::MemoryLoad.as_usize()] = (memory_load, 5);
        instructions[OpCode::MemorySet.as_usize()] = (memory_set, 5);
        instructions[OpCode::MemoryPop.as_usize()] = (memory_pop, 3);
        instructions[OpCode::MemoryLen.as_usize()] = (memory_len, 1);
        instructions[OpCode::MemoryToOwned.as_usize()] = (memory_to_owned, 5);

        instructions[OpCode::SubLoad.as_usize()] = (subload, 5);

        instructions[OpCode::Pop.as_usize()] = (pop, 1);
        instructions[OpCode::PopN.as_usize()] = (pop_n, 1);
        instructions[OpCode::Copy.as_usize()] = (copy, 1);
        instructions[OpCode::CopyN.as_usize()] = (copy_n, 1);
        instructions[OpCode::ToOwned.as_usize()] = (to_owned, 1);

        instructions[OpCode::Swap.as_usize()] = (swap, 1);
        instructions[OpCode::Swap2.as_usize()] = (swap2, 1);
        instructions[OpCode::Jump.as_usize()] = (jump, 2);
        instructions[OpCode::JumpIfFalse.as_usize()] = (jump_if_false, 3);

        instructions[OpCode::IterableLength.as_usize()] = (iterable_length, 3);
        instructions[OpCode::IteratorBegin.as_usize()] = (iterator_begin, 5);
        instructions[OpCode::IteratorNext.as_usize()] = (iterator_next, 1);
        instructions[OpCode::IteratorEnd.as_usize()] = (iterator_end, 1);

        instructions[OpCode::Return.as_usize()] = (return_fn, 1);

        instructions[OpCode::ArrayCall.as_usize()] = (array_call, 2);
        instructions[OpCode::Cast.as_usize()] = (cast, 1);
        instructions[OpCode::InvokeChunk.as_usize()] = (invoke_chunk, 5);
        instructions[OpCode::SysCall.as_usize()] = (syscall, 2);
        instructions[OpCode::NewObject.as_usize()] = (new_array, 1);
        instructions[OpCode::NewRange.as_usize()] = (new_range, 1);
        instructions[OpCode::NewMap.as_usize()] = (new_map, 1);

        instructions[OpCode::Add.as_usize()] = (add, 1);
        instructions[OpCode::Sub.as_usize()] = (sub, 1);
        instructions[OpCode::Mul.as_usize()] = (mul, 3);
        instructions[OpCode::Div.as_usize()] = (div, 8);
        instructions[OpCode::Mod.as_usize()] = (rem, 8);
        instructions[OpCode::Pow.as_usize()] = (pow, 35);
        instructions[OpCode::And.as_usize()] = (and, 2);
        instructions[OpCode::Or.as_usize()] = (or, 1);

        instructions[OpCode::BitwiseAnd.as_usize()] = (bitwise_and, 1);
        instructions[OpCode::BitwiseOr.as_usize()] = (bitwise_or, 1);
        instructions[OpCode::BitwiseXor.as_usize()] = (bitwise_xor, 1);
        instructions[OpCode::BitwiseShl.as_usize()] = (bitwise_shl, 5);
        instructions[OpCode::BitwiseShr.as_usize()] = (bitwise_shr, 5);

        instructions[OpCode::Eq.as_usize()] = (eq, 2);
        instructions[OpCode::Neg.as_usize()] = (neg, 1);
        instructions[OpCode::Gt.as_usize()] = (gt, 2);
        instructions[OpCode::Lt.as_usize()] = (lt, 2);
        instructions[OpCode::Gte.as_usize()] = (gte, 2);
        instructions[OpCode::Lte.as_usize()] = (lte, 2);

        instructions[OpCode::Assign.as_usize()] = (assign, 2);
        instructions[OpCode::AssignAdd.as_usize()] = (add_assign, 3);
        instructions[OpCode::AssignSub.as_usize()] = (sub_assign, 3);
        instructions[OpCode::AssignMul.as_usize()] = (mul_assign, 5);
        instructions[OpCode::AssignDiv.as_usize()] = (div_assign, 10);
        instructions[OpCode::AssignMod.as_usize()] = (rem_assign, 10);
        instructions[OpCode::AssignPow.as_usize()] = (pow_assign, 35);

        instructions[OpCode::AssignBitwiseAnd.as_usize()] = (bitwise_and_assign, 3);
        instructions[OpCode::AssignBitwiseOr.as_usize()] = (bitwise_or_assign, 3);
        instructions[OpCode::AssignBitwiseXor.as_usize()] = (bitwise_xor_assign, 3);
        instructions[OpCode::AssignBitwiseShl.as_usize()] = (bitwise_shl_assign, 7);
        instructions[OpCode::AssignBitwiseShr.as_usize()] = (bitwise_shr_assign, 7);

        instructions[OpCode::Inc.as_usize()] = (increment, 1);
        instructions[OpCode::Dec.as_usize()] = (decrement, 1);
        instructions[OpCode::Flatten.as_usize()] = (flatten, 5);
        instructions[OpCode::Match.as_usize()] = (match_, 2);
        instructions[OpCode::DynamicCall.as_usize()] = (dynamic_call, 5);

        Self { instructions }
    }

    // Allow to overwrite a instruction with a custom handler
    pub fn set_instruction(&mut self, opcode: OpCode, handler: Handler<'a>) {
        self.instructions[opcode.as_usize()] = handler;
    }

    // Allow to overwrite the cost of an instruction
    pub fn set_instruction_cost(&mut self, opcode: OpCode, cost: u64) {
        self.instructions[opcode.as_usize()].1 = cost;
    }

    // Execute an instruction
    pub fn execute(&self, opcode: u8, backend: &Backend<'a>, stack: &mut Stack, chunk_manager: &mut ChunkManager<'a>, context: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
        trace!("Executing opcode: {:?} with {:?}", OpCode::from_byte(opcode), stack.get_inner());
        let (instruction, cost) = self.instructions[opcode as usize];

        // Increase the gas usage
        context.increase_gas_usage(cost)?;

        instruction(backend, stack, chunk_manager, context)
    }
}

fn unimplemented<'a>(_: &Backend<'a>, _: &mut Stack, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    Err(VMError::InvalidOpCode)
}

fn return_fn<'a>(_: &Backend<'a>, _: &mut Stack, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    Ok(InstructionResult::Break)
}

fn jump<'a>(_: &Backend<'a>, _: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let addr = manager.read_u32()?;
    manager.set_index(addr as usize)?;
    Ok(InstructionResult::Nothing)
}

fn jump_if_false<'a>(_: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let addr = manager.read_u32()?;
    let value = stack.pop_stack()?;
    if !value.as_bool()? {
        manager.set_index(addr as usize)?;
    }
    Ok(InstructionResult::Nothing)
}

fn flatten<'a>(_: &Backend<'a>, stack: &mut Stack, _: &mut ChunkManager<'a>, context: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let value = stack.pop_stack()?;
    let values = value.into_owned()?
        .to_vec()?;

    context.increase_gas_usage(values.len() as _)?;
    stack.extend_stack(values.into_iter().map(Into::into))?;

    Ok(InstructionResult::Nothing)
}

fn is_value_in_range<T: PartialOrd>(
    value: T,
    expected_ref: &ValueCell,
    range_matcher: fn(&Box<(Primitive, Primitive)>) -> Option<(T, T)>
) -> bool {
    match expected_ref {
        ValueCell::Default(Primitive::Range(range)) => {
            if let Some((min, max)) = range_matcher(range) {
                value >= min && value <= max
            } else {
                false
            }
        },
        _ => false,
    }
}

fn match_<'a>(backend: &Backend<'a>, stack: &mut Stack, manager: &mut ChunkManager<'a>, context: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let magic_byte = manager.read_u8()?;
    let same = if magic_byte > 0 {
        let actual = stack.last_stack()?
            .as_ref()?;

        // Check if the magic byte is the same
        let same = actual.as_vec()?.get(0) == Some(&Primitive::U8(magic_byte - 1).into());

        // if its the same, flatten it
        if same {
            flatten(backend, stack, manager, context)?;
        }

        same
    } else {
        let expected = stack.pop_stack()?;
        let actual = stack.last_stack()?
            .as_ref()?;
    
        let expected_ref = expected.as_ref()?;
        if let ValueCell::Default(v) = actual {
            macro_rules! match_range {
                ($prim:ident, $val:ident) => {
                    is_value_in_range(*$val, expected_ref, |range| {
                        match &**range {
                            (Primitive::$prim(min), Primitive::$prim(max)) => Some((*min, *max)),
                            _ => None
                        }
                    })
                };
            }
    
            match v {
                Primitive::U8(v)   => match_range!(U8, v),
                Primitive::U16(v)  => match_range!(U16, v),
                Primitive::U32(v)  => match_range!(U32, v),
                Primitive::U64(v)  => match_range!(U64, v),
                Primitive::U128(v) => match_range!(U128, v),
                _ => actual == expected_ref
            }
        } else {
            actual == expected_ref
        }
    };

    let addr = manager.read_u32()?;

    // Do the jump to the next condition
    if !same {
        manager.set_index(addr as usize)?;
    }

    Ok(InstructionResult::Nothing)
}
