mod operator;
mod r#impl;
mod iterator;
mod constructor;
mod memory;

use futures::future::LocalBoxFuture;
use operator::*;
use r#impl::*;
use iterator::*;
use constructor::*;
use memory::*;

use xelis_bytecode::{Module, OpCode};
use xelis_types::{Primitive, ValueCell};

use crate::{ChunkReader, Context, Reference};

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

#[macro_export]
macro_rules! async_handler {
    ($func: expr) => {
        move |a, b, c, d, e| {
          Box::pin($func(a, b, c, d, e))
        }
    };
}

#[derive(Debug)]
pub enum InstructionResult<'a, M> {
    Nothing,
    Break,
    InvokeChunk(u16),
    InvokeDynamicChunk {
        chunk_id: usize,
        from: usize,
    },
    AppendModule {
        module: Reference<'a, Module>,
        metadata: Reference<'a, M>,
        chunk_id: u16
    }
}

// A handler is a function pointer to an instruction
// With its associated cost
pub type AsyncFn<'a, 'ty, 'r, M> = for<'t> fn(
        &'t Backend<'a, 'ty, 'r, M>,
        &'t mut Stack,
        &'t mut ChunkManager,
        &'t mut ChunkReader<'_>,
        &'t mut Context<'ty, 'r>,
    ) -> LocalBoxFuture<'t, Result<InstructionResult<'a, M>, VMError>>;


// Table of instructions
// It contains all the instructions that the VM can execute
// It is a fixed size array of 256 elements
// Each element is a function pointer to the instruction
pub struct InstructionTable<'a: 'r, 'ty: 'a, 'r, M> {
    instructions: [(AsyncFn<'a, 'ty, 'r, M>, u64); 256],
}

impl<'a: 'r, 'ty: 'a, 'r, M> Default for InstructionTable<'a, 'ty, 'r, M> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a: 'r, 'ty: 'a, 'r, M> InstructionTable<'a, 'ty, 'r, M> {
    // Create a new instruction table with all the instructions
    pub const fn new() -> Self {
        let mut table = Self {
            instructions: [(async_handler!(unimplemented), 0); 256],
        };

        table.set_instruction(OpCode::Constant, async_handler!(constant), 1);
        table.set_instruction(OpCode::MemoryLoad, async_handler!(memory_load), 5);
        table.set_instruction(OpCode::MemorySet, async_handler!(memory_set), 5);
        table.set_instruction(OpCode::MemoryPop, async_handler!(memory_pop), 3);
        table.set_instruction(OpCode::MemoryLen, async_handler!(memory_len), 1);
        table.set_instruction(OpCode::MemoryToOwned, async_handler!(memory_to_owned), 5);

        table.set_instruction(OpCode::SubLoad, async_handler!(subload), 5);

        table.set_instruction(OpCode::Pop, async_handler!(pop), 1);
        table.set_instruction(OpCode::PopN, async_handler!(pop_n), 1);
        table.set_instruction(OpCode::Copy, async_handler!(copy), 1);
        table.set_instruction(OpCode::CopyN, async_handler!(copy_n), 1);
        table.set_instruction(OpCode::ToOwned, async_handler!(to_owned), 1);

        table.set_instruction(OpCode::Swap, async_handler!(swap), 1);
        table.set_instruction(OpCode::Swap2, async_handler!(swap2), 1);
        table.set_instruction(OpCode::Jump, async_handler!(jump), 2);
        table.set_instruction(OpCode::JumpIfFalse, async_handler!(jump_if_false), 3);

        table.set_instruction(OpCode::IterableLength, async_handler!(iterable_length), 3);
        table.set_instruction(OpCode::IteratorBegin, async_handler!(iterator_begin), 5);
        table.set_instruction(OpCode::IteratorNext, async_handler!(iterator_next), 1);
        table.set_instruction(OpCode::IteratorEnd, async_handler!(iterator_end), 1);

        table.set_instruction(OpCode::Return, async_handler!(return_fn), 1);

        table.set_instruction(OpCode::ArrayCall, async_handler!(array_call), 2);
        table.set_instruction(OpCode::Cast, async_handler!(cast), 1);
        table.set_instruction(OpCode::InvokeChunk, async_handler!(invoke_chunk), 5);
        table.set_instruction(OpCode::SysCall, async_handler!(syscall), 2);
        table.set_instruction(OpCode::NewObject, async_handler!(new_array), 1);
        table.set_instruction(OpCode::NewRange, async_handler!(new_range), 1);
        table.set_instruction(OpCode::NewMap, async_handler!(new_map), 1);

        table.set_instruction(OpCode::Add, async_handler!(add), 1);
        table.set_instruction(OpCode::Sub, async_handler!(sub), 1);
        table.set_instruction(OpCode::Mul, async_handler!(mul), 3);
        table.set_instruction(OpCode::Div, async_handler!(div), 8);
        table.set_instruction(OpCode::Mod, async_handler!(rem), 8);
        table.set_instruction(OpCode::Pow, async_handler!(pow), 35);
        table.set_instruction(OpCode::And, async_handler!(and), 2);
        table.set_instruction(OpCode::Or, async_handler!(or), 1);

        table.set_instruction(OpCode::BitwiseAnd, async_handler!(bitwise_and), 1);
        table.set_instruction(OpCode::BitwiseOr, async_handler!(bitwise_or), 1);
        table.set_instruction(OpCode::BitwiseXor, async_handler!(bitwise_xor), 1);
        table.set_instruction(OpCode::BitwiseShl, async_handler!(bitwise_shl), 5);
        table.set_instruction(OpCode::BitwiseShr, async_handler!(bitwise_shr), 5);

        table.set_instruction(OpCode::Eq, async_handler!(eq), 2);
        table.set_instruction(OpCode::Neg, async_handler!(neg), 1);
        table.set_instruction(OpCode::Gt, async_handler!(gt), 2);
        table.set_instruction(OpCode::Lt, async_handler!(lt), 2);
        table.set_instruction(OpCode::Gte, async_handler!(gte), 2);
        table.set_instruction(OpCode::Lte, async_handler!(lte), 2);

        table.set_instruction(OpCode::Assign, async_handler!(assign), 2);
        table.set_instruction(OpCode::AssignAdd, async_handler!(add_assign), 3);
        table.set_instruction(OpCode::AssignSub, async_handler!(sub_assign), 3);
        table.set_instruction(OpCode::AssignMul, async_handler!(mul_assign), 5);
        table.set_instruction(OpCode::AssignDiv, async_handler!(div_assign), 10);
        table.set_instruction(OpCode::AssignMod, async_handler!(rem_assign), 10);
        table.set_instruction(OpCode::AssignPow, async_handler!(pow_assign), 35);

        table.set_instruction(OpCode::AssignBitwiseAnd, async_handler!(bitwise_and_assign), 3);
        table.set_instruction(OpCode::AssignBitwiseOr, async_handler!(bitwise_or_assign), 3);
        table.set_instruction(OpCode::AssignBitwiseXor, async_handler!(bitwise_xor_assign), 3);
        table.set_instruction(OpCode::AssignBitwiseShl, async_handler!(bitwise_shl_assign), 7);
        table.set_instruction(OpCode::AssignBitwiseShr, async_handler!(bitwise_shr_assign), 7);

        table.set_instruction(OpCode::Inc, async_handler!(increment), 1);
        table.set_instruction(OpCode::Dec, async_handler!(decrement), 1);
        table.set_instruction(OpCode::Flatten, async_handler!(flatten), 5);
        table.set_instruction(OpCode::Match, async_handler!(match_), 2);
        table.set_instruction(OpCode::DynamicCall, async_handler!(dynamic_call), 8);
        table.set_instruction(OpCode::CaptureContext, async_handler!(capture_context), 5);

        table
    }

    // Allow to overwrite a instruction with a custom handler
    #[inline(always)]
    pub const fn set_instruction(&mut self, opcode: OpCode, ptr: AsyncFn<'a, 'ty, 'r, M>, cost: u64) {
        self.instructions[opcode.as_usize()] = (ptr, cost);
    }

    // Allow to overwrite the cost of an instruction
    #[inline(always)]
    pub const fn set_instruction_cost(&mut self, opcode: OpCode, cost: u64) {
        self.instructions[opcode.as_usize()].1 = cost;
    }

    // Execute an instruction
    #[inline]
    pub async fn execute(&self, opcode: u8, backend: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, chunk_manager: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
        trace!("Executing opcode: {:?} with {:?}", OpCode::from_byte(opcode), stack.get_inner());
        let (instruction, cost) = self.instructions[opcode as usize];

        // Increase the gas usage
        context.increase_gas_usage(cost)?;

        instruction(backend, stack, chunk_manager, reader, context).await
    }
}

async fn unimplemented<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, _: &mut Stack, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    Err(VMError::InvalidOpCode)
}

async fn return_fn<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, _: &mut Stack, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    Ok(InstructionResult::Break)
}

async fn jump<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, _: &mut Stack, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let addr = reader.read_u32()?;
    reader.set_index(addr as usize)?;
    Ok(InstructionResult::Nothing)
}

async fn jump_if_false<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let addr = reader.read_u32()?;
    let value = stack.pop_stack()?;
    if !value.as_bool()? {
        reader.set_index(addr as usize)?;
    }
    Ok(InstructionResult::Nothing)
}

async fn flatten<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, _: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
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

async fn match_<'a: 'r, 'ty: 'a, 'r, M>(backend: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, manager: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let magic_byte = reader.read_u8()?;
    let same = if magic_byte > 0 {
        let actual = stack.last_stack()?
            .as_ref()?;

        // Check if the magic byte is the same
        let same = actual.as_vec()?.get(0) == Some(&Primitive::U8(magic_byte - 1).into());

        // if its the same, flatten it
        if same {
            flatten(backend, stack, manager, reader, context).await?;
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

    let addr = reader.read_u32()?;

    // Do the jump to the next condition
    if !same {
        reader.set_index(addr as usize)?;
    }

    Ok(InstructionResult::Nothing)
}
