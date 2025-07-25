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

pub type SyncFn<'a, 'ty, 'r, M> = fn(
    &Backend<'a, 'ty, 'r, M>,
    &mut Stack,
    &mut ChunkManager,
    &mut ChunkReader<'_>,
    &mut Context<'ty, 'r>,
) -> Result<InstructionResult<'a, M>, VMError>;

pub enum OpCodeHandler<'a: 'r, 'ty: 'a, 'r, M> {
    Async(AsyncFn<'a, 'ty, 'r, M>),
    Sync(SyncFn<'a, 'ty, 'r, M>),
}

impl<'a: 'r, 'ty: 'a, 'r, M> OpCodeHandler<'a, 'ty, 'r, M> {
    pub const fn is_async(&self) -> bool {
        matches!(self, OpCodeHandler::Async(_))
    }
}


impl<'a: 'r, 'ty: 'a, 'r, M> Clone for OpCodeHandler<'a, 'ty, 'r, M> {
    fn clone(&self) -> Self {
        match self {
            OpCodeHandler::Async(f) => OpCodeHandler::Async(*f),
            OpCodeHandler::Sync(f) => OpCodeHandler::Sync(*f),
        }
    }
}

impl<'a: 'r, 'ty: 'a, 'r, M> Copy for OpCodeHandler<'a, 'ty, 'r, M> {}

// Table of instructions
// It contains all the instructions that the VM can execute
// It is a fixed size array of 256 elements
// Each element is a function pointer to the instruction
pub struct InstructionTable<'a: 'r, 'ty: 'a, 'r, M> {
    instructions: [(OpCodeHandler<'a, 'ty, 'r, M>, u64); 256],
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
            instructions: [(OpCodeHandler::Sync(unimplemented), 0); 256],
        };

        table.set_instruction(OpCode::Constant, OpCodeHandler::Sync(constant), 1);
        table.set_instruction(OpCode::MemoryLoad, OpCodeHandler::Sync(memory_load), 5);
        table.set_instruction(OpCode::MemorySet, OpCodeHandler::Sync(memory_set), 5);
        table.set_instruction(OpCode::MemoryPop, OpCodeHandler::Sync(memory_pop), 3);
        table.set_instruction(OpCode::MemoryLen, OpCodeHandler::Sync(memory_len), 1);
        table.set_instruction(OpCode::MemoryToOwned, OpCodeHandler::Sync(memory_to_owned), 5);

        table.set_instruction(OpCode::SubLoad, OpCodeHandler::Sync(subload), 5);

        table.set_instruction(OpCode::Pop, OpCodeHandler::Sync(pop), 1);
        table.set_instruction(OpCode::PopN, OpCodeHandler::Sync(pop_n), 1);
        table.set_instruction(OpCode::Copy, OpCodeHandler::Sync(copy), 1);
        table.set_instruction(OpCode::CopyN, OpCodeHandler::Sync(copy_n), 1);
        table.set_instruction(OpCode::ToOwned, OpCodeHandler::Sync(to_owned), 1);

        table.set_instruction(OpCode::Swap, OpCodeHandler::Sync(swap), 1);
        table.set_instruction(OpCode::Swap2, OpCodeHandler::Sync(swap2), 1);
        table.set_instruction(OpCode::Jump, OpCodeHandler::Sync(jump), 2);
        table.set_instruction(OpCode::JumpIfFalse, OpCodeHandler::Sync(jump_if_false), 3);

        table.set_instruction(OpCode::IterableLength, OpCodeHandler::Sync(iterable_length), 3);
        table.set_instruction(OpCode::IteratorBegin, OpCodeHandler::Sync(iterator_begin), 5);
        table.set_instruction(OpCode::IteratorNext, OpCodeHandler::Sync(iterator_next), 1);
        table.set_instruction(OpCode::IteratorEnd, OpCodeHandler::Sync(iterator_end), 1);

        table.set_instruction(OpCode::Return, OpCodeHandler::Sync(return_fn), 1);

        table.set_instruction(OpCode::ArrayCall, OpCodeHandler::Sync(array_call), 2);
        table.set_instruction(OpCode::Cast, OpCodeHandler::Sync(cast), 1);
        table.set_instruction(OpCode::InvokeChunk, OpCodeHandler::Sync(invoke_chunk), 5);
        table.set_instruction(OpCode::SysCall, OpCodeHandler::Async(async_handler!(syscall)), 2);
        table.set_instruction(OpCode::NewObject, OpCodeHandler::Sync(new_array), 1);
        table.set_instruction(OpCode::NewRange, OpCodeHandler::Sync(new_range), 1);
        table.set_instruction(OpCode::NewMap, OpCodeHandler::Sync(new_map), 1);

        table.set_instruction(OpCode::Add, OpCodeHandler::Sync(add), 1);
        table.set_instruction(OpCode::Sub, OpCodeHandler::Sync(sub), 1);
        table.set_instruction(OpCode::Mul, OpCodeHandler::Sync(mul), 3);
        table.set_instruction(OpCode::Div, OpCodeHandler::Sync(div), 8);
        table.set_instruction(OpCode::Mod, OpCodeHandler::Sync(rem), 8);
        table.set_instruction(OpCode::Pow, OpCodeHandler::Sync(pow), 35);
        table.set_instruction(OpCode::And, OpCodeHandler::Sync(and), 2);
        table.set_instruction(OpCode::Or, OpCodeHandler::Sync(or), 1);

        table.set_instruction(OpCode::BitwiseAnd, OpCodeHandler::Sync(bitwise_and), 1);
        table.set_instruction(OpCode::BitwiseOr, OpCodeHandler::Sync(bitwise_or), 1);
        table.set_instruction(OpCode::BitwiseXor, OpCodeHandler::Sync(bitwise_xor), 1);
        table.set_instruction(OpCode::BitwiseShl, OpCodeHandler::Sync(bitwise_shl), 5);
        table.set_instruction(OpCode::BitwiseShr, OpCodeHandler::Sync(bitwise_shr), 5);

        table.set_instruction(OpCode::Eq, OpCodeHandler::Sync(eq), 2);
        table.set_instruction(OpCode::Neg, OpCodeHandler::Sync(neg), 1);
        table.set_instruction(OpCode::Gt, OpCodeHandler::Sync(gt), 2);
        table.set_instruction(OpCode::Lt, OpCodeHandler::Sync(lt), 2);
        table.set_instruction(OpCode::Gte, OpCodeHandler::Sync(gte), 2);
        table.set_instruction(OpCode::Lte, OpCodeHandler::Sync(lte), 2);

        table.set_instruction(OpCode::Assign, OpCodeHandler::Sync(assign), 2);
        table.set_instruction(OpCode::AssignAdd, OpCodeHandler::Sync(add_assign), 3);
        table.set_instruction(OpCode::AssignSub, OpCodeHandler::Sync(sub_assign), 3);
        table.set_instruction(OpCode::AssignMul, OpCodeHandler::Sync(mul_assign), 5);
        table.set_instruction(OpCode::AssignDiv, OpCodeHandler::Sync(div_assign), 10);
        table.set_instruction(OpCode::AssignMod, OpCodeHandler::Sync(rem_assign), 10);
        table.set_instruction(OpCode::AssignPow, OpCodeHandler::Sync(pow_assign), 35);

        table.set_instruction(OpCode::AssignBitwiseAnd, OpCodeHandler::Sync(bitwise_and_assign), 3);
        table.set_instruction(OpCode::AssignBitwiseOr, OpCodeHandler::Sync(bitwise_or_assign), 3);
        table.set_instruction(OpCode::AssignBitwiseXor, OpCodeHandler::Sync(bitwise_xor_assign), 3);
        table.set_instruction(OpCode::AssignBitwiseShl, OpCodeHandler::Sync(bitwise_shl_assign), 7);
        table.set_instruction(OpCode::AssignBitwiseShr, OpCodeHandler::Sync(bitwise_shr_assign), 7);

        table.set_instruction(OpCode::Inc, OpCodeHandler::Sync(increment), 1);
        table.set_instruction(OpCode::Dec, OpCodeHandler::Sync(decrement), 1);
        table.set_instruction(OpCode::Flatten, OpCodeHandler::Sync(flatten), 5);
        table.set_instruction(OpCode::Match, OpCodeHandler::Sync(match_), 2);
        table.set_instruction(OpCode::DynamicCall, OpCodeHandler::Async(async_handler!(dynamic_call)), 8);
        table.set_instruction(OpCode::CaptureContext, OpCodeHandler::Sync(capture_context), 5);

        table
    }

    // Allow to overwrite a instruction with a custom handler
    #[inline(always)]
    pub const fn set_instruction(&mut self, opcode: OpCode, ptr: OpCodeHandler<'a, 'ty, 'r, M>, cost: u64) {
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
        let (instruction, cost) = &self.instructions[opcode as usize];

        // Increase the gas usage
        context.increase_gas_usage(*cost)?;

        match instruction {
            OpCodeHandler::Async(instruction) => instruction(backend, stack, chunk_manager, reader, context).await,
            OpCodeHandler::Sync(instruction) => instruction(backend, stack, chunk_manager, reader, context)
        }
    }
}

fn unimplemented<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, _: &mut Stack, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    Err(VMError::InvalidOpCode)
}

fn return_fn<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, _: &mut Stack, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    Ok(InstructionResult::Break)
}

fn jump<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, _: &mut Stack, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let addr = reader.read_u32()?;
    reader.set_index(addr as usize)?;
    Ok(InstructionResult::Nothing)
}

fn jump_if_false<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, _: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let addr = reader.read_u32()?;
    let value = stack.pop_stack()?;
    if !value.as_bool()? {
        reader.set_index(addr as usize)?;
    }
    Ok(InstructionResult::Nothing)
}

fn flatten<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, _: &mut ChunkManager, _: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
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

fn match_<'a: 'r, 'ty: 'a, 'r, M>(backend: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack, manager: &mut ChunkManager, reader: &mut ChunkReader<'_>, context: &mut Context<'ty, 'r>) -> Result<InstructionResult<'a, M>, VMError> {
    let magic_byte = reader.read_u8()?;
    let same = if magic_byte > 0 {
        let actual = stack.last_stack()?
            .as_ref()?;

        // Check if the magic byte is the same
        let same = actual.as_vec()?.get(0) == Some(&Primitive::U8(magic_byte - 1).into());

        // if its the same, flatten it
        if same {
            flatten(backend, stack, manager, reader, context)?;
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
