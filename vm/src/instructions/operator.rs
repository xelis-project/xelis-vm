use crate::{
    stack::Stack,
    Backend,
    ChunkManager,
    Context,
    VMError
};
use xelis_types::{Value, Type, Path};

use super::InstructionResult;

macro_rules! op {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a.as_value(), $b.as_value()) {
            (Value::U8(a), Value::U8(b)) => Value::U8(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::U16(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::U32(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::U64(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::U128(a $op b),
            (Value::U256(a), Value::U256(b)) => Value::U256(*a $op *b),
            (a, b) => return Err(VMError::IncompatibleValues(a.clone(), b.clone()))
        }
    }};
}

macro_rules! op_string {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (Value::U8(a), Value::U8(b)) => Value::U8(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::U16(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::U32(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::U64(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::U128(a $op b),
            (Value::U256(a), Value::U256(b)) => Value::U256(a $op b),
            (Value::String(a), Value::String(b)) => Value::String(a $op &b),
            (a, b) => return Err(VMError::IncompatibleValues(a.clone(), b.clone()))
        }
    }};
}

macro_rules! op_bool {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a.as_value(), $b.as_value()) {
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a $op b),
            (Value::U8(a), Value::U8(b)) => Value::Boolean(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::Boolean(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::Boolean(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::Boolean(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::Boolean(a $op b),
            (Value::U256(a), Value::U256(b)) => Value::Boolean(a $op b),
            (a, b) => return Err(VMError::IncompatibleValues(a.clone(), b.clone()))
        }
    }};
}

macro_rules! opcode_op {
    ($self: expr, $macr: tt, $op: tt) => {
        {
            let right = $self.pop_stack()?;
            let left = $self.pop_stack()?;
            // Push the result to the stack, no need to check as we poped 2 values
            $self.push_stack_unchecked(Path::Owned($macr!(left.as_ref(), right.as_ref(), $op)));
        }
    };
}

macro_rules! opcode_op_owned {
    ($self: expr, $macr: tt, $op: tt) => {
        {
            let right = $self.pop_stack()?;
            let left = $self.pop_stack()?;
            // Push the result to the stack, no need to check as we poped 2 values
            $self.push_stack_unchecked(Path::Owned($macr!(left.into_owned(), right.into_owned(), $op)));
        }
    };
}

macro_rules! opcode_op_assign {
    ($self: expr, $macr: tt, $op: tt) => {
        {
            let right = $self.pop_stack()?;
            let mut left = $self.pop_stack()?;
            let result = $macr!(left.as_ref(), right.as_ref(), $op);
            *left.as_mut() = result;
        }
    };
}

macro_rules! opcode_fn {
    ($fn: ident, $macro1: tt, $macro2: tt, $op: tt) => {
        pub fn $fn<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
            $macro1!(stack, $macro2, $op);
            Ok(InstructionResult::Nothing)
        }
    };
}

opcode_fn!(add, opcode_op_owned, op_string, +);
opcode_fn!(sub, opcode_op, op, -);
opcode_fn!(mul, opcode_op, op, *);
opcode_fn!(div, opcode_op, op, /);
opcode_fn!(rem, opcode_op, op, %);
opcode_fn!(xor, opcode_op, op, ^);
opcode_fn!(shl, opcode_op, op, <<);
opcode_fn!(shr, opcode_op, op, >>);

opcode_fn!(eq, opcode_op, op_bool, ==);
opcode_fn!(gt, opcode_op, op_bool, >);
opcode_fn!(lt, opcode_op, op_bool, <);
opcode_fn!(gte, opcode_op, op_bool, >=);
opcode_fn!(lte, opcode_op, op_bool, <=);

opcode_fn!(add_assign, opcode_op_assign, op, +);
opcode_fn!(sub_assign, opcode_op_assign, op, -);
opcode_fn!(mul_assign, opcode_op_assign ,op, *);
opcode_fn!(div_assign, opcode_op_assign, op, /);
opcode_fn!(rem_assign, opcode_op_assign, op, %);
opcode_fn!(xor_assign, opcode_op_assign, op, ^);
opcode_fn!(shl_assign, opcode_op_assign, op, <<);
opcode_fn!(shr_assign, opcode_op_assign, op, >>);

pub fn neg<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
    let value = stack.pop_stack()?;
    stack.push_stack_unchecked(Path::Owned(Value::Boolean(!value.as_bool()?)));
    Ok(InstructionResult::Nothing)
}

pub fn assign<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
    let right = stack.pop_stack()?;
    let mut left = stack.pop_stack()?;
    *left.as_mut() = right.into_owned();
    Ok(InstructionResult::Nothing)
}

pub fn pow<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
    let right = stack.pop_stack()?.into_owned();
    let left = stack.pop_stack()?.into_owned();
    let result = match (left, right) {
        (Value::U8(a), Value::U8(b)) => Value::U8(a.pow(b as u32)),
        (Value::U16(a), Value::U16(b)) => Value::U16(a.pow(b as u32)),
        (Value::U32(a), Value::U32(b)) => Value::U32(a.pow(b as u32)),
        (Value::U64(a), Value::U64(b)) => Value::U64(a.pow(b as u32)),
        (Value::U128(a), Value::U128(b)) => Value::U128(a.pow(b as u32)),
        (Value::U256(a), Value::U256(b)) => Value::U256(a.pow(b.into())),
        (a, b) => return Err(VMError::IncompatibleValues(a.clone(), b.clone()))
    };
    stack.push_stack_unchecked(Path::Owned(result));
    Ok(InstructionResult::Nothing)
}

pub fn cast<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
    let _type = manager.read_type()?;
    let current = stack.pop_stack()?
        .into_owned();

    let value = match _type {
        Type::U8 => Value::U8(current.cast_to_u8()?),
        Type::U16 => Value::U16(current.cast_to_u16()?),
        Type::U32 => Value::U32(current.cast_to_u32()?),
        Type::U64 => Value::U64(current.cast_to_u64()?),
        Type::U128 => Value::U128(current.cast_to_u128()?),
        Type::U256 => Value::U256(current.cast_to_u256()?),
        Type::String => Value::String(current.cast_to_string()?),
        _ => return Err(VMError::UnsupportedCastType)
    };

    stack.push_stack(Path::Owned(value))?;
    Ok(InstructionResult::Nothing)
}

pub fn and<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
    let value = stack.pop_stack()?;
    let value = value.as_bool()?;
    let value = value && stack.pop_stack()?.as_bool()?;
    stack.push_stack_unchecked(Path::Owned(Value::Boolean(value)));

    Ok(InstructionResult::Nothing)
}

pub fn or<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
    let right = stack.pop_stack()?;
    let left = stack.pop_stack()?;
    let value = left.as_bool()? || right.as_bool()?;
    stack.push_stack_unchecked(Path::Owned(Value::Boolean(value)));

    Ok(InstructionResult::Nothing)
}

pub fn increment<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
    let v = stack.last_mut_stack()?;
    v.as_mut().increment()?;
    Ok(InstructionResult::Nothing)
}

pub fn decrement<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a>) -> Result<InstructionResult, VMError> {
    let v = stack.last_mut_stack()?;
    v.as_mut().decrement()?;
    Ok(InstructionResult::Nothing)
}