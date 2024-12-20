use crate::{
    stack::Stack,
    Backend,
    ChunkManager,
    Context,
    VMError
};
use xelis_types::{Value, ValueCell, Type, Path};

use super::InstructionResult;

macro_rules! op {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a.as_value(), $b.as_value()) {
            (ValueCell::Default(a), ValueCell::Default(b)) => match (a, b) {
                (Value::U8(a), Value::U8(b)) => Value::U8(a $op b),
                (Value::U16(a), Value::U16(b)) => Value::U16(a $op b),
                (Value::U32(a), Value::U32(b)) => Value::U32(a $op b),
                (Value::U64(a), Value::U64(b)) => Value::U64(a $op b),
                (Value::U128(a), Value::U128(b)) => Value::U128(a $op b),
                (Value::U256(a), Value::U256(b)) => Value::U256(*a $op *b),
                _ => return Err(VMError::UnexpectedType)
            }
            _ => return Err(VMError::UnexpectedType)
        }
    }};
}

macro_rules! op_bool {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a.as_value(), $b.as_value()) {
            (ValueCell::Default(a), ValueCell::Default(b)) => match (a, b) {
                (Value::U8(a), Value::U8(b)) => Value::U8(a $op b),
                (Value::U16(a), Value::U16(b)) => Value::U16(a $op b),
                (Value::U32(a), Value::U32(b)) => Value::U32(a $op b),
                (Value::U64(a), Value::U64(b)) => Value::U64(a $op b),
                (Value::U128(a), Value::U128(b)) => Value::U128(a $op b),
                (Value::U256(a), Value::U256(b)) => Value::U256(*a $op *b),
                (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a $op b),
                _ => return Err(VMError::UnexpectedType)
            }
            _ => return Err(VMError::UnexpectedType)
        }
    }};
}

macro_rules! op_string {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a.as_value(), $b.as_value()) {
            (ValueCell::Default(a), ValueCell::Default(b)) => match (a, b) {
                (Value::U8(a), Value::U8(b)) => Value::U8(a $op b),
                (Value::U16(a), Value::U16(b)) => Value::U16(a $op b),
                (Value::U32(a), Value::U32(b)) => Value::U32(a $op b),
                (Value::U64(a), Value::U64(b)) => Value::U64(a $op b),
                (Value::U128(a), Value::U128(b)) => Value::U128(a $op b),
                (Value::U256(a), Value::U256(b)) => Value::U256(*a $op *b),
                (Value::String(a), Value::String(b)) => {
                    // Verify the final len is less than u32::MAX
                    let len = (a.len() as u32).checked_add(b.len() as u32);
                    if len.is_none() {
                        return Err(VMError::StringTooLarge);
                    }

                    Value::String(a.to_owned() $op b)
                }
                _ => {
                    // we need to handle if one of the values is a string
                    if a.is_string() || b.is_string() {
                        let left = a.as_string_formatted()?.into_owned();
                        let right = b.as_string_formatted()?;
                        // Verify the final len is less than u32::MAX
                        let len = (left.len() as u32).checked_add(right.len() as u32);
                        if len.is_none() {
                            return Err(VMError::StringTooLarge);
                        }

                        Value::String(left $op &right)
                    } else {
                        return Err(VMError::UnexpectedType)
                    }
                }
            }
            _ => return Err(VMError::UnexpectedType)
        }
    }};
}

macro_rules! op_bool_res {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a.as_value(), $b.as_value()) {
            (ValueCell::Default(a), ValueCell::Default(b)) => match (a, b) {
                (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a $op b),
                (Value::U8(a), Value::U8(b)) => Value::Boolean(a $op b),
                (Value::U16(a), Value::U16(b)) => Value::Boolean(a $op b),
                (Value::U32(a), Value::U32(b)) => Value::Boolean(a $op b),
                (Value::U64(a), Value::U64(b)) => Value::Boolean(a $op b),
                (Value::U128(a), Value::U128(b)) => Value::Boolean(a $op b),
                (Value::U256(a), Value::U256(b)) => Value::Boolean(a $op b),
                (Value::String(a), Value::String(b)) => Value::Boolean(a $op b),
                _ => return Err(VMError::UnexpectedType)
            }
            _ => return Err(VMError::UnexpectedType)
        }
    }};
}

macro_rules! op_bool_all {
    ($a: expr, $b: expr, $op: tt) => {{
        Value::Boolean($a.as_value() $op $b.as_value())
    }};
}

macro_rules! opcode_op {
    ($self: expr, $macr: tt, $op: tt) => {
        {
            let right = $self.pop_stack()?;
            let left = $self.pop_stack()?;
            // Push the result to the stack, no need to check as we poped 2 values
            $self.push_stack_unchecked(Path::Owned($macr!(left.as_ref(), right.as_ref(), $op).into()));
        }
    };
}

macro_rules! op_div {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a.as_value(), $b.as_value()) {
            (ValueCell::Default(a), ValueCell::Default(b)) => match (a, b) {
                (Value::U8(a), Value::U8(b)) => {
                    if *b == 0 {
                        return Err(VMError::DivisionByZero);
                    }
                    Value::U8(a $op b)
                },
                (Value::U16(a), Value::U16(b)) => {
                    if *b == 0 {
                        return Err(VMError::DivisionByZero);
                    }
                    Value::U16(a $op b)
                },
                (Value::U32(a), Value::U32(b)) => {
                    if *b == 0 {
                        return Err(VMError::DivisionByZero);
                    }
                    Value::U32(a $op b)
                },
                (Value::U64(a), Value::U64(b)) => {
                    if *b == 0 {
                        return Err(VMError::DivisionByZero);
                    }
                    Value::U64(a $op b)
                },
                (Value::U128(a), Value::U128(b)) => {
                    if *b == 0 {
                        return Err(VMError::DivisionByZero);
                    }
                    Value::U128(a $op b)
                },
                (Value::U256(a), Value::U256(b)) => {
                    if b.is_zero() {
                        return Err(VMError::DivisionByZero);
                    }
                    Value::U256(*a $op *b)
                },
                _ => return Err(VMError::UnexpectedType)
            }
            _ => return Err(VMError::UnexpectedType)
        }
    }};
}

macro_rules! opcode_op_assign {
    ($self: expr, $macr: tt, $op: tt) => {
        {
            let right = $self.pop_stack()?;
            let mut left = $self.pop_stack()?;
            let result = $macr!(left.as_ref(), right.as_ref(), $op);
            *left.as_mut() = result.into();
        }
    };
}

macro_rules! opcode_fn {
    ($fn: ident, $macro1: tt, $macro2: tt, $op: tt) => {
        pub fn $fn<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
            $macro1!(stack, $macro2, $op);
            Ok(InstructionResult::Nothing)
        }
    };
}

opcode_fn!(add, opcode_op, op_string, +);
opcode_fn!(sub, opcode_op, op, -);
opcode_fn!(mul, opcode_op, op, *);
opcode_fn!(div, opcode_op, op_div, /);
opcode_fn!(rem, opcode_op, op, %);

opcode_fn!(bitwise_and, opcode_op, op_bool, &);
opcode_fn!(bitwise_or, opcode_op, op_bool, |);
opcode_fn!(bitwise_xor, opcode_op, op_bool, ^);
opcode_fn!(bitwise_shl, opcode_op, op, <<);
opcode_fn!(bitwise_shr, opcode_op, op, >>);

opcode_fn!(eq, opcode_op, op_bool_all, ==);
opcode_fn!(gt, opcode_op, op_bool_res, >);
opcode_fn!(lt, opcode_op, op_bool_res, <);
opcode_fn!(gte, opcode_op, op_bool_res, >=);
opcode_fn!(lte, opcode_op, op_bool_res, <=);

opcode_fn!(add_assign, opcode_op_assign, op_string, +);
opcode_fn!(sub_assign, opcode_op_assign, op, -);
opcode_fn!(mul_assign, opcode_op_assign ,op, *);
opcode_fn!(div_assign, opcode_op_assign, op, /);
opcode_fn!(rem_assign, opcode_op_assign, op, %);

opcode_fn!(bitwise_and_assign, opcode_op_assign, op_bool, &);
opcode_fn!(bitwise_or_assign, opcode_op_assign, op_bool, |);
opcode_fn!(bitwise_xor_assign, opcode_op_assign, op_bool, ^);
opcode_fn!(bitwise_shl_assign, opcode_op_assign, op, <<);
opcode_fn!(bitwise_shr_assign, opcode_op_assign, op, >>);

pub fn neg<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let value = stack.pop_stack()?;
    stack.push_stack_unchecked(Path::Owned(Value::Boolean(!value.as_bool()?).into()));
    Ok(InstructionResult::Nothing)
}

pub fn assign<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, context: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let right = stack.pop_stack()?;
    let mut left = stack.pop_stack()?;
    let owned = right.into_owned();

    // Verify the depth of the owned value
    owned.calculate_depth(context.max_value_depth())?;

    *left.as_mut() = owned;
    Ok(InstructionResult::Nothing)
}

pub fn pow<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let right = stack.pop_stack()?.into_owned();
    let left = stack.pop_stack()?.into_owned();
    let result = match (left, right) {
        (ValueCell::Default(a), ValueCell::Default(b)) => {
            let pow_n = b.as_u32()?;
            match a {
                Value::U8(a) => Value::U8(a.pow(pow_n)),
                Value::U16(a) => Value::U16(a.pow(pow_n)),
                Value::U32(a) => Value::U32(a.pow(pow_n)),
                Value::U64(a) => Value::U64(a.pow(pow_n)),
                Value::U128(a) => Value::U128(a.pow(pow_n)),
                Value::U256(a) => Value::U256(a.pow(pow_n)),
                _ => return Err(VMError::UnexpectedType)
            }
        }
        _ => return Err(VMError::UnexpectedType)
    };
    stack.push_stack_unchecked(Path::Owned(result.into()));
    Ok(InstructionResult::Nothing)
}

pub fn pow_assign<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let right = stack.pop_stack()?.into_owned();
    let mut left = stack.pop_stack()?;
    let result = {
        let left_value = left.as_ref();
        match (left_value.as_value(), right) {
            (ValueCell::Default(a), ValueCell::Default(b)) => {
                let pow_n = b.as_u32()?;
                match a {
                    Value::U8(a) => Value::U8(a.pow(pow_n)),
                    Value::U16(a) => Value::U16(a.pow(pow_n)),
                    Value::U32(a) => Value::U32(a.pow(pow_n)),
                    Value::U64(a) => Value::U64(a.pow(pow_n)),
                    Value::U128(a) => Value::U128(a.pow(pow_n)),
                    Value::U256(a) => Value::U256(a.pow(pow_n)),
                    _ => return Err(VMError::UnexpectedType)
                }
            }
            _ => return Err(VMError::UnexpectedType)
        }
    };

    *left.as_mut() = result.into();
    Ok(InstructionResult::Nothing)
}

pub fn cast<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, manager: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
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

    stack.push_stack(Path::Owned(value.into()))?;
    Ok(InstructionResult::Nothing)
}

pub fn and<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let value = stack.pop_stack()?;
    let left = value.as_bool()?;
    let right = stack.pop_stack()?.as_bool()?;
    stack.push_stack_unchecked(Path::Owned(Value::Boolean(left && right).into()));

    Ok(InstructionResult::Nothing)
}

pub fn or<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let right = stack.pop_stack()?;
    let left = stack.pop_stack()?;
    let value = left.as_bool()? || right.as_bool()?;
    stack.push_stack_unchecked(Path::Owned(Value::Boolean(value).into()));

    Ok(InstructionResult::Nothing)
}

pub fn increment<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let v = stack.last_mut_stack()?;
    v.as_mut().increment()?;
    Ok(InstructionResult::Nothing)
}

pub fn decrement<'a>(_: &Backend<'a>, stack: &mut Stack<'a>, _: &mut ChunkManager<'a>, _: &mut Context<'a, '_>) -> Result<InstructionResult, VMError> {
    let v = stack.last_mut_stack()?;
    v.as_mut().decrement()?;
    Ok(InstructionResult::Nothing)
}