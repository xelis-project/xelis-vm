use crate::{
    debug,
    stack::Stack,
    Backend,
    ChunkManager,
    ChunkReader,
    VMContext,
    VMError
};
use silex_types::{Heap, PrimitiveInteger, Primitive, RuntimeValue, Type};

use super::InstructionResult;

macro_rules! op {
    ($values: expr, $a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (RuntimeValue::Primitive(a), RuntimeValue::Primitive(b)) => match (a, b) {
                (Primitive::Integer(PrimitiveInteger::U8(a)), Primitive::Integer(PrimitiveInteger::U8(b))) => Primitive::Integer(PrimitiveInteger::U8(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U16(a)), Primitive::Integer(PrimitiveInteger::U16(b))) => Primitive::Integer(PrimitiveInteger::U16(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U32(a)), Primitive::Integer(PrimitiveInteger::U32(b))) => Primitive::Integer(PrimitiveInteger::U32(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U64(a)), Primitive::Integer(PrimitiveInteger::U64(b))) => Primitive::Integer(PrimitiveInteger::U64(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U128(a)), Primitive::Integer(PrimitiveInteger::U128(b))) => Primitive::Integer(PrimitiveInteger::U128(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U256(a)), Primitive::Integer(PrimitiveInteger::U256(b))) => Primitive::Integer(PrimitiveInteger::U256(*a $op *b)),
                _ => return Err(VMError::IllegalState)
            }
            _ => return Err(VMError::IllegalState)
        }
    }};
}

macro_rules! op_bool {
    ($values: expr, $a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (RuntimeValue::Primitive(a), RuntimeValue::Primitive(b)) => match (a, b) {
                (Primitive::Integer(PrimitiveInteger::U8(a)), Primitive::Integer(PrimitiveInteger::U8(b))) => Primitive::Integer(PrimitiveInteger::U8(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U16(a)), Primitive::Integer(PrimitiveInteger::U16(b))) => Primitive::Integer(PrimitiveInteger::U16(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U32(a)), Primitive::Integer(PrimitiveInteger::U32(b))) => Primitive::Integer(PrimitiveInteger::U32(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U64(a)), Primitive::Integer(PrimitiveInteger::U64(b))) => Primitive::Integer(PrimitiveInteger::U64(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U128(a)), Primitive::Integer(PrimitiveInteger::U128(b))) => Primitive::Integer(PrimitiveInteger::U128(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U256(a)), Primitive::Integer(PrimitiveInteger::U256(b))) => Primitive::Integer(PrimitiveInteger::U256(*a $op *b)),
                (Primitive::Boolean(a), Primitive::Boolean(b)) => Primitive::Boolean(a $op b),
                _ => return Err(VMError::IllegalState)
            }
            _ => return Err(VMError::IllegalState)
        }
    }};
}

macro_rules! op_string {
    ($values: expr, $a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (RuntimeValue::Primitive(a), RuntimeValue::Primitive(b)) => match (a, b) {
                (Primitive::Integer(PrimitiveInteger::U8(a)), Primitive::Integer(PrimitiveInteger::U8(b))) => Primitive::Integer(PrimitiveInteger::U8(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U16(a)), Primitive::Integer(PrimitiveInteger::U16(b))) => Primitive::Integer(PrimitiveInteger::U16(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U32(a)), Primitive::Integer(PrimitiveInteger::U32(b))) => Primitive::Integer(PrimitiveInteger::U32(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U64(a)), Primitive::Integer(PrimitiveInteger::U64(b))) => Primitive::Integer(PrimitiveInteger::U64(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U128(a)), Primitive::Integer(PrimitiveInteger::U128(b))) => Primitive::Integer(PrimitiveInteger::U128(a $op b)),
                (Primitive::Integer(PrimitiveInteger::U256(a)), Primitive::Integer(PrimitiveInteger::U256(b))) => Primitive::Integer(PrimitiveInteger::U256(*a $op *b)),
                (Primitive::String(a), Primitive::String(b)) => {
                    // Verify the final len is less than u32::MAX
                    let len = (a.len() as u32).checked_add(b.len() as u32);
                    if len.is_none() {
                        return Err(VMError::OutOfMemory);
                    }

                    Primitive::String(a.to_owned() $op b)
                }
                _ => {
                    // we need to handle if one of the values is a string
                    if a.is_string() || b.is_string() {
                        let left = a.as_string_formatted()?.into_owned();
                        let right = b.as_string_formatted()?;
                        // Verify the final len is less than u32::MAX
                        let len = (left.len() as u32).checked_add(right.len() as u32);
                        if len.is_none() {
                            return Err(VMError::OutOfMemory);
                        }

                        Primitive::String(left $op &right)
                    } else {
                        return Err(VMError::IllegalState)
                    }
                }
            }
            _ => return Err(VMError::IllegalState)
        }
    }};
}

macro_rules! op_bool_res {
    ($values: expr, $a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (RuntimeValue::Primitive(a), RuntimeValue::Primitive(b)) => match (a, b) {
                (Primitive::Boolean(a), Primitive::Boolean(b)) => Primitive::Boolean(a $op b),
                (Primitive::Integer(PrimitiveInteger::U8(a)), Primitive::Integer(PrimitiveInteger::U8(b))) => Primitive::Boolean(a $op b),
                (Primitive::Integer(PrimitiveInteger::U16(a)), Primitive::Integer(PrimitiveInteger::U16(b))) => Primitive::Boolean(a $op b),
                (Primitive::Integer(PrimitiveInteger::U32(a)), Primitive::Integer(PrimitiveInteger::U32(b))) => Primitive::Boolean(a $op b),
                (Primitive::Integer(PrimitiveInteger::U64(a)), Primitive::Integer(PrimitiveInteger::U64(b))) => Primitive::Boolean(a $op b),
                (Primitive::Integer(PrimitiveInteger::U128(a)), Primitive::Integer(PrimitiveInteger::U128(b))) => Primitive::Boolean(a $op b),
                (Primitive::Integer(PrimitiveInteger::U256(a)), Primitive::Integer(PrimitiveInteger::U256(b))) => Primitive::Boolean(a $op b),
                (Primitive::String(a), Primitive::String(b)) => Primitive::Boolean(a $op b),
                _ => return Err(VMError::IllegalState)
            }
            _ => return Err(VMError::IllegalState)
        }
    }};
}

macro_rules! op_bool_all {
    ($values: expr, $a: expr, $b: expr, $op: tt) => {{
        Primitive::Boolean($a.equals($b, $values))
    }};
}

macro_rules! opcode_op {
    ($self: expr, $heap: expr, $macr: tt, $op: tt) => {
        {
            let right = $self.pop_stack()?;
            let left = $self.pop_stack()?;
            // Push the result to the stack, no need to check as we poped 2 values
            $self.push_stack_unchecked($macr!(&*$heap, left.as_ref(&*$heap), right.as_ref(&*$heap), $op).into());
        }
    };
}

macro_rules! op_div {
    ($values: expr, $a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (RuntimeValue::Primitive(a), RuntimeValue::Primitive(b)) => match (a, b) {
                (Primitive::Integer(PrimitiveInteger::U8(a)), Primitive::Integer(PrimitiveInteger::U8(b))) => {
                    if *b == 0 {
                        return Err(VMError::DivisionByZero);
                    }
                    Primitive::Integer(PrimitiveInteger::U8(a $op b))
                },
                (Primitive::Integer(PrimitiveInteger::U16(a)), Primitive::Integer(PrimitiveInteger::U16(b))) => {
                    if *b == 0 {
                        return Err(VMError::DivisionByZero);
                    }
                    Primitive::Integer(PrimitiveInteger::U16(a $op b))
                },
                (Primitive::Integer(PrimitiveInteger::U32(a)), Primitive::Integer(PrimitiveInteger::U32(b))) => {
                    if *b == 0 {
                        return Err(VMError::DivisionByZero);
                    }
                    Primitive::Integer(PrimitiveInteger::U32(a $op b))
                },
                (Primitive::Integer(PrimitiveInteger::U64(a)), Primitive::Integer(PrimitiveInteger::U64(b))) => {
                    if *b == 0 {
                        return Err(VMError::DivisionByZero);
                    }
                    Primitive::Integer(PrimitiveInteger::U64(a $op b))
                },
                (Primitive::Integer(PrimitiveInteger::U128(a)), Primitive::Integer(PrimitiveInteger::U128(b))) => {
                    if *b == 0 {
                        return Err(VMError::DivisionByZero);
                    }
                    Primitive::Integer(PrimitiveInteger::U128(a $op b))
                },
                (Primitive::Integer(PrimitiveInteger::U256(a)), Primitive::Integer(PrimitiveInteger::U256(b))) => {
                    if b.is_zero() {
                        return Err(VMError::DivisionByZero);
                    }
                    Primitive::Integer(PrimitiveInteger::U256(*a $op *b))
                },
                _ => return Err(VMError::IllegalState)
            }
            _ => return Err(VMError::IllegalState)
        }
    }};
}

macro_rules! opcode_op_assign {
    ($self: expr, $heap: expr, $macr: tt, $op: tt) => {
        {
            let right = $self.pop_stack()?;
            let mut left = $self.pop_stack()?;
            let result = $macr!(&*$heap, left.as_ref(&*$heap), right.as_ref(&*$heap), $op);

            $heap.replace_stack(&mut left, result.into());
        }
    };
}

macro_rules! opcode_fn {
    ($fn: ident, $macro1: tt, $macro2: tt, $op: tt) => {
        pub fn $fn<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
            debug!(stringify!($fn));
            $macro1!(stack, heap, $macro2, $op);
            Ok(InstructionResult::Nothing)
        }
    };
}

opcode_fn!(add, opcode_op, op_string, +);
opcode_fn!(sub, opcode_op, op, -);
opcode_fn!(mul, opcode_op, op, *);
opcode_fn!(div, opcode_op, op_div, /);
opcode_fn!(rem, opcode_op, op_div, %);

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
opcode_fn!(div_assign, opcode_op_assign, op_div, /);
opcode_fn!(rem_assign, opcode_op_assign, op_div, %);

opcode_fn!(bitwise_and_assign, opcode_op_assign, op_bool, &);
opcode_fn!(bitwise_or_assign, opcode_op_assign, op_bool, |);
opcode_fn!(bitwise_xor_assign, opcode_op_assign, op_bool, ^);
opcode_fn!(bitwise_shl_assign, opcode_op_assign, op, <<);
opcode_fn!(bitwise_shr_assign, opcode_op_assign, op, >>);

pub fn neg<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("neg");

    let value = stack.pop_stack()?;
    stack.push_stack_unchecked(Primitive::Boolean(!value.as_bool(heap)?).into());
    Ok(InstructionResult::Nothing)
}

pub fn assign<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("assign");

    let right = stack.pop_stack()?;
    let mut left = stack.pop_stack()?;

    let owned = right.into_owned(heap)?;

    heap.replace_stack(&mut left, owned);

    Ok(InstructionResult::Nothing)
}

pub fn pow<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("pow");

    let right = stack.pop_stack()?.into_owned(heap)?;
    let left = stack.pop_stack()?.into_owned(heap)?;
    let result = match (&left, &right) {
        (RuntimeValue::Primitive(a), RuntimeValue::Primitive(b)) => {
            let pow_n = b.as_u32()?;
            match a {
                Primitive::Integer(PrimitiveInteger::U8(a)) => Primitive::Integer(PrimitiveInteger::U8(a.pow(pow_n))),
                Primitive::Integer(PrimitiveInteger::U16(a)) => Primitive::Integer(PrimitiveInteger::U16(a.pow(pow_n))),
                Primitive::Integer(PrimitiveInteger::U32(a)) => Primitive::Integer(PrimitiveInteger::U32(a.pow(pow_n))),
                Primitive::Integer(PrimitiveInteger::U64(a)) => Primitive::Integer(PrimitiveInteger::U64(a.pow(pow_n))),
                Primitive::Integer(PrimitiveInteger::U128(a)) => Primitive::Integer(PrimitiveInteger::U128(a.pow(pow_n))),
                Primitive::Integer(PrimitiveInteger::U256(a)) => Primitive::Integer(PrimitiveInteger::U256(a.pow(pow_n))),
                _ => return Err(VMError::IllegalState)
            }
        }
        _ => return Err(VMError::IllegalState)
    };
    stack.push_stack_unchecked(result.into());
    Ok(InstructionResult::Nothing)
}

pub fn pow_assign<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("pow_assign");

    let right = stack.pop_stack()?;
    let mut left = stack.pop_stack()?;
    let result = {
        match (left.as_ref(heap), right.as_ref(heap)) {
            (RuntimeValue::Primitive(a), RuntimeValue::Primitive(b)) => {
                let pow_n = b.as_u32()?;
                match a {
                    Primitive::Integer(PrimitiveInteger::U8(a)) => Primitive::Integer(PrimitiveInteger::U8(a.pow(pow_n))),
                    Primitive::Integer(PrimitiveInteger::U16(a)) => Primitive::Integer(PrimitiveInteger::U16(a.pow(pow_n))),
                    Primitive::Integer(PrimitiveInteger::U32(a)) => Primitive::Integer(PrimitiveInteger::U32(a.pow(pow_n))),
                    Primitive::Integer(PrimitiveInteger::U64(a)) => Primitive::Integer(PrimitiveInteger::U64(a.pow(pow_n))),
                    Primitive::Integer(PrimitiveInteger::U128(a)) => Primitive::Integer(PrimitiveInteger::U128(a.pow(pow_n))),
                    Primitive::Integer(PrimitiveInteger::U256(a)) => Primitive::Integer(PrimitiveInteger::U256(a.pow(pow_n))),
                    _ => return Err(VMError::IllegalState)
                }
            }
            _ => return Err(VMError::IllegalState)
        }
    };

    heap.replace_stack(&mut left, result.into());

    Ok(InstructionResult::Nothing)
}

pub fn cast<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, _: &mut ChunkManager, reader: &mut ChunkReader<'_>, _: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("cast");

    let ty = reader.read_type()?;
    let mut current = stack.pop_stack()?
        .into_owned(heap)?;

    let value = match ty {
        Type::U8 => Primitive::Integer(PrimitiveInteger::U8(current.cast_to_u8()?)),
        Type::U16 => Primitive::Integer(PrimitiveInteger::U16(current.cast_to_u16()?)),
        Type::U32 => Primitive::Integer(PrimitiveInteger::U32(current.cast_to_u32()?)),
        Type::U64 => Primitive::Integer(PrimitiveInteger::U64(current.cast_to_u64()?)),
        Type::U128 => Primitive::Integer(PrimitiveInteger::U128(current.cast_to_u128()?)),
        Type::U256 => Primitive::Integer(PrimitiveInteger::U256(current.cast_to_u256()?)),
        Type::String => Primitive::String(current.cast_to_string()?),
        Type::Bool => Primitive::Boolean(current.cast_to_bool()?),
        _ => return Err(VMError::UnsupportedCastType)
    };

    stack.push_stack(value.into())?;
    Ok(InstructionResult::Nothing)
}

pub fn and<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("and");

    let value = stack.pop_stack()?;
    let left = value.as_bool(heap)?;
    let right_value = stack.pop_stack()?;
    let right = right_value.as_bool(heap)?;
    stack.push_stack_unchecked(Primitive::Boolean(left && right).into());

    Ok(InstructionResult::Nothing)
}

pub fn or<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("or");

    let right = stack.pop_stack()?;
    let left = stack.pop_stack()?;
    let value = left.as_bool(heap)? || right.as_bool(heap)?;
    stack.push_stack_unchecked(Primitive::Boolean(value).into());

    Ok(InstructionResult::Nothing)
}

pub fn increment<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("increment");

    let v = stack.last_mut_stack()?;
    v.as_mut(heap).increment()?;
    Ok(InstructionResult::Nothing)
}

pub fn decrement<'a: 'r, 'ty: 'a, 'r, M>(_: &Backend<'a, 'ty, 'r, M>, stack: &mut Stack<M>, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, M>, VMError> {
    debug!("decrement");

    let v = stack.last_mut_stack()?;
    v.as_mut(heap).decrement()?;

    Ok(InstructionResult::Nothing)
}