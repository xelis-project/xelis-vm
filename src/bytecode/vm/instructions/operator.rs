use crate::{
    bytecode::vm::{ChunkManager, VMError, VM}, Path, Type, Value
};

macro_rules! op {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a.as_value(), $b.as_value()) {
            (Value::U8(a), Value::U8(b)) => Value::U8(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::U16(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::U32(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::U64(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::U128(a $op b),
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
        pub fn $fn(vm: &mut VM, _: &mut ChunkManager) -> Result<(), VMError> {
            $macro1!(vm, $macro2, $op);
            Ok(())
        }
    };
}

opcode_fn!(add, opcode_op, op, +);
opcode_fn!(sub, opcode_op, op, -);
opcode_fn!(mul, opcode_op, op, *);
opcode_fn!(div, opcode_op, op, /);
opcode_fn!(rem, opcode_op, op, %);
opcode_fn!(xor, opcode_op, op, ^);
opcode_fn!(shl, opcode_op, op, <<);
opcode_fn!(shr, opcode_op, op, >>);
opcode_fn!(and, opcode_op, op, &);
opcode_fn!(or, opcode_op, op, |);

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

pub fn neg(vm: &mut VM, _: &mut ChunkManager) -> Result<(), VMError> {
    let value = vm.pop_stack()?;
    vm.push_stack_unchecked(Path::Owned(Value::Boolean(!value.as_bool()?)));
    Ok(())
}

pub fn assign(vm: &mut VM, _: &mut ChunkManager) -> Result<(), VMError> {
    let right = vm.pop_stack()?;
    let mut left = vm.pop_stack()?;
    *left.as_mut() = right.into_owned();
    Ok(())
}

pub fn pow(vm: &mut VM, _: &mut ChunkManager) -> Result<(), VMError> {
    let right = vm.pop_stack()?.into_owned();
    let left = vm.pop_stack()?.into_owned();
    let result = match (left, right) {
        (Value::U8(a), Value::U8(b)) => Value::U8(a.pow(b as u32)),
        (Value::U16(a), Value::U16(b)) => Value::U16(a.pow(b as u32)),
        (Value::U32(a), Value::U32(b)) => Value::U32(a.pow(b as u32)),
        (Value::U64(a), Value::U64(b)) => Value::U64(a.pow(b as u32)),
        (Value::U128(a), Value::U128(b)) => Value::U128(a.pow(b as u32)),
        (a, b) => return Err(VMError::IncompatibleValues(a.clone(), b.clone()))
    };
    vm.push_stack_unchecked(Path::Owned(result));
    Ok(())
}

pub fn cast(vm: &mut VM, manager: &mut ChunkManager) -> Result<(), VMError> {
    let _type = manager.read_type()?;
    let current = vm.pop_stack()?
        .into_owned();

    let value = match _type {
        Type::U8 => Value::U8(current.cast_to_u8()?),
        Type::U16 => Value::U16(current.cast_to_u16()?),
        Type::U32 => Value::U32(current.cast_to_u32()?),
        Type::U64 => Value::U64(current.cast_to_u64()?),
        Type::U128 => Value::U128(current.cast_to_u128()?),
        Type::String => Value::String(current.cast_to_string()?),
        _ => return Err(VMError::UnsupportedCastType)
    };

    vm.push_stack(Path::Owned(value))
}