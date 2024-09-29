use crate::{bytecode::vm::{iterator::PathIterator, ChunkManager, VMError, VM}, Path, Value};

pub fn iterable_length<'a>(vm: &mut VM<'a>, _: &mut ChunkManager<'a>) -> Result<(), VMError> {
    let value = vm.pop_stack()?;
    let len = value.as_ref().as_vec()?.len();
    vm.push_stack_unchecked(Path::Owned(Value::U32(len as u32)));
    Ok(())
}

pub fn iterator_begin<'a>(vm: &mut VM<'a>, manager: &mut ChunkManager<'a>) -> Result<(), VMError> {
    let value = vm.pop_stack()?;
    let iterator = PathIterator::new(value);
    manager.add_iterator(iterator);
    Ok(())
}

pub fn iterator_next<'a>(vm: &mut VM<'a>, manager: &mut ChunkManager<'a>) -> Result<(), VMError> {
    let addr = manager.read_u32()?;
    if let Some(value) = manager.next_iterator()? {
        vm.push_stack(value)?;
    } else {
        manager.set_index(addr as usize);
    }
    Ok(())
}

pub fn iterator_end<'a>(_: &mut VM<'a>, manager: &mut ChunkManager<'a>) -> Result<(), VMError> {
    manager.pop_iterator()?;
    Ok(())
}