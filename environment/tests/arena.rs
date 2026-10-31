use silex_environment::{EnvironmentError, VMContext};
use silex_types::{PrimitiveInteger, StackValue, Heap};

#[test]
fn temporary_values_are_restored_when_operations_return_errors() {
    let mut heap = unsafe { Heap::new() };
    let id = heap.insert(PrimitiveInteger::U64(7).into());
    let mut value: StackValue = id.clone().into();
    let result = heap.with_value(&mut value, |cell, _| -> Result<(), EnvironmentError> {
        *cell = PrimitiveInteger::U64(9).into();
        Err(EnvironmentError::Static("native failure"))
    });
    assert!(matches!(result, Err(EnvironmentError::Static("native failure"))));
    assert_eq!(heap.get(&id).as_u64().unwrap(), 9);
}

#[test]
fn heap_can_be_cleared_after_temporary_borrows_finish() {
    let mut heap = unsafe { Heap::new() };
    {
        let id = heap.insert(PrimitiveInteger::U64(7).into());
        let mut value: StackValue = id.clone().into();
        heap.with_value(&mut value, |cell, _| *cell = PrimitiveInteger::U64(9).into());
        assert_eq!(value.as_u64(&heap).unwrap(), 9);
    }
    heap.clear();
    assert!(heap.is_empty());
}

#[test]
fn memory_price_overflow_is_an_error_and_does_not_wrap_gas() {
    let mut context = VMContext::new();
    context.set_memory_price_per_byte(u64::MAX);
    assert!(matches!(context.increase_memory_usage(2), Err(EnvironmentError::GasOverflow)));
    assert_eq!(context.current_gas_usage(), 0);
}

#[test]
fn reducing_the_gas_limit_cannot_underflow_the_remaining_gas() {
    let mut context = VMContext::new();
    context.increase_gas_usage(10).unwrap();
    context.set_gas_limit(1);
    assert_eq!(context.get_gas_left(), 0);
}
