use silex_environment::{EnvironmentError, VMContext};
use silex_types::{PrimitiveInteger, StackValue};

#[test]
fn temporary_cells_are_restored_during_unwinding() {
    let mut context = VMContext::new();
    let id = context.heap_mut().insert(PrimitiveInteger::U64(7).into()).unwrap();
    let mut value: StackValue = id.into();
    let panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let _ = context.with_value(&mut value, |cell, _| {
            *cell = PrimitiveInteger::U64(9).into();
            panic!("native failure");
        });
    }));
    assert!(panic.is_err());
    assert_eq!(context.heap().get(id).unwrap().as_u64().unwrap(), 9);
}

#[test]
fn heap_can_be_cleared_after_temporary_borrows_finish() {
    let mut context = VMContext::new();
    {
        let id = context.heap_mut().insert(PrimitiveInteger::U64(7).into()).unwrap();
        let mut value: StackValue = id.into();
        context.with_value(&mut value, |cell, _| *cell = PrimitiveInteger::U64(9).into());
        assert_eq!(value.as_u64(context.heap()).unwrap(), 9);
    }
    context.heap_mut().clear();
    assert!(context.heap().is_empty());
}

#[test]
fn memory_price_overflow_is_an_error_and_does_not_wrap_gas() {
    let mut context = VMContext::new();
    context.resources_mut().set_memory_price_per_byte(u64::MAX);
    assert!(matches!(context.resources_mut().increase_memory_usage(2), Err(EnvironmentError::GasOverflow)));
    assert_eq!(context.resources().current_gas_usage(), 0);
}

#[test]
fn reducing_the_gas_limit_cannot_underflow_the_remaining_gas() {
    let mut context = VMContext::new();
    context.resources_mut().increase_gas_usage(10).unwrap();
    context.resources_mut().set_gas_limit(1);
    assert_eq!(context.resources().get_gas_left(), 0);
}
