use super::*;
use futures::{executor::block_on, future::pending, task::noop_waker_ref};
use silex_compiler::Compiler;
use silex_lexer::Lexer;
use silex_parser::Parser;
use std::{
    cell::RefCell,
    future::Future,
    panic::{catch_unwind, AssertUnwindSafe},
    rc::Rc,
    task::Context,
};

fn compile<M: 'static>(code: &str, builder: EnvironmentBuilder<'_, M>) -> (Module, Environment<M>) {
    let tokens = Lexer::new(code)
        .into_iter()
        .collect::<Result<Vec<_>, _>>()
        .unwrap();
    let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
    let environment = builder.build();
    let module = Compiler::new(&program, &environment).compile().unwrap();
    ModuleValidator::new(&module, &environment)
        .verify()
        .unwrap();
    (module, environment)
}

#[test]
fn caller_owned_heap_can_be_reused_and_keeps_existing_values() {
    let (module, environment) = compile(
        "entry main() -> u64[] { return [1u64, 2u64, 3u64]; }",
        EnvironmentBuilder::<()>::default(),
    );
    let mut heap = unsafe { Heap::new() };
    let anchor = heap.insert(RuntimeValue::Bytes(vec![7; 32]));
    let mut vm = VM::default();

    for blocking in [true, false] {
        vm.context_mut().reset_usage();
        vm.append_module(ModuleMetadata {
            module: (&module).into(),
            environment: (&environment).into(),
            metadata: (&()).into(),
        })
        .unwrap();
        vm.invoke_entry_chunk(0).unwrap();
        // Dropping an unpolled future must leave the queued execution intact.
        drop(vm.run_with_heap(&mut heap));
        assert!(!vm.is_idle());
        let result = if blocking {
            vm.run_blocking_with_heap(&mut heap)
        } else {
            block_on(vm.run_with_heap(&mut heap))
        }
        .unwrap();
        assert_eq!(result.as_vec().unwrap().len(), 3);
        assert!(vm.is_idle());
        assert_eq!(heap.len(), 1);
        assert_eq!(heap.get(&anchor).as_bytes().unwrap(), &[7; 32]);
    }
}

type Retained = Rc<RefCell<Option<ValuePointer>>>;

fn retain(
    _: FnInstance,
    params: FnParams,
    metadata: &ModuleMetadata<'_, Retained>,
    _: &mut VMContext,
    heap: &mut Heap,
) -> FnReturnType<Retained> {
    let mut value = params.remove(0);
    *metadata.metadata.borrow_mut() = Some(value.reference_pointer(heap));
    Ok(SysCallResult::None)
}

fn retaining_builder() -> EnvironmentBuilder<'static, Retained> {
    let mut builder = EnvironmentBuilder::default();
    // This environment is used only with caller-owned, same-thread heaps.
    // Escaped handles are accessed sequentially through their originating heap.
    builder.register_native_function(
        "retain",
        None,
        vec![("a", Type::Array(Box::new(Type::U64)))],
        unsafe { FunctionHandler::sync_unchecked(retain) },
        0,
        None,
    );
    builder
}

#[test]
fn externally_retained_vm_values_remain_accessible_after_completion() {
    let (module, environment) = compile(
        "entry main() { let a: u64[] = [1, 2, 3]; retain(a); a[1] = 42; return a[1]; }",
        retaining_builder(),
    );
    let retained = Retained::default();
    let mut heap = unsafe { Heap::new() };
    let mut vm = VM::default();
    vm.append_module(ModuleMetadata {
        module: (&module).into(),
        environment: (&environment).into(),
        metadata: (&retained).into(),
    })
    .unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    assert_eq!(
        block_on(vm.run_with_heap(&mut heap))
            .unwrap()
            .as_u64()
            .unwrap(),
        42
    );
    {
        let owner = retained.borrow();
        let array = heap.get(owner.as_ref().unwrap()).as_vec().unwrap();
        assert_eq!(array[1].as_ref(&heap).as_u64().unwrap(), 42);
        assert_eq!(heap.len(), 4);
    }
    drop(retained.borrow_mut().take());
    assert!(heap.is_empty());
}

fn resume(_: NativeState, _: &mut NativeScope<'_, Retained>) -> Result<(), EnvironmentError> {
    Ok(())
}

fn wait(
    scope: &mut NativeScope<'_, Retained>,
    _: &ModuleMetadata<'_, Retained>,
    _: &mut VMContext,
) -> Result<(), EnvironmentError> {
    let argument = scope.argument(0)?;
    scope.await_and_resume(Box::pin(pending()), &[argument], Box::new(()), resume);
    Ok(())
}

fn fail(
    _: &mut NativeScope<'_, Retained>,
    _: &ModuleMetadata<'_, Retained>,
    _: &mut VMContext,
) -> Result<(), EnvironmentError> {
    Err(EnvironmentError::Static("deliberate failure"))
}

fn unwind(
    _: &mut NativeScope<'_, Retained>,
    _: &ModuleMetadata<'_, Retained>,
    _: &mut VMContext,
) -> Result<(), EnvironmentError> {
    panic!("deliberate native panic")
}

#[test]
fn errors_cancellation_and_unwinding_preserve_caller_owned_values() {
    let scenarios: [(ScopedNativeFn<Retained>, &str); 3] =
        [(wait, "pending"), (fail, "error"), (unwind, "panic")];
    for (stop, expected) in scenarios {
        let mut builder = retaining_builder();
        builder.register_native_function(
            "stop",
            None,
            vec![("a", Type::Array(Box::new(Type::U64)))],
            FunctionHandler::Sync(stop),
            0,
            None,
        );
        let (module, environment) = compile("entry main() { let a: u64[] = [1, 2, 3]; retain(a); let b: u64[] = [4, 5, 6]; stop(b); return 0u64; }", builder);
        let retained = Retained::default();
        let mut heap = unsafe { Heap::new() };
        let anchor = heap.insert(PrimitiveInteger::U64(99).into());
        let mut vm = VM::default();
        vm.append_module(ModuleMetadata {
            module: (&module).into(),
            environment: (&environment).into(),
            metadata: (&retained).into(),
        })
        .unwrap();
        vm.invoke_entry_chunk(0).unwrap();
        let execution = catch_unwind(AssertUnwindSafe(|| {
            let mut future = Box::pin(vm.run_with_heap(&mut heap));
            let mut context = Context::from_waker(noop_waker_ref());
            future.as_mut().poll(&mut context)
        }));
        match (expected, execution) {
            ("pending", Ok(std::task::Poll::Pending)) => {}
            ("error", Ok(std::task::Poll::Ready(result))) => {
                assert!(matches!(
                    result,
                    Err(VMError::EnvironmentError(EnvironmentError::Static(
                        "deliberate failure"
                    )))
                ));
            }
            ("panic", Err(_)) => {}
            _ => panic!("unexpected outcome for {expected}"),
        }
        assert!(vm.is_idle());
        assert_eq!(heap.get(&anchor).as_u64().unwrap(), 99);
        {
            let owner = retained.borrow();
            assert_eq!(heap.get(owner.as_ref().unwrap()).as_vec().unwrap().len(), 3);
            assert_eq!(
                heap.len(),
                5,
                "only the anchor and externally retained graph should remain"
            );
        }
        drop(retained.borrow_mut().take());
        assert_eq!(heap.len(), 1);
    }
}
