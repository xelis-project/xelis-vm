use std::{
    future::Future,
    panic::{catch_unwind, AssertUnwindSafe},
    task::{Context, Poll},
    thread::{self, ThreadId},
};

use futures::{
    executor::block_on,
    future::{pending, poll_fn},
    task::noop_waker_ref,
};
use silex_compiler::Compiler;
use silex_lexer::Lexer;
use silex_parser::Parser;
use silex_types::GasMeter;

use super::*;

#[derive(Default)]
struct Storage {
    retained: Option<ValuePointer>,
    exported: Option<OwnedValue>,
    events: Vec<Option<bool>>,
    cleanup_had_live_value: bool,
    empty_after_cleanup: bool,
    finish_thread: Option<ThreadId>,
}

// SAFETY: these tests access Storage only through the exclusively borrowed VM
// context while run_with_scope owns the complete runtime graph. The finish
// callback releases its handle before the VM borrow ends, including on unwind.
unsafe impl Send for Storage {}
unsafe impl Sync for Storage {}
tid! { impl<'a> TidAble<'a> for Storage }

fn compile(code: &str, stop: Option<ScopedNativeFn<()>>) -> (Module, Environment<()>) {
    let mut builder = EnvironmentBuilder::default();
    // SAFETY: only used with run_with_scope and finish below. The context is
    // exclusively borrowed during execution and finish releases its owner.
    builder.register_native_function(
        "retain", None, vec![("a", Type::Array(Box::new(Type::U64)))],
        unsafe { FunctionHandler::sync_unchecked(retain) }, 0, None,
    );
    if let Some(stop) = stop {
        builder.register_native_function(
            "stop", None, vec![("a", Type::Array(Box::new(Type::U64)))],
            FunctionHandler::Sync(stop), 0, None,
        );
    }
    let tokens = Lexer::new(code).into_iter().collect::<Result<Vec<_>, _>>().unwrap();
    let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
    let environment = builder.build();
    let module = Compiler::new(&program, &environment).compile().unwrap();
    ModuleValidator::new(&module, &environment).verify().unwrap();
    (module, environment)
}

fn queue<'a: 'r, 'ty: 'a, 'r>(vm: &mut VM<'a, 'ty, 'r, ()>, module: &'a Module, environment: &'a Environment<()>) {
    vm.append_module(ModuleMetadata {
        module: module.into(),
        environment: environment.into(),
        metadata: (&()).into(),
    }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
}

fn retain<'metadata>(_: FnInstance, params: FnParams, _: &ModuleMetadata<'metadata, ()>, context: &mut VMContext, heap: &mut Heap) -> FnReturnType<()> {
    let mut value = params.remove(0);
    context.get_mut::<Storage>().unwrap().retained = Some(value.reference_pointer(heap));
    Ok(SysCallResult::None)
}

fn finish(context: &mut VMContext, heap: &Heap, result: Option<&Result<OwnedValue, VMError>>) -> Result<(), VMError> {
    let storage = context.get_mut::<Storage>().unwrap();
    storage.events.push(result.map(Result::is_ok));
    storage.finish_thread = Some(thread::current().id());
    if result.is_some() {
        let owner = storage.retained.as_ref().unwrap();
        storage.exported = Some(heap.snapshot(heap.get(owner))?);
    } else {
        // Take ownership first so snapshot errors also release the handle.
        if let Some(owner) = storage.retained.take() {
            let value = heap.snapshot(heap.get(&owner))?;
            assert_eq!(heap.len(), 4, "VM-only roots must drop before scope cleanup");
            storage.cleanup_had_live_value = true;
            storage.exported = Some(value);
            drop(owner);
        }
        storage.empty_after_cleanup = heap.is_empty();
    }
    Ok(())
}

fn assert_released<'vm, 'vm2, 'vm3>(vm: &VM<'vm, 'vm2, 'vm3, ()>, events: &[Option<bool>]) {
    assert!(vm.is_idle());
    let storage = vm.context().get::<Storage>().unwrap();
    assert!(storage.retained.is_none());
    assert_eq!(storage.events, events);
    assert!(storage.cleanup_had_live_value);
    assert!(storage.empty_after_cleanup);
    let values = storage.exported.as_ref().unwrap().as_vec().unwrap();
    assert_eq!(values[1].as_u64().unwrap(), 42);
}

const CODE: &str = "entry main() { let a: u64[] = [1, 2, 3]; retain(a); a[1] = 42; return a[1]; }";
const STOP_CODE: &str = "entry main() { let a: u64[] = [1, 2, 3]; retain(a); a[1] = 42; let b: u64[] = [4, 5, 6]; stop(b); return a[1]; }";

#[test]
fn scope_exports_mutations_and_releases_handles_before_heap_destruction() {
    let (module, environment) = compile(CODE, None);
    let mut vm = VM::default();
    // A fresh scope can be used for each successive run of the same VM.
    for _ in 0..2 {
        vm.context_mut().insert(Storage::default());
        queue(&mut vm, &module, &environment);
        // SAFETY: finish releases the only context-retained runtime owner.
        let value = block_on(unsafe { vm.run_with_scope(finish) }).unwrap();
        assert_eq!(value.as_u64().unwrap(), 42);
        assert_released(&vm, &[Some(true), None]);
    }
}

fn resume<'call>(_: NativeState, _: &mut NativeScope<'call, ()>, _: &mut GasMeter) -> Result<(), EnvironmentError> { Ok(()) }

fn wait<'call, 'metadata>(scope: &mut NativeScope<'call, ()>, _: &ModuleMetadata<'metadata, ()>, _: &mut VMContext) -> Result<(), EnvironmentError> {
    let argument = scope.argument(0)?;
    scope.await_and_resume(Box::pin(pending()), &[argument], Box::new(()), resume);
    Ok(())
}

fn fail<'call, 'metadata>(_: &mut NativeScope<'call, ()>, _: &ModuleMetadata<'metadata, ()>, _: &mut VMContext) -> Result<(), EnvironmentError> {
    Err(EnvironmentError::Static("deliberate failure"))
}

fn unwind<'call, 'metadata>(_: &mut NativeScope<'call, ()>, _: &ModuleMetadata<'metadata, ()>, _: &mut VMContext) -> Result<(), EnvironmentError> {
    panic!("deliberate native panic")
}

#[test]
fn scope_cleanup_runs_after_execution_errors_cancellation_and_unwind() {
    let scenarios: [(ScopedNativeFn<()>, &str); 3] = [(wait, "pending"), (fail, "error"), (unwind, "panic")];
    for (stop, expected) in scenarios {
        let (module, environment) = compile(STOP_CODE, Some(stop));
        let mut vm = VM::default();
        vm.context_mut().insert(Storage::default());
        queue(&mut vm, &module, &environment);
        let execution = catch_unwind(AssertUnwindSafe(|| {
            // SAFETY: finish releases context-retained owners on every exit.
            let mut future = Box::pin(unsafe { vm.run_with_scope(finish) });
            let mut context = Context::from_waker(noop_waker_ref());
            future.as_mut().poll(&mut context)
        }));
        match (expected, execution) {
            ("pending", Ok(Poll::Pending)) | ("panic", Err(_)) => assert_released(&vm, &[None]),
            ("error", Ok(Poll::Ready(result))) => {
                assert!(matches!(result, Err(VMError::EnvironmentError(EnvironmentError::Static("deliberate failure")))));
                assert_released(&vm, &[Some(false), None]);
            }
            _ => panic!("unexpected outcome for {expected}"),
        }
    }
}

fn yield_once<'call, 'metadata>(scope: &mut NativeScope<'call, ()>, _: &ModuleMetadata<'metadata, ()>, _: &mut VMContext) -> Result<(), EnvironmentError> {
    let mut yielded = false;
    let future = poll_fn(move |context| {
        if yielded {
            Poll::Ready(Ok(OwnedValue::default()))
        } else {
            yielded = true;
            context.waker().wake_by_ref();
            Poll::Pending
        }
    });
    let argument = scope.argument(0)?;
    scope.await_and_resume(Box::pin(future), &[argument], Box::new(()), resume);
    Ok(())
}

#[test]
fn scoped_future_can_complete_or_be_cancelled_after_thread_migration() {
    let (module, environment) = compile(STOP_CODE, Some(yield_once));
    for complete in [true, false] {
        let mut vm = VM::default();
        vm.context_mut().insert(Storage::default());
        queue(&mut vm, &module, &environment);
        // SAFETY: no alias is accessed outside the VM borrow and finish releases
        // all context handles on the destination thread before the borrow ends.
        let mut future = Box::pin(unsafe { vm.run_with_scope(finish) });
        let mut context = Context::from_waker(noop_waker_ref());
        assert!(future.as_mut().poll(&mut context).is_pending());
        thread::scope(|threads| {
            threads.spawn(move || {
                if complete {
                    assert_eq!(block_on(future).unwrap().as_u64().unwrap(), 42);
                } else {
                    drop(future);
                }
            }).join().unwrap();
        });
        assert_released(&vm, if complete { &[Some(true), None] } else { &[None] });
        assert_ne!(vm.context().get::<Storage>().unwrap().finish_thread.unwrap(), thread::current().id());
    }
}

#[test]
fn finish_errors_and_panics_still_run_scope_cleanup() {
    let (module, environment) = compile(CODE, None);
    for panic in [true, false] {
        let mut vm = VM::default();
        vm.context_mut().insert(Storage::default());
        queue(&mut vm, &module, &environment);
        let execution = catch_unwind(AssertUnwindSafe(|| {
            // SAFETY: the None path calls finish even when Some fails or panics.
            block_on(unsafe { vm.run_with_scope(|context, heap, result| {
                finish(context, heap, result)?;
                if result.is_some() {
                    if panic { panic!("deliberate finish panic"); }
                    return Err(EnvironmentError::Static("finish failure").into());
                }
                Ok(())
            }) })
        }));
        if panic {
            assert!(execution.is_err());
        } else {
            assert!(matches!(execution.unwrap(), Err(VMError::EnvironmentError(EnvironmentError::Static("finish failure")))));
        }
        assert_released(&vm, &[Some(true), None]);
    }
}

#[test]
fn cleanup_errors_are_ignored_after_releasing_handles() {
    let (module, environment) = compile(CODE, None);
    let mut vm = VM::default();
    vm.context_mut().insert(Storage::default());
    queue(&mut vm, &module, &environment);
    // SAFETY: cleanup releases the handle before returning its ignored error.
    let value = block_on(unsafe { vm.run_with_scope(|context, heap, result| {
        finish(context, heap, result)?;
        if result.is_none() { return Err(EnvironmentError::Static("cleanup failure").into()); }
        Ok(())
    }) }).unwrap();
    assert_eq!(value.as_u64().unwrap(), 42);
    assert_released(&vm, &[Some(true), None]);
}

#[test]
fn dropping_an_unpolled_scope_leaves_queued_work_intact() {
    let (module, environment) = compile(CODE, None);
    let mut vm = VM::default();
    vm.context_mut().insert(Storage::default());
    queue(&mut vm, &module, &environment);
    // SAFETY: no scope starts until polling; finish releases owners if it does.
    drop(unsafe { vm.run_with_scope(finish) });
    assert!(!vm.is_idle());
    assert!(vm.context().get::<Storage>().unwrap().events.is_empty());
    // SAFETY: finish releases the only context-retained runtime owner.
    assert_eq!(block_on(unsafe { vm.run_with_scope(finish) }).unwrap().as_u64().unwrap(), 42);
    assert_released(&vm, &[Some(true), None]);
}
