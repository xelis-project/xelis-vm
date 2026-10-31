use super::*;
use crate::stack::Stack;
use futures::{executor::block_on, future::{pending, poll_fn, BoxFuture, LocalBoxFuture}, task::noop_waker_ref};
use std::{future::Future, sync::{Arc, Weak, Mutex, atomic::{AtomicPtr, Ordering}}, task::{Context, Poll}, thread};
use silex_compiler::Compiler;
use silex_lexer::Lexer;
use silex_parser::Parser;

fn compile<M: 'static>(code: &str, builder: EnvironmentBuilder<'_, M>) -> (Module, Environment<M>) {
    let tokens = Lexer::new(code).into_iter().collect::<Result<Vec<_>, _>>().unwrap();
    let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
    let environment = builder.build();
    let module = Compiler::new(&program, &environment).compile().unwrap();
    ModuleValidator::new(&module, &environment).verify().unwrap();
    (module, environment)
}

fn pop(scope: &mut NativeScope<'_, ()>, _: &ModuleMetadata<'_, ()>, _: &mut VMContext) -> Result<(), EnvironmentError> {
    let instance = scope.instance()?;
    let value = scope.array_pop(instance)?;
    scope.return_value(value);
    Ok(())
}

#[test]
fn scoped_borrowed_returns_preserve_gas_and_memory_accounting() {
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_native_function("scoped_pop", Some(Type::Array(Box::new(Type::U64))), vec![], FunctionHandler::Sync(pop), 1, Some(Type::Optional(Box::new(Type::U64))));
    let mut results = Vec::new();
    for name in ["pop", "scoped_pop"] {
        let code = format!("entry main() {{ let a: u64[] = [1, 2, 3]; return a.{name}().unwrap() }}");
        let tokens = Lexer::new(&code).into_iter().collect::<Result<Vec<_>, _>>().unwrap();
        let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
        let environment = builder.environment();
        let module = Compiler::new(&program, environment).compile().unwrap();
        let mut vm = VM::default();
        vm.append_module(ModuleMetadata { module: (&module).into(), environment: environment.into(), metadata: (&()).into() }).unwrap();
        vm.invoke_entry_chunk(0).unwrap();
        let result = vm.run_blocking().unwrap();
        results.push((result, vm.context().current_gas_usage(), vm.context().current_memory_usage(), vm.context().peak_memory_usage()));
    }
    assert_eq!(results[0], results[1]);
}

fn resume_array(_: NativeState, scope: &mut NativeScope<'_, ()>) -> Result<(), EnvironmentError> {
    let response = scope.argument(0)?;
    let index = scope.as_u32(response)? as usize;
    let array = scope.retained(0)?;
    let result = scope.array_get(array, index)?;
    scope.return_value(result);
    Ok(())
}

fn suspend_array(scope: &mut NativeScope<'_, ()>, _: &ModuleMetadata<'_, ()>, _: &mut VMContext) -> Result<(), EnvironmentError> {
    let array = scope.argument(0)?;
    let mut yielded = false;
    let io = Box::pin(poll_fn(move |context| {
        if yielded {
            Poll::Ready(Ok(OwnedValue::from(1u32)))
        } else {
            yielded = true;
            context.waker().wake_by_ref();
            Poll::Pending
        }
    }));
    scope.await_and_resume(io, &[array], Box::new(()), resume_array);
    Ok(())
}

#[test]
fn scoped_io_resumes_with_retained_runtime_values_on_another_thread() {
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_native_function("suspend", None, vec![("a", Type::Array(Box::new(Type::U64)))], FunctionHandler::Sync(suspend_array), 0, Some(Type::U64));
    let (module, environment) = compile("entry main() { return suspend([11u64, 42u64]) }", builder);
    let mut vm = VM::default();
    fn public_bounds<T: Send + Sync>(_: &T) {
    }
    public_bounds(&vm);
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    let mut future = Box::pin(vm.run());
    let mut context = Context::from_waker(noop_waker_ref());
    assert!(future.as_mut().poll(&mut context).is_pending());
    let value = thread::scope(|threads| {
        threads.spawn(move || block_on(future)).join().unwrap()
    }).unwrap();
    assert_eq!(value.as_u64().unwrap(), 42);
    assert!(vm.backend().modules.is_empty());
}

#[derive(Debug, Clone, Opaque)]
struct Tracked(Arc<()>);

fn hold(scope: &mut NativeScope<'_, ()>, _: &ModuleMetadata<'_, ()>, _: &mut VMContext) -> Result<(), EnvironmentError> {
    let value = scope.argument(0)?;
    assert!(Arc::strong_count(&scope.opaque::<Tracked>(value)?.0) > 0);
    scope.await_and_resume(Box::pin(pending()), &[value], Box::new(()), resume_array);
    Ok(())
}

#[test]
fn cancelling_after_thread_migration_releases_every_retained_owner() {
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_opaque::<Tracked>("Tracked", true);
    builder.register_native_function("hold", None, vec![("value", Type::Any)], FunctionHandler::Sync(hold), 0, Some(Type::U64));
    let (module, environment) = compile("entry main(value: any) { return hold(value) }", builder);
    let tracked = Arc::new(());
    let weak: Weak<()> = Arc::downgrade(&tracked);
    let value = OwnedValue::from(Tracked(tracked));
    let mut vm = VM::default();
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_chunk_with_args(0, vec![value].into_iter()).unwrap();
    let mut future = Box::pin(vm.run());
    let mut context = Context::from_waker(noop_waker_ref());
    assert!(future.as_mut().poll(&mut context).is_pending());
    assert_eq!(weak.strong_count(), 1);
    thread::scope(|threads| {
        threads.spawn(move || drop(future)).join().unwrap();
    });
    assert!(weak.upgrade().is_none());
    assert!(vm.backend().modules.is_empty());
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_chunk_with_args(0, vec![OwnedValue::from(Tracked(Arc::new(())))].into_iter()).unwrap();
    let mut future = Box::pin(vm.run());
    assert!(future.as_mut().poll(&mut context).is_pending());
}

fn resume_closure(_: NativeState, scope: &mut NativeScope<'_, ()>) -> Result<(), EnvironmentError> {
    let result = scope.as_u64(scope.argument(0)?)?;
    let array = scope.retained(0)?;
    let element = scope.array_pop(array)?;
    scope.return_owned(OwnedValue::from(result + scope.as_u64(element)?))
}

fn invoke_closure(scope: &mut NativeScope<'_, ()>, _: &ModuleMetadata<'_, ()>, _: &mut VMContext) -> Result<(), EnvironmentError> {
    let function = scope.argument(0)?;
    let array = scope.argument(1)?;
    let element = scope.array_get(array, 0)?;
    scope.call_and_resume(function, &[element], &[array], Box::new(()), 1, resume_closure);
    Ok(())
}

#[test]
fn scoped_closure_callbacks_retain_their_runtime_graph() {
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_native_function("invoke", None, vec![
        ("function", Type::Closure(ClosureType::new(vec![Type::U64], Some(Type::U64)))),
        ("array", Type::Array(Box::new(Type::U64))),
    ], FunctionHandler::Sync(invoke_closure), 0, Some(Type::U64));
    let (module, environment) = compile("entry main() { return invoke(|x: u64| { return x + 1 }, [7u64]) }", builder);
    let mut vm = VM::default();
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    assert_eq!(vm.run_blocking().unwrap().as_u64().unwrap(), 15);
}

fn multiply<'a: 'r, 'ty: 'a, 'r>(_: &Backend<'a, 'ty, 'r, ()>, stack: &mut Stack<()>, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, ()>, VMError> {
    let right = stack.pop_stack()?;
    let left = stack.pop_stack()?;
    let value = left.as_u64(heap)? * right.as_u64(heap)?;
    stack.push_stack(PrimitiveInteger::U64(value).into())?;
    Ok(InstructionResult::Nothing)
}

#[test]
fn custom_instructions_receive_stack_arguments() {
    let (module, environment) = compile("entry main(a: u64, b: u64) { return a + b }", EnvironmentBuilder::<()>::default());
    let mut table = InstructionTable::default();
    unsafe { table.set_instruction(OpCode::Add, multiply, 1); }
    let mut vm = VM::new(table, VMContext::default());
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_chunk_with_args(0, vec![OwnedValue::from(6u64), OwnedValue::from(7u64)].into_iter()).unwrap();
    assert_eq!(vm.run_blocking().unwrap().as_u64().unwrap(), 42);
}

#[test]
fn custom_instruction_replacement_preserves_other_handlers_and_costs() {
    fn add<'a: 'r, 'ty: 'a, 'r>(_: &Backend<'a, 'ty, 'r, ()>, stack: &mut Stack<()>, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut VMContext<'ty, 'r>, heap: &mut Heap) -> Result<InstructionResult<'a, ()>, VMError> {
        let right = stack.pop_stack()?;
        let left = stack.pop_stack()?;
        let value = left.as_u64(heap)? + right.as_u64(heap)?;
        stack.push_stack(PrimitiveInteger::U64(value).into())?;
        Ok(InstructionResult::Nothing)
    }

    let (module, environment) = compile("entry main(a: u64, b: u64) { return (a + b) - (a - b) }", EnvironmentBuilder::<()>::default());
    let run = |add_cost, sub_cost| {
        let mut table = InstructionTable::default();
        unsafe {
            table.set_instruction(OpCode::Add, multiply, 1);
            table.set_instruction(OpCode::Sub, multiply, sub_cost);
            table.set_instruction(OpCode::Add, add, add_cost);
        }
        let mut vm = VM::new(table, VMContext::default());
        vm.append_module(ModuleMetadata {
            module: (&module).into(),
            environment: (&environment).into(),
            metadata: (&()).into(),
        }).unwrap();
        vm.invoke_chunk_with_args(0, vec![OwnedValue::from(6u64), OwnedValue::from(7u64)].into_iter()).unwrap();
        let value = vm.run_blocking().unwrap().as_u64().unwrap();
        (value, vm.context().current_gas_usage())
    };

    let (value, gas) = run(13, 11);
    let (repriced_value, repriced_gas) = run(17, 18);
    assert_eq!(value, 546);
    assert_eq!(repriced_value, value);
    assert_eq!(repriced_gas - gas, 4 + 2 * 7);
}

fn append_self(scope: &mut NativeScope<'_, ()>, _: &ModuleMetadata<'_, ()>, _context: &mut VMContext) -> Result<(), EnvironmentError> {
    let array = scope.argument(0)?;
    scope.array_push(array, array)?;
    scope.return_value(array);
    Ok(())
}

#[test]
fn scoped_array_insertion_cannot_create_a_reference_cycle() {
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_native_function("append_self", None, vec![("array", Type::Array(Box::new(Type::Any)))], FunctionHandler::Sync(append_self), 0, Some(Type::Array(Box::new(Type::Any))));
    let (module, environment) = compile("entry main() -> any[] { let a: any[] = [7u64]; return append_self(a) }", builder);
    let mut vm = VM::default();
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    let result = vm.run_blocking().unwrap();
    let array = result.as_vec().unwrap();
    assert_eq!(array[0].as_u64().unwrap(), 7);
    assert_eq!(array[1].as_vec().unwrap()[0].as_u64().unwrap(), 7);
}

fn owned_io<'a, 'ty, 'r>(_: Option<OwnedValue>, params: Vec<OwnedValue>, _: &'a ModuleMetadata<'_, ()>, _: &'a mut VMContext<'ty, 'r>) -> BoxFuture<'a, Result<OwnedValue, EnvironmentError>> {
    Box::pin(async move {
        Ok(OwnedValue::from(params[0].as_u64()? + 1))
    })
}

#[test]
fn async_native_functions_receive_only_portable_data() {
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_native_function("owned_io", None, vec![("value", Type::U64)], FunctionHandler::Async(owned_io), 0, Some(Type::U64));
    let (module, environment) = compile("entry main() { return owned_io(41u64) }", builder);
    let mut vm = VM::default();
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    assert_eq!(block_on(vm.run()).unwrap().as_u64().unwrap(), 42);
}

#[derive(Clone)]
struct ModuleTarget(Arc<Module>);

fn call_target(scope: &mut NativeScope<'_, ModuleTarget>, metadata: &ModuleMetadata<'_, ModuleTarget>, _: &mut VMContext) -> Result<(), EnvironmentError> {
    let argument = scope.argument(0)?;
    scope.call_module(metadata.metadata.0.clone(), Arc::new(ModuleTarget(metadata.metadata.0.clone())), None, 0, &[argument]);
    Ok(())
}

#[test]
fn scoped_module_calls_transfer_their_arguments_without_exporting_handles() {
    let (target, _) = compile("pub fn target(value: u64) -> u64 { return value * 2 }", EnvironmentBuilder::<ModuleTarget>::default());
    let metadata = ModuleTarget(Arc::new(target));
    let mut builder = EnvironmentBuilder::<ModuleTarget>::default();
    builder.register_native_function("call_target", None, vec![("value", Type::U64)], FunctionHandler::Sync(call_target), 0, Some(Type::U64));
    let (module, environment) = compile("entry main() { return call_target(21u64) }", builder);
    let mut vm = VM::default();
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&metadata).into() }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    assert_eq!(vm.run_blocking().unwrap().as_u64().unwrap(), 42);
}

#[test]
fn runtime_iterators_cannot_escape_into_owned_outputs_or_io() {
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_native_function("owned_io", None, vec![("value", Type::Any)], FunctionHandler::Async(owned_io), 0, Some(Type::U64));
    for code in [
        "entry main() -> Iterator<u64> { return Iterator::once(1u64) }",
        "entry main() { return owned_io(Iterator::once(1u64)) }",
    ] {
        let tokens = Lexer::new(code).into_iter().collect::<Result<Vec<_>, _>>().unwrap();
        let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
        let environment = builder.environment();
        let module = Compiler::new(&program, environment).compile().unwrap();
        let mut vm = VM::default();
        vm.append_module(ModuleMetadata { module: (&module).into(), environment: environment.into(), metadata: (&()).into() }).unwrap();
        vm.invoke_entry_chunk(0).unwrap();
        assert!(matches!(block_on(vm.run()), Err(VMError::HeapError(HeapError::NonPortableValue))));
        assert!(vm.is_idle());
    }
}

fn produce_buffer(scope: &mut NativeScope<'_, AtomicPtr<u8>>, metadata: &ModuleMetadata<'_, AtomicPtr<u8>>, _: &mut VMContext) -> Result<(), EnvironmentError> {
    let mut bytes = vec![7; 1024];
    metadata.metadata.store(bytes.as_mut_ptr(), Ordering::Relaxed);
    scope.return_owned(OwnedValue::Bytes(bytes))
}

fn echo_buffer<'a, 'ty, 'r>(_: Option<OwnedValue>, mut params: Vec<OwnedValue>, metadata: &'a ModuleMetadata<'_, AtomicPtr<u8>>, _: &'a mut VMContext<'ty, 'r>) -> BoxFuture<'a, Result<OwnedValue, EnvironmentError>> {
    Box::pin(async move {
        let value = params.remove(0);
        assert_eq!(value.as_bytes()?.as_ptr(), metadata.metadata.load(Ordering::Relaxed).cast_const());
        Ok(value)
    })
}

#[test]
fn unique_buffers_move_through_native_owned_io_without_copying() {
    let mut builder = EnvironmentBuilder::<AtomicPtr<u8>>::default();
    builder.register_native_function("produce", None, vec![], FunctionHandler::Sync(produce_buffer), 0, Some(Type::Bytes));
    builder.register_native_function("echo", None, vec![("value", Type::Bytes)], FunctionHandler::Async(echo_buffer), 0, Some(Type::Bytes));
    let (module, environment) = compile("entry main() -> bytes { return echo(produce()) }", builder);
    let metadata = AtomicPtr::default();
    let mut vm = VM::default();
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&metadata).into() }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    let value = block_on(vm.run()).unwrap();
    assert_eq!(value.as_bytes().unwrap().as_ptr(), metadata.load(Ordering::Relaxed).cast_const());
}

fn delayed_double<'a, 'ty, 'r>(_: Option<OwnedValue>, params: Vec<OwnedValue>, _: &'a ModuleMetadata<'_, ()>, _: &'a mut VMContext<'ty, 'r>) -> BoxFuture<'a, Result<OwnedValue, EnvironmentError>> {
    let mut yielded = false;
    Box::pin(poll_fn(move |context| {
        if yielded {
            Poll::Ready(params[0].as_u64().map(|value| OwnedValue::from(value * 2)).map_err(Into::into))
        } else {
            yielded = true;
            context.waker().wake_by_ref();
            Poll::Pending
        }
    }))
}

#[test]
fn pending_iterator_callbacks_migrate_with_their_local_opaque_state() {
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_native_function("delayed_double", None, vec![("value", Type::U64)], FunctionHandler::Async(delayed_double), 0, Some(Type::U64));
    let (module, environment) = compile("entry main() { let values: u64[] = [11, 21, 31]; let result = values.iter().map(|x: u64| { return delayed_double(x) }).collect(); return result[1] }", builder);
    let mut vm = VM::default();
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    let mut future = Box::pin(vm.run());
    let mut context = Context::from_waker(noop_waker_ref());
    // Suspension happens inside the mapper, with runtime-only iterator and
    // callback owners alive in the private execution.
    assert!(future.as_mut().poll(&mut context).is_pending());
    let result = thread::scope(|threads| {
        threads.spawn(move || block_on(future)).join().unwrap()
    }).unwrap();
    assert_eq!(result.as_u64().unwrap(), 42);
    assert!(vm.is_idle());
}

#[test]
fn pending_iterator_callbacks_can_migrate_at_every_suspension() {
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_native_function("delayed_double", None, vec![("value", Type::U64)], FunctionHandler::Async(delayed_double), 0, Some(Type::U64));
    let (module, environment) = compile("entry main() -> u64[] { return [11u64, 21u64, 31u64].iter().map(|x: u64| { return delayed_double(x) }).collect() }", builder);
    let mut vm = VM::default();
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    let mut future = Box::pin(vm.run());
    let mut suspensions = 0;
    let result = loop {
        // Each poll, including final cleanup, runs on a newly spawned thread.
        // Spawn/join synchronizes access to the complete runtime owner family.
        let (next, result) = thread::scope(|threads| {
            threads.spawn(move || {
                let mut context = Context::from_waker(noop_waker_ref());
                let result = future.as_mut().poll(&mut context);
                (future, result)
            }).join().unwrap()
        });
        future = next;
        match result {
            Poll::Pending => {
                suspensions += 1;
                assert!(suspensions <= 3);
            }
            Poll::Ready(result) => break result.unwrap(),
        }
    };
    drop(future);
    assert_eq!(suspensions, 3);
    let values: Vec<_> = result.as_vec().unwrap().iter().map(|value| value.as_u64().unwrap()).collect();
    assert_eq!(values, [22, 42, 62]);
    assert!(vm.is_idle());
}

#[test]
fn concurrent_vms_share_modules_without_sharing_runtime_mutations() {
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_native_function("delayed_double", None, vec![("value", Type::U64)], FunctionHandler::Async(delayed_double), 0, Some(Type::U64));
    let (module, environment) = compile("entry main(seed: u64) -> u64[] { let values: u64[] = [1, 2]; values.push(seed); return values.iter().map(|x: u64| { return delayed_double(x) }).collect() }", builder);
    let barrier = std::sync::Barrier::new(4);
    thread::scope(|threads| {
        let workers: Vec<_> = (0..4u64).map(|worker| {
            let (module, environment, barrier) = (&module, &environment, &barrier);
            threads.spawn(move || {
                let mut vm = VM::default();
                barrier.wait();
                for iteration in 0..4 {
                    let seed = worker * 10 + iteration;
                    vm.context_mut().reset_usage();
                    vm.append_module(ModuleMetadata { module: module.into(), environment: environment.into(), metadata: (&()).into() }).unwrap();
                    vm.invoke_chunk_with_args(0, vec![OwnedValue::from(seed)].into_iter()).unwrap();
                    let result = block_on(vm.run()).unwrap();
                    let values: Vec<_> = result.as_vec().unwrap().iter().map(|value| value.as_u64().unwrap()).collect();
                    assert_eq!(values, [2, 4, seed * 2]);
                    assert!(vm.is_idle());
                }
            })
        }).collect();
        for worker in workers {
            worker.join().unwrap();
        }
    });
}

type IoResponse = Mutex<Option<(
    futures::channel::oneshot::Receiver<OwnedValue>,
    futures::channel::oneshot::Sender<()>,
)>>;

fn resume_io(_: NativeState, scope: &mut NativeScope<'_, IoResponse>) -> Result<(), EnvironmentError> {
    // The VM value survives while only the portable response and waker are
    // accessible to the I/O thread.
    let retained = scope.retained(0)?;
    assert!(Arc::strong_count(&scope.opaque::<Tracked>(retained)?.0) > 0);
    scope.return_value(scope.argument(0)?);
    Ok(())
}

fn await_io(scope: &mut NativeScope<'_, IoResponse>, metadata: &ModuleMetadata<'_, IoResponse>, _: &mut VMContext) -> Result<(), EnvironmentError> {
    let (receiver, armed) = metadata.metadata.lock().unwrap().take().unwrap();
    let mut receiver = Box::pin(receiver);
    let mut armed = Some(armed);
    let mut polled = false;
    let retained = scope.argument(0)?;
    scope.await_and_resume(Box::pin(poll_fn(move |context| {
        let result = receiver.as_mut().poll(context);
        if polled {
            // The worker's executor has registered its waker with the receiver.
            if let Some(armed) = armed.take() {
                armed.send(()).unwrap();
            }
        }
        polled = true;
        result.map(|response| response.map_err(|_| EnvironmentError::Static("I/O sender dropped")))
    })), &[retained], Box::new(()), resume_io);
    Ok(())
}

#[test]
fn external_io_wakes_a_migrated_execution_with_retained_values() {
    let mut builder = EnvironmentBuilder::<IoResponse>::default();
    builder.register_native_function("await_io", None, vec![("value", Type::Any)], FunctionHandler::Sync(await_io), 0, Some(Type::U64));
    let (module, environment) = compile("entry main(value: any) { return await_io(value) }", builder);
    let (sender, receiver) = futures::channel::oneshot::channel();
    let (armed, waiting) = futures::channel::oneshot::channel();
    let metadata = Mutex::new(Some((receiver, armed)));
    let tracked = Arc::new(());
    let weak = Arc::downgrade(&tracked);
    let mut vm = VM::default();
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&metadata).into() }).unwrap();
    vm.invoke_chunk_with_args(0, vec![OwnedValue::from(Tracked(tracked))].into_iter()).unwrap();
    let mut future = Box::pin(vm.run());
    let mut context = Context::from_waker(noop_waker_ref());
    assert!(future.as_mut().poll(&mut context).is_pending());
    let result = thread::scope(|threads| {
        let worker = threads.spawn(move || block_on(future));
        block_on(waiting).unwrap();
        threads.spawn(move || sender.send(OwnedValue::from(42u64)).unwrap()).join().unwrap();
        worker.join().unwrap().unwrap()
    });
    assert_eq!(result.as_u64().unwrap(), 42);
    assert!(weak.upgrade().is_none());
    assert!(vm.is_idle());
}

fn panic_on_resume(_: NativeState, _: &mut NativeScope<'_, ()>) -> Result<(), EnvironmentError> {
    panic!("native callback panic");
}

fn suspend_then_panic(scope: &mut NativeScope<'_, ()>, _: &ModuleMetadata<'_, ()>, _: &mut VMContext) -> Result<(), EnvironmentError> {
    let value = scope.argument(0)?;
    let mut yielded = false;
    scope.await_and_resume(Box::pin(poll_fn(move |context| {
        if yielded {
            Poll::Ready(Ok(OwnedValue::default()))
        } else {
            yielded = true;
            context.waker().wake_by_ref();
            Poll::Pending
        }
    })), &[value], Box::new(()), panic_on_resume);
    Ok(())
}

#[test]
fn panic_after_migration_releases_runtime_state_and_allows_vm_reuse() {
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_native_function("suspend_then_panic", None, vec![("value", Type::Any)], FunctionHandler::Sync(suspend_then_panic), 0, None);
    let (module, environment) = compile("entry main(value: any) { suspend_then_panic(value); return 0u64 }", builder);
    let tracked = Arc::new(());
    let weak = Arc::downgrade(&tracked);
    let mut vm = VM::default();
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_chunk_with_args(0, vec![OwnedValue::from(Tracked(tracked))].into_iter()).unwrap();
    let mut future = Box::pin(vm.run());
    let mut context = Context::from_waker(noop_waker_ref());
    assert!(future.as_mut().poll(&mut context).is_pending());
    let panic = thread::scope(|threads| threads.spawn(move || block_on(future)).join().unwrap_err());
    assert_eq!(panic.downcast_ref::<&str>(), Some(&"native callback panic"));
    assert!(weak.upgrade().is_none());
    assert!(vm.is_idle());

    let (module, environment) = compile("entry main() { return 42u64 }", EnvironmentBuilder::<()>::default());
    vm.context_mut().reset_usage();
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    assert_eq!(block_on(vm.run()).unwrap().as_u64().unwrap(), 42);
    assert!(vm.is_idle());
}


#[derive(Debug, Clone, Opaque)]
struct ArgumentDrop {
    id: u8,
    dropped: Arc<Mutex<Vec<u8>>>,
}

impl Drop for ArgumentDrop {
    fn drop(&mut self) {
        self.dropped.lock().unwrap().push(self.id);
    }
}

fn argument_stack(dropped: &Arc<Mutex<Vec<u8>>>) -> Stack<()> {
    let mut stack = Stack::new();
    for id in 0..4 {
        let value = RuntimeValue::from(ArgumentDrop { id, dropped: Arc::clone(dropped) });
        stack.push_stack(value.into()).unwrap();
    }
    stack
}

#[test]
fn extracting_arguments_preserves_source_and_drop_order() {
    let heap = unsafe { Heap::new() };
    let dropped = Arc::new(Mutex::new(Vec::new()));
    let mut stack = argument_stack(&dropped);
    {
        let arguments = stack.pop_arguments(3).unwrap();
        let ids: Vec<_> = arguments.iter().map(|value| value.as_opaque_type::<ArgumentDrop>(&heap).unwrap().id).collect();
        assert_eq!(ids, [1, 2, 3]);
        assert_eq!(stack.count(), 1);
        assert!(dropped.lock().unwrap().is_empty());
    }
    assert_eq!(*dropped.lock().unwrap(), [1, 2, 3]);
}

#[test]
fn argument_underflow_consumes_and_drops_available_operands_in_source_order() {
    for checked in [false, true] {
        let dropped = Arc::new(Mutex::new(Vec::new()));
        let mut stack = argument_stack(&dropped);
        let result = if checked {
            stack.pop_arguments_with(5, |_| Ok(()))
        } else {
            stack.pop_arguments(5)
        };
        assert!(matches!(result, Err(VMError::EmptyStack)));
        assert_eq!(stack.count(), 0);
        assert_eq!(*dropped.lock().unwrap(), [0, 1, 2, 3]);
    }
}

#[test]
fn failed_argument_checks_stop_before_the_next_pop_and_preserve_drop_order() {
    let heap = unsafe { Heap::new() };
    let dropped = Arc::new(Mutex::new(Vec::new()));
    let mut stack = argument_stack(&dropped);
    let mut checked = Vec::new();
    let result = stack.pop_arguments_with(4, |value| {
        let id = value.as_opaque_type::<ArgumentDrop>(&heap)?.id;
        checked.push(id);
        if id == 2 {
            return Err(VMError::IllegalState);
        }
        Ok(())
    });
    assert!(matches!(result, Err(VMError::IllegalState)));
    assert_eq!(checked, [3, 2]);
    assert_eq!(stack.count(), 2);
    assert_eq!(*dropped.lock().unwrap(), [2, 3]);
}


#[test]
fn synchronous_native_errors_drop_arguments_before_instance() {
    fn fail(_: FnInstance, _: FnParams, _: &ModuleMetadata<'_, ()>, _: &mut VMContext, _: &mut Heap) -> FnReturnType<()> {
        Err(EnvironmentError::Static("native failure"))
    }

    let dropped = Arc::new(Mutex::new(Vec::new()));
    let instance = RuntimeValue::from(ArgumentDrop { id: 0, dropped: Arc::clone(&dropped) });
    let argument = RuntimeValue::from(ArgumentDrop { id: 1, dropped: Arc::clone(&dropped) });
    let function = NativeFunction::new(None, true, vec![Type::Any], unsafe { FunctionHandler::sync_unchecked(fail) }, 0, None);
    let module = Module::new();
    let environment = Environment::<()>::new();
    let metadata = ModuleMetadata {
        module: (&module).into(),
        environment: (&environment).into(),
        metadata: (&()).into(),
    };
    let mut heap = unsafe { Heap::new() };
    let result = function.call_function(vec![instance.into(), argument.into()], &metadata, &mut VMContext::default(), &mut heap);
    assert!(matches!(result, Err(EnvironmentError::Static("native failure"))));
    assert_eq!(*dropped.lock().unwrap(), [1, 0]);
}

#[test]
fn asynchronous_native_errors_drop_arguments_before_instance() {
    fn fail<'a, 'ty, 'r>(_: FnInstance<'a>, _: FnParams<'a>, _: &'a ModuleMetadata<'_, Arc<Mutex<Vec<u8>>>>, _: &'a mut VMContext<'ty, 'r>, _: &'a mut Heap) -> LocalBoxFuture<'a, FnReturnType<Arc<Mutex<Vec<u8>>>>> {
        Box::pin(async { Err(EnvironmentError::Static("native failure")) })
    }

    fn invoke<'a: 'r, 'ty: 'a, 'r>(backend: &Backend<'a, 'ty, 'r, Arc<Mutex<Vec<u8>>>>, _: &mut Stack<Arc<Mutex<Vec<u8>>>>, _: &mut ChunkManager, _: &mut ChunkReader<'_>, _: &mut VMContext<'ty, 'r>, _: &mut Heap) -> Result<InstructionResult<'a, Arc<Mutex<Vec<u8>>>>, VMError> {
        let dropped = Arc::clone(&backend.current()?.metadata);
        let instance = RuntimeValue::from(ArgumentDrop { id: 0, dropped: Arc::clone(&dropped) });
        let argument = RuntimeValue::from(ArgumentDrop { id: 1, dropped });
        let FunctionHandler::Unchecked(handler) = (unsafe { FunctionHandler::async_unchecked(fail) }) else {
            unreachable!();
        };
        Ok(InstructionResult::AsyncCall {
            ptr: AsyncHandler::Unchecked(handler),
            instance: true,
            params: vec![instance.into(), argument.into()],
        })
    }

    let dropped = Arc::new(Mutex::new(Vec::new()));
    let (module, environment) = compile("entry main() { return 0u64 }", EnvironmentBuilder::<Arc<Mutex<Vec<u8>>>>::default());
    let mut table = InstructionTable::default();
    unsafe {
        table.set_instruction(OpCode::Return, invoke, 1);
    }
    let mut vm = VM::new(table, VMContext::default());
    vm.append_module(ModuleMetadata {
        module: (&module).into(),
        environment: (&environment).into(),
        metadata: (&dropped).into(),
    }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    assert!(matches!(vm.run_blocking(), Err(VMError::EnvironmentError(EnvironmentError::Static("native failure")))));
    assert_eq!(*dropped.lock().unwrap(), [1, 0]);
}
