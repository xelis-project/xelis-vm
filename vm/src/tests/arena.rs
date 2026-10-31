use futures::future::LocalBoxFuture;
use super::*;
use silex_compiler::Compiler;
use silex_lexer::Lexer;
use silex_parser::Parser;

/// Captured from the pre-heap VM at eab83a7c. Storage changes must preserve
/// gas, current memory accounting, and peak memory accounting independently.
#[test]
fn heap_preserves_legacy_gas_and_memory_accounting() {
    let cases: &[(&str, &str, (u64, usize, usize))] = &[
        ("test_array_push", r#"
        entry main() {
            let arr: u64[] = [1]
            arr.push(2)
            return arr.len() as u64
        }
    "#, (153, 70, 90)),
        ("test_array_remove", r#"
        entry main() {
            let arr: u64[] = [10, 20, 30]
            let removed = arr.remove(1)
            return removed
        }
    "#, (211, 110, 126)),
        ("test_map_insert_and_get", r#"
        entry main() {
            let mymap: map<u64, u64> = {}
            mymap.insert(1, 100)
            let v = mymap.get(1)
            if v.is_some() {
                return v.unwrap()
            }
            return 0
        }
    "#, (360, 121, 144)),
        ("test_map_entries_and_removals", r#"
        entry main() {
            let mymap: map<u64, u64> = {}
            mymap.insert(1, 10)
            mymap.insert(2, 20)
            let shifted = mymap.shift_remove(1).unwrap_or(0)
            let swapped = mymap.swap_remove(2).unwrap_or(0)

            mymap.insert(3, 30)
            mymap.insert(4, 40)

            let keys = mymap.keys()
            let values = mymap.values()
            let entries = mymap.entries()

            return shifted + swapped + keys.len() as u64 + values.len() as u64 + entries.len() as u64
        }
    "#, (1499, 556, 608)),
        ("test_iter_next", r#"
        entry main() {
            let arr: u64[] = [10, 20, 30];
            let it = arr.iter();
            let a: u64 = it.next().unwrap();
            let b: u64 = it.next().unwrap();
            return a + b
        }
    "#, (323, 118, 150)),
        ("test_iter_map_filter_chain", r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let result: u64[] = arr.iter()
                .map(|x: u64| { return x * 2 })
                .filter(|x: u64| { return x > 4 })
                .collect();
            return result.len() as u64
        }
    "#, (957, 396, 416)),
        ("test_iter_fold", r#"
        entry main() {
            let arr: u64[] = [1, 2, 3, 4, 5];
            let sum: u64 = arr.iter().fold(0u64, |acc: u64, x: u64| { return acc + x });
            return sum
        }
    "#, (688, 218, 250)),
        ("test_iter_zip", r#"
        entry main() {
            let a: u64[] = [1, 2, 3];
            let b: u64[] = [10, 20, 30];
            let it = a.iter().zip(b.iter());
            let result: any[] = it.collect();
            let first: u64[] = result[0];
            let second: u64[] = result[1];
            return first[0] + first[1] + second[0] + second[1]
        }
    "#, (1013, 656, 704)),
        ("test_iter_enumerate", r#"
        entry main() {
            let arr: u64[] = [10, 20, 30];
            let it = arr.iter().enumerate();
            let (idx, val): (u32, u64) = it.next().unwrap();
            return idx as u64 + val
        }
    "#, (352, 148, 180)),
        ("test_iter_static_unfold_collect", r#"
        entry main() {
            let generated: u64[] = Iterator::unfold(1u64, |state: u64| {
                if state <= 5u64 {
                    return (state * 2u64, state + 1u64)
                }
                return null
            }).collect();
            assert(generated == [2, 4, 6, 8, 10]);

            return generated[0] + generated[1] + generated[2] + generated[3] + generated[4]
        }
    "#, (1543, 829, 837)),
    ];
    for (name, code, expected) in cases {
        let builder = EnvironmentBuilder::<()>::default();
        let tokens = Lexer::new(code).into_iter().collect::<Result<Vec<_>, _>>().unwrap();
        let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
        let environment = builder.build();
        let module = Compiler::new(&program, &environment).compile().unwrap();
        ModuleValidator::new(&module, &environment).verify().unwrap();
        let mut vm = VM::default();
        vm.context_mut().set_gas_limit(100_000_000);
        vm.append_module(ModuleMetadata {
            module: (&module).into(),
            environment: (&environment).into(),
            metadata: (&()).into(),
        }).unwrap();
        vm.invoke_entry_chunk(0).unwrap();
        vm.run_blocking().unwrap_or_else(|error| panic!("{name}: {error:?}"));
        assert_eq!(
            (vm.context().current_gas_usage(), vm.context().current_memory_usage(), vm.context().peak_memory_usage()),
            *expected,
            "{name}",
        );
    }
}

#[test]
fn native_parameter_aliases_can_be_read_then_updated() {
    let mut runtime_heap = unsafe { Heap::new() };
    let heap = &mut runtime_heap;
    let id = heap.insert(PrimitiveInteger::U64(1).into());
    let reader: StackValue = id.into();
    let mut writer = reader.clone();
    let observed = reader.as_u64(&*heap).unwrap();
    *writer.as_mut(&mut *heap) = PrimitiveInteger::U64(observed + 1).into();
    assert_eq!(reader.as_u64(&*heap).unwrap(), 2);
}

#[test]
fn heap_errors_keep_their_category_at_vm_boundaries() {
    for error in [HeapError::InvalidPointer, HeapError::CapacityExceeded] {
        let cases = [
            VMError::from(error),
            VMError::from(ValueError::from(error)),
            VMError::from(EnvironmentError::from(error)),
            VMError::from(EnvironmentError::from(ValueError::from(error))),
            VMError::from(EnvironmentError::ValueError(ValueError::from(error))),
        ];
        for result in cases {
            assert!(matches!(result, VMError::HeapError(found) if found == error));
        }
    }
}


#[test]
fn execution_reclaims_temporary_heap_values_without_changing_the_result() {
    let code = r#"
        entry main() {
            let i: u64 = 0
            while i < 1000 {
                let values: u64[] = [i]
                values.push(i)
                i += 1
            }
            return i
        }
    "#;
    let builder = EnvironmentBuilder::<()>::default();
    let tokens = Lexer::new(code).into_iter().collect::<Result<Vec<_>, _>>().unwrap();
    let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
    let environment = builder.build();
    let module = Compiler::new(&program, &environment).compile().unwrap();
    let mut vm = VM::default();
    vm.context_mut().set_gas_limit(100_000_000);
    vm.append_module(ModuleMetadata {
        module: (&module).into(), environment: (&environment).into(), metadata: (&()).into(),
    }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    assert_eq!(vm.run_blocking().unwrap().as_u64().unwrap(), 1000);
    assert!(vm.is_idle());
}

#[test]
fn failed_executions_discard_unreleased_runtime_state() {
    let code = "entry main() { let values: u64[] = [1, 2, 3]; assert(values.len() == 0); return 0u64 }";
    let builder = EnvironmentBuilder::<()>::default();
    let tokens = Lexer::new(code).into_iter().collect::<Result<Vec<_>, _>>().unwrap();
    let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
    let environment = builder.build();
    let module = Compiler::new(&program, &environment).compile().unwrap();
    let mut vm = VM::default();
    vm.context_mut().set_gas_limit(100_000_000);
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    assert!(vm.run_blocking().is_err());
    assert!(vm.is_idle());
    assert!(vm.context().current_gas_usage() > 0);
}

#[test]
fn cancelling_a_pending_async_execution_discards_runtime_state() {
    use futures::{future::{pending}, task::noop_waker_ref};
    use std::{future::Future, task::{Context as TaskContext, Poll}};

    fn suspend<'a, 'ty, 'r>(
        _: FnInstance<'a>, _: FnParams<'a>, _: &'a ModuleMetadata<'_, ()>, _context: &'a mut VMContext<'ty, 'r>, heap: &'a mut Heap,
    ) -> LocalBoxFuture<'a, FnReturnType<()>> {
        assert!(!heap.is_empty(), "script must allocate before awaiting");
        Box::pin(pending())
    }
    let code = "entry main() { let values: u64[] = [1, 2, 3]; suspend(); return values.len() as u64 }";
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_native_function("suspend", None, vec![], unsafe { FunctionHandler::async_unchecked(suspend) }, 0, None);
    let tokens = Lexer::new(code).into_iter().collect::<Result<Vec<_>, _>>().unwrap();
    let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
    let environment = builder.build();
    let module = Compiler::new(&program, &environment).compile().unwrap();
    let mut vm = VM::default();
    vm.context_mut().set_gas_limit(100_000_000);
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    let mut future = Box::pin(vm.run());
    let mut context = TaskContext::from_waker(noop_waker_ref());
    assert!(matches!(future.as_mut().poll(&mut context), Poll::Pending));
    drop(future);
    assert!(vm.is_idle());
}

#[test]
fn run_local_heap_releases_missed_owners_and_cycles_between_runs() {
    use std::sync::{Arc, atomic::{AtomicUsize, Ordering}};

    #[derive(Debug, Clone, Opaque)]
    struct Probe(Arc<AtomicUsize>);
    impl Drop for Probe {
        fn drop(&mut self) { self.0.fetch_add(1, Ordering::Relaxed); }
    }
    struct Metadata { drops: Arc<AtomicUsize>, cycle: bool }
    fn leave_unused(_: FnInstance, _: FnParams, metadata: &ModuleMetadata<'_, Metadata>, _: &mut VMContext, heap: &mut Heap) -> FnReturnType<Metadata> {
        assert!(heap.is_empty(), "each run must start with a fresh heap");
        let payload = heap.insert(RuntimeValue::Opaque(OpaqueWrapper::new(Probe(metadata.metadata.drops.clone())).into()));
        if metadata.metadata.cycle {
            let root = heap.insert(RuntimeValue::default());
            let cycle = root.clone();
            *heap.get_mut(&root) = RuntimeValue::Object(vec![cycle, payload]);
            drop(root);
        }
        // Intentionally leave the payload or self-cycle without a release.
        Ok(SysCallResult::None)
    }
    let code = "entry main() { leave_unused(); return 0u64 }";
    for cycle in [false, true] {
        let mut builder = EnvironmentBuilder::<Metadata>::default();
        builder.register_native_function("leave_unused", None, vec![], unsafe { FunctionHandler::sync_unchecked(leave_unused) }, 0, None);
        let tokens = Lexer::new(code).into_iter().collect::<Result<Vec<_>, _>>().unwrap();
        let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
        let environment = builder.build();
        let module = Compiler::new(&program, &environment).compile().unwrap();
        let drops = Arc::new(AtomicUsize::new(0));
        let metadata = Metadata { drops: drops.clone(), cycle };
        let mut vm = VM::default();
        for iteration in 1..=3 {
            vm.context_mut().set_gas_limit(100_000_000);
            vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&metadata).into() }).unwrap();
            vm.invoke_entry_chunk(0).unwrap();
            assert_eq!(vm.run_blocking().unwrap().as_u64().unwrap(), 0);
            assert_eq!(drops.load(Ordering::Relaxed), iteration, "cycle={cycle}");
            vm.context_mut().reset_usage();
        }
    }
}

#[test]
fn queued_inputs_remain_owned_until_run() {
    let code = "entry main(values: u64[]) { return values[0] }";
    let builder = EnvironmentBuilder::<()>::default();
    let tokens = Lexer::new(code).into_iter().collect::<Result<Vec<_>, _>>().unwrap();
    let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
    let environment = builder.build();
    let module = Compiler::new(&program, &environment).compile().unwrap();
    let mut vm = VM::default();
    vm.context_mut().set_gas_limit(100_000_000);
    vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
    vm.invoke_chunk_with_args(0, vec![OwnedValue::Object(vec![OwnedValue::from(7u64)])].into_iter()).unwrap();
    assert_eq!(vm.pending_values.len(), 1);
    assert!(!vm.is_idle());
    assert_eq!(vm.context().current_memory_usage(), 41);
    assert_eq!(vm.context().current_gas_usage(), 41);
    assert_eq!(vm.run_blocking().unwrap().as_u64().unwrap(), 7);
    assert!(vm.pending_values.is_empty());
}

#[test]
fn returned_containers_outlive_the_run_and_vm() {
    let code = "entry main() -> u64[][] { return [[1u64, 2u64], [3u64, 4u64]] }";
    let builder = EnvironmentBuilder::<()>::default();
    let tokens = Lexer::new(code).into_iter().collect::<Result<Vec<_>, _>>().unwrap();
    let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
    let environment = builder.build();
    let module = Compiler::new(&program, &environment).compile().unwrap();
    let value = {
        let mut vm = VM::default();
        vm.context_mut().set_gas_limit(100_000_000);
        vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
        vm.invoke_entry_chunk(0).unwrap();
        vm.run_blocking().unwrap()
    };

    assert_eq!(value.as_vec().unwrap()[1].as_vec().unwrap()[0].as_u64().unwrap(), 3);
}


#[test]
fn pending_execution_migrates_with_its_heap_between_threads() {
    use futures::{executor::block_on, future::{poll_fn}, task::noop_waker_ref};
    use std::{future::Future, task::{Context as TaskContext, Poll}, thread};

    fn yield_once<'a, 'ty, 'r>(
        _: FnInstance<'a>,
        _: FnParams<'a>,
        _: &'a ModuleMetadata<'_, ()>,
        _: &'a mut VMContext<'ty, 'r>,
        heap: &'a mut Heap,
    ) -> LocalBoxFuture<'a, FnReturnType<()>> {
        assert!(!heap.is_empty());
        let mut yielded = false;
        Box::pin(poll_fn(move |context| {
            if !yielded {
                yielded = true;
                context.waker().wake_by_ref();
                Poll::Pending
            } else {
                Poll::Ready(Ok(SysCallResult::None))
            }
        }))
    }

    let code = "entry main() { let values: u64[] = [1, 2, 3]; yield_once(); return values[1] }";
    let mut builder = EnvironmentBuilder::<()>::default();
    builder.register_native_function("yield_once", None, vec![], unsafe { FunctionHandler::async_unchecked(yield_once) }, 0, None);
    let tokens = Lexer::new(code).into_iter().collect::<Result<Vec<_>, _>>().unwrap();
    let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
    let environment = builder.build();
    let module = Compiler::new(&program, &environment).compile().unwrap();
    let mut vm = VM::default();
    vm.context_mut().set_gas_limit(100_000_000);
    vm.append_module(ModuleMetadata {
        module: (&module).into(),
        environment: (&environment).into(),
        metadata: (&()).into(),
    }).unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    // Every handle remains in this future; the original thread parks
    // while the worker resumes it, then joins before accessing the VM again.
    let mut future = Box::pin(vm.run());
    let mut context = TaskContext::from_waker(noop_waker_ref());
    assert!(matches!(future.as_mut().poll(&mut context), Poll::Pending));
    let result = thread::scope(|scope| {
        scope.spawn(move || block_on(future)).join().unwrap()
    }).unwrap();
    assert_eq!(result.as_u64().unwrap(), 2);
    assert!(vm.is_idle());
}
