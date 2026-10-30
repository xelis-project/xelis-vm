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
        vm.context_mut().resources_mut().set_gas_limit(100_000_000);
        vm.append_module(ModuleMetadata {
            module: (&module).into(),
            environment: (&environment).into(),
            metadata: (&()).into(),
        }).unwrap();
        vm.invoke_entry_chunk(0).unwrap();
        vm.run_blocking().unwrap();
        assert_eq!(
            (vm.context().resources().current_gas_usage(), vm.context().resources().current_memory_usage(), vm.context().resources().peak_memory_usage()),
            *expected,
            "{name}",
        );
    }
}

#[test]
fn native_parameter_aliases_can_be_read_then_updated() {
    let mut context = VMContext::new();
    let id = context.heap_mut().insert(PrimitiveInteger::U64(1).into()).unwrap();
    let reader: StackValue = id.into();
    let mut writer = reader.clone();
    let observed = reader.as_u64(context.heap()).unwrap();
    *writer.as_mut(context.heap_mut()) = PrimitiveInteger::U64(observed + 1).into();
    assert_eq!(reader.as_u64(context.heap()).unwrap(), 2);
}

#[test]
fn native_returns_reject_out_of_bounds_heap_handles() {
    let id = {
        let mut heap = Heap::new();
        heap.insert(PrimitiveInteger::U64(1).into()).unwrap()
    };
    let mut context = VMContext::new();
    let mut stack = crate::stack::Stack::<()>::new();
    let environment = EnvironmentBuilder::<()>::default().build();
    let module = Module::new();
    let metadata = ModuleMetadata {
        module: (&module).into(),
        environment: (&environment).into(),
        metadata: (&()).into(),
    };
    let result = handle_perform_syscall(&mut stack, &mut context, SysCallResult::Return(id.into()), &metadata);
    assert!(matches!(result, Err(VMError::HeapError(HeapError::InvalidPointer))));
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
