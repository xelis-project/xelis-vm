use std::{
    alloc::{GlobalAlloc, Layout, System},
    future::Future,
    sync::atomic::{AtomicUsize, Ordering},
    task::{Context, Poll},
};
use futures::{executor::block_on, future::pending, task::noop_waker_ref};
use silex_builder::EnvironmentBuilder;
use silex_compiler::Compiler;
use silex_lexer::Lexer;
use silex_parser::Parser;
use xelis_vm::{Environment, EnvironmentError, FunctionHandler, Heap, Module, ModuleMetadata, NativeScope, NativeState, OwnedValue, RuntimeValue, Type, VM, VMContext};
struct CountingAllocator;
static LIVE: AtomicUsize = AtomicUsize::new(0);
static PEAK: AtomicUsize = AtomicUsize::new(0);

fn allocated(bytes: usize) {
    let live = LIVE.fetch_add(bytes, Ordering::Relaxed) + bytes;
    PEAK.fetch_max(live, Ordering::Relaxed);
}

unsafe impl GlobalAlloc for CountingAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc(layout) };
        if !pointer.is_null() {
            allocated(layout.size());
        }
        pointer
    }

    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        let pointer = unsafe { System.alloc_zeroed(layout) };
        if !pointer.is_null() {
            allocated(layout.size());
        }
        pointer
    }

    unsafe fn dealloc(&self, pointer: *mut u8, layout: Layout) {
        unsafe { System.dealloc(pointer, layout) };
        LIVE.fetch_sub(layout.size(), Ordering::Relaxed);
    }

    unsafe fn realloc(&self, pointer: *mut u8, layout: Layout, size: usize) -> *mut u8 {
        let result = unsafe { System.realloc(pointer, layout, size) };
        if !result.is_null() {
            if size >= layout.size() {
                allocated(size - layout.size());
            } else {
                LIVE.fetch_sub(layout.size() - size, Ordering::Relaxed);
            }
        }
        result
    }
}

#[global_allocator]
static ALLOCATOR: CountingAllocator = CountingAllocator;

fn live() -> usize {
    LIVE.load(Ordering::Relaxed)
}

fn prepare(code: &str, builder: EnvironmentBuilder<()>) -> (Module, Environment<()>) {
    let tokens = Lexer::new(code).into_iter().collect::<Result<Vec<_>, _>>().unwrap();
    let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
    let environment = builder.build();
    let module = Compiler::new(&program, &environment).compile().unwrap();
    (module, environment)
}

fn resume(_: NativeState, _: &mut NativeScope<'_, ()>) -> Result<(), EnvironmentError> {
    Ok(())
}

fn suspend(scope: &mut NativeScope<'_, ()>, _: &ModuleMetadata<'_, ()>, _: &mut VMContext) -> Result<(), EnvironmentError> {
    let value = scope.argument(0)?;
    scope.await_and_resume(Box::pin(pending()), &[value], Box::new(()), resume);
    Ok(())
}

fn main() {
    println!("Counting live Rust allocation bytes, including VM bookkeeping.");
    block_on(async {});
    // The low-level heap is used sequentially with handles from this heap only.
    {
        let mut heap = unsafe { Heap::new() };
        heap.insert(RuntimeValue::Bytes(vec![0; 1024]));
    }
    let base = live();
    {
        let mut heap = unsafe { Heap::new() };
        let anchor = heap.insert(RuntimeValue::Bytes(vec![7; 4096]));
        let mut plateau = None;
        for _ in 0..200 {
            {
                let values: Vec<_> = (0..1024).map(|_| heap.insert(RuntimeValue::Bytes(vec![1; 1024]))).collect();
                assert_eq!(heap.len(), values.len() + 1);
            }
            assert_eq!(heap.len(), 1);
            let bytes = live();
            assert_eq!(*plateau.get_or_insert(bytes), bytes);
        }
        assert_eq!(heap.get(&anchor).as_bytes().unwrap().len(), 4096);
    }
    assert_eq!(live(), base);
    println!("PASS: 204800 temporary buffers reclaimed during one live heap; stable plateau and zero residual bytes.");

    let base = live();
    for _ in 0..100 {
        let mut heap = unsafe { Heap::new() };
        let root = heap.insert(RuntimeValue::default());
        let payload = heap.insert(RuntimeValue::Bytes(vec![3; 65536]));
        *heap.get_mut(&root) = RuntimeValue::Object(vec![root.clone(), payload]);
        heap.clear();
        assert!(heap.is_empty());
    }
    assert_eq!(live(), base);
    println!("PASS: 100 raw-extension cycles released on clear, with no residual bytes after handle drop.");

    for (name, code) in [
        ("success", "entry main() -> u64[][] { return [[1u64, 2u64], [3u64, 4u64]] }"),
        ("error", "fn fail() -> u64 { assert(false); return 0u64 } entry main() { let a: u64[] = [1, 2, 3]; return fail() }"),
    ] {
        let (module, environment) = prepare(code, EnvironmentBuilder::default());
        let entry = (0..module.chunks().len()).find(|&id| module.is_entry_chunk(id)).unwrap() as u16;
        let mut vm = VM::default();
        let mut baseline = None;
        for _ in 0..201 {
            vm.context_mut().reset_usage();
            vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
            vm.invoke_entry_chunk(entry).unwrap();
            {
                let result = vm.run_blocking();
                assert_eq!(result.is_ok(), name == "success");
            }
            assert!(vm.is_idle());
            let bytes = live();
            assert_eq!(*baseline.get_or_insert(bytes), bytes);
        }
        println!("PASS: 200 repeated {name} runs return to their warmed live-allocation baseline.");
    }

    let mut builder = EnvironmentBuilder::default();
    builder.register_native_function("suspend", None, vec![("value", Type::Any)], FunctionHandler::Sync(suspend), 0, None);
    let (module, environment) = prepare("entry main(value: any) { suspend(value); return 0u64 }", builder);
    let mut vm = VM::default();
    let mut baseline = None;
    for _ in 0..201 {
        vm.context_mut().reset_usage();
        vm.append_module(ModuleMetadata { module: (&module).into(), environment: (&environment).into(), metadata: (&()).into() }).unwrap();
        vm.invoke_chunk_with_args(0, vec![OwnedValue::Object(vec![OwnedValue::Bytes(vec![1; 65536])])].into_iter()).unwrap();
        {
            let mut future = Box::pin(vm.run());
            let mut context = Context::from_waker(noop_waker_ref());
            assert!(matches!(future.as_mut().poll(&mut context), Poll::Pending));
        }
        assert!(vm.is_idle());
        let bytes = live();
        assert_eq!(*baseline.get_or_insert(bytes), bytes);
    }
    println!("PASS: 200 cancellations with retained runtime buffers return to their warmed live-allocation baseline.");
    println!("Peak tracked live allocation: {} bytes", PEAK.load(Ordering::Relaxed));
}
