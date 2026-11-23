use std::{
    hash::Hasher,
    sync::atomic::{AtomicUsize, Ordering},
};

use silex_builder::EnvironmentBuilder;
use silex_compiler::Compiler;
use silex_lexer::Lexer;
use silex_parser::Parser;
use xelis_vm::{
    traits::{DynEq, DynHash},
    EnvironmentError,
    FunctionHandler,
    GasMeter,
    ModuleMetadata,
    Opaque,
    OwnedValue,
    SysCallResult,
    Type,
    VMError,
    ValueError,
    VM,
};

static HASH_CALLS: AtomicUsize = AtomicUsize::new(0);
static EQ_CALLS: AtomicUsize = AtomicUsize::new(0);
static HASH_COSTS: AtomicUsize = AtomicUsize::new(0);
static EQ_COSTS: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Clone, Opaque)]
#[opaque(hash, eq)]
struct Probe {
    id: u64,
    hash_cost: u64,
    eq_cost: u64,
}

impl DynHash for Probe {
    fn pre_hash_cost(&self, gas: &mut GasMeter) -> Result<(), ValueError> {
        gas.charge(self.hash_cost)?;
        HASH_COSTS.fetch_add(1, Ordering::Relaxed);
        Ok(())
    }

    fn dyn_hash(&self, state: &mut dyn Hasher) {
        HASH_CALLS.fetch_add(1, Ordering::Relaxed);
        // Deliberately collide to exercise every candidate comparison.
        state.write_u8(0);
    }
}

impl DynEq for Probe {
    fn pre_eq_cost(&self, gas: &mut GasMeter) -> Result<(), ValueError> {
        gas.charge(self.eq_cost)?;
        EQ_COSTS.fetch_add(1, Ordering::Relaxed);
        Ok(())
    }

    fn is_equal(&self, other: &dyn DynEq) -> bool {
        EQ_CALLS.fetch_add(1, Ordering::Relaxed);
        other
            .as_any()
            .downcast_ref::<Self>()
            .is_some_and(|other| self.id == other.id)
    }
}

fn execute(
    code: &str,
    hash_cost: u64,
    eq_cost: u64,
    limit: u64,
) -> (Result<OwnedValue, VMError>, u64) {
    for counter in [&HASH_CALLS, &EQ_CALLS, &HASH_COSTS, &EQ_COSTS] {
        counter.store(0, Ordering::Relaxed);
    }
    let mut builder = EnvironmentBuilder::<(u64, u64)>::default();
    let ty = Type::Opaque(builder.register_opaque::<Probe>("Probe", true));
    builder.register_native_function(
        "probe",
        None,
        vec![("id", Type::U64)],
        unsafe {
            FunctionHandler::<(u64, u64)>::sync_unchecked(|_, params, metadata, _, heap| {
                Ok(SysCallResult::Return(
                    Probe {
                        id: params[0].as_u64(heap)?,
                        hash_cost: metadata.metadata.0,
                        eq_cost: metadata.metadata.1,
                    }
                    .into(),
                ))
            })
        },
        0,
        Some(ty),
    );
    let tokens = Lexer::new(code).collect::<Result<Vec<_>, _>>().unwrap();
    let (program, _) = Parser::with(tokens.into_iter(), &builder).parse().unwrap();
    let environment = builder.build();
    let module = Compiler::new(&program, &environment).compile().unwrap();
    let rates = (hash_cost, eq_cost);
    let mut vm = VM::default();
    vm.append_module(ModuleMetadata {
        module: (&module).into(),
        environment: (&environment).into(),
        metadata: (&rates).into(),
    })
    .unwrap();
    vm.invoke_entry_chunk(0).unwrap();
    vm.context_mut().set_gas_limit(limit);
    let result = vm.run_blocking();
    (result, vm.context().current_gas_usage())
}

// One test keeps the operation counters isolated across VM executions.
#[test]
fn programs_pay_for_collisions_and_result_export() {
    let returning_map = "entry main() -> map<Probe, u64> { let m: map<Probe, u64> = { probe(1u64): 1u64 }; return m }";
    for (code, hashes, comparisons) in [
        (returning_map, 2, 0),
        ("entry main() { let m: map<Probe, u64> = {}; m.insert(probe(1u64), 1u64); m.insert(probe(2u64), 2u64); m.insert(probe(3u64), 3u64); assert(!m.contains_key(probe(4u64))); return 0 }", 4, 6),
        ("entry main() -> map<Probe, u64> { let m: map<Probe, u64> = { probe(1u64): 1u64, probe(2u64): 2u64, probe(3u64): 3u64 }; return m }", 6, 6),
        ("entry main() -> map<u64, map<Probe, u64>> { let m: map<Probe, u64> = { probe(1u64): 1u64 }; let outer: map<u64, map<Probe, u64>> = { 0u64: m }; return outer }", 2, 0),
    ] {
        let (result, baseline) = execute(code, 0, 0, u64::MAX);
        result.unwrap();
        let (result, charged) = execute(code, 7, 11, u64::MAX);
        result.unwrap();
        assert_eq!(charged - baseline, hashes as u64 * 7 + comparisons as u64 * 11, "{code}");
        assert_eq!(HASH_CALLS.load(Ordering::Relaxed), hashes);
        assert_eq!(HASH_COSTS.load(Ordering::Relaxed), hashes);
        assert_eq!(EQ_CALLS.load(Ordering::Relaxed), comparisons);
        assert_eq!(EQ_COSTS.load(Ordering::Relaxed), comparisons);
    }

    let (_, baseline) = execute(returning_map, 0, 0, u64::MAX);
    // The constructor fits. The second hash, during final export, must fail
    // before invoking the dynamic implementation rather than run unpaid.
    let (result, used) = execute(returning_map, 7, 11, baseline + 7);
    assert!(matches!(
        result,
        Err(VMError::EnvironmentError(
            EnvironmentError::NotEnoughGas { .. }
        ))
    ));
    assert_eq!(used, baseline + 7);
    assert_eq!(HASH_CALLS.load(Ordering::Relaxed), 1);
}
