use std::{
    hash::{DefaultHasher, Hash, Hasher},
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    },
};

use silex_types::{
    traits::{DynEq, DynHash},
    GasError,
    GasMap,
    GasMapKey,
    GasMeter,
    Heap,
    Opaque,
    OwnedMap,
    OwnedValue,
    Primitive,
    PrimitiveInteger,
    RuntimeMap,
    RuntimeOpaqueWrapper,
    RuntimeValue,
    U256,
    ValueError,
};

#[derive(Debug, Default)]
struct Calls {
    hash_cost: AtomicUsize,
    eq_cost: AtomicUsize,
    hash: AtomicUsize,
    eq: AtomicUsize,
}

// The derive can price custom hashing and equality implementations.
#[derive(Debug, Clone, Opaque)]
#[opaque(
    hash_cost = {
        self.calls.hash_cost.fetch_add(1, Ordering::Relaxed);
        7
    },
    eq_cost = {
        self.calls.eq_cost.fetch_add(1, Ordering::Relaxed);
        11
    }
)]
struct Charged {
    id: u64,
    calls: Arc<Calls>,
}

impl DynHash for Charged {
    fn dyn_hash(&self, state: &mut dyn Hasher) {
        self.calls.hash.fetch_add(1, Ordering::Relaxed);
        // Force collisions to distinguish operation pricing from the actual
        // number of IndexMap equality callbacks.
        state.write_u8(0);
    }
}

impl DynEq for Charged {
    fn is_equal(&self, other: &dyn DynEq) -> bool {
        self.calls.eq.fetch_add(1, Ordering::Relaxed);
        other
            .as_any()
            .downcast_ref::<Self>()
            .is_some_and(|other| self.id == other.id)
    }
}

fn key(id: u64, calls: &Arc<Calls>) -> OwnedValue {
    OwnedValue::Object(vec![OwnedValue::from(Charged {
        id,
        calls: calls.clone(),
    })])
}

#[test]
fn omitted_derive_costs_add_no_charge() {
    #[derive(Debug, Clone, PartialEq, Eq, Hash, Opaque)]
    #[opaque(hash, eq)]
    struct Ordinary(u64);

    let mut heap = unsafe { Heap::new() };
    let mut gas = GasMeter::new(0);
    let key = OwnedValue::from(Ordinary(42));
    key.hash_in(&mut DefaultHasher::new(), &mut gas).unwrap();
    assert!(key.equals(&key.clone(), &mut gas).unwrap());
    let value = heap.import(&key);
    assert!(value.equals(&value.clone(), &heap, &mut gas).unwrap());
    assert_eq!(gas.used(), 0);
}

#[test]
fn derive_costs_apply_to_metered_hashes_and_comparisons() {
    #[derive(Debug, Clone, PartialEq, Eq, Hash, Opaque)]
    #[opaque(hash, eq, hash_cost = 7, eq_cost = self.0)]
    struct Key(u64);

    let key = OwnedValue::from(Key(11));
    let mut gas = GasMeter::new(18);
    let mut metered = DefaultHasher::new();
    key.hash_in(&mut metered, &mut gas).unwrap();
    assert_eq!(gas.used(), 7);
    assert!(key.equals(&key.clone(), &mut gas).unwrap());
    assert_eq!(gas.used(), 18);

    let mut unmetered = DefaultHasher::new();
    std::hash::Hash::hash(&key, &mut unmetered);
    assert_eq!(metered.finish(), unmetered.finish());

    let mut heap = unsafe { Heap::new() };
    let mut map = RuntimeMap::new();
    let value = heap.insert(PrimitiveInteger::U64(42).into());
    map.insert(key.clone(), value, &mut GasMeter::new(7)).unwrap();
    assert!(map.contains_key(&key, &mut GasMeter::new(18)).unwrap());
    assert!(matches!(
        map.contains_key(&key, &mut GasMeter::new(17)),
        Err(ValueError::Gas(GasError::NotEnoughGas { limit: 17, actual: 18 }))
    ));
}

#[test]
fn gas_map_accepts_custom_keys_and_values() {
    #[derive(PartialEq, Eq, Hash)]
    struct Key(u64);

    impl GasMapKey for Key {
        fn hash_in<H: Hasher>(&self, state: &mut H, gas: &mut GasMeter) -> Result<(), ValueError> {
            gas.charge(2)?;
            self.hash(state);
            Ok(())
        }

        fn equals(&self, other: &Self, gas: &mut GasMeter) -> Result<bool, ValueError> {
            gas.charge(3)?;
            Ok(self == other)
        }
    }

    let mut gas = GasMeter::new(7);
    let mut map = GasMap::from_entries([(Key(1), "original")], &mut gas).unwrap();
    assert_eq!(gas.used(), 2);
    assert_eq!(map.get(&Key(1), &mut gas).unwrap(), Some(&"original"));
    assert_eq!(gas.used(), 7);

    assert!(matches!(
        map.insert(Key(1), "replacement", &mut GasMeter::new(4)),
        Err(ValueError::Gas(GasError::NotEnoughGas { limit: 4, actual: 5 }))
    ));
    assert_eq!(map.into_iter().map(|(_, value)| value).collect::<Vec<_>>(), ["original"]);
}

#[test]
fn u256_hashing_and_equality_charge_four_gas() {
    let number = U256::MAX;
    let mut gas = GasMeter::new(4);
    let mut direct = GasMap::new();
    direct.insert(number, (), &mut gas).unwrap();
    assert_eq!(gas.used(), 4);
    assert!(matches!(
        direct.get(&number, &mut GasMeter::new(7)),
        Err(ValueError::Gas(GasError::NotEnoughGas { limit: 7, actual: 8 }))
    ));

    let key = OwnedValue::Object(vec![OwnedValue::from(number)]);
    let mut metered = DefaultHasher::new();
    let mut unmetered = DefaultHasher::new();
    gas.reset();
    key.hash_in(&mut metered, &mut gas).unwrap();
    key.hash(&mut unmetered);
    assert_eq!(metered.finish(), unmetered.finish());
    assert_eq!(gas.used(), 4);

    let mut heap = unsafe { Heap::new() };
    let runtime = heap.import(&key);
    assert!(matches!(
        runtime.equals(&runtime, &heap, &mut GasMeter::new(3)),
        Err(ValueError::Gas(GasError::NotEnoughGas { limit: 3, actual: 4 }))
    ));
    gas.reset();
    assert!(runtime.equals(&runtime, &heap, &mut gas).unwrap());
    assert_eq!(gas.used(), 4);
}

#[test]
fn u256_ranges_meter_each_endpoint_reached() {
    let range = |start: U256| {
        let start = Primitive::Integer(PrimitiveInteger::U256(start));
        let end = Primitive::Integer(PrimitiveInteger::U256(U256::MAX));
        OwnedValue::from(Primitive::new_range(start, end).unwrap())
    };
    let value = range(U256::ZERO);
    let mut gas = GasMeter::new(8);
    value.hash_in(&mut DefaultHasher::new(), &mut gas).unwrap();
    assert_eq!(gas.used(), 8);

    gas.reset();
    assert!(value.equals(&value, &mut gas).unwrap());
    assert_eq!(gas.used(), 8);

    gas.reset();
    assert!(!value.equals(&range(U256::ONE), &mut gas).unwrap());
    assert_eq!(gas.used(), 4);
}

#[test]
fn derive_equality_cost_alone_enables_runtime_equality() {
    #[derive(Debug, Clone, PartialEq, Opaque)]
    #[opaque(runtime, eq_cost = 3)]
    struct Local;

    let value = RuntimeOpaqueWrapper::local(Local);
    let mut gas = GasMeter::new(3);
    assert!(value.equals(&value.clone(), &mut gas).unwrap());
    assert_eq!(gas.used(), 3);
    assert!(value.as_hashable().is_none());
}

#[test]
fn nested_hashes_and_comparisons_charge_for_each_opaque_value() {
    let calls = Arc::new(Calls::default());
    let value = OwnedValue::Object(vec![key(1, &calls), key(2, &calls)]);
    let mut gas = GasMeter::new(36);
    value.hash_in(&mut DefaultHasher::new(), &mut gas).unwrap();
    assert_eq!(gas.used(), 14);
    assert!(value.equals(&value.clone(), &mut gas).unwrap());
    assert_eq!(gas.used(), 36);
    assert_eq!(calls.hash_cost.load(Ordering::Relaxed), 2);
    assert_eq!(calls.eq_cost.load(Ordering::Relaxed), 2);
    assert_eq!(calls.hash.load(Ordering::Relaxed), 2);
    assert_eq!(calls.eq.load(Ordering::Relaxed), 2);
}

#[test]
fn failed_prepaid_costs_stop_before_expensive_operations() {
    let calls = Arc::new(Calls::default());
    let value = key(1, &calls);
    let mut state = DefaultHasher::new();
    assert!(matches!(
        value.hash_in(&mut state, &mut GasMeter::new(6)),
        Err(ValueError::Gas(GasError::NotEnoughGas { .. }))
    ));
    assert!(matches!(
        value.equals(&value.clone(), &mut GasMeter::new(10)),
        Err(ValueError::Gas(GasError::NotEnoughGas { .. }))
    ));
    assert_eq!(calls.hash.load(Ordering::Relaxed), 0);
    assert_eq!(calls.eq.load(Ordering::Relaxed), 0);
}

#[test]
fn hashing_checks_nested_map_keys_as_it_goes() {
    #[derive(Debug, Clone, Hash, Opaque)]
    #[opaque(hash)]
    struct HashOnly;

    let calls = Arc::new(Calls::default());
    let invalid_key = OwnedValue::Object(vec![
        key(1, &calls),
        OwnedValue::from(HashOnly),
        key(2, &calls),
    ]);
    let mut map = OwnedMap::new();
    let mut gas = GasMeter::default();
    assert!(matches!(
        map.insert(invalid_key, OwnedValue::from(0u8), &mut gas),
        Err(ValueError::InvalidMapKey)
    ));
    assert!(map.is_empty());
    assert_eq!(gas.used(), 7);
    assert_eq!(calls.hash.load(Ordering::Relaxed), 1);
}

#[test]
fn runtime_map_meters_every_collision_and_reuses_hashes() {
    let calls = Arc::new(Calls::default());
    let mut heap = unsafe { Heap::new() };
    let mut map = RuntimeMap::new();
    let mut gas = GasMeter::default();
    for id in 0..16 {
        let value = heap.insert(PrimitiveInteger::U64(id).into());
        map.insert(key(id, &calls), value, &mut gas).unwrap();
    }
    assert_eq!(calls.hash.load(Ordering::Relaxed), 16);
    assert_eq!(calls.eq.load(Ordering::Relaxed), 16 * 15 / 2);
    assert_eq!(calls.hash_cost.load(Ordering::Relaxed), 16);
    assert_eq!(calls.eq_cost.load(Ordering::Relaxed), 16 * 15 / 2);
    let before = gas.used();
    assert!(!map.contains_key(&key(100, &calls), &mut gas).unwrap());
    assert_eq!(gas.used() - before, 7 + 16 * 11);
    assert_eq!(calls.hash.load(Ordering::Relaxed), 17);
    assert_eq!(
        calls.eq.load(Ordering::Relaxed),
        calls.eq_cost.load(Ordering::Relaxed)
    );

    // Cloning storage copies cached hashes; it performs no equality or hashing.
    let before = (
        calls.hash.load(Ordering::Relaxed),
        calls.eq.load(Ordering::Relaxed),
    );
    let cloned = map.clone();
    assert_eq!(cloned.len(), map.len());
    assert_eq!(
        before,
        (
            calls.hash.load(Ordering::Relaxed),
            calls.eq.load(Ordering::Relaxed)
        )
    );
}

#[test]
fn exhausted_map_operations_do_not_mutate_or_execute_unpaid_comparisons() {
    let calls = Arc::new(Calls::default());
    let mut heap = unsafe { Heap::new() };
    let original = heap.insert(RuntimeValue::default());
    let replacement = heap.insert(RuntimeValue::default());
    let mut map = RuntimeMap::new();
    map.insert(key(1, &calls), original.clone(), &mut GasMeter::default())
        .unwrap();
    for operation in 0..5 {
        let before_eq = calls.eq.load(Ordering::Relaxed);
        let mut gas = GasMeter::new(17); // hashing fits, equality does not
        let result = match operation {
            0 => map
                .insert(key(2, &calls), replacement.clone(), &mut gas)
                .map(|_| ()),
            1 => map
                .insert(key(1, &calls), replacement.clone(), &mut gas)
                .map(|_| ()),
            2 => map.shift_remove(&key(1, &calls), &mut gas).map(|_| ()),
            3 => map.swap_remove(&key(1, &calls), &mut gas).map(|_| ()),
            _ => map.get(&key(1, &calls), &mut gas).map(|_| ()),
        };
        assert!(matches!(
            result,
            Err(ValueError::Gas(GasError::NotEnoughGas { .. }))
        ));
        assert_eq!(gas.used(), 17);
        assert_eq!(calls.eq.load(Ordering::Relaxed), before_eq);
        assert_eq!(map.len(), 1);
        assert_eq!(map.values().next(), Some(&original));
    }

    let hashes = calls.hash.load(Ordering::Relaxed);
    assert!(map.get(&key(1, &calls), &mut GasMeter::new(6)).is_err());
    assert_eq!(calls.hash.load(Ordering::Relaxed), hashes);
}

#[test]
fn runtime_map_preserves_update_and_removal_order() {
    let mut heap = unsafe { Heap::new() };
    let mut gas = GasMeter::new(0); // default scalar operations add no gas
    let mut map = RuntimeMap::new();
    let values: Vec<_> = (0..4)
        .map(|_| heap.insert(RuntimeValue::default()))
        .collect();
    for id in 0..3u64 {
        map.insert(id.into(), values[id as usize].clone(), &mut gas)
            .unwrap();
    }
    assert_eq!(
        map.insert(1u64.into(), values[3].clone(), &mut gas)
            .unwrap(),
        Some(values[1].clone())
    );
    assert_eq!(
        map.shift_remove(&0u64.into(), &mut gas).unwrap(),
        Some(values[0].clone())
    );
    assert_eq!(
        map.keys().collect::<Vec<_>>(),
        vec![&OwnedValue::from(1u64), &OwnedValue::from(2u64)]
    );
    map.insert(3u64.into(), values[0].clone(), &mut gas)
        .unwrap();
    assert_eq!(
        map.swap_remove(&1u64.into(), &mut gas).unwrap(),
        Some(values[3].clone())
    );
    assert_eq!(
        map.keys().collect::<Vec<_>>(),
        vec![&OwnedValue::from(3u64), &OwnedValue::from(2u64)]
    );
    assert_eq!(gas.used(), 0);
}

#[test]
fn execution_conversions_meter_every_rebuilt_map() {
    let calls = Arc::new(Calls::default());
    let mut heap = unsafe { Heap::new() };
    let entries = (0..3)
        .map(|id| (key(id, &calls), heap.insert(RuntimeValue::default())))
        .collect::<Vec<_>>();
    let value = RuntimeValue::Map(Box::new(
        RuntimeMap::from_entries(entries, &mut GasMeter::default()).unwrap(),
    ));
    let expected = 3 * 7 + 3 * 11;
    let mut gas = GasMeter::new(expected);
    let before = (
        calls.hash.load(Ordering::Relaxed),
        calls.eq.load(Ordering::Relaxed),
    );
    let snapshot = heap.snapshot_in(&value, &mut gas).unwrap();
    assert_eq!(gas.used(), expected);
    assert_eq!(calls.hash.load(Ordering::Relaxed) - before.0, 3);
    assert_eq!(calls.eq.load(Ordering::Relaxed) - before.1, 3);

    let mut gas = GasMeter::new(expected);
    let imported = heap.import_in(&snapshot, &mut gas).unwrap();
    assert_eq!(gas.used(), expected);
    let mut gas = GasMeter::new(expected);
    let exported = heap.export_in(imported.into(), &mut gas).unwrap();
    assert_eq!(gas.used(), expected);
    let mut gas = GasMeter::new(expected);
    heap.import_owned_in(exported, &mut gas).unwrap();
    assert_eq!(gas.used(), expected);

    let before = calls.hash.load(Ordering::Relaxed);
    assert!(heap.snapshot_in(&value, &mut GasMeter::new(6)).is_err());
    assert_eq!(calls.hash.load(Ordering::Relaxed), before);
    assert_eq!(value.as_map().unwrap().len(), 3);
}

#[test]
fn local_opaque_equality_prepays_without_exporting() {
    use std::{cell::Cell, rc::Rc};

    #[derive(Debug, Clone, Opaque)]
    #[opaque(runtime, eq_cost = 13)]
    struct Local(Rc<Cell<u64>>);

    impl DynEq for Local {
        fn is_equal(&self, other: &dyn DynEq) -> bool {
            other
                .as_any()
                .downcast_ref::<Self>()
                .is_some_and(|other| self.0.get() == other.0.get())
        }
    }

    let mut heap = unsafe { Heap::new() };
    let value = heap.insert(RuntimeValue::Opaque(RuntimeOpaqueWrapper::local(Local(
        Rc::new(Cell::new(42)),
    ))));
    let nested = RuntimeValue::Object(vec![value]);
    assert!(nested
        .equals(&nested, &heap, &mut GasMeter::new(13))
        .unwrap());
    assert!(matches!(
        nested.equals(&nested, &heap, &mut GasMeter::new(12)),
        Err(ValueError::Gas(GasError::NotEnoughGas { .. }))
    ));
    assert!(matches!(
        heap.snapshot(&nested),
        Err(ValueError::HeapError(_))
    ));
}
