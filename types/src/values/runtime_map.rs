use std::{
    collections::hash_map::RandomState,
    hash::{BuildHasher, Hasher},
};

use indexmap::{
    map::{
        raw_entry_v1::RawEntryMut,
        IntoIter,
        Iter,
        Keys,
        RawEntryApiV1,
        Values,
        ValuesMut,
    },
    IndexMap,
};

use crate::{
    GasMeter,
    OwnedValue,
    ValueError,
    ValuePointer,
};

/// An insertion-ordered execution map. Key operations always use the caller's
/// gas meter; the backing map and mutable keys are deliberately not exposed.
/// Cloning copies IndexMap's cached hashes without hashing or comparing keys.
#[derive(Debug, Clone, Default)]
pub struct RuntimeMap {
    entries: IndexMap<OwnedValue, ValuePointer>,
}

/// Compute the query hash once, charging each opaque value before hashing it.
/// Raw-entry lookups and insertions reuse this hash rather than computing it again.
#[inline]
fn key_hash<V>(
    map: &IndexMap<OwnedValue, V>,
    key: &OwnedValue,
    gas: &mut GasMeter,
) -> Result<u64, ValueError> {
    if !key.is_hashable() {
        return Err(ValueError::InvalidMapKey);
    }

    let mut state = map.hasher().build_hasher();
    key.hash_in(&mut state, gas)?;

    Ok(state.finish())
}

/// Adapt fallible metered equality to IndexMap's boolean match predicate.
/// Returning true on error stops probing immediately; callers must propagate
/// the captured error before reading or modifying the returned entry.
#[inline]
fn compare_key(
    key: &OwnedValue,
    candidate: &OwnedValue,
    gas: &mut GasMeter,
    error: &mut Option<ValueError>,
) -> bool {
    match key.equals(candidate, gas) {
        Ok(equal) => equal,
        Err(err) => {
            *error = Some(err);
            true
        }
    }
}

/// Probe using a hash whose computation has already been paid for.
/// Each candidate comparison charges equality through `compare_key`.
#[inline]
fn entry<'a, V>(
    map: &'a mut IndexMap<OwnedValue, V>,
    key: &OwnedValue,
    hash: u64,
    gas: &mut GasMeter,
) -> Result<RawEntryMut<'a, OwnedValue, V, RandomState>, ValueError> {
    let mut error = None;
    let entry = map.raw_entry_mut_v1().from_hash(hash, |candidate| {
        compare_key(key, candidate, gas, &mut error)
    });

    if let Some(error) = error {
        return Err(error);
    }

    Ok(entry)
}

/// Meter an insertion or update, including every collision comparison.
/// Portable-data conversions use this same path when rebuilding owned maps.
#[inline]
pub(crate) fn insert_metered<V>(
    map: &mut IndexMap<OwnedValue, V>,
    key: OwnedValue,
    value: V,
    gas: &mut GasMeter,
) -> Result<Option<V>, ValueError> {
    let hash = key_hash(map, &key, gas)?;

    match entry(map, &key, hash, gas)? {
        RawEntryMut::Occupied(mut entry) => Ok(Some(entry.insert(value))),
        RawEntryMut::Vacant(entry) => {
            // A regular insertion would compute the hash a second time.
            entry.insert_hashed_nocheck(hash, key, value);
            Ok(None)
        }
    }
}

impl RuntimeMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            entries: IndexMap::with_capacity(capacity),
        }
    }

    /// Build a map using the same metering as individual insertions.
    pub fn from_entries(
        entries: impl IntoIterator<Item = (OwnedValue, ValuePointer)>,
        gas: &mut GasMeter,
    ) -> Result<Self, ValueError> {
        let entries = entries.into_iter();
        let mut map = Self::with_capacity(entries.size_hint().0);

        for (key, value) in entries {
            map.insert(key, value, gas)?;
        }

        Ok(map)
    }

    /// Build host data outside execution. VM execution must use `from_entries`.
    pub(crate) fn from_entries_unmetered(
        entries: impl IntoIterator<Item = (OwnedValue, ValuePointer)>,
    ) -> Self {
        Self {
            entries: entries.into_iter().collect(),
        }
    }

    /// Hash the query once and meter each candidate comparison until it matches.
    #[inline]
    pub fn get<'a>(
        &'a self,
        key: &OwnedValue,
        gas: &mut GasMeter,
    ) -> Result<Option<&'a ValuePointer>, ValueError> {
        let hash = key_hash(&self.entries, key, gas)?;
        let mut error = None;
        let found = self.entries.raw_entry_v1().from_hash(hash, |candidate| {
            compare_key(key, candidate, gas, &mut error)
        });

        if let Some(error) = error {
            return Err(error);
        }

        Ok(found.map(|(_, value)| value))
    }

    #[inline]
    pub fn contains_key(&self, key: &OwnedValue, gas: &mut GasMeter) -> Result<bool, ValueError> {
        Ok(self.get(key, gas)?.is_some())
    }

    #[inline]
    pub fn insert(
        &mut self,
        key: OwnedValue,
        value: ValuePointer,
        gas: &mut GasMeter,
    ) -> Result<Option<ValuePointer>, ValueError> {
        insert_metered(&mut self.entries, key, value, gas)
    }

    #[inline]
    pub fn shift_remove(
        &mut self,
        key: &OwnedValue,
        gas: &mut GasMeter,
    ) -> Result<Option<ValuePointer>, ValueError> {
        let hash = key_hash(&self.entries, key, gas)?;

        match entry(&mut self.entries, key, hash, gas)? {
            RawEntryMut::Occupied(entry) => Ok(Some(entry.shift_remove())),
            RawEntryMut::Vacant(_) => Ok(None),
        }
    }

    #[inline]
    pub fn swap_remove(
        &mut self,
        key: &OwnedValue,
        gas: &mut GasMeter,
    ) -> Result<Option<ValuePointer>, ValueError> {
        let hash = key_hash(&self.entries, key, gas)?;

        match entry(&mut self.entries, key, hash, gas)? {
            RawEntryMut::Occupied(entry) => Ok(Some(entry.swap_remove())),
            RawEntryMut::Vacant(_) => Ok(None),
        }
    }

    pub fn len(&self) -> usize {
        self.entries.len()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn clear(&mut self) {
        self.entries.clear();
    }

    // Iteration and value mutation neither hash nor compare keys.
    pub fn iter<'a>(&'a self) -> Iter<'a, OwnedValue, ValuePointer> {
        self.entries.iter()
    }

    pub fn keys<'a>(&'a self) -> Keys<'a, OwnedValue, ValuePointer> {
        self.entries.keys()
    }

    pub fn values<'a>(&'a self) -> Values<'a, OwnedValue, ValuePointer> {
        self.entries.values()
    }

    pub fn values_mut<'a>(&'a mut self) -> ValuesMut<'a, OwnedValue, ValuePointer> {
        self.entries.values_mut()
    }

    pub(crate) fn get_index_mut<'a>(
        &'a mut self,
        index: usize,
    ) -> Option<(&'a OwnedValue, &'a mut ValuePointer)> {
        self.entries
            .get_index_mut(index)
            .map(|(key, value)| (&*key, value))
    }
}

impl IntoIterator for RuntimeMap {
    type Item = (OwnedValue, ValuePointer);
    type IntoIter = IntoIter<OwnedValue, ValuePointer>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.into_iter()
    }
}
