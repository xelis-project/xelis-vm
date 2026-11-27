use std::{
    collections::hash_map::RandomState,
    hash::{BuildHasher, Hash, Hasher},
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

use crate::{GasMeter, ValueError};

/// A key that can hash and compare itself with an execution gas meter.
/// Successful operations must agree with `Hash` and `Eq`.
/// The default implementations use those traits without charging gas.
pub trait GasMapKey: Hash + Eq {
    /// Hash this key, charging any gas due before the work it covers.
    fn hash_in<H: Hasher>(&self, state: &mut H, _: &mut GasMeter) -> Result<(), ValueError> {
        self.hash(state);
        Ok(())
    }

    /// Compare two keys, charging any gas due before the work it covers.
    fn equals(&self, other: &Self, _: &mut GasMeter) -> Result<bool, ValueError> {
        Ok(self == other)
    }
}

/// An insertion-ordered map that passes the caller's gas meter to key operations.
/// The backing map and mutable keys are private.
/// Cloning copies IndexMap's cached hashes without hashing or comparing keys.
#[derive(Debug, Clone)]
pub struct GasMap<K, V> {
    entries: IndexMap<K, V>,
}

/// Compute and meter the query hash once.
/// Raw-entry lookups and insertions reuse this hash rather than computing it again.
#[inline]
fn key_hash<K: GasMapKey, V>(
    map: &IndexMap<K, V>,
    key: &K,
    gas: &mut GasMeter,
) -> Result<u64, ValueError> {
    let mut state = map.hasher().build_hasher();
    key.hash_in(&mut state, gas)?;

    Ok(state.finish())
}

/// Adapt fallible metered equality to IndexMap's boolean match predicate.
/// Returning true on error stops probing immediately; callers must propagate
/// the captured error before reading or modifying the returned entry.
#[inline]
fn compare_key<K: GasMapKey>(
    key: &K,
    candidate: &K,
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
fn entry<'a, K: GasMapKey, V>(
    map: &'a mut IndexMap<K, V>,
    key: &K,
    hash: u64,
    gas: &mut GasMeter,
) -> Result<RawEntryMut<'a, K, V, RandomState>, ValueError> {
    let mut error = None;
    let entry = map.raw_entry_mut_v1().from_hash(hash, |candidate| {
        compare_key(key, candidate, gas, &mut error)
    });

    if let Some(error) = error {
        return Err(error);
    }

    Ok(entry)
}

impl<K, V> Default for GasMap<K, V> {
    fn default() -> Self {
        Self {
            entries: IndexMap::new(),
        }
    }
}

impl<K: GasMapKey, V> GasMap<K, V> {
    /// Build a map using the same metering as individual insertions.
    pub fn from_entries(
        entries: impl IntoIterator<Item = (K, V)>,
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
    pub(crate) fn from_entries_unmetered(entries: impl IntoIterator<Item = (K, V)>) -> Self {
        Self {
            entries: entries.into_iter().collect(),
        }
    }

    /// Hash the query once and meter each candidate comparison until it matches.
    #[inline]
    pub fn get<'a>(&'a self, key: &K, gas: &mut GasMeter) -> Result<Option<&'a V>, ValueError> {
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
    pub fn contains_key(&self, key: &K, gas: &mut GasMeter) -> Result<bool, ValueError> {
        Ok(self.get(key, gas)?.is_some())
    }

    /// Meter an insertion or update, including every collision comparison.
    #[inline]
    pub fn insert(
        &mut self,
        key: K,
        value: V,
        gas: &mut GasMeter,
    ) -> Result<Option<V>, ValueError> {
        let hash = key_hash(&self.entries, &key, gas)?;

        match entry(&mut self.entries, &key, hash, gas)? {
            RawEntryMut::Occupied(mut entry) => Ok(Some(entry.insert(value))),
            RawEntryMut::Vacant(entry) => {
                // A regular insertion would compute the hash a second time.
                entry.insert_hashed_nocheck(hash, key, value);
                Ok(None)
            }
        }
    }

    #[inline]
    pub fn shift_remove(&mut self, key: &K, gas: &mut GasMeter) -> Result<Option<V>, ValueError> {
        let hash = key_hash(&self.entries, key, gas)?;

        match entry(&mut self.entries, key, hash, gas)? {
            RawEntryMut::Occupied(entry) => Ok(Some(entry.shift_remove())),
            RawEntryMut::Vacant(_) => Ok(None),
        }
    }

    #[inline]
    pub fn swap_remove(&mut self, key: &K, gas: &mut GasMeter) -> Result<Option<V>, ValueError> {
        let hash = key_hash(&self.entries, key, gas)?;

        match entry(&mut self.entries, key, hash, gas)? {
            RawEntryMut::Occupied(entry) => Ok(Some(entry.swap_remove())),
            RawEntryMut::Vacant(_) => Ok(None),
        }
    }
}

impl<K, V> GasMap<K, V> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            entries: IndexMap::with_capacity(capacity),
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
    pub fn iter(&self) -> Iter<'_, K, V> {
        self.entries.iter()
    }

    pub fn keys(&self) -> Keys<'_, K, V> {
        self.entries.keys()
    }

    pub fn values(&self) -> Values<'_, K, V> {
        self.entries.values()
    }

    pub fn values_mut(&mut self) -> ValuesMut<'_, K, V> {
        self.entries.values_mut()
    }

    pub(crate) fn get_index_mut(&mut self, index: usize) -> Option<(&K, &mut V)> {
        self.entries
            .get_index_mut(index)
            .map(|(key, value)| (&*key, value))
    }

    pub(crate) fn into_entries(self) -> IndexMap<K, V> {
        self.entries
    }
}

impl<K, V> IntoIterator for GasMap<K, V> {
    type Item = (K, V);
    type IntoIter = IntoIter<K, V>;

    fn into_iter(self) -> Self::IntoIter {
        self.entries.into_iter()
    }
}
