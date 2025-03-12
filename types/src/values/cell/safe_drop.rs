use std::ops::{Deref, DerefMut};

use serde::{Deserialize, Serialize};

use super::ValueCell;

// SafeDrop prevent to have any stackoverflow during dropping
// We will drop iteratively all cell from the deepest part to the nearest
// by deconstructing it into one flat list
#[derive(Debug, Hash, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(transparent)]
pub struct SafeDropValueCell(pub ValueCell);

impl Deref for SafeDropValueCell {
    type Target = ValueCell;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for SafeDropValueCell {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Drop for SafeDropValueCell {
    fn drop(&mut self) {
        // Fast path prevent any allocation below
        if matches!(self.0, ValueCell::Default(_) | ValueCell::Bytes(_)) {
            return
        }

        let mut stack = vec![std::mem::take(&mut self.0)];
        while let Some(value) = stack.pop() {
            match value {
                ValueCell::Default(_) => {},
                ValueCell::Array(values) => stack.extend(values),
                ValueCell::Bytes(_) => {},
                ValueCell::Map(map) => stack.extend(map.into_iter().flat_map(|(k, v)| [k, v])),
            }
        }
    }
}
