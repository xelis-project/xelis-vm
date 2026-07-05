use std::{
    borrow::Cow,
    fmt::{self, Write},
    hash::{Hash, Hasher},
    mem,
};

use indexmap::IndexMap;
use schemars::{json_schema, JsonSchema, Schema, SchemaGenerator};
use serde::{Deserialize, Serialize};
use serde_json::json;

use crate::{Constant, DefinedType, U256, ValueError, values::Primitive};

use super::{CellArray, CellMap, ValueCell, ValuePointer};

fn serialize_map<S>(map: &IndexMap<ConstantValueCell, ConstantValueCell>, serializer: S) -> Result<S::Ok, S::Error>
where
    S: serde::Serializer,
{
    use serde::ser::SerializeSeq;
    let mut seq = serializer.serialize_seq(Some(map.len()))?;
    for (k, v) in map {
        seq.serialize_element(&(k, v))?;
    }
    seq.end()
}

fn deserialize_map<'de, D>(
    deserializer: D,
) -> Result<IndexMap<ConstantValueCell, ConstantValueCell>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    use serde::de::{SeqAccess, Visitor};
    struct V;
    impl<'de> Visitor<'de> for V {
        type Value = IndexMap<ConstantValueCell, ConstantValueCell>;
        fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
            f.write_str("a list of [key, value] pairs")
        }
        fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
            let mut map = IndexMap::new();
            while let Some((k, v)) = seq.next_element::<(ConstantValueCell, ConstantValueCell)>()? {
                map.insert(k, v);
            }
            Ok(map)
        }
    }
    deserializer.deserialize_seq(V)
}

/// A recursion-safe owned constant value cell.
///
/// Nested values are also `ConstantValueCell` (no `ValuePointer` / `UnsafeCell`).
/// `Clone`, `Drop`, `PartialEq`, and `Hash` are all iterative to prevent stack
/// overflow for deeply nested structures.
#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case", tag = "type", content = "value")]
pub enum ConstantValueCell {
    Primitive(Primitive),
    #[serde(with = "hex::serde")]
    Bytes(Vec<u8>),
    Object(Vec<ConstantValueCell>),
    #[serde(
        serialize_with = "serialize_map",
        deserialize_with = "deserialize_map"
    )]
    Map(IndexMap<ConstantValueCell, ConstantValueCell>),
}

impl Eq for ConstantValueCell {}

impl PartialEq for ConstantValueCell {
    fn eq(&self, other: &Self) -> bool {
        // fast check:
        if mem::discriminant(self) != mem::discriminant(other) {
            return false;
        }

        let mut stack = vec![(self, other)];
        while let Some((l, r)) = stack.pop() {
            match (l, r) {
                (Self::Primitive(a), Self::Primitive(b)) => {
                    if a != b {
                        return false;
                    }
                }
                (Self::Bytes(a), Self::Bytes(b)) => {
                    if a != b {
                        return false;
                    }
                }
                (Self::Object(a), Self::Object(b)) => {
                    if a.len() != b.len() {
                        return false;
                    }
                    stack.extend(a.iter().zip(b.iter()).rev());
                }
                (Self::Map(a), Self::Map(b)) => {
                    if a.len() != b.len() {
                        return false;
                    }
                    stack.extend(
                        a.iter()
                            .zip(b.iter())
                            .rev()
                            .flat_map(|((ka, va), (kb, vb))| [(ka, kb), (va, vb)])
                    );
                }
                _ => return false,
            }
        }

        true
    }
}

impl Hash for ConstantValueCell {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let mut stack = vec![self];
        while let Some(cell) = stack.pop() {
            match cell {
                Self::Primitive(v) => v.hash(state),
                Self::Bytes(bytes) => {
                    12u8.hash(state);
                    bytes.hash(state);
                }
                Self::Object(values) => {
                    11u8.hash(state);
                    stack.extend(values.iter().rev());
                }
                Self::Map(map) => {
                    13u8.hash(state);
                    stack.extend(map.iter().rev().flat_map(|(k, v)| [k, v]));
                }
            }
        }
    }
}

impl ConstantValueCell {
    // Convert to a ValueCell by cloning (reference-based, iterative).
    pub fn to_value_cell(&self) -> ValueCell {
        match self {
            Self::Primitive(p) => return ValueCell::Primitive(p.clone()),
            Self::Bytes(b) => return ValueCell::Bytes(b.clone()),
            _ => {}
        }

        enum Work<'a> {
            Visit(&'a ConstantValueCell),
            BuildArray(usize),
            BuildMap(usize),
        }

        let mut work: Vec<Work<'_>> = vec![Work::Visit(self)];
        let mut results: Vec<ValueCell> = Vec::new();

        while let Some(item) = work.pop() {
            match item {
                Work::BuildArray(len) => {
                    let start = results.len() - len;
                    let values: CellArray = results.drain(start..).map(ValuePointer::new).collect();
                    results.push(ValueCell::Object(values));
                }
                Work::BuildMap(len) => {
                    let start = results.len() - len * 2;
                    let drained: Vec<ValueCell> = results.drain(start..).collect();
                    let mut map = CellMap::with_capacity(len);
                    let mut iter = drained.into_iter();
                    while let Some(k) = iter.next() {
                        let v = iter.next().unwrap();
                        map.insert(k, ValuePointer::new(v));
                    }
                    results.push(ValueCell::Map(Box::new(map)));
                }
                Work::Visit(cell) => match cell {
                    Self::Primitive(p) => results.push(ValueCell::Primitive(p.clone())),
                    Self::Bytes(b) => results.push(ValueCell::Bytes(b.clone())),
                    Self::Object(values) => {
                        work.push(Work::BuildArray(values.len()));
                        work.extend(values.iter().rev().map(Work::Visit));
                    }
                    Self::Map(map) => {
                        work.push(Work::BuildMap(map.len()));
                        work.extend(map.iter().rev().flat_map(|(k, v)| [Work::Visit(v), Work::Visit(k)]));
                    }
                }
            }
        }

        debug_assert_eq!(results.len(), 1);
        results.remove(0)
    }

    // Calculate the memory usage of this constant cell iteratively.
    pub fn calculate_memory_usage(&self, max_memory: usize) -> Result<usize, ValueError> {
        if let Self::Primitive(p) = self {
            return Ok(p.get_memory_usage());
        }

        let mut stack: Vec<&ConstantValueCell> = vec![self];
        let mut memory = 0usize;

        while let Some(cell) = stack.pop() {
            if memory > max_memory {
                return Err(ValueError::MaxMemoryReached(memory, max_memory));
            }

            match cell {
                Self::Primitive(p) => {
                    memory += 1;
                    memory += p.get_memory_usage();
                }
                Self::Bytes(bytes) => {
                    memory += 32;
                    memory += bytes.len();
                }
                Self::Object(values) => {
                    memory += 32;
                    stack.extend(values.iter());
                }
                Self::Map(map) => {
                    memory += 64;
                    for (k, v) in map.iter() {
                        stack.push(v);
                        stack.push(k);
                    }
                }
            }
        }

        Ok(memory)
    }
}

// Drains direct children of `cell` into `stack`, leaving `cell` with empty/null contents.
// Used by the iterative Drop impl to avoid recursive stack overflow.
fn drain_children(cell: &mut ConstantValueCell, stack: &mut Vec<ConstantValueCell>) {
    match cell {
        ConstantValueCell::Object(values) => stack.extend(std::mem::take(values)),
        ConstantValueCell::Map(map) => {
            stack.extend(
                std::mem::take(map)
                    .into_iter()
                    .flat_map(|(k, v)| [k, v])
            );
        }
        _ => {}
    }
}

// Iterative drop to prevent stack overflow on deep nesting.
impl Drop for ConstantValueCell {
    fn drop(&mut self) {
        match self {
            Self::Primitive(_) | Self::Bytes(_) => return, // no nested heap data to drop
            Self::Object(_) | Self::Map(_) => {}
        }

        let mut stack = Vec::new();
        drain_children(self, &mut stack);
        while let Some(mut child) = stack.pop() {
            drain_children(&mut child, &mut stack);
        }
    }
}

// Iterative deep clone to avoid stack overflow on deeply nested structures.
impl Clone for ConstantValueCell {
    fn clone(&self) -> Self {
        match self {
            Self::Primitive(p) => return Self::Primitive(p.clone()),
            Self::Bytes(b) => return Self::Bytes(b.clone()),
            _ => {}
        }

        enum Work<'a> {
            Visit(&'a ConstantValueCell),
            BuildArray(usize),
            BuildMap(usize),
        }

        let mut work = vec![Work::Visit(self)];
        let mut results = Vec::new();

        while let Some(item) = work.pop() {
            match item {
                Work::Visit(cell) => match cell {
                    Self::Primitive(p) => results.push(Self::Primitive(p.clone())),
                    Self::Bytes(b) => results.push(Self::Bytes(b.clone())),
                    Self::Object(values) => {
                        work.push(Work::BuildArray(values.len()));
                        for v in values.iter().rev() { work.push(Work::Visit(v)); }
                    }
                    Self::Map(map) => {
                        work.push(Work::BuildMap(map.len()));
                        for (k, v) in map.iter().rev() {
                            work.push(Work::Visit(v));
                            work.push(Work::Visit(k));
                        }
                    }
                },
                Work::BuildArray(len) => {
                    let start = results.len() - len;
                    let values: Vec<ConstantValueCell> = results.drain(start..).collect();
                    results.push(Self::Object(values));
                }
                Work::BuildMap(len) => {
                    let start = results.len() - len * 2;
                    let drained: Vec<ConstantValueCell> = results.drain(start..).collect();
                    let mut map = IndexMap::with_capacity(len);
                    let mut iter = drained.into_iter();
                    while let (Some(k), Some(v)) = (iter.next(), iter.next()) {
                        map.insert(k, v);
                    }
                    results.push(Self::Map(map));
                }
            }
        }

        debug_assert_eq!(results.len(), 1);
        results.remove(0)
    }
}

impl Default for ConstantValueCell {
    fn default() -> Self {
        Self::Primitive(Default::default())
    }
}

impl From<Primitive> for ConstantValueCell {
    fn from(p: Primitive) -> Self {
        Self::Primitive(p)
    }
}

// Convert from the compiler/AST Constant type into ConstantValueCell.
// Mirrors the existing From<Constant> for ValueCell logic.
impl From<Constant> for ConstantValueCell {
    fn from(c: Constant) -> Self {
        // Iterative conversion to avoid stack overflow
        enum Work {
            Visit(Constant),
            BuildArray(usize),
            BuildMap(usize),
            BuildTyped(usize, DefinedType),
        }

        let mut work: Vec<Work> = vec![Work::Visit(c)];
        let mut results: Vec<ConstantValueCell> = Vec::new();

        while let Some(item) = work.pop() {
            match item {
                Work::Visit(c) => match c {
                    Constant::Primitive(p) => results.push(Self::Primitive(p)),
                    Constant::Bytes(b) => results.push(Self::Bytes(b)),
                    Constant::Array(values) => {
                        let len = values.len();
                        work.push(Work::BuildArray(len));
                        work.extend(values.into_iter().rev().map(Work::Visit));
                    }
                    Constant::Map(map) => {
                        let len = map.len();
                        work.push(Work::BuildMap(len));
                        for (k, v) in map.into_iter().rev() {
                            work.push(Work::Visit(v));
                            work.push(Work::Visit(k));
                        }
                    }
                    Constant::Typed(fields, ty) => {
                        let len = fields.len();
                        let is_enum = matches!(ty, DefinedType::Enum(_));
                        let extra = if is_enum { 1 } else { 0 };
                        work.push(Work::BuildTyped(len + extra, ty));
                        work.extend(fields.into_iter().rev().map(Work::Visit));
                    }
                },
                Work::BuildArray(len) => {
                    let start = results.len() - len;
                    let values: Vec<ConstantValueCell> = results.drain(start..).collect();
                    results.push(Self::Object(values));
                }
                Work::BuildMap(len) => {
                    let start = results.len() - len * 2;
                    let drained: Vec<ConstantValueCell> = results.drain(start..).collect();
                    let mut map = IndexMap::with_capacity(len);
                    let mut iter = drained.into_iter();
                    while let (Some(k), Some(v)) = (iter.next(), iter.next()) {
                        map.insert(k, v);
                    }
                    results.push(Self::Map(map));
                }
                Work::BuildTyped(len, ty) => {
                    let start = results.len() - (len - if matches!(ty, DefinedType::Enum(_)) { 1 } else { 0 });
                    let mut values: Vec<ConstantValueCell> = results.drain(start..).collect();
                    if let DefinedType::Enum(ref e) = ty {
                        values.insert(0, Self::Primitive(Primitive::U8(e.variant_id())));
                    }
                    results.push(Self::Object(values));
                }
            }
        }

        debug_assert_eq!(results.len(), 1);
        results.remove(0)
    }
}

impl fmt::Display for ConstantValueCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        enum Item<'a> {
            Cell(&'a ConstantValueCell),
            OpenArray,
            CloseArray,
            OpenMap,
            CloseMap,
            MapSep,
            ArrSep,
        }

        let mut output = String::new();
        let mut stack: Vec<Item<'_>> = vec![Item::Cell(self)];
        let mut first_in: Vec<bool> = Vec::new();

        while let Some(item) = stack.pop() {
            match item {
                Item::OpenArray => { output.push('['); continue; }
                Item::CloseArray => { output.push(']'); first_in.pop(); continue; }
                Item::OpenMap => { output.push_str("map{"); continue; }
                Item::CloseMap => { output.push('}'); first_in.pop(); continue; }
                Item::MapSep => { output.push_str(": "); continue; }
                Item::ArrSep => {
                    if let Some(first) = first_in.last_mut() {
                        if *first { *first = false; } else { output.push_str(", "); }
                    }
                    continue;
                }
                Item::Cell(cell) => match cell {
                    Self::Primitive(v) => write!(&mut output, "{}", v).map_err(|_| fmt::Error)?,
                    Self::Bytes(b) => write!(&mut output, "bytes{:?}", b).map_err(|_| fmt::Error)?,
                    Self::Object(values) => {
                        if values.is_empty() {
                            output.push_str("[]");
                        } else {
                            stack.push(Item::CloseArray);
                            first_in.push(true);
                            for v in values.iter().rev() {
                                stack.push(Item::Cell(v));
                                stack.push(Item::ArrSep);
                            }
                            stack.push(Item::OpenArray);
                        }
                    }
                    Self::Map(map) => {
                        if map.is_empty() {
                            output.push_str("map{}");
                        } else {
                            stack.push(Item::CloseMap);
                            first_in.push(true);
                            for (k, v) in map.iter().rev() {
                                stack.push(Item::Cell(v));
                                stack.push(Item::MapSep);
                                stack.push(Item::Cell(k));
                                stack.push(Item::ArrSep);
                            }
                            stack.push(Item::OpenMap);
                        }
                    }
                }
            }
        }
        write!(f, "{}", output)
    }
}

impl JsonSchema for ConstantValueCell {
    fn schema_name() -> Cow<'static, str> {
        Cow::Borrowed("ConstantValueCell")
    }

    fn json_schema(gen: &mut SchemaGenerator) -> Schema {
        let primitive_schema = gen.subschema_for::<Primitive>();
        let self_schema = gen.subschema_for::<ConstantValueCell>();

        let primitive_variant = json!({
            "type": "object",
            "properties": {
                "type": { "type": "string", "const": "primitive" },
                "value": primitive_schema
            },
            "required": ["type", "value"]
        });
        let bytes_variant = json!({
            "type": "object",
            "properties": {
                "type": { "type": "string", "const": "bytes" },
                "value": { "type": "string", "description": "hex-encoded bytes" }
            },
            "required": ["type", "value"]
        });
        let object_variant = json!({
            "type": "object",
            "properties": {
                "type": { "type": "string", "const": "object" },
                "value": { "type": "array", "items": self_schema }
            },
            "required": ["type", "value"]
        });
        let map_pair = json!({
            "type": "array",
            "prefixItems": [ self_schema.clone(), self_schema.clone() ],
            "minItems": 2, "maxItems": 2
        });
        let map_variant = json!({
            "type": "object",
            "properties": {
                "type": { "type": "string", "const": "map" },
                "value": { "type": "array", "items": map_pair, "description": "A list of [key, value] pairs" }
            },
            "required": ["type", "value"]
        });

        json_schema!({
            "oneOf": [ primitive_variant, bytes_variant, object_variant, map_variant ],
            "title": "ConstantValueCell",
            "description": "A cheap-to-clone constant cell (primitive, bytes, array or map as pairs)"
        })
    }
}

impl From<&ConstantValueCell> for ValueCell {
    #[inline]
    fn from(c: &ConstantValueCell) -> Self {
        c.to_value_cell()
    }
}

// Allow using u8/u16/etc directly
macro_rules! impl_from_primitive_type {
    ($($t:ty => $variant:ident),*) => {
        $(impl From<$t> for ConstantValueCell {
            fn from(v: $t) -> Self {
                Self::Primitive(Primitive::$variant(v))
            }
        })*
    }
}

impl_from_primitive_type!(
    u8 => U8,
    u16 => U16,
    u32 => U32,
    u64 => U64,
    u128 => U128,
    bool => Boolean
);

impl From<U256> for ConstantValueCell {
    fn from(v: U256) -> Self {
        Self::Primitive(Primitive::U256(v))
    }
}

impl From<String> for ConstantValueCell {
    fn from(s: String) -> Self {
        Self::Primitive(Primitive::String(s))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clone() {
        let cell = ConstantValueCell::from(42u64);
        let clone = cell.clone();
        assert_eq!(cell, clone);
    }

    #[test]
    fn test_drop_deep_array() {
        let depth = 10_000;
        let mut cell = ConstantValueCell::default();
        for _ in 0..depth {
            cell = ConstantValueCell::Object(vec![cell]);
        }
        drop(cell); // should not stack overflow
    }

    #[test]
    fn test_into_value_cell() {
        let cell = ConstantValueCell::Object(vec![
            ConstantValueCell::from(1u8),
            ConstantValueCell::from(2u8),
        ]);
        let vc = ValueCell::from(&cell);
        match vc {
            ValueCell::Object(arr) => {
                assert_eq!(arr[0].as_ref().as_u8().unwrap(), 1);
                assert_eq!(arr[1].as_ref().as_u8().unwrap(), 2);
            }
            _ => panic!("expected Object"),
        }
    }
}
