use std::fmt::{Debug, Formatter, Result as FmtResult};
use indexmap::IndexMap;
use serde::{ser::{Error, SerializeSeq, SerializeStruct, SerializeTuple}, Serialize, Serializer};
use super::OwnedValue;

// Generic Serde backends recurse through nested Serialize calls. Keep that
// external boundary bounded independently of the execution-local heap.
const MAX_SERIALIZATION_DEPTH: usize = 256;

struct Value<'a> {
    value: &'a OwnedValue,
    depth: usize,
}

struct Array<'a> {
    values: &'a [OwnedValue],
    depth: usize,
}

struct Map<'a> {
    values: &'a IndexMap<OwnedValue, OwnedValue>,
    depth: usize,
}

struct Entry<'a> {
    key: &'a OwnedValue,
    value: &'a OwnedValue,
    depth: usize,
}

impl Serialize for OwnedValue {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        Value { value: self, depth: 0 }.serialize(serializer)
    }
}

impl Serialize for Value<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        if self.depth > MAX_SERIALIZATION_DEPTH {
            return Err(S::Error::custom("owned value exceeds serializer nesting limit"));
        }
        if let OwnedValue::Primitive(value) = self.value {
            return value.serialize(serializer);
        }
        let mut state = serializer.serialize_struct("OwnedValue", 2)?;
        match self.value {
            OwnedValue::Opaque(value) => {
                state.serialize_field("type", "opaque")?;
                state.serialize_field("value", value)?;
            }
            OwnedValue::Bytes(value) => {
                state.serialize_field("type", "bytes")?;
                state.serialize_field("value", value)?;
            }
            OwnedValue::Object(values) => {
                state.serialize_field("type", "object")?;
                state.serialize_field("value", &Array { values, depth: self.depth + 1 })?;
            }
            OwnedValue::Map(values) => {
                state.serialize_field("type", "map")?;
                state.serialize_field("value", &Map { values, depth: self.depth + 1 })?;
            }
            OwnedValue::Primitive(_) => unreachable!(),
        }
        state.end()
    }
}

impl Serialize for Array<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut seq = serializer.serialize_seq(Some(self.values.len()))?;
        for value in self.values {
            seq.serialize_element(&Value { value, depth: self.depth })?;
        }
        seq.end()
    }
}

impl Serialize for Map<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut seq = serializer.serialize_seq(Some(self.values.len()))?;
        for (key, value) in self.values {
            seq.serialize_element(&Entry { key, value, depth: self.depth })?;
        }
        seq.end()
    }
}

impl Serialize for Entry<'_> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut pair = serializer.serialize_tuple(2)?;
        pair.serialize_element(&Value { value: self.key, depth: self.depth })?;
        pair.serialize_element(&Value { value: self.value, depth: self.depth })?;
        pair.end()
    }
}

impl Debug for OwnedValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        enum Item<'a> {
            Value(&'a OwnedValue),
            Text(&'static str),
        }
        let mut work = vec![Item::Value(self)];
        while let Some(item) = work.pop() {
            match item {
                Item::Text(text) => f.write_str(text)?,
                Item::Value(value) => match value {
                    OwnedValue::Primitive(value) => write!(f, "Primitive({value:?})")?,
                    OwnedValue::Opaque(value) => write!(f, "Opaque({value:?})")?,
                    OwnedValue::Bytes(value) => write!(f, "Bytes({value:?})")?,
                    OwnedValue::Object(values) => {
                        f.write_str("Object([")?;
                        work.push(Item::Text("])"));
                        for (index, value) in values.iter().enumerate().rev() {
                            work.push(Item::Value(value));
                            if index > 0 {
                                work.push(Item::Text(", "));
                            }
                        }
                    }
                    OwnedValue::Map(values) => {
                        f.write_str("Map({")?;
                        work.push(Item::Text("})"));
                        for (index, (key, value)) in values.iter().enumerate().rev() {
                            work.push(Item::Value(value));
                            work.push(Item::Text(": "));
                            work.push(Item::Value(key));
                            if index > 0 {
                                work.push(Item::Text(", "));
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }
}
