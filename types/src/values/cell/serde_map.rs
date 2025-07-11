use super::*;
use serde::ser::SerializeSeq;
use serde::de::{SeqAccess, Visitor};
use serde::{Deserializer, Serializer};
use std::fmt;

pub fn serialize<S>(
    map: &IndexMap<ValueCell, ValueCell>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    let mut seq = serializer.serialize_seq(Some(map.len()))?;
    for (k, v) in map {
        seq.serialize_element(&(k, v))?;
    }
    seq.end()
}

pub fn deserialize<'de, D>(
    deserializer: D,
) -> Result<IndexMap<ValueCell, ValueCell>, D::Error>
where
    D: Deserializer<'de>,
{
    struct MapVecVisitor;

    impl<'de> Visitor<'de> for MapVecVisitor {
        type Value = IndexMap<ValueCell, ValueCell>;

        fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
            formatter.write_str("a list of key-value pairs")
        }

        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
        where
            A: SeqAccess<'de>,
        {
            let mut map = IndexMap::new();
            while let Some((k, v)) = seq.next_element::<(ValueCell, ValueCell)>()? {
                map.insert(k, v);
            }
            Ok(map)
        }
    }

    deserializer.deserialize_seq(MapVecVisitor)
}
