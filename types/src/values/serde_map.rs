use std::{fmt, hash::Hash, marker::PhantomData};
use indexmap::IndexMap;
use serde::{de::{SeqAccess, Visitor}, Deserialize, Deserializer, Serialize, Serializer};

// Encode entries as pairs so JSON maps can contain arbitrary value keys.
pub fn serialize<K: Serialize, V: Serialize, S: Serializer>(map: &IndexMap<K, V>, serializer: S) -> Result<S::Ok, S::Error> {
    serializer.collect_seq(map.iter())
}

pub fn deserialize<'de, K, V, D>(deserializer: D) -> Result<IndexMap<K, V>, D::Error>
where
    K: Deserialize<'de> + Eq + Hash,
    V: Deserialize<'de>,
    D: Deserializer<'de>,
{
    struct MapVisitor<K, V>(PhantomData<(K, V)>);

    impl<'de, K, V> Visitor<'de> for MapVisitor<K, V>
    where
        K: Deserialize<'de> + Eq + Hash,
        V: Deserialize<'de>,
    {
        type Value = IndexMap<K, V>;

        fn expecting(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
            formatter.write_str("a list of [key, value] pairs")
        }

        fn visit_seq<A: SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
            let mut map = IndexMap::new();
            while let Some((key, value)) = seq.next_element()? {
                map.insert(key, value);
            }
            Ok(map)
        }
    }

    deserializer.deserialize_seq(MapVisitor(PhantomData))
}
