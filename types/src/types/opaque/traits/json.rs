use std::{collections::HashMap, sync::RwLock};
use anyhow::anyhow;
use lazy_static::lazy_static;
use serde_json::Value;
use crate::opaque::OpaqueWrapper;

pub type DeserializeFn = dyn Fn(Value) -> Result<OpaqueWrapper, anyhow::Error> + Send + Sync;

lazy_static! {
    pub static ref JSON_REGISTRY: RwLock<HashMap<&'static str, Box<DeserializeFn>>> = RwLock::new(HashMap::new());
}

#[macro_export]
macro_rules! register_opaque_json {
    ($registry:expr, $name:expr, $type:ty) => {
        $registry.insert($name, Box::new(|value| {
            use anyhow::Context;

            let value: $type = serde_json::from_value(value)
                .context("Failed to deserialize JSON")?;

            Ok($crate::opaque::OpaqueWrapper::new(value))
        }));
    };
}

pub trait JSONHelper {
    // Serialize the type to JSON
    fn serialize_json(&self) -> Result<Value, anyhow::Error> {
        Err(anyhow!("Serialization not supported for this type"))
    }

    // Check if the type is supported by the JSON serialization
    // By default, return false
    fn is_json_supported(&self) -> bool {
        false
    }
}