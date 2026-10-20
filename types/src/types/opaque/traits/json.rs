use std::{collections::HashMap, sync::RwLock};
use matriochka::Error;
use lazy_static::lazy_static;
use serde_json::Value;
use crate::opaque::OpaqueWrapper;

pub type DeserializeFn = dyn Fn(Value) -> Result<OpaqueWrapper, Error> + Send + Sync;

lazy_static! {
    pub static ref JSON_REGISTRY: RwLock<HashMap<&'static str, Box<DeserializeFn>>> = RwLock::new(HashMap::new());
}

#[macro_export]
macro_rules! register_opaque_json {
    ($registry:expr, $name:expr, $type:ty) => {
        $registry.insert($name, Box::new(|value| {
            use $crate::__private::matriochka::ResultExt as _;

            let value: $type = $crate::__private::serde_json::from_value(value)
                .with_context(|| format!("deserializing opaque type {} from JSON", stringify!($type)))?;

            Ok($crate::opaque::OpaqueWrapper::new(value))
        }));
    };
}

pub trait JSONHelper {
    // Serialize the type to JSON
    fn serialize_json(&self) -> Result<Value, Error> {
        Err(Error::new(std::io::Error::new(
            std::io::ErrorKind::Unsupported,
            "Serialization not supported for this type",
        )))
    }

    // Check if the type is supported by the JSON serialization
    // By default, return false
    fn is_json_supported(&self) -> bool {
        false
    }
}