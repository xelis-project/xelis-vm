use schemars::JsonSchema;
use serde::{Deserialize, Serialize};


#[derive(Debug, PartialEq, Eq, Clone, Copy, Serialize, Deserialize, JsonSchema)]
#[serde(rename_all = "lowercase")]
pub enum NumberType {
    U8,
    U16,
    U32,
    U64,
    U128,
    U256
}

impl NumberType {
    pub fn value_of(s: &str) -> Option<NumberType> {
        Some(match s {
            "u8" => Self::U8,
            "u16" => Self::U16,
            "u32" => Self::U32,
            "u64" => Self::U64,
            "u128" => Self::U128,
            "u256" => Self::U256,
            _ => return None,
        })
    }
}
