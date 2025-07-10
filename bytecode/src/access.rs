use serde::{Deserialize, Serialize};

#[derive(Default, Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum Access {
    // Can be called by anything
    All,
    // Only callable from the same program
    // Private functions / closures
    #[default]
    Internal,
    Hook {
        id: u8
    },
    // Program entry
    Entry,
}
