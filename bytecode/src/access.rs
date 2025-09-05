use std::fmt;

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

impl fmt::Display for Access {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::All => write!(f, "all"),
            Self::Entry => write!(f, "entry"),
            Self::Internal => write!(f, "internal"),
            Self::Hook { id } => write!(f, "hook {id}"),
        }
    }
}
