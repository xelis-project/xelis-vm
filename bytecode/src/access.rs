use std::fmt;

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use xelis_types::TypePacked;

#[derive(Default, Debug, Clone, Serialize, Deserialize, PartialEq, Eq, JsonSchema)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "type", content = "value")]
pub enum Access {
    // Can be called by anything
    All {
        // Parameter types
        // Used for type checking when calling the chunk
        parameters: Option<Vec<TypePacked>>,
    },
    // Only callable from the same program
    // Private functions / closures
    #[default]
    Internal,
    Hook {
        id: u8
    },
    // Program entry
    Entry {
        // Parameter types
        // Used for type checking when calling the chunk
        parameters: Option<Vec<TypePacked>>,
    },
}

impl Access {
    // Get the parameter types if any
    #[inline]
    pub fn parameters(&self) -> Option<&Vec<TypePacked>> {
        match self {
            Self::All { parameters } => parameters.as_ref(),
            Self::Entry { parameters } => parameters.as_ref(),
            Self::Hook { .. } | Self::Internal => None,
        }
    }
}

impl fmt::Display for Access {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::All { .. } => write!(f, "all"),
            Self::Entry { .. } => write!(f, "entry"),
            Self::Internal => write!(f, "internal"),
            Self::Hook { id } => write!(f, "hook {id}"),
        }
    }
}