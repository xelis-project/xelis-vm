use std::{hash::{Hash, Hasher}, sync::Arc};
use serde::{Deserialize, Serialize};

use crate::IdentifierType;
use super::Type;

#[derive(Clone, Eq, Debug, Serialize, Deserialize)]
pub struct Namespace {
    id: IdentifierType,
    path: Vec<String>
}

impl Hash for Namespace {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.path.hash(state);
        self.id.hash(state);
    }
}

impl PartialEq for Namespace {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id && self.path == other.path
    }
}

#[derive(Clone, Hash, PartialEq, Eq, Debug)]
pub struct NamespaceType(Arc<Namespace>);

impl NamespaceType {
    pub fn new(id: IdentifierType, path: Vec<String>) -> Self {
        Self(Arc::new(Namespace { id, path }))
    }

    #[inline(always)]
    pub fn id(&self) -> IdentifierType {
        self.0.id
    }

    #[inline(always)]
    pub fn path(&self) -> &Vec<String> {
        &self.0.path
    }

    #[inline(always)]
    pub fn full_path(&self) -> String {
        self.0.path.join("::")
    }
}

impl Serialize for NamespaceType {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.0.serialize(serializer)
    }
}

impl<'a> Deserialize<'a> for NamespaceType {
    fn deserialize<D: serde::Deserializer<'a>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(Self(Arc::new(Namespace {
            id: IdentifierType::deserialize(deserializer)?,
            path: Vec::new()
        })))
    }
}