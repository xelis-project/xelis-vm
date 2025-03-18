mod environment;
mod manager;
mod mapper;
mod hook;

use thiserror::Error;
use xelis_types::ValueError;

pub use environment::*;
pub use manager::*;
pub use mapper::*;
pub use hook::Hook;

#[derive(Debug, Error)]
pub enum BuilderError {
    #[error("Struct name already used")]
    StructNameAlreadyUsed,
    #[error("Struct not found")]
    StructNotFound,
    #[error("mapping not found")]
    MappingNotFound,
    #[error("mapping already exists")]
    MappingExists,
    #[error("Signature already registered")]
    SignatureAlreadyRegistered,
    #[error(transparent)]
    ValueError(#[from] ValueError),
    #[error("Invalid const fn parameters")]
    InvalidConstFnParameters,
    #[error(transparent)]
    Any(#[from] anyhow::Error)
}