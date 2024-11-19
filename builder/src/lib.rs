mod environment;
mod manager;
mod mapper;

pub use environment::*;
pub use manager::*;
pub use mapper::*;
use thiserror::Error;
use xelis_types::ValueError;

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
}