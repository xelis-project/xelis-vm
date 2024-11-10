mod environment;
mod struct_manager;
mod mapper;

pub use environment::*;
pub use struct_manager::*;
pub use mapper::*;
use thiserror::Error;
use types::ValueError;

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
    #[error(transparent)]
    ValueError(#[from] ValueError),
}