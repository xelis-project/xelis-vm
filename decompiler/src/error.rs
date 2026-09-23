use thiserror::Error;

/// A location in the original module, even when source reconstruction fails.
#[derive(Debug, Error)]
#[error("decompiler error in chunk {chunk} at byte {offset}: {message}")]
pub struct DecompilerError {
    pub chunk: usize,
    pub offset: usize,
    pub message: String,
}

impl DecompilerError {
    pub(crate) fn new(chunk: usize, offset: usize, message: impl Into<String>) -> Self {
        Self {
            chunk,
            offset,
            message: message.into(),
        }
    }
}
