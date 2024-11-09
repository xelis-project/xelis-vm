use thiserror::Error;

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("expected a 'break' statement")]
    ExpectedBreak,
    #[error("expected a 'continue' statement")]
    ExpectedContinue,
    #[error("expected a assignment operator")]
    ExpectedOperatorAssignment,
    #[error("expected a comparison operator")]
    UnexpectedOperator,
    #[error("expected a memory store id")]
    ExpectedMemstoreId,
    #[error("expected a variable")]
    ExpectedVariable,
}