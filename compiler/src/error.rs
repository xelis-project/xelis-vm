use thiserror::Error;

#[derive(Debug, Error)]
pub enum CompilerError {
    #[error("expected a 'break' statement")]
    ExpectedBreak,
    #[error("expected a 'continue' statement")]
    ExpectedContinue,
    #[error("Missing break patch")]
    MissingBreakPatch,
    #[error("Missing continue patch")]
    MissingContinuePatch,
    #[error("memory store is not empty")]
    MemoryStoreNotEmpty,
    #[error("expected a assignment operator")]
    ExpectedOperatorAssignment,
    #[error("expected a comparison operator")]
    UnexpectedOperator,
    #[error("expected a memory store id")]
    ExpectedMemstoreId,
    #[error("expected a variable")]
    ExpectedVariable,
    #[error("expected a primitive type")]
    ExpectedPrimitiveType,
    #[error("expected a value on the stack")]
    ExpectedValueOnStack,
    #[error("less value on the stack than previous")]
    LessValueOnStackThanPrevious,
    #[error("expected a stack scope")]
    ExpectedStackScope,
    #[error("dangling value on the stack")]
    DanglingValueOnStack,
    #[error("too much dangling value on the stack")]
    TooMuchDanglingValueOnStack,
    #[error("expected a memory scope")]
    ExpectedMemoryScope,
}