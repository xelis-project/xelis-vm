use xelis_bytecode::{Reference, Module};

// Module with provided metadata
#[derive(Debug)]
pub struct ModuleMetadata<'a, M> {
    pub module: Reference<'a, Module>,
    pub metadata: Reference<'a, M>
}

impl<'a, M> Clone for ModuleMetadata<'a, M> {
    fn clone(&self) -> Self {
        Self {
            module: self.module.clone(),
            metadata: self.metadata.clone()
        }
    }
}