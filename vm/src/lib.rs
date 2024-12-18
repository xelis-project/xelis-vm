mod chunk;
mod error;
mod iterator;
mod stack;
mod validator;
mod instructions;

#[cfg(test)]
mod tests;

use stack::Stack;

// Re-export the necessary types
pub use xelis_environment::*;
pub use xelis_types::*;
pub use xelis_bytecode::*;

pub use validator::*;
pub use instructions::*;
pub use error::VMError;
pub use chunk::*;

// 64 elements maximum in the call stack
const CALL_STACK_SIZE: usize = 64;

// Backend of the VM
// This is the immutable part of the VM
pub struct Backend<'a> {
    // The module to execute
    module: &'a Module,
    // The environment of the VM
    environment: &'a Environment,
    // The instruction table of the VM
    table: InstructionTable<'a>,
}

impl<'a> Backend<'a> {
    // Get a struct with an id
    #[inline]
    pub fn get_struct_with_id(&self, mut id: usize) -> Result<&StructType, VMError> {
        let env_structs = self.environment.get_structures();
        if let Some(struct_type) = env_structs.get_index(id) {
            return Ok(struct_type);
        } else {
            id -= env_structs.len();
        }

        self.module.get_struct_at(id).ok_or(VMError::StructNotFound)
    }

    // Get a constant registered in the module using its id
    #[inline(always)]
    pub fn get_constant_with_id(&self, id: usize) -> Result<&Constant, VMError> {
        self.module.get_constant_at(id)
            .ok_or(VMError::ConstantNotFound)
    }

    // Get an enum with an id
    #[inline]
    pub fn get_enum_with_id(&self, mut id: usize) -> Result<&EnumType, VMError> {
        let env_enums = self.environment.get_enums();
        if let Some(enum_type) = env_enums.get_index(id) {
            return Ok(enum_type);
        } else {
            id -= env_enums.len();
        }

        self.module.get_enum_at(id).ok_or(VMError::EnumNotFound)
    }
}

// Virtual Machine to execute the bytecode from chunks of a Module.
pub struct VM<'a, 'r> {
    backend: Backend<'a>,
    // The call stack of the VM
    // Every chunks to proceed are stored here
    call_stack: Vec<ChunkManager<'a>>,
    // The stack of the VM
    // Every values are stored here
    stack: Stack<'a>,
    // Context given to each instruction
    context: Context<'a, 'r>,
}

impl<'a, 'r> VM<'a, 'r> {
    // Create a new VM
    // Insert the environment as a reference in the context
    pub fn new(module: &'a Module, environment: &'a Environment) -> Self {
        let mut context = Context::default();
        context.insert_ref(environment);

        Self::with(module, environment, InstructionTable::new(), context)
    }

    // Create a new VM with a given table and context
    pub fn with(module: &'a Module, environment: &'a Environment, table: InstructionTable<'a>, context: Context<'a, 'r>) -> Self {
        Self {
            backend: Backend {
                module,
                environment,
                table,
            },
            call_stack: Vec::with_capacity(4),
            stack: Stack::new(),
            context,
        }
    }

    // Get the stack
    #[inline]
    pub fn get_stack(&self) -> &Stack<'a> {
        &self.stack
    }

    // Get the context
    #[inline]
    pub fn context(&self) -> &Context<'a, 'r> {
        &self.context
    }

    // Get a mutable reference to the context
    #[inline]
    pub fn context_mut(&mut self) -> &mut Context<'a, 'r> {
        &mut self.context
    }

    // Get the instruction table
    #[inline]
    pub fn table(&self) -> &InstructionTable<'a> {
        &self.backend.table
    }

    // Get a mutable reference to the instruction table
    #[inline]
    pub fn table_mut(&mut self) -> &mut InstructionTable<'a> {
        &mut self.backend.table
    }

    // Get the environment
    #[inline]
    pub fn environment(&self) -> &Environment {
        self.backend.environment
    }

    // Invoke a chunk using its id
    pub(crate) fn invoke_chunk_id(&mut self, id: u16) -> Result<(), VMError> {
        if self.call_stack.len() >= CALL_STACK_SIZE {
            return Err(VMError::CallStackOverflow);
        }

        let chunk = self.backend.module.get_chunk_at(id as usize)
            .ok_or(VMError::ChunkNotFound)?;

        let manager = ChunkManager::new(chunk);
        self.call_stack.push(manager);
        Ok(())
    }

    // Invoke a chunk using its id and arguments
    pub fn invoke_chunk_with_args<V: Into<Path<'a>>, I: Iterator<Item = V> + ExactSizeIterator>(&mut self, id: u16, args: I) -> Result<(), VMError> {
        self.stack.extend_stack(args.map(Into::into))?;
        self.invoke_chunk_id(id)
    }

    // Invoke an entry chunk using its id
    pub fn invoke_entry_chunk(&mut self, id: u16) -> Result<(), VMError> {
        if !self.backend.module.is_entry_chunk(id as usize) {
            return Err(VMError::ChunkNotEntry);
        }
        self.invoke_chunk_id(id)
    }

    // Push a value to the stack
    pub fn push_stack<V: Into<Path<'a>>>(&mut self, value: V) -> Result<(), VMError> {
        self.stack.push_stack(value.into())
    }

    // Invoke an entry chunk using its id
    pub fn invoke_entry_chunk_with_args<V: Into<Path<'a>>, I: Iterator<Item = V> + ExactSizeIterator>(&mut self, id: u16, args: I) -> Result<(), VMError> {
        self.invoke_entry_chunk(id)?;
        self.stack.extend_stack(args.map(Into::into))?;
        Ok(())
    }

    // Run the VM
    // It will execute the bytecode
    // First chunk executed should always return a value
    pub fn run(&mut self) -> Result<ValueCell, VMError> {
        while let Some(mut manager) = self.call_stack.pop() {
            while let Some(opcode) = manager.next_u8() {
                match self.backend.table.execute(opcode, &self.backend, &mut self.stack, &mut manager, &mut self.context)? {
                    InstructionResult::Nothing => {},
                    InstructionResult::InvokeChunk(id) => {
                        if self.backend.module.is_entry_chunk(id as usize) {
                            return Err(VMError::EntryChunkCalled);
                        }

                        self.call_stack.push(manager);
                        self.invoke_chunk_id(id)?;
                        break;
                    },
                    InstructionResult::Break => {
                        break;
                    }
                }
            }
        }

        let end_value = self.stack.pop_stack()?.into_owned();
        if self.stack.count() != 0 {
            return Err(VMError::StackNotCleaned);
        }

        Ok(end_value)
    }
}