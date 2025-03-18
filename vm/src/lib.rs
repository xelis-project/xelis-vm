mod chunk;
mod error;
mod iterator;
mod stack;
mod validator;
mod instructions;

#[cfg(test)]
mod tests;

use stack::Stack;
use log::trace;

// Re-export the necessary types
pub use xelis_environment::*;
pub use xelis_types::*;
pub use xelis_bytecode::*;

pub use validator::*;
pub use instructions::*;
pub use error::VMError;
pub use chunk::*;

// 64 elements maximum in the call stack
// This represents how many calls can be chained
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
    // Get a constant registered in the module using its id
    #[inline(always)]
    pub fn get_constant_with_id(&self, id: usize) -> Result<&ValueCell, VMError> {
        self.module.get_constant_at(id)
            .ok_or(VMError::ConstantNotFound)
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
    stack: Stack,
    // Context given to each instruction
    context: Context<'a, 'r>,
    // Flag to enable/disable the tail call optimization
    // in our VM
    tail_call_optimization: bool
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
            tail_call_optimization: false
        }
    }

    // Check if the tail call optimization flag is enabled or not
    #[inline]
    pub fn has_tail_call_optimization(&self) -> bool {
        self.tail_call_optimization
    }

    // Enable/disable the tail call optimization flag
    #[inline]
    pub fn set_tail_call_optimization(&mut self, value: bool) {
        self.tail_call_optimization = value;
    }

    // Get the stack
    #[inline]
    pub fn get_stack(&self) -> &Stack {
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

    // Invoke an entry chunk using its id
    pub fn invoke_entry_chunk(&mut self, id: u16) -> Result<(), VMError> {
        if !self.backend.module.is_entry_chunk(id as usize) {
            return Err(VMError::ChunkNotEntry);
        }
        self.invoke_chunk_id(id)
    }

    // Invoke an entry chunk using its id
    pub fn invoke_entry_chunk_with_args<V: Into<StackValue>, I: Iterator<Item = V> + ExactSizeIterator>(&mut self, id: u16, args: I) -> Result<(), VMError> {
        self.invoke_entry_chunk(id)?;
        self.stack.extend_stack(args.map(Into::into))?;
        Ok(())
    }

    // Invoke a hook
    // Return true if the Module has an implementation for the hook
    // Return false if the hook isn't supported
    pub fn invoke_hook_id(&mut self, hook_id: u8) -> Result<bool, VMError> {
        match self.backend.module.get_chunk_id_of_hook(hook_id) {
            Some(id) => self.invoke_chunk_id(id as _).map(|_| true),
            None => Ok(false)
        }
    }

    // Invoke a hook with args
    // Return true if the Module has an implementation for the hook
    // Return false if the hook isn't supported
    pub fn invoke_hook_id_with_args<V: Into<StackValue>, I: Iterator<Item = V> + ExactSizeIterator>(&mut self, hook_id: u8, args: I) -> Result<bool, VMError> {
        match self.backend.module.get_chunk_id_of_hook(hook_id) {
            Some(id) => {
                self.invoke_chunk_id(id as _)?;
                self.stack.extend_stack(args.map(Into::into))?;
                Ok(true)
            },
            None => Ok(false)
        }
    }

    // Push a value to the stack
    pub fn push_stack<V: Into<StackValue>>(&mut self, value: V) -> Result<(), VMError> {
        self.stack.push_stack(value.into())
    }

    // Run the VM
    // It will execute the bytecode
    // First chunk executed should always return a value
    pub fn run(&mut self) -> Result<ValueCell, VMError> {
        while let Some(mut manager) = self.call_stack.pop() {
            let mut clean_pointers = true;
            while let Some(opcode) = manager.next_u8() {
                match self.backend.table.execute(opcode, &self.backend, &mut self.stack, &mut manager, &mut self.context) {
                    Ok(InstructionResult::Nothing) => {},
                    Ok(InstructionResult::InvokeChunk(id)) => {
                        if self.backend.module.is_entry_chunk(id as usize) {
                            return Err(VMError::EntryChunkCalled);
                        }

                        // If tail call optimization is enabled,
                        // we have another instruction and that its not a OpCode::Return
                        // push current frame back to our call_stack
                        // Otherwise, clean pointers for safety reasons
                        if !self.tail_call_optimization || manager.has_next_instruction() {
                            // We don't check the call stack size
                            // because we've pop it from call stack
                            // and it will be done below for next invoke
                            self.call_stack.push(manager);
                            clean_pointers = false;
                        }

                        self.invoke_chunk_id(id)?;
                        break;
                    },
                    Ok(InstructionResult::Break) => {
                        break;
                    },
                    Err(e) => {
                        trace!("Error: {:?}", e);
                        trace!("Stack: {:?}", self.stack.get_inner());
                        trace!("Call stack left: {}", self.call_stack.len());
                        trace!("Current registers: {:?}", manager.get_registers());
                        return Err(e);
                    }
                }
            }

            if clean_pointers {
                self.stack.checkpoint_clean()?;
            }
        }

        let end_value = self.stack.pop_stack()?
            .into_owned()?;
        if self.stack.count() != 0 {
            return Err(VMError::StackNotCleaned(self.stack.count()));
        }

        Ok(end_value)
    }
}