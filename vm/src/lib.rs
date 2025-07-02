mod chunk;
mod error;
mod iterator;
mod stack;
mod validator;
mod instructions;
mod module;

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
pub use module::*;

// 64 elements maximum in the call stack
// This represents how many calls can be chained
const CALL_STACK_SIZE: usize = 64;

// 8 modules maximum in the stack
// This represents how many modules can be chained
const MODULES_STACK_SIZE: usize = 8;

// Backend of the VM
// This is the immutable part of the VM
pub struct Backend<'a: 'r, 'ty: 'a, 'r, M> {
    // The instruction table of the VM
    table: InstructionTable<'a, 'ty, 'r, M>,
    // The module to execute
    modules: Vec<ModuleMetadata<'a, M>>,
    // The environment of the VM
    environment: &'a Environment,
}

impl<'a: 'r, 'ty: 'a, 'r, M> Backend<'a, 'ty, 'r, M> {
    // Get a constant registered in the module using its id
    #[inline(always)]
    pub fn get_constant_with_id(&self, id: usize) -> Result<&ValueCell, VMError> {
        self.modules.last()
            .and_then(|m| m.module.get_constant_at(id))
            .ok_or(VMError::ConstantNotFound)
    }

    // Get the current module with its associated metadata
    #[inline(always)]
    pub fn current(&self) -> Result<&ModuleMetadata<'a, M>, VMError> {
        self.modules.last()
            .ok_or(VMError::NoModule)
    }
}

// Virtual Machine to execute the bytecode from chunks of a Module.
pub struct VM<'a: 'r, 'ty: 'a, 'r, M> {
    backend: Backend<'a, 'ty, 'r, M>,
    // The call stack of the VM
    // Every chunks to proceed are stored here
    // It is behind an Option so we know
    // when we have to switch the module
    call_stack: Vec<Option<ChunkManager>>,
    // Real call stack size counting
    // only Some entries
    call_stack_size: usize,
    // The stack of the VM
    // Every values are stored here
    stack: Stack,
    // Context given to each instruction
    context: Context<'ty, 'r>,
    // Flag to enable/disable the tail call optimization
    // in our VM
    tail_call_optimization: bool
}

impl<'a: 'r, 'ty: 'a, 'r, M> VM<'a, 'ty, 'r, M> {
    // Create a new VM
    // Insert the environment as a reference in the context
    pub fn new(environment: &'a Environment) -> Self {
        let mut context = Context::default();
        context.insert_ref(environment);

        Self::with(environment, Default::default(), context)
    }

    // Create a new VM with a given table and context
    pub fn with(environment: &'a Environment, table: InstructionTable<'a, 'ty, 'r, M>, context: Context<'ty, 'r>) -> Self {
        Self {
            backend: Backend {
                table,
                modules: Vec::with_capacity(1),
                environment,
            },
            call_stack: Vec::with_capacity(4),
            call_stack_size: 0,
            stack: Stack::new(),
            context,
            tail_call_optimization: false
        }
    }

    // Check if the tail call optimization flag is enabled or not
    #[inline(always)]
    pub fn has_tail_call_optimization(&self) -> bool {
        self.tail_call_optimization
    }

    // Enable/disable the tail call optimization flag
    #[inline(always)]
    pub fn set_tail_call_optimization(&mut self, value: bool) {
        self.tail_call_optimization = value;
    }

    // Get the stack
    #[inline(always)]
    pub fn get_stack(&self) -> &Stack {
        &self.stack
    }

    // Get the context
    #[inline(always)]
    pub fn context(&self) -> &Context<'ty, 'r> {
        &self.context
    }

    // Get a mutable reference to the context
    #[inline(always)]
    pub fn context_mut(&mut self) -> &mut Context<'ty, 'r> {
        &mut self.context
    }

    // Get the instruction table
    #[inline(always)]
    pub fn table(&self) -> &InstructionTable<'a, 'ty, 'r, M> {
        &self.backend.table
    }

    // Get a mutable reference to the instruction table
    #[inline(always)]
    pub fn table_mut(&mut self) -> &mut InstructionTable<'a, 'ty, 'r, M> {
        &mut self.backend.table
    }

    // Get the environment
    #[inline(always)]
    pub fn environment(&self) -> &Environment {
        self.backend.environment
    }

    // Invoke a chunk using its id
    pub(crate) fn invoke_chunk_id(&mut self, id: usize) -> Result<(), VMError> {
        if self.call_stack_size + 1 >= CALL_STACK_SIZE {
            return Err(VMError::CallStackOverflow);
        }

        let manager = ChunkManager::new(id);
        self.stack.mark_checkpoint();
        self.call_stack.push(Some(manager));
        self.call_stack_size += 1;

        Ok(())
    }

    // Append a new module to execute
    // Once added, you can invoke a chunk / entry / hook
    pub fn append_module(&mut self, module: impl Into<Reference<'a, Module>>, metadata: impl Into<Reference<'a, M>>) -> Result<(), VMError> {
        if self.backend.modules.len() + 1 >= MODULES_STACK_SIZE {
            return Err(VMError::ModulesStackOverflow)
        }

        if !self.backend.modules.is_empty() {
            // Add a None call_stack to inform
            // that we should switch to the next module
            self.call_stack.push(None);
        }

        self.backend.modules.push(ModuleMetadata { module: module.into(), metadata: metadata.into() });
        Ok(())
    }

    // Invoke an entry chunk using its id
    // This will use the latest module added
    pub fn invoke_entry_chunk(&mut self, id: u16) -> Result<(), VMError> {
        if !self.backend.modules.last().map_or(false, |m| m.module.is_entry_chunk(id as usize)) {
            return Err(VMError::ChunkNotEntry);
        }
        self.invoke_chunk_id(id as _)
    }

    // Invoke an entry chunk using its id
    // This will use the latest module added
    pub fn invoke_entry_chunk_with_args<V: Into<StackValue>, I: Iterator<Item = V> + ExactSizeIterator>(&mut self, id: u16, args: I) -> Result<(), VMError> {
        self.invoke_entry_chunk(id)?;
        self.stack.extend_stack(args.map(Into::into))?;
        Ok(())
    }

    // Invoke a hook
    // Return true if the Module has an implementation for the hook
    // Return false if the hook isn't supported
    pub fn invoke_hook_id(&mut self, hook_id: u8) -> Result<bool, VMError> {
        let m = self.backend.modules.last()
            .ok_or(VMError::NoModule)?;

        match m.module.get_chunk_id_of_hook(hook_id) {
            Some(id) => self.invoke_chunk_id(id as _).map(|_| true),
            None => Ok(false)
        }
    }

    // Invoke a hook with args
    // Return true if the Module has an implementation for the hook
    // Return false if the hook isn't supported
    pub fn invoke_hook_id_with_args<V: Into<StackValue>, I: Iterator<Item = V> + ExactSizeIterator>(&mut self, hook_id: u8, args: I) -> Result<bool, VMError> {
        let m = self.backend.modules.last()
            .ok_or(VMError::NoModule)?;

        match m.module.get_chunk_id_of_hook(hook_id) {
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

    // Add the chunk manager to the call stack if required
    // Returns `false` if not added back
    #[inline]
    fn push_back_call_stack(&mut self, mut manager: ChunkManager, reader: ChunkReader<'_>) -> Result<(), VMError> {
        // If tail call optimization is enabled,
        // we have another instruction and that its not a OpCode::Return
        // push current frame back to our call_stack
        // Otherwise, clean pointers for safety reasons
        if !self.tail_call_optimization || reader.has_next_instruction() {
            // Store the current index in the manager
            // so we can store from it again
            manager.set_ip(reader.index());
            // add back our call stack as we need it
            self.call_stack.push(Some(manager));

            Ok(())
        } else {
            self.on_call_stack_end()
        }
    }

    // Called at the end of a call stack
    #[inline]
    fn on_call_stack_end(&mut self) -> Result<(), VMError> {
        // call stack has been fully consummed
        // don't push it back but clean pointers
        self.call_stack_size -= 1;
        self.stack.checkpoint_clean()
    }

    // Run the VM
    // It will execute the bytecode
    // First chunk executed should always return a value
    pub fn run(&mut self) -> Result<ValueCell, VMError> {
        // Freely copy the module has its a reference only
        // We go through every modules injected
        'modules: while let Some(m) = self.backend.modules.last().cloned() {
            'call_stack: while let Some(Some(mut manager)) = self.call_stack.pop() {
                // Retrieve the required chunk
                let chunk = m.module.get_chunk_at(manager.chunk_id())
                    .ok_or(VMError::ChunkNotFound)?;

                // Create the chunk reader for it
                let mut reader = ChunkReader::new(chunk, manager.ip());
                while let Some(opcode) = reader.next_u8() {
                    match self.backend.table.execute(opcode, &self.backend, &mut self.stack, &mut manager, &mut reader, &mut self.context) {
                        Ok(InstructionResult::Nothing) => {},
                        Ok(InstructionResult::InvokeChunk(id)) => {
                            if m.module.is_entry_chunk(id as usize) {
                                return Err(VMError::EntryChunkCalled);
                            }

                            self.push_back_call_stack(manager, reader)?;
                            self.invoke_chunk_id(id as _)?;

                            // Jump to the next call stack
                            continue 'call_stack;
                        },
                        Ok(InstructionResult::AppendModule {
                            module: new_module,
                            metadata,
                            chunk_id
                        }) => {
                            // Push back the current callstack
                            self.push_back_call_stack(manager, reader)?;

                            self.append_module(new_module, metadata)?;
                            self.invoke_chunk_id(chunk_id as _)?;

                            // Jump to the next module
                            continue 'modules;
                        },
                        Ok(InstructionResult::Break) => {
                            break;
                        },
                        Err(e) => {
                            trace!("Error: {:?}", e);
                            trace!("Stack: {:?}", self.stack.get_inner());
                            trace!("Call stack left: {}", self.call_stack.len());
                            trace!("Call stack size: {}", self.call_stack_size);
                            trace!("Current registers: {:?}", manager.get_registers());
                            return Err(e);
                        }
                    }
                }

                self.on_call_stack_end()?;
            }


            // Pop the module because we fully executed it
            // This allow the VM to be fully reusable
            let res = self.backend.modules.pop();
            debug_assert!(res.is_some(), "backend modules must be some");
        }

        debug_assert!(self.call_stack_size == 0, "call stack size is not zero");

        let end_value = self.stack.pop_stack()?
            .into_owned()?;
        if self.stack.count() != 0 {
            return Err(VMError::StackNotCleaned(self.stack.count()));
        }

        Ok(end_value)
    }
}