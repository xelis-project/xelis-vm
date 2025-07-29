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
    environment: &'a Environment<M>,
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

#[derive(Debug)]
enum CallStack {
    // When we should move to the next module
    SwitchModule,
    // When we have a new chunk manager in the queue
    Chunk(ChunkManager),
    // only kept has an alive context
    Context(ChunkManager),
}

// Virtual Machine to execute the bytecode from chunks of a Module.
pub struct VM<'a: 'r, 'ty: 'a, 'r, M: 'static> {
    backend: Backend<'a, 'ty, 'r, M>,
    // The call stack of the VM
    // Every chunks to proceed are stored here
    // It is behind an Option so we know
    // when we have to switch the module
    call_stack: Vec<CallStack>,
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

impl<'a: 'r, 'ty: 'a, 'r, M: 'static> VM<'a, 'ty, 'r, M> {
    // Create a new VM
    // Insert the environment as a reference in the context
    pub fn new(environment: &'a Environment<M>) -> Self {
        let mut context = Context::default();
        context.insert_ref(environment);

        Self::with(environment, Default::default(), context)
    }

    // Create a new VM with a given table and context
    pub fn with(environment: &'a Environment<M>, table: InstructionTable<'a, 'ty, 'r, M>, context: Context<'ty, 'r>) -> Self {
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
    pub fn environment(&self) -> &Environment<M> {
        self.backend.environment
    }

    // Invoke a chunk using its id
    #[inline(always)]
    pub(crate) fn invoke_chunk_id(&mut self, id: usize) -> Result<(), VMError> {
        self.invoke_chunk_id_internal(ChunkManager::new(id))
    }

    // Invoke a chunk using its id
    pub(crate) fn invoke_chunk_id_internal(&mut self, manager: ChunkManager) -> Result<(), VMError> {
        debug!("Invoking chunk id: {}", manager.chunk_id());
        if self.call_stack_size + 1 >= CALL_STACK_SIZE {
            return Err(VMError::CallStackOverflow);
        }

        self.call_stack.push(CallStack::Chunk(manager));
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
            self.call_stack.push(CallStack::SwitchModule);
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
    #[inline(always)]
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
        if !self.tail_call_optimization || reader.has_next_instruction() || matches!(manager.context(), ChunkContext::ShouldKeep) {
            // Store the current index in the manager
            // so we can store from it again
            manager.set_ip(reader.index());
            manager.set_context(ChunkContext::Pending);
            // add back our call stack as we need it
            self.call_stack.push(CallStack::Chunk(manager));

            Ok(())
        } else {
            self.on_call_stack_end(&mut manager)
        }
    }

    // Called at the end of a call stack
    #[inline]
    fn on_call_stack_end(&mut self, manager: &mut ChunkManager) -> Result<(), VMError> {
        debug!("on call stack end: {:?}", manager);
        // call stack has been fully consummed
        // don't push it back but clean pointers
        self.call_stack_size -= 1;
        if let Some(origin) = manager.registers_origin() {
            debug!("Swapping registers for origin: {}", origin);
            // Swap back our registers
            let previous = self.find_manager_with_chunk_id(origin)
                .ok_or(VMError::ChunkManagerNotFound(origin))?;
            previous.swap_registers(manager);
        }

        Ok(())
    }

    // Find the chunk manager for the requested chunk id
    // If we change from one to another module, don't return it
    #[inline]
    fn find_manager_with_chunk_id(&mut self, chunk_id: usize) -> Option<&mut ChunkManager> {
        for call_stack in self.call_stack.iter_mut().rev() {
            match call_stack {
                CallStack::Chunk(chunk)
                | CallStack::Context(chunk)
                if chunk.chunk_id() == chunk_id => return Some(chunk),
                CallStack::SwitchModule => break,
                _ => {},
            }
        }

        None
    }

    // Run the VM in a blocking way
    #[inline(always)]
    pub fn run_blocking(&mut self) -> Result<ValueCell, VMError> {
        futures::executor::block_on(self.run())
    }

    // Run the VM
    // It will execute the bytecode
    // First chunk executed should always return a value
    // Due to the `async` support, VM is 10-20% slower
    // than its full `sync` mode.
    // TODO: provide a `sync` only mode for performance
    pub async fn run(&mut self) -> Result<ValueCell, VMError> {
        // Freely copy the module has its a reference only
        // We go through every modules injected
        'modules: while let Some(m) = self.backend.modules.last().cloned() {
            'call_stack: while let Some(call_stack) = self.call_stack.pop() {
                match call_stack {
                    CallStack::Chunk(mut manager) => {
                        // Retrieve the required chunk
                        let chunk = m.module.get_chunk_at(manager.chunk_id())
                            .ok_or(VMError::ChunkNotFound)?;

                        // Create the chunk reader for it
                        let mut reader = ChunkReader::new(chunk, manager.ip());
                        'opcodes: while let Some(opcode) = reader.next_u8() {
                            let mut result = self.backend.table.execute(
                                opcode,
                                &self.backend,
                                &mut self.stack,
                                &mut manager,
                                &mut reader,
                                &mut self.context
                            );

                            // Loop is required in the case of recursive async call
                            // And to prevent duplicated code for the final result
                            // This still cause us a 10-20% performance downgrade
                            loop {
                                match result {
                                    Ok(InstructionResult::Nothing) => {},
                                    Ok(InstructionResult::Break) => break 'opcodes,
                                    Ok(InstructionResult::InvokeChunk(id)) => {
                                        if m.module.is_entry_chunk(id as usize) {
                                            return Err(VMError::EntryChunkCalled);
                                        }
    
                                        self.push_back_call_stack(manager, reader)?;
                                        self.invoke_chunk_id(id as _)?;
    
                                        // Jump to the next call stack
                                        continue 'call_stack;
                                    },
                                    Ok(InstructionResult::InvokeDynamicChunk { chunk_id, from }) => {
                                        if m.module.is_entry_chunk(chunk_id) {
                                            return Err(VMError::EntryChunkCalled);
                                        }
    
                                        let mut new = ChunkManager::with(
                                            chunk_id,
                                            None,
                                            Vec::new()
                                        );
    
                                        if let Some(previous) = self.find_manager_with_chunk_id(from) {
                                            new.swap_registers(previous);
                                            previous.set_context(ChunkContext::Used);
                                        } else {
                                            trace!("No chunk manager found for origin {}, skipping it", from);
                                            new.set_registers_origin(None);
                                        }
    
                                        self.push_back_call_stack(manager, reader)?;
    
                                        // Add our new chunk
                                        self.invoke_chunk_id_internal(new)?;
    
                                        // Jump to the next call stack
                                        continue 'call_stack;
                                    },
                                    Ok(InstructionResult::AsyncCall { .. }) => {
                                        while let Ok(InstructionResult::AsyncCall { ptr, instance, mut params }) = result {
                                            let mut on_value = if instance {
                                                let instance = params.pop_front()
                                                    .ok_or(EnvironmentError::MissingInstanceFnCall)?;

                                                // Required to prevent having a BorrowMut & BorrowRef at same time
                                                if !instance.is_owned() {
                                                    for param in params.iter_mut() {
                                                        if param.ptr_eq(&param) {
                                                            *param = param.to_owned();
                                                        }
                                                    }
                                                }

                                                Some(instance)
                                            } else {
                                                None
                                            };

                                            let instance = on_value.as_mut()
                                                .map(|v| v.as_mut())
                                                .ok_or(EnvironmentError::FnExpectedInstance);

                                            let res = ptr(instance, params.into(), &mut self.context).await?;
                                            result = match handle_perform_syscall(&self.backend, &mut self.stack, &mut self.context, res) {
                                                Ok(PerformSysCallHelper::Next { f, params }) => perform_syscall(&self.backend, f, params, &mut self.stack, &mut self.context),
                                                Ok(PerformSysCallHelper::End(res)) => Ok(res),
                                                Err(e) => Err(e)
                                            };
                                        }

                                        // Skip the break at the end of the loop to handle
                                        // our new result
                                        continue;
                                    },
                                    Ok(InstructionResult::AppendModule {
                                        module: new_module,
                                        metadata,
                                        chunk_id
                                    }) => {
                                        // Can only call entry chunks from new module
                                        if !new_module.is_entry_chunk(chunk_id as usize) {
                                            return Err(VMError::EntryChunkCalled);
                                        }
    
                                        // Push back the current callstack
                                        self.push_back_call_stack(manager, reader)?;
    
                                        self.append_module(new_module, metadata)?;
                                        self.invoke_chunk_id(chunk_id as _)?;
    
                                        // Jump to the next module
                                        continue 'modules;
                                    },
                                    Err(e) => {
                                        trace!("Error: {:?}", e);
                                        trace!("Stack: {:?}", self.stack.get_inner());
                                        trace!("Call stack left: {}", self.call_stack.len());
                                        trace!("Call stack size: {}", self.call_stack_size);
                                        trace!("Current registers: {:?}", manager.get_registers());
                                        return Err(e);
                                    }
                                };

                                break;
                            }
                        }

                        self.on_call_stack_end(&mut manager)?;

                        if matches!(manager.context(), ChunkContext::ShouldKeep) && self.call_stack_size != 0 {
                            manager.set_context(ChunkContext::Pending);
                            // we must keep it, only clean the stack
                            self.call_stack.insert(self.call_stack.len() - 1, CallStack::Context(manager));

                            // We keep it to prevent any DoS using closures
                            self.call_stack_size += 1;
                        }
                    },
                    CallStack::SwitchModule => break 'call_stack,
                    CallStack::Context(manager) => {
                        if matches!(manager.context(), ChunkContext::Pending) {
                            // If our previous call stack isn't a chunk, return an error
                            if self.call_stack.last().map_or(true, |v| !matches!(v, CallStack::Chunk(_))) {
                                return Err(VMError::CallStackUnderflow)
                            }

                            // re-inject it to the previous one over and over until its finally used
                            self.call_stack.insert(self.call_stack.len() - 1, CallStack::Context(manager));
                        } else {
                            self.call_stack_size -= 1;
                        }

                        continue 'call_stack;
                    },
                };
            }

            // Pop the module because we fully executed it
            // This allow the VM to be fully reusable
            let res = self.backend.modules.pop();
            debug_assert!(res.is_some(), "backend modules must be some");
        }

        debug_assert!(self.call_stack_size == 0, "call stack size is not zero");

        let end_value = self.stack.pop_stack()?
            .into_owned();
        if self.stack.count() != 0 {
            return Err(VMError::StackNotCleaned(self.stack.count()));
        }

        Ok(end_value)
    }
}

// SAFETY: it is safe to move it between threads
// because no pointer is available from our API
// unsafe impl<'a: 'r, 'ty: 'a, 'r, M: Send + 'static> Send for VM<'a, 'ty, 'r, M> {}