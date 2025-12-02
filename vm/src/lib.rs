mod chunk;
mod error;
mod iterator;
mod stack;
mod validator;
mod instructions;

#[cfg(test)]
mod tests;

use std::collections::VecDeque;

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
}

impl<'a: 'r, 'ty: 'a, 'r, M> Backend<'a, 'ty, 'r, M> {
    // Get a constant registered in the module using its id
    #[inline(always)]
    pub fn get_constant_with_id(&self, id: usize) -> Result<&ValueCell, VMError> {
        self.modules.last()
            .and_then(|m| m.module.get_constant_at(id))
            .ok_or(VMError::ConstantNotFound(id))
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
    stack: Stack<M>,
    // Context given to each instruction
    context: Context<'ty, 'r>,
    // Flag to enable/disable the tail call optimization
    // in our VM
    tail_call_optimization: bool
}

impl<'a: 'r, 'ty: 'a, 'r, M: 'static> Default for VM<'a, 'ty, 'r, M> {
    fn default() -> Self {
        Self::new(InstructionTable::default(), Context::default())
    }
}

impl<'a: 'r, 'ty: 'a, 'r, M: 'static> VM<'a, 'ty, 'r, M> {
    // Create a new VM with a given table and context
    pub fn new(table: InstructionTable<'a, 'ty, 'r, M>, context: Context<'ty, 'r>) -> Self {
        Self {
            backend: Backend {
                table,
                modules: Vec::with_capacity(1),
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
    pub fn get_stack(&self) -> &Stack<M> {
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

    pub fn backend(&self) -> &Backend<'a, 'ty, 'r, M> {
        &self.backend
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

    // Invoke a chunk using its id
    #[inline(always)]
    pub fn invoke_chunk_id(&mut self, id: usize) -> Result<(), VMError> {
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
    pub fn append_module(&mut self, module: ModuleMetadata<'a, M>) -> Result<(), VMError> {
        if self.backend.modules.len() + 1 >= MODULES_STACK_SIZE {
            return Err(VMError::ModulesStackOverflow)
        }

        if !self.backend.modules.is_empty() {
            // Add a None call_stack to inform
            // that we should switch to the next module
            self.call_stack.push(CallStack::SwitchModule);
        }

        self.backend.modules.push(module);

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

        for arg in args {
            self.push_stack(arg)?;
        }

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

                for arg in args {
                    self.push_stack(arg)?;
                }

                Ok(true)
            },
            None => Ok(false)
        }
    }

    // Push a value to the stack
    #[inline(always)]
    pub fn push_stack<V: Into<StackValue>>(&mut self, value: V) -> Result<(), VMError> {
        let tmp = value.into();

        // we make sure to deep clone it to prevent any issues later
        let value: StackValue = tmp.deep_clone().into();

        let memory_usage = value.estimate_memory_usage(self.context.memory_left())?;
        self.context.increase_memory_usage_unchecked(memory_usage)?;

        self.stack.push_stack(value)
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
        if let Some((origin, max_size)) = manager.registers_origin() {
            debug!("Swapping registers for origin: {}", origin);
            // Swap back our registers
            manager.truncate_registers_to(max_size);
            let previous = self.find_manager_with_chunk_id(origin)
                .ok_or(VMError::ChunkManagerNotFound(origin))?;
            previous.swap_registers(manager);
            manager.set_registers_origin(None);
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
                debug!("Processing call stack: {:?}", call_stack);
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
                                trace!("Result: {:?}", result);
                                match result {
                                    Ok(InstructionResult::Nothing) => {},
                                    Ok(InstructionResult::Break) => {
                                        if let Some(callback) = self.stack.get_callback() {
                                            let mut params = VecDeque::with_capacity(callback.params_len);
                                            for _ in 0..callback.params_len {
                                                params.push_front(self.stack.pop_stack().unwrap());
                                            }

                                            let res = (callback.callback)(
                                                callback.state,
                                                params.into(),
                                            )?;

                                            let tmp = handle_perform_syscall(&mut self.stack, &mut self.context, res, &m)?;
                                            result = perform_syscall(&self.backend, tmp, &mut self.stack, &mut self.context);
                                            continue;
                                        }

                                        trace!("Breaking execution");
                                        break 'opcodes;
                                    },
                                    Ok(InstructionResult::InvokeChunk(id)) => {
                                        trace!("Invoking chunk id: {}", id);
                                        if !m.module.is_callable_chunk(id as usize) {
                                            return Err(VMError::ExpectedNormalChunk);
                                        }

                                        self.push_back_call_stack(manager, reader)?;
                                        self.invoke_chunk_id(id as _)?;
    
                                        // Jump to the next call stack
                                        continue 'call_stack;
                                    },
                                    Ok(InstructionResult::InvokeDynamicChunk { chunk_id, from }) => {
                                        if !m.module.is_callable_chunk(chunk_id) {
                                            return Err(VMError::ExpectedNormalChunk);
                                        }

                                        // If we need to swap the registers, check if from our current manager
                                        // we already took the pointers from another chunk
                                        if let Some((_, len)) = manager.registers_origin().filter(|(origin, _)| *origin == from && chunk_id == manager.chunk_id()) {
                                            trace!("reusing current chunk manager for dynamic call");
                                            manager.truncate_registers_to(len);
                                            manager.set_ip(0);

                                            // Push back our current chunk manager
                                            self.call_stack.push(CallStack::Chunk(manager));
                                        } else {
                                            let mut new = ChunkManager::with(
                                                chunk_id,
                                                None,
                                                Vec::new()
                                            );

                                            trace!("Invoking dynamic chunk id: {} from {:?}", chunk_id, from);
                                            let previous = if manager.chunk_id() == from {
                                                Some(&mut manager)
                                            } else {
                                                self.find_manager_with_chunk_id(from)
                                            };

                                            if let Some(previous) = previous {

                                                new.set_registers_origin(Some((from, previous.get_registers().len())));
                                                new.swap_registers(previous);
                                                previous.set_context(ChunkContext::Used);
                                            } else {
                                                debug!("No chunk manager found for origin {}, skipping it", from);
                                            }

                                            self.push_back_call_stack(manager, reader)?;

                                            // Add our new chunk
                                            self.invoke_chunk_id_internal(new)?;
                                        }

                                        // Jump to the next call stack
                                        continue 'call_stack;
                                    },
                                    Ok(InstructionResult::AsyncCall { .. }) => {
                                        while let Ok(InstructionResult::AsyncCall { ptr, instance, mut params }) = result {
                                            let on_value = if instance {
                                                let instance = params.pop_front()
                                                    .ok_or(EnvironmentError::MissingInstanceFnCall)?;

                                                verify_instance_against_parameters(&instance, params.make_contiguous())?;

                                                Ok(instance)
                                            } else {
                                                Err(EnvironmentError::FnExpectedInstance)
                                            };

                                            let res = ptr(on_value, params.into(), &m, &mut self.context).await?;
                                            result = match handle_perform_syscall(&mut self.stack, &mut self.context, res, &m) {
                                                Ok(syscall @ PerformSysCallHelper::Next { .. }) => perform_syscall(&self.backend, syscall, &mut self.stack, &mut self.context),
                                                Ok(PerformSysCallHelper::End(res)) => Ok(res),
                                                Err(e) => return Err(e)
                                            };
                                        }

                                        // Skip the break at the end of the loop to handle
                                        // our new result
                                        continue;
                                    },
                                    Ok(InstructionResult::AppendModule {
                                        module: new_module,
                                        chunk_id
                                    }) => {
                                        // Can only call public chunks from new module
                                        // This allow for a module to be fully private if wanted
                                        if !new_module.module.is_public_chunk(chunk_id as usize) {
                                            return Err(VMError::ExpectedPublicChunk);
                                        }

                                        // Push back the current callstack
                                        self.push_back_call_stack(manager, reader)?;
    
                                        self.append_module(new_module)?;
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
                        } else {
                            for value in manager.get_registers() {
                                let memory = value.estimate_memory_usage(self.context.max_memory_usage())?;
                                self.context.decrease_memory_usage(memory);
                            }
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
            if self.backend.modules.pop().is_none() {
                return Err(VMError::NoModule);
            }
        }

        if self.call_stack_size != 0 {
            debug!("Call stack size left: {:?}", self.call_stack);
            return Err(VMError::CallStackNotEmpty(self.call_stack_size));
        }

        let end_value = self.stack.pop_stack()?
            .into_owned();
        if self.stack.count() != 0 {
            return Err(VMError::StackNotCleaned(self.stack.count()));
        }

        Ok(end_value)
    }
}