mod chunk;
mod error;
mod iterator;
mod stack;
mod validator;
mod instructions;
mod execution;

#[cfg(test)]
mod tests;

use std::future::Future;
use execution::{ConfinedRun, Execution};


// Re-export the necessary types
pub use silex_environment::*;
pub use silex_types::*;
pub use silex_bytecode::*;

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
    pub fn get_constant_with_id(&self, id: usize) -> Result<&OwnedValue, VMError> {
        self.modules.last()
            .and_then(|m| m.module.get_constant_at(id))
            .ok_or_else(|| VMError::ConstantNotFound(id))
    }

    // Get the current module with its associated metadata
    #[inline(always)]
    pub fn current(&self) -> Result<&ModuleMetadata<'a, M>, VMError> {
        self.modules.last()
            .ok_or_else(|| VMError::NoModule)
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
    // Queued calls and inputs contain portable data only.
    queued_calls: Vec<Option<usize>>,
    call_stack_size: usize,
    pending_values: Vec<OwnedValue>,
    // Context given to each instruction
    context: VMContext<'ty, 'r>,
    // Flag to enable/disable the tail call optimization
    // in our VM
    tail_call_optimization: bool
}

impl<'a: 'r, 'ty: 'a, 'r, M: 'static> Default for VM<'a, 'ty, 'r, M> {
    fn default() -> Self {
        Self::new(InstructionTable::default(), VMContext::default())
    }
}

impl<'a: 'r, 'ty: 'a, 'r, M: 'static> VM<'a, 'ty, 'r, M> {
    // Create a new VM with a given table and context
    pub fn new(table: InstructionTable<'a, 'ty, 'r, M>, context: VMContext<'ty, 'r>) -> Self {
        Self {
            backend: Backend {
                table,
                modules: Vec::with_capacity(1),
            },
            queued_calls: Vec::with_capacity(4),
            call_stack_size: 0,
            pending_values: Vec::new(),
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

    // Whether any portable work remains queued.
    #[inline(always)]
    pub fn is_idle(&self) -> bool {
        self.queued_calls.is_empty() && self.pending_values.is_empty() && self.backend.modules.is_empty()
    }

    // Get the context
    #[inline(always)]
    pub fn context(&self) -> &VMContext<'ty, 'r> {
        &self.context
    }

    // Get a mutable reference to the context
    #[inline(always)]
    pub fn context_mut(&mut self) -> &mut VMContext<'ty, 'r> {
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
    pub fn invoke_chunk_id_unchecked(&mut self, id: usize) -> Result<(), VMError> {
        if self.call_stack_size + 1 >= CALL_STACK_SIZE {
            return Err(VMError::CallStackOverflow);
        }
        self.queued_calls.push(Some(id));
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
            self.queued_calls.push(None);
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

        self.invoke_chunk_id_unchecked(id as _)
    }

    // Invoke a chunk using its id with args
    // This will use the latest module added
    // You can provide arguments to push to the stack before invoking the chunk
    // The arguments will be verified against the chunk parameters if any
    // The parameters are reversed when pushed to the stack to respect the stack order
    pub fn invoke_chunk_with_args<V: Into<OwnedValue>, I: DoubleEndedIterator<Item = V> + ExactSizeIterator>(&mut self, id: u16, args: I) -> Result<(), VMError> {
        let Some(m) = self.backend.modules.last() else {
            return Err(VMError::NoModule);
        };

        let chunk = m.module.get_chunk_at(id as usize)
            .ok_or_else(|| VMError::ChunkNotFound)?;

        let args_count = args.len();
        match &chunk.access {
            Access::Entry { parameters } | Access::All { parameters } => {
                // If we have enforced parameters to verify, process it
                if let Some(params) = parameters {
                    if params.len() != args_count {
                        return Err(VMError::InvalidEntryParametersCount {
                            expected: params.len(),
                            found: args_count,
                        });
                    }

                    // We reverse the params because we need to push them in the stack order
                    // So the first parameter will be the first to be popped from stack
                    let mut checked_args = Vec::with_capacity(args_count);
                    for (i, (expected, provided)) in params.iter().zip(args).enumerate() {
                        let value: OwnedValue = provided.into();
                        if !expected.check_with_fn(&value, |opaque, ty| {
                            m.environment.get_opaques()
                                .get_index(ty as usize)
                                .is_some_and(|(v, external)| *v == opaque.get_type_id() && *external)
                        }) {
                            return Err(VMError::InvalidEntryParameterType(i));
                        }

                        checked_args.push(value);
                    }

                    for arg in checked_args.into_iter().rev() {
                        self.push_stack(arg)?;
                    }
                } else {
                    // no enforced parameters set for the chunk
                    // just push them as is
                    for arg in args.rev() {
                        self.push_stack(arg)?;
                    }
                }
            },
            _ => {
                for arg in args.rev() {
                    self.push_stack(arg)?;
                }
            }
        }

        self.invoke_chunk_id_unchecked(id as _)
    }

    // Invoke a hook
    // Return true if the Module has an implementation for the hook
    // Return false if the hook isn't supported
    pub fn invoke_hook_id(&mut self, hook_id: u8) -> Result<bool, VMError> {
        let m = self.backend.modules.last()
            .ok_or_else(|| VMError::NoModule)?;

        match m.module.get_chunk_id_of_hook(hook_id) {
            Some(id) => self.invoke_chunk_id_unchecked(id as _).map(|_| true),
            None => Ok(false)
        }
    }

    // Invoke a hook with args
    // Return true if the Module has an implementation for the hook
    // Return false if the hook isn't supported
    pub fn invoke_hook_id_with_args<V: Into<OwnedValue>, I: DoubleEndedIterator<Item = V> + ExactSizeIterator>(&mut self, hook_id: u8, args: I) -> Result<bool, VMError> {
        let m = self.backend.modules.last()
            .ok_or_else(|| VMError::NoModule)?;

        match m.module.get_chunk_id_of_hook(hook_id) {
            Some(id) => {
                self.invoke_chunk_with_args(id as _, args)?;
                Ok(true)
            },
            None => Ok(false)
        }
    }

    // Push a value to the stack
    #[inline]
    fn push_stack<V: Into<OwnedValue>>(&mut self, value: V) -> Result<(), VMError> {
        let value: OwnedValue = value.into();
        let memory_usage = value.calculate_memory_usage(self.context.memory_left())?;
        self.context.increase_memory_usage_unchecked(memory_usage)?;
        if self.pending_values.len() >= stack::STACK_SIZE {
            return Err(VMError::StackOverflow);
        }
        self.pending_values.push(value);
        Ok(())
    }

    /// Execute on the current thread. Runtime ownership is private to this call.
    pub fn run_blocking(&mut self) -> Result<OwnedValue, VMError> {
        futures::executor::block_on(self.run_local())
    }

    async fn run_local(&mut self) -> Result<OwnedValue, VMError> {
        let mut execution = Execution::new(self);
        execution.run().await
    }

    /// Execute with all runtime owners confined to one movable, exclusive future.
    /// Only portable values cross the host boundary.
    ///
    /// ```compile_fail
    /// use xelis_vm::VM;
    /// fn require_sync<T: Sync>(_: T) {}
    /// let mut vm = VM::<()>::default();
    /// require_sync(vm.run());
    /// ```
    pub fn run(&mut self) -> impl Future<Output = Result<OwnedValue, VMError>> + Send + '_
    where
        M: Send + Sync,
    {
        ConfinedRun::new(self)
    }
}