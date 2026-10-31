use std::{mem, future::Future, pin::Pin, task::{Context, Poll}};
use futures::future::LocalBoxFuture;
use super::*;
use crate::stack::Stack;

/// No constructor accepts an arbitrary local future. Only VM::run constructs
/// this wrapper around this engine, with Send foreign captures and scoped natives.
pub(super) struct ConfinedRun<'run> {
    inner: LocalBoxFuture<'run, Result<OwnedValue, VMError>>,
}

// SAFETY: all strong/weak Rc owners live inside this concrete engine future.
// Safe extensions receive scoped views or owned data; trusted raw extensions
// explicitly opt into this invariant with an unsafe constructor. Poll and Drop
// require exclusive ownership. A synchronized handoff moves every alias together.
unsafe impl Send for ConfinedRun<'_> {}

impl<'run> ConfinedRun<'run> {
    pub(super) fn new<'a: 'r, 'ty: 'a, 'r, M: Send + Sync + 'static>(vm: &'run mut VM<'a, 'ty, 'r, M>) -> Self {
        // These foreign captures must be Send independently of the private Rc graph.
        fn require_send<T: Send>(_: &T) {}
        require_send(&vm.backend);
        require_send(&vm.context);

        Self {
            inner: Box::pin(vm.run_local()),
        }
    }
}

impl Future for ConfinedRun<'_> {
    type Output = Result<OwnedValue, VMError>;

    fn poll(mut self: Pin<&mut Self>, context: &mut Context<'_>) -> Poll<Self::Output> {
        self.inner.as_mut().poll(context)
    }
}

// Keep ordinary instructions in a synchronous loop. Control flow and native
// suspension return to the async engine with the same per-instruction gas checks.
// We never inline the function to prevent being wrapped into the async state machine.
#[inline(never)]
fn execute_sync<'a: 'r, 'ty: 'a, 'r, M: 'static>(
    backend: &Backend<'a, 'ty, 'r, M>,
    stack: &mut Stack<M>,
    manager: &mut ChunkManager,
    reader: &mut ChunkReader<'_>,
    context: &mut VMContext<'ty, 'r>,
    heap: &mut Heap,
) -> Option<Result<InstructionResult<'a, M>, VMError>> {
    while let Some(opcode) = reader.next_u8() {
        let result = backend.table.execute(opcode, backend, stack, manager, reader, context, heap);
        if !matches!(result, Ok(InstructionResult::Nothing)) {
            return Some(result);
        }

        trace!("opcode result: {:?}", result);
    }
    None
}

pub(super) struct Execution<'vm, 'a: 'r, 'ty: 'a, 'r, M: 'static> {
    backend: &'vm mut Backend<'a, 'ty, 'r, M>,
    context: &'vm mut VMContext<'ty, 'r>,
    stack: Stack<M>,
    call_stack: Vec<CallStack>,
    call_stack_size: usize,
    pending_values: Vec<OwnedValue>,
    tail_call_optimization: bool,
}

impl<M: 'static> Drop for Execution<'_, '_, '_, '_, M> {
    fn drop(&mut self) {
        self.backend.modules.clear();
    }
}

impl<'vm, 'a: 'r, 'ty: 'a, 'r, M: 'static> Execution<'vm, 'a, 'ty, 'r, M> {
    pub(super) fn new(vm: &'vm mut VM<'a, 'ty, 'r, M>) -> Self {
        let calls = mem::take(&mut vm.queued_calls).into_iter().map(|call| {
            match call {
                Some(id) => CallStack::Chunk(ChunkManager::new(id)),
                None => CallStack::SwitchModule,
            }
        }).collect();
        let count = mem::take(&mut vm.call_stack_size);
        let pending_values = mem::take(&mut vm.pending_values);
        Self {
            backend: &mut vm.backend,
            context: &mut vm.context,
            call_stack: calls,
            call_stack_size: count,
            stack: Stack::new(),
            pending_values,
            tail_call_optimization: vm.tail_call_optimization,
        }
    }

    fn invoke_chunk_id_unchecked(&mut self, id: usize) -> Result<(), VMError> {
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

    fn append_module(&mut self, module: ModuleMetadata<'a, M>) -> Result<(), VMError> {
        if self.backend.modules.len() + 1 >= MODULES_STACK_SIZE {
            return Err(VMError::ModulesStackOverflow);
        }
        if !self.backend.modules.is_empty() {
            self.call_stack.push(CallStack::SwitchModule);
        }
        self.backend.modules.push(module);
        Ok(())
    }

    // Add the chunk manager to the call stack if required
    // Returns `false` if not added back
    #[inline]
    fn push_back_call_stack(&mut self, mut manager: ChunkManager, reader: ChunkReader<'_>) {
        // If tail call optimization is enabled,
        // we have another instruction and that its not a OpCode::Return
        // push current frame back to our call_stack
        // Otherwise, dropping the frame releases its registers and iterators
        if !self.tail_call_optimization || reader.has_next_instruction() || matches!(manager.context(), ChunkContext::ShouldKeep) {
            // Store the current index in the manager
            // so we can store from it again
            manager.set_ip(reader.index());
            manager.set_context(ChunkContext::Pending);
            // add back our call stack as we need it
            self.call_stack.push(CallStack::Chunk(manager));
        } else {
            self.on_call_stack_end(&mut manager)
        }
    }

    // Called at the end of a call stack
    #[inline]
    fn on_call_stack_end(&mut self, manager: &mut ChunkManager) {
        debug!("on call stack end: {:?}", manager);
        self.call_stack_size -= 1;
        // Closure managers hold a Vec of heap handles into the parent's register slots.
        // Because every VM mutation writes through those heap handles, the parent's registers are
        // always up-to-date when the closure finishes. No swap or snapshot restore needed;
        // just clear the origin tag so the slot can be reused.
        manager.set_registers_origin(None);
    }

    // Find the chunk manager for the requested chunk id
    // If we change from one to another module, don't return it
    #[inline]
    fn find_manager_with_chunk_id(call_stack: &mut [CallStack], chunk_id: usize) -> Option<&mut ChunkManager> {
        for call_stack in call_stack.iter_mut().rev() {
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

    pub(super) async fn run(&mut self) -> Result<OwnedValue, VMError> {
        // No heap or owning runtime handle can escape through a safe extension.
        let mut heap = unsafe { Heap::new() };
        for value in mem::take(&mut self.pending_values) {
            let value = heap.import_owned(value);
            self.stack.push_stack(value.into())?;
        }

        // Clone module metadata while executing each queued module.
        'modules: while let Some(m) = self.backend.modules.last().cloned() {
            'call_stack: while let Some(call_stack) = self.call_stack.pop() {
                debug!("Processing call stack: {:?} with stack [{}]", call_stack, self.stack.get_inner().iter().map(|v| format!("{}", v.as_ref(&heap).display(&heap))).collect::<Vec<_>>().join(", "));
                match call_stack {
                    CallStack::Chunk(mut manager) => {
                        // Retrieve the required chunk
                        let chunk = m.module.get_chunk_at(manager.chunk_id())
                            .ok_or_else(|| VMError::ChunkNotFound)?;

                        // Create the chunk reader for it
                        let mut reader = ChunkReader::new(&chunk.chunk, manager.ip());
                        'opcodes: while let Some(mut result) = execute_sync(
                            self.backend,
                            &mut self.stack,
                            &mut manager,
                            &mut reader,
                            self.context,
                            &mut heap,
                        ) {
                            // Native calls can return another async call to process.
                            loop {
                                trace!("opcode result: {:?}", result);
                                match result {
                                    Ok(InstructionResult::Nothing) => {},
                                    Ok(InstructionResult::Break) => {
                                        if let Some(callback) = self.stack.get_callback() {
                                            let mut params = self.stack.pop_arguments(callback.params_len)?;
                                            let res = match callback.callback {
                                                CallbackType::Sync(f) => f(
                                                    callback.state,
                                                    &mut params,
                                                    &mut heap,
                                                ),
                                                CallbackType::Async(f) => f(
                                                    callback.state,
                                                    &mut params,
                                                    &mut heap,
                                                ).await,
                                            };
                                            let res = res?;

                                            result = complete_syscall(self.backend, &mut self.stack, self.context, res, &m, &mut heap);
                                            // If the callback's result is Nothing, the closure that
                                            // triggered the Return has finished. Break out of the
                                            // opcodes loop to avoid executing dead code that may
                                            // follow the Return instruction (e.g. the else-branch
                                            // of an early-return if/else in the closure body).
                                            if !matches!(result, Ok(InstructionResult::Nothing)) {
                                                continue;
                                            }
                                        }

                                        trace!("Breaking execution");
                                        break 'opcodes;
                                    },
                                    Ok(InstructionResult::InvokeChunk(id)) => {
                                        trace!("Invoking chunk id: {}", id);
                                        if !m.module.is_callable_chunk(id as usize) {
                                            return Err(VMError::ExpectedNormalChunk);
                                        }

                                        self.push_back_call_stack(manager, reader);
                                        self.invoke_chunk_id_unchecked(id as _)?;
    
                                        // Jump to the next call stack
                                        continue 'call_stack;
                                    },
                                    Ok(InstructionResult::InvokeDynamicChunk { chunk_id, from }) => {
                                        if !m.module.is_callable_chunk(chunk_id) {
                                            return Err(VMError::ExpectedNormalChunk);
                                        }

                                        // If we need to swap the registers, check if from our current manager
                                        // we already took the pointers from another chunk
                                        if manager.registers_origin().is_some_and(|origin| origin == from && chunk_id == manager.chunk_id()) {
                                            trace!("reusing current chunk manager for dynamic call");
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
                                            // The source of captured registers is either the current manager
                                            // itself (when it IS `from`, or already proxies `from`'s captures
                                            // via heap handles) or a manager found deeper on the call stack.
                                            let manager_is_proxy = manager.registers_origin() == Some(from);
                                            let previous = if manager.chunk_id() == from || manager_is_proxy {
                                                Some(&mut manager)
                                            } else {
                                                Self::find_manager_with_chunk_id(&mut self.call_stack, from)
                                            };

                                            if let Some(previous) = previous {
                                                new.set_registers_origin(Some(from));
                                                // Give the new closure a Vec of heap handles into the
                                                // parent's register slots. Because every assignment in the
                                                // VM writes through those heap handles (via `as_mut()`), the
                                                // parent's values stay up-to-date automatically – no swap
                                                // or snapshot restore is needed when the closure exits.
                                                *new.get_registers_mut() = previous
                                                    .get_registers_mut()
                                                    .iter_mut()
                                                    .map(|value| value.reference(&mut heap))
                                                    .collect();
                                                previous.set_context(ChunkContext::Used);
                                            } else {
                                                debug!("No chunk manager found for origin {}, skipping it", from);
                                            }

                                            // If the current manager was itself a proxy
                                            // we can remove it directly
                                            if manager_is_proxy {
                                                self.on_call_stack_end(&mut manager);
                                            } else {
                                                self.push_back_call_stack(manager, reader);
                                            }

                                            // Add our new chunk
                                            self.invoke_chunk_id_internal(new)?;
                                        }

                                        // Jump to the next call stack
                                        continue 'call_stack;
                                    },
                                    Ok(InstructionResult::AsyncCall { .. }) => {
                                        while let Ok(InstructionResult::AsyncCall { ptr, instance, mut params }) = result {
                                            let mut on_value = if instance {
                                                if params.is_empty() {
                                                    return Err(EnvironmentError::MissingInstanceFnCall.into());
                                                }
                                                let instance = params.remove(0);

                                                verify_instance_against_parameters(&instance, &mut params, &mut heap)?;

                                                Some(instance)
                                            } else {
                                                None
                                            };

                                            // Keep argument cleanup before instance cleanup across suspension.
                                            let mut arguments = params;
                                            let instance = on_value.as_mut()
                                                .ok_or_else(|| EnvironmentError::FnExpectedInstance);
                                            let res = ptr.call(instance, &mut arguments, &m, self.context, &mut heap).await?;
                                            result = complete_syscall(self.backend, &mut self.stack, self.context, res, &m, &mut heap);
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
                                        self.push_back_call_stack(manager, reader);
    
                                        self.append_module(new_module)?;
                                        self.invoke_chunk_id_unchecked(chunk_id as _)?;
    
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

                        self.on_call_stack_end(&mut manager);

                        if matches!(manager.context(), ChunkContext::ShouldKeep) && self.call_stack_size != 0 {
                            manager.set_context(ChunkContext::Pending);
                            // we must keep it, only clean the stack
                            self.call_stack.push(CallStack::Context(manager));

                            // We keep it to prevent any DoS using closures
                            self.call_stack_size += 1;
                        } else {
                            for value in manager.get_registers() {
                                let memory = value.estimate_memory_usage(&heap, self.context.max_memory_usage())?;
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

        let end_value = self.stack.pop_stack()?;
        if self.stack.count() != 0 {
            return Err(VMError::StackNotCleaned(self.stack.count()));
        }

        heap.export(end_value).map_err(VMError::from)
    }
}