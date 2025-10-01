mod error;

use std::{collections::HashSet, iter};
use log::{trace, warn};
use xelis_ast::{
    Expression,
    FunctionType,
    FunctionVisibility,
    MatchStatement,
    Operator,
    Program,
    Statement,
    TupleStatement
};
use xelis_environment::Environment;
use xelis_bytecode::{Chunk, Module, OpCode};

pub use error::CompilerError;
use xelis_types::{Constant, Primitive};

// Temporary invalid address to patch jumps
const INVALID_ADDR: u32 = 0xDEADBEEF;

pub struct Compiler<'a, M> {
    // Program to compile
    program: &'a Program,
    environment: &'a Environment<M>,
    // Final module to return
    module: Module,
    // Index of break jump to patch
    loop_break_patch: Vec<Vec<usize>>,
    // Index of continue jump to patch
    loop_continue_patch: Vec<Vec<usize>>,
    // Used for OpCode::MemorySet
    // For each scope, we store the next id to use
    // So, outside of a scope we reset to the same level
    memstore_ids: Vec<u16>,
    // Track each values that are pushed on the stack
    // This is used to know how many values are on the stack
    // and prevent any dangling values
    // Each element is a scope, where its elements are the index of each
    values_on_stack: Vec<Vec<usize>>,
    // We must track function parameters
    // and clone them on first assignation
    parameters_ids: HashSet<u16>,
}

impl<'a, M> Compiler<'a, M> {
    // Create a new compiler
    pub fn new(program: &'a Program, environment: &'a Environment<M>) -> Self {
        Self {
            program,
            environment,
            module: Module::new(),
            loop_break_patch: Vec::new(),
            loop_continue_patch: Vec::new(),
            memstore_ids: Vec::new(),
            values_on_stack: Vec::new(),
            parameters_ids: HashSet::new()
        }
    }

    // Map the operator to the opcode
    fn map_operator_to_opcode(op: &Operator) -> Result<OpCode, CompilerError> {
        trace!("Mapping operator to opcode: {:?}", op);
        Ok(match op {
            Operator::Add => OpCode::Add,
            Operator::Sub => OpCode::Sub,
            Operator::Mul => OpCode::Mul,
            Operator::Div => OpCode::Div,
            Operator::Mod => OpCode::Mod,
            Operator::Pow => OpCode::Pow,

            Operator::BitwiseAnd => OpCode::BitwiseAnd,
            Operator::BitwiseOr => OpCode::BitwiseOr,
            Operator::BitwiseXor => OpCode::BitwiseXor,
            Operator::BitwiseShl => OpCode::BitwiseShl,
            Operator::BitwiseShr => OpCode::BitwiseShr,

            Operator::Eq => OpCode::Eq,
            Operator::Neq => OpCode::Neg,
            Operator::Gt => OpCode::Gt,
            Operator::Gte => OpCode::Gte,
            Operator::Lt => OpCode::Lt,
            Operator::Lte => OpCode::Lte,

            // Assigns
            Operator::Assign(Some(inner)) => Self::map_operator_to_opcode(inner)?
                .as_assign_operator()
                .ok_or(CompilerError::ExpectedOperatorAssignment(*inner.clone()))?,

            // These operators are handled differently
            Operator::Assign(None)
            | Operator::And
            | Operator::Or => return Err(CompilerError::UnexpectedOperator(op.clone())),
        })
    }

    // Emit a memory store
    fn memstore(&mut self, chunk: &mut Chunk) -> Result<(), CompilerError> {
        trace!("Emitting memory store");
        chunk.emit_opcode(OpCode::MemorySet);
        let id = self.memstore_ids.last_mut()
            .ok_or(CompilerError::ExpectedMemstoreId)?;

        trace!("memory set at {}", id);
        chunk.write_u16(*id);
        *id += 1;

        self.decrease_values_on_stack()?;

        Ok(())
    }

    #[inline(always)]
    fn add_values_on_stack(&mut self, index: usize, count: usize) -> Result<(), CompilerError> {
        trace!("Adding {} values on stack at instruction {}", count, index);
        let on_stack = self.values_on_stack.last_mut()
            .ok_or(CompilerError::ExpectedStackScope)?;
        on_stack.extend(iter::repeat(index).take(count));

        Ok(())
    }

    #[inline(always)]
    fn add_value_on_stack(&mut self, index: usize) -> Result<(), CompilerError> {
        self.add_values_on_stack(index, 1)
    }

    #[inline(always)]
    fn decrease_values_on_stack(&mut self) -> Result<(), CompilerError> {
        self.decrease_values_on_stack_by(1)
    }

    #[inline(always)]
    fn decrease_values_on_stack_by(&mut self, amount: usize) -> Result<(), CompilerError> {
        trace!("Decreasing values on stack by {}", amount);
        let on_stack = self.values_on_stack.last_mut()
            .ok_or(CompilerError::ExpectedStackScope)?;

        // Pop N first values
        for _ in 0..amount {
            on_stack.pop()
                .ok_or(CompilerError::ExpectedValueOnStack)?;
        }

        Ok(())
    }

    // Compile the expression
    fn compile_expr(&mut self, chunk: &mut Chunk, chunk_id: u16, expr: &Expression) -> Result<(), CompilerError> {
        trace!("Compiling expression: {:?}", expr);
        match expr {
            Expression::Constant(v) => {
                // Compile the value
                let index = self.module.add_constant(v.clone());
                chunk.emit_opcode(OpCode::Constant);
                chunk.write_u16(index as u16);

                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::ArrayConstructor(exprs) | Expression::TuplesConstructor(exprs) => {
                if exprs.len() > u8::MAX as usize {
                    return Err(CompilerError::TooManyArrayValues(exprs.len()));
                }

                for expr in exprs {
                    self.compile_expr(chunk, chunk_id, expr)?;
                }
                chunk.emit_opcode(OpCode::NewObject);
                chunk.write_u8(exprs.len() as u8);

                self.decrease_values_on_stack_by(exprs.len())?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::StructConstructor(exprs, _) => {
                for expr in exprs {
                    self.compile_expr(chunk, chunk_id, expr)?;
                }

                // We don't verify the struct ID, the parser should have done it
                chunk.emit_opcode(OpCode::NewObject);
                chunk.write_u8(exprs.len() as u8);

                self.decrease_values_on_stack_by(exprs.len())?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::RangeConstructor(min, max) => {
                self.compile_expr(chunk, chunk_id, min)?;
                self.compile_expr(chunk, chunk_id, max)?;
                chunk.emit_opcode(OpCode::NewRange);

                // Decrease and mark the last value
                self.decrease_values_on_stack_by(2)?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            // Map types aren't forced in the VM, we ignore them
            Expression::MapConstructor(exprs, _, _) => {
                if exprs.len() > u8::MAX as usize {
                    return Err(CompilerError::TooManyMapValues(exprs.len()));
                }

                for (key, value) in exprs {
                    self.compile_expr(chunk, chunk_id, key)?;
                    self.compile_expr(chunk, chunk_id, value)?;
                }

                chunk.emit_opcode(OpCode::NewMap);
                chunk.write_u8(exprs.len() as u8);

                self.decrease_values_on_stack_by(exprs.len() * 2)?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::EnumConstructor(exprs, ty) => {
                // Inject the variant id
                self.compile_expr(chunk, chunk_id, &Expression::Constant(Constant::Default(Primitive::U8(ty.variant_id()))))?;

                for expr in exprs {
                    self.compile_expr(chunk, chunk_id, expr)?;
                }

                chunk.emit_opcode(OpCode::NewObject);
                chunk.write_u8(exprs.len() as u8 + 1);

                self.decrease_values_on_stack_by(exprs.len() + 1)?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::EnumPattern(exprs, _) => {
                chunk.emit_opcode(OpCode::Flatten);
                self.add_values_on_stack(chunk.last_index(), exprs.len())?;

                for _ in exprs {
                    self.memstore(chunk)?;
                }

                // Pop the variant id from enum
                chunk.emit_opcode(OpCode::Pop);
            },
            Expression::Path(left, right) => {
                // Compile the path
                self.compile_expr(chunk, chunk_id, left)?;
                if let Expression::Variable(id) = right.as_ref() {
                    chunk.emit_opcode(OpCode::SubLoad);
                    chunk.write_u8(*id as u8);
                    self.decrease_values_on_stack()?;
                    self.add_value_on_stack(chunk.last_index())?;
                } else {
                    return Err(CompilerError::ExpectedVariable);
                }
            },
            Expression::Variable(id) => {
                chunk.emit_opcode(OpCode::MemoryLoad);
                chunk.write_u16(*id);

                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::IsNot(expr) => {
                self.compile_expr(chunk, chunk_id, expr)?;
                chunk.emit_opcode(OpCode::Neg);

                self.decrease_values_on_stack()?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::ArrayCall(expr, expr_index) => {
                self.compile_expr(chunk, chunk_id, expr)?;
                self.compile_expr(chunk, chunk_id, expr_index)?;
                chunk.emit_opcode(OpCode::ArrayCall);

                self.decrease_values_on_stack_by(2)?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::SubExpression(expr) => {
                self.compile_expr(chunk, chunk_id, expr)?;
            },
            Expression::Ternary(condition, valid, invalid) => {
                self.compile_expr(chunk, chunk_id, condition)?;

                // Emit the jump if false
                // We will overwrite the addr later
                chunk.emit_opcode(OpCode::JumpIfFalse);
                chunk.write_u32(INVALID_ADDR);
                let jump_addr = chunk.last_index();

                // Compile the valid condition
                self.compile_expr(chunk, chunk_id, valid)?;

                // Once finished, we must jump the false condition
                chunk.emit_opcode(OpCode::Jump);
                chunk.write_u32(INVALID_ADDR);
                let jump_valid_index = chunk.last_index();

                // Patch the jump if false
                let jump_false_addr = chunk.index();
                chunk.patch_jump(jump_addr, jump_false_addr as u32);

                // Compile the invalid condition
                self.compile_expr(chunk, chunk_id, invalid)?;

                // Patch the jump if valid
                let jump_valid_addr = chunk.index();
                chunk.patch_jump(jump_valid_index, jump_valid_addr as u32);

                // 1 for the condition, 1 for the valid, 1 for the invalid
                self.decrease_values_on_stack_by(3)?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::Cast(expr, primitive_type) => {
                self.compile_expr(chunk, chunk_id, expr)?;
                chunk.emit_opcode(OpCode::Cast);
                chunk.write_u8(primitive_type.primitive_byte().ok_or(CompilerError::ExpectedPrimitiveType)?);

                self.decrease_values_on_stack()?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::ForceType(expr, _) => {
                self.compile_expr(chunk, chunk_id, expr)?;
            },
            Expression::FunctionPointer(mut id, closure) => {
                // Compile the fn pointer id as a stack value
                // In order to be future-proof
                // we also add a bool bit to tell if its an id for syscall
                // or if its an id for the module
                // Because its dynamic, we can't write it as opcode
                let len = self.environment.get_functions().len();
                let is_syscall = (id as usize) < len;
                if !is_syscall {
                    id -= len as u16;
                }

                let id = Primitive::U16(id).into();
                let syscall = Primitive::Boolean(is_syscall).into();
                let from = Primitive::U16(chunk_id).into();

                if *closure {
                    chunk.emit_opcode(OpCode::CaptureContext);
                }

                self.compile_expr(chunk, chunk_id, &Expression::Constant(Constant::Array(vec![id, syscall, from])))?;
            },
            Expression::DynamicCall(id, params, return_value) => {
                for param in params {
                    self.compile_expr(chunk, chunk_id, param)?;
                }

                // Load the variable that is storing the current closure id
                self.compile_expr(chunk, chunk_id, &Expression::Variable(*id))?;
                // + 1 for the variable being loaded
                self.decrease_values_on_stack_by(params.len() + 1)?;

                if *return_value {
                    self.add_value_on_stack(chunk.last_index())?;
                }

                chunk.emit_opcode(OpCode::DynamicCall);
                chunk.write_u8(params.len() as _);
            },
            Expression::FunctionCall(expr_on, id, params, _) => {
                if let Some(expr_on) = expr_on {
                    self.compile_expr(chunk, chunk_id, expr_on)?;
                }

                for param in params {
                    self.compile_expr(chunk, chunk_id, param)?;
                }

                // Functions from the environment are system calls
                let len = self.environment.get_functions().len();
                let return_value = if (*id as usize) < len {
                    chunk.emit_opcode(OpCode::SysCall);
                    chunk.write_u16(*id);

                    self.environment.get_functions()
                        .get(*id as usize)
                        .ok_or(CompilerError::ExpectedFunction)?
                        .return_type()
                        .is_some()
                } else {
                    chunk.emit_opcode(OpCode::InvokeChunk);
                    let id = *id as usize - len;
                    chunk.write_u16(id as u16);
                    chunk.write_u8(params.len() as u8 + expr_on.is_some() as u8);

                    self.program.functions()
                        .get(id)
                        .ok_or(CompilerError::ExpectedFunction)?
                        .return_type()
                        .is_some()
                };

                if expr_on.is_some() {
                    self.decrease_values_on_stack()?;
                }

                self.decrease_values_on_stack_by(params.len())?;

                // If the function returns a value, we push one
                if return_value {
                    self.add_value_on_stack(chunk.last_index())?;
                }
            },
            Expression::Operator(op, left, right) => {
                match op {
                    Operator::Assign(None) => {
                        self.function_param_copy_on_assign(chunk, left);
                        self.compile_expr(chunk, chunk_id, left)?;

                        self.compile_expr(chunk, chunk_id, right)?;
                        chunk.emit_opcode(OpCode::Assign);

                        // None left on the stack
                        self.decrease_values_on_stack_by(2)?;
                    },
                    Operator::And => {
                        self.compile_expr(chunk, chunk_id, left)?;

                        chunk.emit_opcode(OpCode::Copy);
                        // Emit the jump if false
                        // We will overwrite the addr later
                        chunk.emit_opcode(OpCode::JumpIfFalse);
                        chunk.write_u32(INVALID_ADDR);
                        let jump_addr = chunk.last_index();

                        // Compile the next condition
                        self.compile_expr(chunk, chunk_id, right)?;

                        chunk.emit_opcode(OpCode::And);

                        // Patch the jump if false
                        let jump_false_addr = chunk.index();
                        chunk.patch_jump(jump_addr, jump_false_addr as u32);

                        self.decrease_values_on_stack_by(2)?;
                        self.add_value_on_stack(chunk.last_index())?;
                    },
                    Operator::Or => {
                        self.compile_expr(chunk, chunk_id, left)?;

                        chunk.emit_opcode(OpCode::Copy);
                        chunk.emit_opcode(OpCode::Neg);

                        // We will overwrite the addr later
                        chunk.emit_opcode(OpCode::JumpIfFalse);
                        chunk.write_u32(INVALID_ADDR);
                        let jump_addr = chunk.last_index();

                        // Compile the next condition
                        self.compile_expr(chunk, chunk_id, right)?;

                        chunk.emit_opcode(OpCode::Or);

                        // Patch the jump if true
                        let jump_true_addr = chunk.index();
                        chunk.patch_jump(jump_addr, jump_true_addr as u32);

                        self.decrease_values_on_stack_by(2)?;
                        self.add_value_on_stack(chunk.last_index())?;
                    },
                    Operator::Neq => {
                        self.compile_expr(chunk, chunk_id, left)?;
                        self.compile_expr(chunk, chunk_id, right)?;
                        chunk.emit_opcode(OpCode::Eq);
                        chunk.emit_opcode(OpCode::Neg);

                        self.decrease_values_on_stack_by(2)?;
                        self.add_value_on_stack(chunk.last_index())?;
                    },
                    _ => {
                        if op.is_assignation() {
                            self.function_param_copy_on_assign(chunk, left);
                        }

                        self.compile_expr(chunk, chunk_id, left)?;
                        self.compile_expr(chunk, chunk_id, right)?;

                        let opcode = Self::map_operator_to_opcode(op)?;
                        chunk.emit_opcode(opcode);

                        self.decrease_values_on_stack_by(2)?;
                        if !op.is_assignation() {
                            // one left on the stack
                            self.add_value_on_stack(chunk.last_index())?;
                        }
                    }
                };
            },
        }

        Ok(())
    }

    // To prevent any change in function caller variables
    // Because VM allow us to changes values from one to another chunk invoke
    fn function_param_copy_on_assign(&mut self, chunk: &mut Chunk, expr: &Expression) {
        if let Expression::Variable(id) = expr {
            if self.parameters_ids.remove(id) {
                trace!("Copying function param {} for assignation", id);
                chunk.emit_opcode(OpCode::MemoryToOwned);
                chunk.write_u16(*id);
            }
        }
    }

    // Push the next register store id
    fn push_mem_scope(&mut self) {
        let last_id = self.memstore_ids.last()
            .copied()
            .unwrap_or(0);

        trace!("Pushing memory scope with last id {last_id}");
        self.memstore_ids.push(last_id);
        self.values_on_stack.push(Vec::new());
    }

    // Handle dangling values on the stack if any
    fn handle_dangle_values_on_stack(&mut self, chunk: &mut Chunk) -> Result<(), CompilerError> {
        let mut on_stack = self.values_on_stack.pop()
            .ok_or(CompilerError::ExpectedStackScope)?;

        // if we have dangling values on the stack
        if !on_stack.is_empty() {
            let stack_len = on_stack.len();
            let previous_stack = self.values_on_stack.last()
                .map(|v| v.len())
                .filter(|v| *v != stack_len)
                .unwrap_or(0);

            trace!("Previous stack: {}, Current stack: {}", previous_stack, stack_len);

            Self::handle_dangle_values_internal(&mut on_stack, previous_stack, chunk)?;
        }

        Ok(())
    }

    fn handle_dangle_values_internal(on_stack: &mut Vec<usize>, previous_stack: usize, chunk: &mut Chunk) -> Result<(), CompilerError> {
        let stack_len = on_stack.len();
        let dangling = stack_len.checked_sub(previous_stack)
            .ok_or(CompilerError::LessValueOnStackThanPrevious)?;

        if dangling > 0 {
            warn!("Dangling values on the stack: {}", dangling);
            if dangling > u8::MAX as usize {
                return Err(CompilerError::TooMuchDanglingValueOnStack);
            }

            // Reverse it, otherwise it will be shifted
            for index in on_stack.drain(..dangling).rev() {
                trace!("inject OpCode Pop at {}", index + 1);
                chunk.inject_opcode_at(OpCode::Pop, index + 1);
            }
        }

        Ok(())
    }

    // Pop the next register store id
    fn pop_mem_scope(&mut self, chunk: &mut Chunk) -> Result<(), CompilerError> {
        trace!("Popping memory scope");
        self.memstore_ids.pop().ok_or(CompilerError::ExpectedMemoryScope)?;
        self.handle_dangle_values_on_stack(chunk)?;

        Ok(())
    }

    // Start a loop by pushing the break/continue vec to track them
    fn start_loop(&mut self) {
        trace!("Starting loop");
        self.loop_break_patch.push(Vec::new());
        self.loop_continue_patch.push(Vec::new());
    }

    // End the loop by patching all continue/break
    fn end_loop(&mut self, chunk: &mut Chunk, start_index: usize, end_index: usize) -> Result<(), CompilerError> {
        trace!("Ending loop");
        // Patch all the break jumps
        for jump in self.loop_break_patch.pop().ok_or(CompilerError::ExpectedBreak)? {
            trace!("Patching break jump at index {}", jump);
            chunk.patch_jump(jump, end_index as u32);
        }

        // Patch all the continue jumps
        for jump in self.loop_continue_patch.pop().ok_or(CompilerError::ExpectedContinue)? {
            trace!("Patching continue jump at index {}", jump);
            chunk.patch_jump(jump, start_index as u32);
        }

        Ok(())
    }

    // Compile the tuples deconstruct
    fn compile_tuples(&mut self, chunk: &mut Chunk, tuples: &[TupleStatement]) -> Result<(), CompilerError> {
        for el in tuples.iter() {
            match el {
                TupleStatement::Depth(len) => {
                    chunk.emit_opcode(OpCode::Flatten);

                    if *len > 1 {
                        self.add_values_on_stack(chunk.last_index(), *len - 1)?;
                    }
                },
                TupleStatement::Deconstruct(ty) => {
                    if ty.id.is_some() {
                        self.memstore(chunk)?;
                    } else {
                        chunk.emit_opcode(OpCode::Pop);
                        self.decrease_values_on_stack()?;
                    }
                }
            }
        }

        Ok(())
    }

    // Compile a single statement
    fn compile_statement(&mut self, chunk: &mut Chunk, chunk_id: u16, statement: &Statement) -> Result<(), CompilerError> {
        trace!("compile statement {:?}", statement);
        match statement {
            Statement::Expression(expr) => self.compile_expr(chunk, chunk_id, expr)?,
            Statement::Return(expr) => {
                if let Some(expr) = expr {
                    self.compile_expr(chunk, chunk_id, expr)?;
                    self.decrease_values_on_stack()?;
                }

                chunk.emit_opcode(OpCode::Return);
            },
            Statement::Variable(declaration) => {
                self.compile_expr(chunk, chunk_id, &declaration.value)?;
                if declaration.id.is_some() {
                    self.memstore(chunk)?;
                } else {
                    chunk.emit_opcode(OpCode::Pop);
                    self.decrease_values_on_stack()?;
                }
            },
            Statement::TuplesDeconstruction(value, declaration) => {
                // Compile the value
                self.compile_expr(chunk, chunk_id, value)?;

                // Store the values
                self.compile_tuples(chunk, &declaration)?;
            },
            Statement::Scope(statements) => {
                self.push_mem_scope();
                self.compile_statements(chunk, chunk_id, statements)?;
                self.pop_mem_scope(chunk)?;
            },
            Statement::Match(expr, patterns, default, _) => {
                self.push_mem_scope();
                self.compile_expr(chunk, chunk_id, expr)?;

                let mut jumps_end = Vec::with_capacity(patterns.len());
                for (condition, statement) in patterns {
                    trace!("compiling pattern {:?} with {:?}", condition, statement);
                    self.push_mem_scope();

                    match condition {
                        MatchStatement::Cond(expr) => {
                            self.compile_expr(chunk, chunk_id, expr)?;
                            chunk.emit_opcode(OpCode::Match);
                            // magic byte set to zero for conditions
                            chunk.write_u8(0);
                        },
                        MatchStatement::Variant(_, ty) => {
                            chunk.emit_opcode(OpCode::Match);
                            // magic byte set to > 0 for variant id
                            chunk.write_u8(ty.variant_id() + 1);
                        }
                    }

                    // We will overwrite the addr later
                    // to jump to the next condition
                    chunk.write_u32(INVALID_ADDR);
                    let jump_addr = chunk.last_index();

                    if let MatchStatement::Variant(exprs, _) = condition {
                        trace!("storing {} values for variant match", exprs);

                        // Match OpCode add N values on stack + the magic byte
                        self.add_values_on_stack(chunk.last_index(), exprs + 1)?;
                        for _ in 0..*exprs {
                            self.memstore(chunk)?;
                        }

                    }

                    // pop the magic byte is also popped with following
                    // opcode
                    chunk.emit_opcode(OpCode::Pop);
                    // One is used for the jump if false
                    self.decrease_values_on_stack()?;

                    self.compile_statement(chunk, chunk_id, statement)?;
                    self.pop_mem_scope(chunk)?;

                    // Jump to the end of the match
                    chunk.emit_opcode(OpCode::Jump);
                    chunk.write_u32(INVALID_ADDR);
                    jumps_end.push(chunk.last_index());

                    // Patch it directly to jump to the next condition
                    chunk.patch_jump(jump_addr, chunk.index() as _);
                }

                // Compile the default statement
                if let Some(statement) = default {
                    trace!("compiling default case {:?}", statement);

                    self.memstore(chunk)?;

                    self.push_mem_scope();
                    self.compile_statement(chunk, chunk_id, statement)?;
                    self.pop_mem_scope(chunk)?;

                } else {
                    // This pop only occurs if NO condition was matched
                    chunk.emit_opcode(OpCode::Pop);
                    self.decrease_values_on_stack()?;
                }

                // Patch every pattern in case of success
                let jump_addr = chunk.index();

                for index in jumps_end {
                    chunk.patch_jump(index, jump_addr as u32);
                }

                self.pop_mem_scope(chunk)?;
            },
            Statement::If(condition, statements, else_statements) => {
                self.push_mem_scope();
                self.compile_expr(chunk, chunk_id, condition)?;

                // Emit the jump if false
                // We will overwrite the addr later
                chunk.emit_opcode(OpCode::JumpIfFalse);
                chunk.write_u32(INVALID_ADDR);
                let jump_addr = chunk.last_index();

                // One is used for the jump if false
                self.decrease_values_on_stack()?;

                // Compile the valid condition
                self.compile_statements(chunk, chunk_id, statements)?;

                self.pop_mem_scope(chunk)?;

                let append_jump = else_statements.is_some() && chunk.last_instruction() != Some(&OpCode::Return.as_byte());
                // Once finished, we must jump the false condition
                let jump_valid_index = if append_jump {
                    chunk.emit_opcode(OpCode::Jump);
                    chunk.write_u32(INVALID_ADDR);
                    Some(chunk.last_index())
                } else {
                    None
                };

                // Patch the jump if false
                let jump_false_addr = chunk.index();
                chunk.patch_jump(jump_addr, jump_false_addr as u32);

                // Compile the else condition
                if let Some(else_statements) = else_statements {
                    self.push_mem_scope();
                    self.compile_statements(chunk, chunk_id, else_statements)?;
                    self.pop_mem_scope(chunk)?;
                }

                if let Some(jump_valid_index) = jump_valid_index {
                    // Patch the jump if valid
                    let jump_valid_addr = chunk.index();
                    chunk.patch_jump(jump_valid_index, jump_valid_addr as u32);
                }
            },
            Statement::While(expr, statements) => {
                let start_index = chunk.index();
                self.compile_expr(chunk, chunk_id, expr)?;

                // Emit the jump if false
                // We will overwrite the addr later
                chunk.emit_opcode(OpCode::JumpIfFalse);
                chunk.write_u32(INVALID_ADDR);
                let jump_addr = chunk.last_index();

                // One is used for the jump if false
                self.decrease_values_on_stack()?;

                self.push_mem_scope();

                self.start_loop();
                // Compile the valid condition
                self.compile_statements(chunk, chunk_id, statements)?;

                self.pop_mem_scope(chunk)?;

                // Jump back to the start
                chunk.emit_opcode(OpCode::Jump);
                chunk.write_u32(start_index as u32);

                // Patch the jump if false
                let jump_false_addr = chunk.index();
                chunk.patch_jump(jump_addr, jump_false_addr as u32);

                self.end_loop(chunk, start_index, jump_false_addr)?;
            },
            Statement::ForEach(tuples, expr_values, statements) => {
                // Compile the expression
                self.compile_expr(chunk, chunk_id, expr_values)?;

                // It is used by the IteratorBegin
                self.decrease_values_on_stack()?;

                chunk.emit_opcode(OpCode::IteratorBegin);
                let start_index = chunk.index();
                chunk.emit_opcode(OpCode::IteratorNext);

                let iterator_index = chunk.last_index();
                chunk.write_u32(INVALID_ADDR);
                let jump_end = chunk.last_index();

                self.push_mem_scope();
                // IteratorNext is adding a value on stack if not
                self.add_value_on_stack(iterator_index)?;

                // OpCode IteratorNext will push a value, mark it
                self.compile_tuples(chunk, &tuples)?;

                self.start_loop();
                // Compile the valid condition
                self.compile_statements(chunk, chunk_id, statements)?;
                self.pop_mem_scope(chunk)?;

                // Jump back to the start
                chunk.emit_opcode(OpCode::Jump);
                chunk.write_u32(start_index as u32);

                // End of the iterator
                chunk.emit_opcode(OpCode::IteratorEnd);
                let end_index = chunk.last_index();

                // Patch the IterableNext to jump on IteratorEnd
                chunk.patch_jump(jump_end, end_index as u32);

                self.end_loop(chunk, start_index, end_index)?;
            }
            Statement::For(var, expr_condition, expr_op, statements) => {
                self.push_mem_scope();
                // Compile the variable
                self.compile_expr(chunk, chunk_id, &var.value)?;
                self.memstore(chunk)?;

                // Compile the condition
                let start_index = chunk.index();
                self.compile_expr(chunk, chunk_id, expr_condition)?;

                // Emit the jump if false
                // We will overwrite the addr later
                chunk.emit_opcode(OpCode::JumpIfFalse);
                chunk.write_u32(INVALID_ADDR);
                let jump_addr = chunk.last_index();

                // One is used for the jump if false
                self.decrease_values_on_stack()?;

                self.start_loop();
                // Compile the valid condition
                self.compile_statements(chunk, chunk_id, statements)?;

                // Compile the operation
                let continue_index = chunk.index();
                self.compile_expr(chunk, chunk_id, expr_op)?;
                self.pop_mem_scope(chunk)?;

                // Jump back to the start
                chunk.emit_opcode(OpCode::Jump);
                chunk.write_u32(start_index as u32);

                // Patch the jump if false
                let jump_false_addr = chunk.index();
                chunk.patch_jump(jump_addr, jump_false_addr as u32);

                self.end_loop(chunk, continue_index, jump_false_addr)?;
            },
            Statement::Break => {
                chunk.emit_opcode(OpCode::Jump);
                chunk.write_u32(INVALID_ADDR);

                let last = self.loop_break_patch.last_mut()
                    .ok_or(CompilerError::ExpectedBreak)?;
                last.push(chunk.last_index());
            },
            Statement::Continue => {
                chunk.emit_opcode(OpCode::Jump);
                chunk.write_u32(INVALID_ADDR);

                let last = self.loop_continue_patch.last_mut()
                    .ok_or(CompilerError::ExpectedContinue)?;
                last.push(chunk.last_index());
            }
        };

        // Prevent dangling values between statements
        // for that we basically do the same thing as when popping a scope
        // but without popping the stack scope
        // we handle every potential dangling values that happened
        // in the current statement
        let stack_len = self.values_on_stack.last()
            .map(|v| v.len());

        let previous_stack = stack_len.and_then(|stack_len| stack_len.checked_sub(1)
            .and_then(|v| {
                self.values_on_stack.get(v)
                    .map(|v| v.len())
                    .filter(|v| *v != stack_len)
            }))
            .unwrap_or(0);

        let on_stack = self.values_on_stack.last_mut()
            .ok_or(CompilerError::ExpectedStackScope)?;

        Self::handle_dangle_values_internal(on_stack, previous_stack, chunk)?;

        Ok(())
    }

    // Compile the statements
    fn compile_statements(&mut self, chunk: &mut Chunk, chunk_id: u16, statements: &[Statement]) -> Result<(), CompilerError> {
        trace!("Compiling statements: {:?}", statements);
        // Compile the statements
        for statement in statements {
            self.compile_statement(chunk, chunk_id, statement)?;
        }

        Ok(())
    }

    // Compile the function
    fn compile_function(&mut self, function: &FunctionType, id: u16) -> Result<(), CompilerError> {
        trace!("Compiling function: {:?}", function);
        let mut chunk = Chunk::new();

        // Push the new scope for ids
        self.push_mem_scope();

        // Push the total expected values on the stack (due to the param and instance)
        let total_on_stack = function.get_parameters().len() + function.get_instance_name().is_some() as usize;
        self.values_on_stack.last_mut()
            .ok_or(CompilerError::ExpectedStackScope)?
            .extend(iter::repeat(0).take(total_on_stack));

        if function.get_instance_name().is_some() {
            trace!("Adding instance variable");
            self.memstore(&mut chunk)?;
        }

        // Store the parameters
        self.parameters_ids.clear();
        for param in function.get_parameters() {
            trace!("Adding parameter: {:?}", param);
            self.memstore(&mut chunk)?;

            self.parameters_ids.insert(*param.get_name());
        }

        self.compile_statements(&mut chunk, id, function.get_statements())?;

        // Pop the scope for ids
        self.pop_mem_scope(&mut chunk)?;

        // Add the chunk to the module
        match function {
            FunctionType::Declared(f) => match f.visibility() {
                FunctionVisibility::Public => self.module.add_public_chunk(chunk),
                FunctionVisibility::Anonymous | FunctionVisibility::Private => self.module.add_internal_chunk(chunk),
            },
            FunctionType::Entry(_) => self.module.add_entry_chunk(chunk),
            FunctionType::Hook(h) => {
                if self.module.add_hook_chunk(h.hook_id(), chunk).is_some() {
                    return Err(CompilerError::HookAlreadyRegistered(h.hook_id()));
                }
            }
        };

        Ok(())
    }

    // Compile the program
    pub fn compile(mut self) -> Result<Module, CompilerError> {
        // Compile the program
        for (id, function) in self.program.functions().iter().enumerate() {
            self.compile_function(function, id as _)?;
        }

        // Sanity checks
        if !self.values_on_stack.is_empty() {
            return Err(CompilerError::DanglingValueOnStack);
        }

        if !self.loop_break_patch.is_empty() {
            return Err(CompilerError::MissingBreakPatch);
        }

        if !self.loop_continue_patch.is_empty() {
            return Err(CompilerError::MissingContinuePatch);
        }

        if !self.memstore_ids.is_empty() {
            return Err(CompilerError::MemoryStoreNotEmpty);
        }

        // Return the module
        Ok(self.module)
    }
}

#[cfg(test)]
mod tests {
    use xelis_builder::EnvironmentBuilder;
    use xelis_environment::{FunctionHandler, SysCallResult};
    use xelis_lexer::Lexer;
    use xelis_parser::Parser;
    use xelis_types::{Primitive, Type};

    use super::*;

    #[test]
    fn test_empty_program() {
        let program = Program::new();
        let environment = Environment::<()>::new();
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();
        assert_eq!(module.chunks().len(), 0);
        assert_eq!(module.constants().len(), 0);
    }

    #[track_caller]
    fn prepare_program(code: &str) -> Module {
        let environment = EnvironmentBuilder::default();
        prepare_program_with_env(code, &environment)
    }


    #[track_caller]
    fn prepare_program_with_env<'a>(code: &'a str, environment: &'a EnvironmentBuilder<'a, ()>) -> Module {
        let tokens = Lexer::new(code).get().unwrap();
        let mut parser = Parser::new(tokens, environment);
        parser.set_const_upgrading_disabled(true);


        let (program, _) = parser.parse().unwrap();
        let compiler = Compiler::new(&program, environment.environment());
        let module = compiler.compile().unwrap();

        module
    }


    #[track_caller]
    fn prepare_program_with_const_enabled<'a>(code: &'a str) -> Module {
        let tokens = Lexer::new(code).get().unwrap();
        let environment = EnvironmentBuilder::<()>::default();
        let parser = Parser::new(tokens, &environment);

        let (program, _) = parser.parse().unwrap();
        let compiler = Compiler::new(&program, environment.environment());
        let module = compiler.compile().unwrap();

        module
    }

    #[test]
    fn test_program_with_constants() {
        let module = prepare_program_with_const_enabled("const A: u64 = 1; const B: u64 = 2; entry main() { return A + B }");
        assert_eq!(module.chunks().len(), 1);
        assert!(module.is_entry_chunk(0));
        assert_eq!(module.constants().len(), 1);

        assert_eq!(
            module.get_constant_at(0),
            Some(&Primitive::U64(3).into())
        );

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                // The program did the opt to add the constants
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_simple_program() {
        let module = prepare_program("fn main() {}");

        assert_eq!(module.chunks().len(), 1);
        assert!(!module.is_entry_chunk(0));
        assert_eq!(module.constants().len(), 0);
    }

    #[test]
    fn test_entry_program() {
        let module =  prepare_program("entry main() { return 0 }");

        assert_eq!(module.chunks().len(), 1);
        assert!(module.is_entry_chunk(0));
        assert_eq!(module.constants().len(), 1);

        assert_eq!(
            module.get_constant_at(0),
            Some(&Primitive::U64(0).into())
        );

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_dangling_value_on_stack() {
        let _ = prepare_program("entry main() { 1 + 1; return 1 }");
    }

    #[test]
    fn test_simple_expression() {
        let module = prepare_program("entry main() { return 1 + 2 }");

        assert_eq!(module.chunks().len(), 1);
        assert!(module.is_entry_chunk(0));
        assert_eq!(module.constants().len(), 2);

        assert_eq!(
            module.get_constant_at(0),
            Some(&Primitive::U64(1).into())
        );

        assert_eq!(
            module.get_constant_at(1),
            Some(&Primitive::U64(2).into())
        );

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 1, 0,
                OpCode::Add.as_byte(),
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_if() {
        let module = prepare_program("entry main() { if true { return 0 } return 1 }");

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::JumpIfFalse.as_byte(), 12, 0, 0, 0,
                OpCode::Constant.as_byte(), 1, 0,
                OpCode::Return.as_byte(),
                OpCode::Constant.as_byte(), 2, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_if_else() {
       let module = prepare_program("entry main() { if true { return 0 } else { return 1 } }");

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::JumpIfFalse.as_byte(), 12, 0, 0, 0,
                OpCode::Constant.as_byte(), 1, 0,
                OpCode::Return.as_byte(),
                OpCode::Constant.as_byte(), 2, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_while() {
       let module = prepare_program("entry main() { let i: u64 = 0; while i < 10 { i += 1; } return i }");

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::MemorySet.as_byte(), 0, 0,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 1, 0,
                OpCode::Lt.as_byte(),
                OpCode::JumpIfFalse.as_byte(), 30, 0, 0, 0,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 2, 0,
                OpCode::AssignAdd.as_byte(),
                OpCode::Jump.as_byte(), 6, 0, 0, 0,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_for_each() {
       let module = prepare_program("entry main() { foreach i in [1, 2, 3] { return i } return 1 }");

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                // 1
                OpCode::Constant.as_byte(), 0, 0,
                // 2
                OpCode::Constant.as_byte(), 1, 0,
                // 3
                OpCode::Constant.as_byte(), 2, 0,
                // [1, 2, 3]
                OpCode::NewObject.as_byte(), 3,
                OpCode::IteratorBegin.as_byte(),
                OpCode::IteratorNext.as_byte(), 29, 0, 0, 0,
                OpCode::MemorySet.as_byte(), 0, 0,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Return.as_byte(),
                OpCode::Jump.as_byte(), 12, 0, 0, 0,
                OpCode::IteratorEnd.as_byte(),
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_for() {
       let module = prepare_program("entry main() { for i: u64 = 0; i < 10; i += 1 { return i } return 1 }");

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::MemorySet.as_byte(), 0, 0,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 1, 0,
                OpCode::Lt.as_byte(),
                OpCode::JumpIfFalse.as_byte(), 34, 0, 0, 0,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Return.as_byte(),
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 2, 0,
                OpCode::AssignAdd.as_byte(),
                OpCode::Jump.as_byte(), 6, 0, 0, 0,
                OpCode::Constant.as_byte(), 2, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_struct() {
       let module = prepare_program("struct Test { a: u64, b: u64 } entry main() { let t: Test = Test { a: 1, b: 2 }; return t.a }");

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 1, 0,
                OpCode::NewObject.as_byte(), 2,
                OpCode::MemorySet.as_byte(), 0, 0,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::SubLoad.as_byte(), 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_struct_const() {
       let module = prepare_program_with_const_enabled("struct Test { a: u64, b: u64 } entry main() { return Test { a: 1, b: 2 }.a }");

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                // Load struct
                OpCode::Constant.as_byte(), 0, 0,
                // New struct
                OpCode::SubLoad.as_byte(), 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_function_call() {
        let module = prepare_program("fn test() -> u64 { return 1 } entry main() { return test() }");

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::Return.as_byte()
            ]
        );

        let chunk = module.get_chunk_at(1).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::InvokeChunk.as_byte(), 0, 0, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_break() {
       let module = prepare_program("entry main() { while true { break } return 1 }");

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::JumpIfFalse.as_byte(), 18, 0, 0, 0,
                // Jump by the break
                OpCode::Jump.as_byte(), 18, 0, 0, 0,
                // Jump by the while to go back
                OpCode::Jump.as_byte(), 0, 0, 0, 0,
                OpCode::Constant.as_byte(), 1, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_continue() {
       let module = prepare_program("entry main() { for i: u64 = 0; i < 10; i += 1 { continue; } return 0 }");

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::MemorySet.as_byte(), 0, 0,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 1, 0,
                OpCode::Lt.as_byte(),
                OpCode::JumpIfFalse.as_byte(), 35, 0, 0, 0,
                // Jump by the continue
                // It must jump to the increment instruction
                OpCode::Jump.as_byte(), 23, 0, 0, 0,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 2, 0,
                OpCode::AssignAdd.as_byte(),
                OpCode::Jump.as_byte(), 6, 0, 0, 0,
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_enum() {
        let module = prepare_program("enum Test { A, B } fn main() -> Test { return Test::A }");

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::NewObject.as_byte(), 1,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_static_function_call() {
        let mut env = EnvironmentBuilder::new();
        env.register_static_function("test", Type::Bool, vec![], FunctionHandler::Sync(|_, _, _, _| Ok(SysCallResult::Return(Primitive::Null.into()))), 0, Some(Type::Bool));

        let module = prepare_program_with_env("fn main() -> bool { return bool::test() }", &env);

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::SysCall.as_byte(), 0, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_enum_with_fields() {
        let module = prepare_program("enum Test { A, B { value: u64 } } fn main() -> Test { return Test::B { value: 1 } }");

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 1, 0,
                OpCode::NewObject.as_byte(), 2,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_dangling_values() {
        let code = r#"
            entry main() {
                let x: map<string, u64> = {};
                x.insert("a", 10);
                let dummy: u64 = x.get("a").unwrap();
                x.insert("b", dummy);

                return 0
            }
        "#;

        let module = prepare_program(code);

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                // let x: map<string, u64> = {};
                OpCode::NewMap.as_byte(), 0,
                OpCode::MemorySet.as_byte(), 0, 0,
                // x.insert("a", 10);
                // load
                OpCode::MemoryLoad.as_byte(), 0, 0,
                // a
                OpCode::Constant.as_byte(), 0, 0,
                // 10
                OpCode::Constant.as_byte(), 1, 0,
                // insert
                OpCode::SysCall.as_byte(), 208, 0,
                // Expected POP
                OpCode::Pop.as_byte(),
                // x.get("a")
                // Load x
                OpCode::MemoryLoad.as_byte(), 0, 0,
                // a
                OpCode::Constant.as_byte(), 0, 0,
                // get
                OpCode::SysCall.as_byte(), 207, 0,
                // unwrap (u16 id)
                OpCode::SysCall.as_byte(), 26, 0,
                // let dummy: u64 = x.get("a").unwrap();
                OpCode::MemorySet.as_byte(), 1, 0,
                // x.insert("b", dummy);
                // Load x
                OpCode::MemoryLoad.as_byte(), 0, 0,
                // "b"
                OpCode::Constant.as_byte(), 2, 0,
                // Load dummy
                OpCode::MemoryLoad.as_byte(), 1, 0,
                // insert (u16 id)
                OpCode::SysCall.as_byte(), 208, 0,
                // Expected POP
                OpCode::Pop.as_byte(),

                // return 0
                OpCode::Constant.as_byte(), 3, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_constants_compiled() {
        let code = r#"
            struct CAsset {
                asset: u64
            }

            entry deploy_asset() {
                return 0;
            }

            entry transfer_ownership() {
                return 0;
            }
        "#;

        let module = prepare_program(code);
        assert_eq!(module.constants().len(), 1);
        assert_eq!(
            module.get_constant_at(0),
            Some(&Primitive::U64(0).into())
        );
    }

    #[test]
    fn test_constants() {
        let code = r#"
            entry main() {
                let _: u64 = foo("test", 42);
                return 0;
            }
        "#;

        let mut env = EnvironmentBuilder::new();
        env.register_native_function("foo", None, vec![("b", Type::String), ("bar", Type::U8)], FunctionHandler::Sync(|_, _, _, _| { todo!() }), 0, Some(Type::U64));
        let module = prepare_program_with_env(code, &env);

        assert_eq!(
            module.constants().iter().cloned().collect::<Vec<_>>(),
            vec![
                Primitive::String("test".into()).into(),
                Primitive::U8(42).into(),
                Primitive::U64(0).into()
            ]
        );
    }

    #[test]
    fn test_ignored_var_tuples() {
        let code = r#"
            entry main() {
                let (a, _, c): (u64, u64, u64) = (0, 1, 2);
                return a + c;
            }
        "#;

        let module = prepare_program(code);
        assert_eq!(
            module.get_chunk_at(0).unwrap().get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 1, 0,
                OpCode::Constant.as_byte(), 2, 0,
                OpCode::NewObject.as_byte(), 3,
                // flatten in 3 vars
                OpCode::Flatten.as_byte(),
                // We store a
                OpCode::MemorySet.as_byte(), 0, 0,
                // We pop expected "b"
                OpCode::Pop.as_byte(),
                OpCode::MemorySet.as_byte(), 1, 0,
                OpCode::MemoryLoad.as_byte(), 1, 0,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Add.as_byte(),
                OpCode::Return.as_byte()

            ]
        );
    }
}