mod error;

use std::iter;
use log::{trace, warn};
use xelis_ast::{
    Expression,
    FunctionType,
    Operator,
    Statement,
    Program
};
use xelis_environment::Environment;
use xelis_bytecode::{Chunk, Module, OpCode};

pub use error::CompilerError;

// Temporary invalid address to patch jumps
const INVALID_ADDR: u32 = 0xDEADBEEF;

pub struct Compiler<'a> {
    // Program to compile
    program: &'a Program,
    environment: &'a Environment,
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
}

impl<'a> Compiler<'a> {
    // Create a new compiler
    pub fn new(program: &'a Program, environment: &'a Environment) -> Self {
        Self {
            program,
            environment,
            module: Module::new(),
            loop_break_patch: Vec::new(),
            loop_continue_patch: Vec::new(),
            memstore_ids: Vec::new(),
            values_on_stack: Vec::new(),
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
        let id = self.memstore_ids.last_mut().ok_or(CompilerError::ExpectedMemstoreId)?;
        chunk.write_u16(*id);
        *id += 1;

        self.decrease_values_on_stack()?;

        Ok(())
    }

    #[inline(always)]
    fn add_value_on_stack(&mut self, index: usize) -> Result<(), CompilerError> {
        trace!("Adding value on stack at instruction {}", index);
        let on_stack = self.values_on_stack.last_mut()
        .ok_or(CompilerError::ExpectedStackScope)?;
        on_stack.push(index);

        Ok(())
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
    fn compile_expr(&mut self, chunk: &mut Chunk, expr: &Expression) -> Result<(), CompilerError> {
        trace!("Compiling expression: {:?}", expr);
        match expr {
            Expression::Constant(v) => {
                // Compile the value
                let index = self.module.add_constant(v.clone());
                chunk.emit_opcode(OpCode::Constant);
                chunk.write_u16(index as u16);

                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::ArrayConstructor(exprs) => {
                if exprs.len() > u8::MAX as usize {
                    return Err(CompilerError::TooManyArrayValues(exprs.len()));
                }

                for expr in exprs {
                    self.compile_expr(chunk, expr)?;
                }
                chunk.emit_opcode(OpCode::NewArray);
                chunk.write_u8(exprs.len() as u8);

                self.decrease_values_on_stack_by(exprs.len())?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::StructConstructor(exprs, _type) => {
                for expr in exprs {
                    self.compile_expr(chunk, expr)?;
                }

                // We don't verify the struct ID, the parser should have done it
                chunk.emit_opcode(OpCode::NewStruct);
                // Because we write the type ID, we know how many expressions to read
                chunk.write_u16(_type.id());

                self.decrease_values_on_stack_by(exprs.len())?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::RangeConstructor(min, max) => {
                self.compile_expr(chunk, min)?;
                self.compile_expr(chunk, max)?;
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
                    self.compile_expr(chunk, key)?;
                    self.compile_expr(chunk, value)?;
                }

                chunk.emit_opcode(OpCode::NewMap);
                chunk.write_u8(exprs.len() as u8);

                self.decrease_values_on_stack_by(exprs.len() * 2)?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::EnumConstructor(exprs, enum_type) => {
                for expr in exprs {
                    self.compile_expr(chunk, expr)?;
                }

                chunk.emit_opcode(OpCode::NewEnum);
                // Because we write the type ID, we know how many expressions to read
                chunk.write_u16(enum_type.id());
                chunk.write_u8(enum_type.variant_id());

                self.decrease_values_on_stack_by(exprs.len())?;
                self.add_value_on_stack(chunk.last_index())?;
            }
            Expression::Path(left, right) => {
                // Compile the path
                self.compile_expr(chunk, left)?;
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
                self.compile_expr(chunk, expr)?;
                chunk.emit_opcode(OpCode::Neg);

                self.decrease_values_on_stack()?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::ArrayCall(expr, expr_index) => {
                self.compile_expr(chunk, expr)?;
                self.compile_expr(chunk, expr_index)?;
                chunk.emit_opcode(OpCode::ArrayCall);

                self.decrease_values_on_stack_by(2)?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::SubExpression(expr) => {
                self.compile_expr(chunk, expr)?;
            },
            Expression::Ternary(condition, valid, invalid) => {
                self.compile_expr(chunk, condition)?;

                // Emit the jump if false
                // We will overwrite the addr later
                chunk.emit_opcode(OpCode::JumpIfFalse);
                chunk.write_u32(INVALID_ADDR);
                let jump_addr = chunk.last_index();

                // Compile the valid condition
                self.compile_expr(chunk, valid)?;

                // Once finished, we must jump the false condition
                chunk.emit_opcode(OpCode::Jump);
                chunk.write_u32(INVALID_ADDR);
                let jump_valid_index = chunk.last_index();

                // Patch the jump if false
                let jump_false_addr = chunk.index();
                chunk.patch_jump(jump_addr, jump_false_addr as u32);

                // Compile the invalid condition
                self.compile_expr(chunk, invalid)?;

                // Patch the jump if valid
                let jump_valid_addr = chunk.index();
                chunk.patch_jump(jump_valid_index, jump_valid_addr as u32);

                // 1 for the condition, 1 for the valid, 1 for the invalid
                self.decrease_values_on_stack_by(3)?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::Cast(expr, primitive_type) => {
                self.compile_expr(chunk, expr)?;
                chunk.emit_opcode(OpCode::Cast);
                chunk.write_u8(primitive_type.primitive_byte().ok_or(CompilerError::ExpectedPrimitiveType)?);

                self.decrease_values_on_stack()?;
                self.add_value_on_stack(chunk.last_index())?;
            },
            Expression::FunctionCall(expr_on, id, params) => {
                if let Some(expr_on) = expr_on {
                    self.compile_expr(chunk, expr_on)?;
                }

                for param in params {
                    self.compile_expr(chunk, param)?;
                }

                // Functions from the environment are system calls
                let len = self.environment.get_functions().len();
                let return_value = if (*id as usize) < len {
                    chunk.emit_opcode(OpCode::SysCall);
                    chunk.write_u16(*id);

                    self.environment.get_functions()
                        .get(*id as usize)
                        .ok_or(CompilerError::ExpectedVariable)?
                        .return_type()
                        .is_some()
                } else {
                    chunk.emit_opcode(OpCode::InvokeChunk);
                    let id = *id as usize - len;
                    chunk.write_u16(id as u16);

                    self.program.functions()
                        .get(id)
                        .ok_or(CompilerError::ExpectedVariable)?
                        .return_type()
                        .is_some()
                };

                chunk.write_bool(expr_on.is_some());
                chunk.write_u8(params.len() as u8);

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
                        self.compile_expr(chunk, left)?;
                        self.compile_expr(chunk, right)?;
                        chunk.emit_opcode(OpCode::Assign);

                        // None left on the stack
                        self.decrease_values_on_stack_by(2)?;
                    },
                    Operator::And => {
                        self.compile_expr(chunk, left)?;

                        chunk.emit_opcode(OpCode::Copy);
                        // Emit the jump if false
                        // We will overwrite the addr later
                        chunk.emit_opcode(OpCode::JumpIfFalse);
                        chunk.write_u32(INVALID_ADDR);
                        let jump_addr = chunk.last_index();

                        // Compile the next condition
                        self.compile_expr(chunk, right)?;

                        chunk.emit_opcode(OpCode::And);

                        // Patch the jump if false
                        let jump_false_addr = chunk.index();
                        chunk.patch_jump(jump_addr, jump_false_addr as u32);

                        self.decrease_values_on_stack_by(2)?;
                        self.add_value_on_stack(chunk.last_index())?;
                    },
                    Operator::Or => {
                        self.compile_expr(chunk, left)?;

                        chunk.emit_opcode(OpCode::Copy);
                        chunk.emit_opcode(OpCode::Neg);

                        // We will overwrite the addr later
                        chunk.emit_opcode(OpCode::JumpIfFalse);
                        chunk.write_u32(INVALID_ADDR);
                        let jump_addr = chunk.last_index();

                        // Compile the next condition
                        self.compile_expr(chunk, right)?;

                        chunk.emit_opcode(OpCode::Or);

                        // Patch the jump if true
                        let jump_true_addr = chunk.index();
                        chunk.patch_jump(jump_addr, jump_true_addr as u32);

                        self.decrease_values_on_stack_by(2)?;
                        self.add_value_on_stack(chunk.last_index())?;
                    },
                    Operator::Neq => {
                        self.compile_expr(chunk, left)?;
                        self.compile_expr(chunk, right)?;
                        chunk.emit_opcode(OpCode::Eq);
                        chunk.emit_opcode(OpCode::Neg);

                        self.decrease_values_on_stack_by(2)?;
                        self.add_value_on_stack(chunk.last_index())?;
                    },
                    _ => {
                        self.compile_expr(chunk, left)?;
                        self.compile_expr(chunk, right)?;
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

    // Push the next register store id
    fn push_mem_scope(&mut self) {
        trace!("Pushing memory scope");
        self.memstore_ids.push(self.memstore_ids.last().copied().unwrap_or(0));
        self.values_on_stack.push(Vec::new());
    }

    // Handle dangling values on the stack if any
    fn handle_dangle_values_on_stack(&mut self, chunk: &mut Chunk) -> Result<(), CompilerError> {
        let on_stack = self.values_on_stack.pop()
            .ok_or(CompilerError::ExpectedStackScope)?;

        // if we have dangling values on the stack
        if !on_stack.is_empty() {
            let stack_len = on_stack.len();
            let previous_stack = self.values_on_stack.last()
                .map(|v| v.len())
                .filter(|v| *v != stack_len)
                .unwrap_or(0);

            trace!("Previous stack: {}, Current stack: {}", previous_stack, stack_len);

            let dangling = stack_len.checked_sub(previous_stack)
                .ok_or(CompilerError::LessValueOnStackThanPrevious)?;
            
            if dangling > 0 {
                warn!("Dangling values on the stack: {}", dangling);
                if dangling > u8::MAX as usize {
                    return Err(CompilerError::TooMuchDanglingValueOnStack);
                }

                // Reverse it, otherwise it will be shifted
                for index in on_stack.into_iter().take(dangling).rev() {
                    chunk.inject_opcode_at(OpCode::Pop, index + 1);
                }
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

    // Compile the statements
    fn compile_statements(&mut self, chunk: &mut Chunk, statements: &[Statement]) -> Result<(), CompilerError> {
        trace!("Compiling statements: {:?}", statements);
        // Compile the statements
        for statement in statements {
            match statement {
                Statement::Expression(expr) => self.compile_expr(chunk, expr)?,
                Statement::Return(expr) => {
                    if let Some(expr) = expr {
                        self.compile_expr(chunk, expr)?;
                        self.decrease_values_on_stack()?;
                    }

                    chunk.emit_opcode(OpCode::Return);
                },
                Statement::Variable(declaration) => {
                    self.compile_expr(chunk, &declaration.value)?;
                    self.memstore(chunk)?;
                },
                Statement::Scope(statements) => {
                    self.push_mem_scope();
                    self.compile_statements(chunk, statements)?;
                    self.pop_mem_scope(chunk)?;
                },
                Statement::If(condition, statements, else_statements) => {
                    self.push_mem_scope();
                    self.compile_expr(chunk, condition)?;

                    // Emit the jump if false
                    // We will overwrite the addr later
                    chunk.emit_opcode(OpCode::JumpIfFalse);
                    chunk.write_u32(INVALID_ADDR);
                    let jump_addr = chunk.last_index();

                    // One is used for the jump if false
                    self.decrease_values_on_stack()?;

                    // Compile the valid condition
                    self.compile_statements(chunk, statements)?;

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
                        self.compile_statements(chunk, else_statements)?;
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
                    self.compile_expr(chunk, expr)?;

                    // Emit the jump if false
                    // We will overwrite the addr later
                    chunk.emit_opcode(OpCode::JumpIfFalse);
                    chunk.write_u32(INVALID_ADDR);
                    let jump_addr = chunk.last_index();

                    // One is used for the jump if false
                    self.decrease_values_on_stack()?;

                    self.start_loop();
                    // Compile the valid condition
                    self.compile_statements(chunk, statements)?;

                    // Jump back to the start
                    chunk.emit_opcode(OpCode::Jump);
                    chunk.write_u32(start_index as u32);

                    // Patch the jump if false
                    let jump_false_addr = chunk.index();
                    chunk.patch_jump(jump_addr, jump_false_addr as u32);

                    self.end_loop(chunk, start_index, jump_false_addr)?;
                },
                Statement::ForEach(_, expr_values, statements) => {
                    // Compile the expression
                    self.compile_expr(chunk, expr_values)?;
                    // It is used by the IteratorBegin
                    self.decrease_values_on_stack()?;

                    chunk.emit_opcode(OpCode::IteratorBegin);
                    let start_index = chunk.index();
                    chunk.emit_opcode(OpCode::IteratorNext);
                    chunk.write_u32(INVALID_ADDR);
                    let jump_end = chunk.last_index();

                    self.push_mem_scope();

                    // OpCode IteratorNext will push a value, mark it
                    self.add_value_on_stack(chunk.last_index())?;

                    // Store the value
                    self.memstore(chunk)?;

                    self.start_loop();
                    // Compile the valid condition
                    self.compile_statements(chunk, statements)?;

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
                    self.compile_expr(chunk, &var.value)?;
                    self.memstore(chunk)?;

                    // Compile the condition
                    let start_index = chunk.index();
                    self.compile_expr(chunk, expr_condition)?;

                    // Emit the jump if false
                    // We will overwrite the addr later
                    chunk.emit_opcode(OpCode::JumpIfFalse);
                    chunk.write_u32(INVALID_ADDR);
                    let jump_addr = chunk.last_index();

                    // One is used for the jump if false
                    self.decrease_values_on_stack()?;

                    self.start_loop();
                    // Compile the valid condition
                    self.compile_statements(chunk, statements)?;

                    // Compile the operation
                    let continue_index = chunk.index();
                    self.compile_expr(chunk, expr_op)?;
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
        }

        Ok(())
    }

    // Compile the function
    fn compile_function(&mut self, function: &FunctionType) -> Result<(), CompilerError> {
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
        for param in function.get_parameters() {
            trace!("Adding parameter: {:?}", param);
            self.memstore(&mut chunk)?;
        }

        self.compile_statements(&mut chunk, function.get_statements())?;

        // Pop the scope for ids
        self.pop_mem_scope(&mut chunk)?;

        // Add the chunk to the module
        if function.is_entry() {
            self.module.add_entry_chunk(chunk);
        } else {
            self.module.add_chunk(chunk);
        }

        Ok(())
    }

    // Compile the program
    pub fn compile(mut self) -> Result<Module, CompilerError> {
        // Include the structs created
        for struct_type in self.program.structures() {
            trace!("Adding struct: {:?}", struct_type);
            if !self.module.add_struct(struct_type.clone()) {
                return Err(CompilerError::DuplicatedStruct(struct_type.id()));
            }
        }

        for enum_type in self.program.enums() {
            trace!("Adding enum: {:?}", enum_type);
            if !self.module.add_enum(enum_type.clone()) {
                return Err(CompilerError::DuplicatedEnum(enum_type.id()));
            }
        }

        // Compile the program
        for function in self.program.functions() {
            self.compile_function(function)?;
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
    use xelis_lexer::Lexer;
    use xelis_parser::Parser;
    use xelis_types::Value;

    use super::*;

    #[test]
    fn test_empty_program() {
        let program = Program::new();
        let environment = Environment::new();
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();
        assert_eq!(module.chunks().len(), 0);
        assert_eq!(module.constants().len(), 0);
    }

    #[track_caller]
    fn prepare_program(code: &str) -> (Program, Environment) {
        let tokens = Lexer::new(code).get().unwrap();
        let environment = EnvironmentBuilder::default();
        let mut parser = Parser::new(tokens, &environment);
        parser.set_disable_const_upgrading(true);

        let (program, _) = parser.parse().unwrap();
        (program, environment.build())
    }

    #[track_caller]
    fn prepare_program_with_const_enabled(code: &str) -> (Program, Environment) {
        let tokens = Lexer::new(code).get().unwrap();
        let environment = EnvironmentBuilder::default();
        let parser = Parser::new(tokens, &environment);

        let (program, _) = parser.parse().unwrap();
        (program, environment.build())
    }

    #[test]
    fn test_program_with_constants() {
        let (program, environment) = prepare_program_with_const_enabled("const A: u64 = 1; const B: u64 = 2; entry main() { return A + B }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();
        assert_eq!(module.chunks().len(), 1);
        assert!(module.is_entry_chunk(0));
        assert_eq!(module.constants().len(), 1);

        assert_eq!(
            module.get_constant_at(0),
            Some(&Value::U64(3).into())
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
        let (program, environment) = prepare_program("fn main() {}");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();
        assert_eq!(module.chunks().len(), 1);
        assert!(!module.is_entry_chunk(0));
        assert_eq!(module.constants().len(), 0);
    }

    #[test]
    fn test_entry_program() {
        let (program, environment) = prepare_program("entry main() { return 0 }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();
        assert_eq!(module.chunks().len(), 1);
        assert!(module.is_entry_chunk(0));
        assert_eq!(module.constants().len(), 1);

        assert_eq!(
            module.get_constant_at(0),
            Some(&Value::U64(0).into())
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
        let (program, environment) = prepare_program("entry main() { 1 + 1; return 1 }");
        let compiler = Compiler::new(&program, &environment);
        let result = compiler.compile();
        result.unwrap();
    }

    #[test]
    fn test_simple_expression() {
        let (program, environment) = prepare_program("entry main() { return 1 + 2 }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();
        assert_eq!(module.chunks().len(), 1);
        assert!(module.is_entry_chunk(0));
        assert_eq!(module.constants().len(), 2);

        assert_eq!(
            module.get_constant_at(0),
            Some(&Value::U64(1).into())
        );

        assert_eq!(
            module.get_constant_at(1),
            Some(&Value::U64(2).into())
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
        let (program, environment) = prepare_program("entry main() { if true { return 0 } return 1 }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();
    
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
        let (program, environment) = prepare_program("entry main() { if true { return 0 } else { return 1 } }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();

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
        let (program, environment) = prepare_program("entry main() { let i: u64 = 0; while i < 10 { i += 1; } return i }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();

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
        let (program, environment) = prepare_program("entry main() { foreach i in [1, 2, 3] { return i } return 1 }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();

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
                OpCode::NewArray.as_byte(), 3,
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
        let (program, environment) = prepare_program("entry main() { for i: u64 = 0; i < 10; i += 1 { return i } return 1 }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();

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
        let (program, environment) = prepare_program("struct Test { a: u64, b: u64 } entry main() { let t: Test = Test { a: 1, b: 2 }; return t.a }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 1, 0,
                OpCode::NewStruct.as_byte(), 0, 0,
                OpCode::MemorySet.as_byte(), 0, 0,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::SubLoad.as_byte(), 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_struct_const() {
        let (program, environment) = prepare_program_with_const_enabled("struct Test { a: u64, b: u64 } entry main() { return Test { a: 1, b: 2 }.a }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();

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
        let (program, environment) = prepare_program("fn test() -> u64 { return 1 } entry main() { return test() }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();

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
                OpCode::InvokeChunk.as_byte(), 0, 0, 0, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_break() {
        let (program, environment) = prepare_program("entry main() { while true { break } return 1 }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();

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
        let (program, environment) = prepare_program("entry main() { for i: u64 = 0; i < 10; i += 1 { continue; } return 0 }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();

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
        let (program, environment) = prepare_program("enum Test { A, B } fn main() -> Test { return Test::A }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::NewEnum.as_byte(), 0, 0, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_enum_with_fields() {
        let (program, environment) = prepare_program("enum Test { A, B { value: u64 } } fn main() -> Test { return Test::B { value: 1 } }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();

        let chunk = module.get_chunk_at(0).unwrap();
        assert_eq!(
            chunk.get_instructions(),
            &[
                OpCode::Constant.as_byte(), 0, 0,
                OpCode::NewEnum.as_byte(), 0, 0, 1,
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

        let (program, environment) = prepare_program(code);
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();

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
                OpCode::SysCall.as_byte(), 79, 0, 1, 2,
                // Expected POP
                OpCode::Pop.as_byte(),
                // x.get("a")
                // Load x
                OpCode::MemoryLoad.as_byte(), 0, 0,
                // a
                OpCode::Constant.as_byte(), 0, 0,
                // get
                OpCode::SysCall.as_byte(), 78, 0, 1, 1,
                // unwrap (u16 id, on type bool, params u8)
                OpCode::SysCall.as_byte(), 11, 0, 1, 0,
                // let dummy: u64 = x.get("a").unwrap();
                OpCode::MemorySet.as_byte(), 1, 0,
                // x.insert("b", dummy);
                // Load x
                OpCode::MemoryLoad.as_byte(), 0, 0,
                // "b"
                OpCode::Constant.as_byte(), 2, 0,
                // Load dummy
                OpCode::MemoryLoad.as_byte(), 1, 0,
                // insert (u16 id, on type map, params u8)
                OpCode::SysCall.as_byte(), 79, 0, 1, 2,
                // Expected POP
                OpCode::Pop.as_byte(),

                // return 0
                OpCode::Constant.as_byte(), 3, 0,
                OpCode::Return.as_byte()
            ]
        );
    }
}