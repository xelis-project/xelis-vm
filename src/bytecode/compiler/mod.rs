use crate::{ast::{Expression, FunctionType, Operator, Statement}, Environment, Program};

use super::{Chunk, Module, OpCode};

// Error type for the compiler
#[derive(Debug)]
pub enum CompilerError {
    // Error when trying to compile a program
    CompileError,
}

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
}

impl<'a> Compiler<'a> {
    // Create a new compiler
    pub fn new(program: &'a Program, environment: &'a Environment) -> Self {
        Compiler {
            program,
            environment,
            module: Module::new(),
            loop_break_patch: Vec::new(),
            loop_continue_patch: Vec::new(),
            memstore_ids: vec![0],
        }
    }

    // Map the operator to the opcode
    fn map_operator_to_opcode(op: &Operator) -> OpCode {
        match op {
            Operator::Plus => OpCode::Add,
            Operator::Minus => OpCode::Sub,
            Operator::Multiply => OpCode::Mul,
            Operator::Divide => OpCode::Div,
            Operator::Rem => OpCode::Mod,
            Operator::And => OpCode::And,
            Operator::Or => OpCode::Or,
            Operator::BitwiseXor => OpCode::Xor,
            Operator::BitwiseLeft => OpCode::Shl,
            Operator::BitwiseRight => OpCode::Shr,
            Operator::Equals => OpCode::Eq,
            Operator::NotEquals => OpCode::Neg,
            Operator::GreaterThan => OpCode::Gt,
            Operator::GreaterOrEqual => OpCode::Gte,
            Operator::LessThan => OpCode::Lt,
            Operator::LessOrEqual => OpCode::Lte,

            // Assigns
            Operator::Assign(Some(inner)) => Self::map_operator_to_opcode(inner).as_assign_operator().unwrap(),
            _ => {
                panic!("Operator {:?} not implemented", op);
            }
        }
    }

    // Emit a memory store
    fn memstore(&mut self, chunk: &mut Chunk) {
        chunk.emit_opcode(OpCode::MemorySet);
        let id = self.memstore_ids.last_mut().unwrap();
        chunk.write_u16(*id);
        *id += 1;
    }

    // Compile the expression
    fn compile_expr(&mut self, chunk: &mut Chunk, expr: &Expression) {
        match expr {
            Expression::Value(v) => {
                // Compile the value
                let index = self.module.add_constant(v.clone());
                chunk.emit_opcode(OpCode::Constant);
                chunk.write_u16(index as u16);
            },
            Expression::ArrayConstructor(exprs) => {
                for expr in exprs {
                    self.compile_expr(chunk, expr);
                }
                chunk.emit_opcode(OpCode::NewArray);
                chunk.write_u32(exprs.len() as u32);
            },
            Expression::StructConstructor(id, exprs) => {
                for expr in exprs {
                    self.compile_expr(chunk, expr);
                }

                // We don't verify the struct ID, the parser should have done it
                chunk.emit_opcode(OpCode::NewStruct);
                chunk.write_u16(*id);
            },
            Expression::Path(left, right) => {
                // Compile the path
                self.compile_expr(chunk, left);
                if let Expression::Variable(id) = right.as_ref() {
                    chunk.emit_opcode(OpCode::SubLoad);
                    chunk.write_u16(*id);
                } else {
                    panic!("Right side of the path is not a variable, got {:?}", right);
                }
            },
            Expression::Variable(id) => {
                chunk.emit_opcode(OpCode::MemoryLoad);
                chunk.write_u16(*id);
            },
            Expression::IsNot(expr) => {
                self.compile_expr(chunk, expr);
                chunk.emit_opcode(OpCode::Neg);
            },
            Expression::ArrayCall(expr, expr_index) => {
                self.compile_expr(chunk, expr);
                self.compile_expr(chunk, expr_index);
                chunk.emit_opcode(OpCode::ArrayCall);
            },
            Expression::SubExpression(expr) => {
                self.compile_expr(chunk, expr);
            },
            Expression::Ternary(condition, valid, invalid) => {
                self.compile_expr(chunk, condition);

                // Emit the jump if false
                // We will overwrite the addr later
                chunk.emit_opcode(OpCode::JumpIfFalse);
                chunk.write_u32(INVALID_ADDR);
                let jump_addr = chunk.last_index();

                // Compile the valid condition
                self.compile_expr(chunk, valid);

                // Once finished, we must jump the false condition
                chunk.emit_opcode(OpCode::Jump);
                chunk.write_u32(INVALID_ADDR);
                let jump_valid_index = chunk.last_index();

                // Patch the jump if false
                let jump_false_addr = chunk.index();
                chunk.patch_jump(jump_addr, jump_false_addr as u32);

                // Compile the invalid condition
                self.compile_expr(chunk, invalid);

                // Patch the jump if valid
                let jump_valid_addr = chunk.index();
                chunk.patch_jump(jump_valid_index, jump_valid_addr as u32);
            },
            Expression::Cast(expr, primitive_type) => {
                self.compile_expr(chunk, expr);
                chunk.emit_opcode(OpCode::Cast);
                chunk.write_u8(primitive_type.primitive_byte().unwrap());
            },
            Expression::FunctionCall(expr_on, id, params) => {
                if let Some(expr_on) = expr_on {
                    self.compile_expr(chunk, expr_on);
                }

                for param in params {
                    self.compile_expr(chunk, param);
                }

                // Functions from the environment are system calls
                let len = self.environment.get_functions().len();
                if (*id as usize) < len {
                    chunk.emit_opcode(OpCode::SysCall);
                    chunk.write_u16(*id);
                } else {
                    chunk.emit_opcode(OpCode::InvokeChunk);
                    chunk.write_u16((*id as usize - len) as u16);
                }

                chunk.write_bool(expr_on.is_some());
                chunk.write_u8(params.len() as u8);
            },
            Expression::Operator(op, left, right) => {
                self.compile_expr(chunk, left);
                self.compile_expr(chunk, right);
                match op {
                    Operator::Assign(None) => {
                        chunk.emit_opcode(OpCode::Assign);
                    },
                    _ => {
                        let op = Self::map_operator_to_opcode(op);
                        chunk.emit_opcode(op);
                    }
                };
            }
        }
    }

    // Push the next register store id
    fn push_next_id(&mut self) {
        self.memstore_ids.push(self.memstore_ids.last().unwrap().clone());
    }

    // Pop the next register store id
    fn pop_next_id(&mut self) {
        self.memstore_ids.pop();
    }

    // Start a loop by pushing the break/continue vec to track them
    fn start_loop(&mut self) {
        self.loop_break_patch.push(Vec::new());
        self.loop_continue_patch.push(Vec::new());
    }

    // End the loop by patching all continue/break
    fn end_loop(&mut self, chunk: &mut Chunk, start_index: usize, end_index: usize) {
        // Patch the break
        for jump in self.loop_break_patch.pop().unwrap() {
            chunk.patch_jump(jump, end_index as u32);
        }

        // Patch the continue
        for jump in self.loop_continue_patch.pop().unwrap() {
            chunk.patch_jump(jump, start_index as u32);
        }
    }

    // Compile the statements
    fn compile_statements(&mut self, chunk: &mut Chunk, statements: &[Statement]) {
        // Compile the statements
        for statement in statements {
            match statement {
                Statement::Expression(expr) => self.compile_expr(chunk, expr),
                Statement::Return(expr) => {
                    if let Some(expr) = expr {
                        self.compile_expr(chunk, expr);
                    }
                    chunk.emit_opcode(OpCode::Return);
                },
                Statement::Variable(declaration) => {
                    self.compile_expr(chunk, &declaration.value);
                    self.memstore(chunk);
                },
                Statement::Scope(statements) => {
                    self.push_next_id();
                    self.compile_statements(chunk, statements);
                    self.pop_next_id();
                },
                Statement::If(condition, statements, else_statements) => {
                    self.compile_expr(chunk, condition);

                    self.push_next_id();

                    // Emit the jump if false
                    // We will overwrite the addr later
                    chunk.emit_opcode(OpCode::JumpIfFalse);
                    chunk.write_u32(INVALID_ADDR);
                    let jump_addr = chunk.last_index();

                    // Compile the valid condition
                    self.compile_statements(chunk, statements);

                    self.pop_next_id();

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
                        self.push_next_id();
                        self.compile_statements(chunk, else_statements);
                        self.pop_next_id();
                    }

                    if let Some(jump_valid_index) = jump_valid_index {
                        // Patch the jump if valid
                        let jump_valid_addr = chunk.index();
                        chunk.patch_jump(jump_valid_index, jump_valid_addr as u32);
                    }
                },
                Statement::While(expr, statements) => {
                    let start_index = chunk.index();
                    self.compile_expr(chunk, expr);

                    // Emit the jump if false
                    // We will overwrite the addr later
                    chunk.emit_opcode(OpCode::JumpIfFalse);
                    chunk.write_u32(INVALID_ADDR);
                    let jump_addr = chunk.last_index();

                    self.start_loop();
                    // Compile the valid condition
                    self.compile_statements(chunk, statements);

                    // Jump back to the start
                    chunk.emit_opcode(OpCode::Jump);
                    chunk.write_u32(start_index as u32);

                    // Patch the jump if false
                    let jump_false_addr = chunk.index();
                    chunk.patch_jump(jump_addr, jump_false_addr as u32);

                    self.end_loop(chunk, start_index, jump_false_addr);
                },
                Statement::ForEach(_, expr_values, statements) => {
                    // Compile the expression
                    self.compile_expr(chunk, expr_values);

                    chunk.emit_opcode(OpCode::IteratorBegin);
                    let start_index = chunk.index();
                    chunk.emit_opcode(OpCode::IteratorNext);
                    chunk.write_u32(INVALID_ADDR);
                    let jump_end = chunk.last_index();

                    self.push_next_id();

                    // Store the value
                    self.memstore(chunk);

                    self.start_loop();
                    // Compile the valid condition
                    self.compile_statements(chunk, statements);

                    self.pop_next_id();

                    // Jump back to the start
                    chunk.emit_opcode(OpCode::Jump);
                    chunk.write_u32(start_index as u32);

                    // End of the iterator
                    chunk.emit_opcode(OpCode::IteratorEnd);
                    let end_index = chunk.last_index();

                    // Patch the IterableNext to jump on IteratorEnd
                    chunk.patch_jump(jump_end, end_index as u32);

                    self.end_loop(chunk, start_index, end_index);
                }
                Statement::For(var, expr_condition, expr_op, statements) => {
                    self.push_next_id();
                    // Compile the variable
                    self.compile_expr(chunk, &var.value);
                    self.memstore(chunk);

                    // Compile the condition
                    let start_index = chunk.index();
                    self.compile_expr(chunk, expr_condition);

                    // Emit the jump if false
                    // We will overwrite the addr later
                    chunk.emit_opcode(OpCode::JumpIfFalse);
                    chunk.write_u32(INVALID_ADDR);
                    let jump_addr = chunk.last_index();

                    self.start_loop();
                    // Compile the valid condition
                    self.compile_statements(chunk, statements);

                    // Compile the operation
                    let continue_index = chunk.index();
                    self.compile_expr(chunk, expr_op);
                    self.pop_next_id();

                    // Jump back to the start
                    chunk.emit_opcode(OpCode::Jump);
                    chunk.write_u32(start_index as u32);

                    // Patch the jump if false
                    let jump_false_addr = chunk.index();
                    chunk.patch_jump(jump_addr, jump_false_addr as u32);

                    self.end_loop(chunk, continue_index, jump_false_addr);
                },
                Statement::Break => {
                    chunk.emit_opcode(OpCode::Jump);
                    chunk.write_u32(INVALID_ADDR);
                    self.loop_break_patch.last_mut().unwrap().push(chunk.last_index());
                },
                Statement::Continue => {
                    chunk.emit_opcode(OpCode::Jump);
                    chunk.write_u32(INVALID_ADDR);
                    self.loop_continue_patch.last_mut().unwrap().push(chunk.last_index());
                }
            }
        }
    }

    // Compile the function
    fn compile_function(&mut self, function: &FunctionType) {
        assert!(self.memstore_ids.len() == 1);
        assert!(*self.memstore_ids.last_mut().unwrap() == 0);
        let mut chunk = Chunk::new();

        if function.get_instance_name().is_some() {
            self.memstore(&mut chunk);
        }

        // Store the parameters
        for _ in function.get_parameters() {
            self.memstore(&mut chunk);
        }

        self.compile_statements(&mut chunk, function.get_statements());

        // Add the chunk to the module
        if function.is_entry() {
            self.module.add_entry_chunk(chunk);
        } else {
            self.module.add_chunk(chunk);
        }
    }

    // Compile the program
    pub fn compile(mut self) -> Result<Module, CompilerError> {
        // Include the structs created
        for struct_type in self.program.structures() {
            self.module.add_struct(struct_type.clone());
        }

        // Compile the program
        for function in self.program.functions() {
            self.compile_function(function);
        }

        assert!(self.loop_break_patch.is_empty(), "Loop break patch is not empty: {:?}", self.loop_break_patch);
        assert!(self.loop_continue_patch.is_empty(), "Loop continue patch is not empty: {:?}", self.loop_continue_patch);

        // Return the module
        Ok(self.module)
    }
}

#[cfg(test)]
mod tests {
    use crate::{EnvironmentBuilder, Lexer, Parser, Value};

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
        let environment = EnvironmentBuilder::new();
        let (program, _) = Parser::new(tokens, &environment).parse().unwrap();
        (program, environment.build())
    }

    #[test]
    fn test_simple_program() {
        let (program, environment) = prepare_program("func main() {}");
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
            Some(&Value::U64(0))
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
    fn test_simple_expression() {
        let (program, environment) = prepare_program("entry main() { return 1 + 2 }");
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();
        assert_eq!(module.chunks().len(), 1);
        assert!(module.is_entry_chunk(0));
        assert_eq!(module.constants().len(), 2);

        assert_eq!(
            module.get_constant_at(0),
            Some(&Value::U64(1))
        );

        assert_eq!(
            module.get_constant_at(1),
            Some(&Value::U64(2))
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
                OpCode::NewArray.as_byte(), 3, 0, 0, 0,
                OpCode::IteratorBegin.as_byte(),
                OpCode::IteratorNext.as_byte(), 32, 0, 0, 0,
                OpCode::MemorySet.as_byte(), 0, 0,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Return.as_byte(),
                OpCode::Jump.as_byte(), 15, 0, 0, 0,
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
                OpCode::SubLoad.as_byte(), 0, 0,
                OpCode::Return.as_byte()
            ]
        );
    }

    #[test]
    fn test_function_call() {
        let (program, environment) = prepare_program("func test(): u64 { return 1 } entry main() { return test() }");
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
}