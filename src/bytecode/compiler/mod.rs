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
}

impl<'a> Compiler<'a> {
    // Create a new compiler
    pub fn new(program: &'a Program, environment: &'a Environment) -> Self {
        Compiler {
            program,
            environment,
            module: Module::new(),
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

    // Compile the expression
    fn compile_expr(&mut self, chunk: &mut Chunk, expr: &Expression) {
        match expr {
            Expression::Value(v) => {
                // Compile the value
                let index = self.module.add_constant(v.clone());
                chunk.emit_opcode(OpCode::Constant);
                chunk.write_u8(index as u8);
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
                let jump_addr = chunk.index();

                // Compile the valid condition
                self.compile_expr(chunk, valid);

                // Once finished, we must jump the false condition
                chunk.emit_opcode(OpCode::Jump);
                chunk.write_u32(INVALID_ADDR);
                let jump_valid_index = chunk.index();

                // Patch the jump if false
                // we do + 1 so we are on the next instruction built below
                let jump_false_addr = chunk.index() + 1;
                chunk.patch_jump(jump_addr, jump_false_addr as u32);

                // Compile the invalid condition
                self.compile_expr(chunk, invalid);

                // Patch the jump if valid
                let jump_valid_addr = chunk.index() + 1;
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
                if (*id as usize) < self.environment.get_functions().len() {
                    chunk.emit_opcode(OpCode::SysCall);
                } else {
                    chunk.emit_opcode(OpCode::InvokeChunk);
                }

                chunk.write_u16(*id);
                chunk.write_u8(params.len() as u8);
                chunk.write_bool(expr_on.is_some());
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
            },
            _ => {

            }
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
                    chunk.emit_opcode(OpCode::MemoryStore);
                },
                Statement::Scope(statements) => self.compile_statements(chunk, statements),
                Statement::If(condition, statements, else_statements) => {
                    self.compile_expr(chunk, condition);

                    // Emit the jump if false
                    // We will overwrite the addr later
                    chunk.emit_opcode(OpCode::JumpIfFalse);
                    chunk.write_u32(INVALID_ADDR);
                    let jump_addr = chunk.index();

                    // Compile the valid condition
                    self.compile_statements(chunk, statements);

                    let append_jump = else_statements.is_some() && chunk.last_instruction() != Some(&OpCode::Return.as_byte());
                    // Once finished, we must jump the false condition
                    let jump_valid_index = if append_jump {
                        chunk.emit_opcode(OpCode::Jump);
                        chunk.write_u32(INVALID_ADDR);
                        Some(chunk.index())
                    } else {
                        None
                    };

                    // Patch the jump if false
                    let jump_false_addr = chunk.index() + 1;
                    chunk.patch_jump(jump_addr, jump_false_addr as u32);

                    // Compile the else condition
                    if let Some(else_statements) = else_statements {
                        self.compile_statements(chunk, else_statements);
                    }

                    if let Some(jump_valid_index) = jump_valid_index {
                        // Patch the jump if valid
                        let jump_valid_addr = chunk.index() + 1;
                        chunk.patch_jump(jump_valid_index, jump_valid_addr as u32);
                    }
                },
                Statement::While(expr, statements) => {
                    let start_index = chunk.index() + 1;
                    self.compile_expr(chunk, expr);

                    // Emit the jump if false
                    // We will overwrite the addr later
                    chunk.emit_opcode(OpCode::JumpIfFalse);
                    chunk.write_u32(INVALID_ADDR);
                    let jump_addr = chunk.index();

                    // Compile the valid condition
                    self.compile_statements(chunk, statements);

                    // Jump back to the start
                    chunk.emit_opcode(OpCode::Jump);
                    chunk.write_u32(start_index as u32);

                    // Patch the jump if false
                    let jump_false_addr = chunk.index() + 1;
                    chunk.patch_jump(jump_addr, jump_false_addr as u32);
                },
                Statement::ForEach(_, expr_values, statements) => {
                    // Compile the expression
                    self.compile_expr(chunk, expr_values);

                    // TODO fixme

                    // Emit the jump if false
                    // We will overwrite the addr later
                    chunk.emit_opcode(OpCode::JumpIfFalse);
                    chunk.write_u32(INVALID_ADDR);
                    let jump_addr = chunk.index();

                    // Compile the valid condition
                    self.compile_statements(chunk, statements);

                    // Jump back to the start
                    chunk.emit_opcode(OpCode::Jump);
                    chunk.write_u32(jump_addr as u32);

                    // Patch the jump if false
                    let jump_false_addr = chunk.index() + 1;
                    chunk.patch_jump(jump_addr, jump_false_addr as u32);
                }
                Statement::For(var, expr_condition, expr_op, statements) => {
                    // Compile the variable
                    self.compile_expr(chunk, &var.value);
                    chunk.emit_opcode(OpCode::MemoryStore);

                    // Compile the condition
                    let start_index = chunk.index() + 1;
                    self.compile_expr(chunk, expr_condition);

                    // Emit the jump if false
                    // We will overwrite the addr later
                    chunk.emit_opcode(OpCode::JumpIfFalse);
                    chunk.write_u32(INVALID_ADDR);
                    let jump_addr = chunk.index();

                    // Compile the valid condition
                    self.compile_statements(chunk, statements);

                    // Compile the operation
                    self.compile_expr(chunk, expr_op);

                    // Jump back to the start
                    chunk.emit_opcode(OpCode::Jump);
                    chunk.write_u32(start_index as u32);

                    // Patch the jump if false
                    let jump_false_addr = chunk.index() + 1;
                    chunk.patch_jump(jump_addr, jump_false_addr as u32);
                },
                _ => {}
            }
        }
    }

    // Compile the function
    fn compile_function(&mut self, function: &FunctionType) {
        let mut chunk = Chunk::new();
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
            &[OpCode::Constant.as_byte(), 0, OpCode::Return.as_byte()]
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
                OpCode::Constant.as_byte(), 0,
                OpCode::Constant.as_byte(), 1,
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
                OpCode::Constant.as_byte(), 0,
                OpCode::JumpIfFalse.as_byte(), 0, 0, 0, 10,
                OpCode::Constant.as_byte(), 1,
                OpCode::Return.as_byte(),
                OpCode::Constant.as_byte(), 2,
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
                OpCode::Constant.as_byte(), 0,
                OpCode::JumpIfFalse.as_byte(), 0, 0, 0, 10,
                OpCode::Constant.as_byte(), 1,
                OpCode::Return.as_byte(),
                OpCode::Constant.as_byte(), 2,
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
                OpCode::Constant.as_byte(), 0,
                OpCode::MemoryStore.as_byte(),
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 1,
                OpCode::Lt.as_byte(),
                OpCode::JumpIfFalse.as_byte(), 0, 0, 0, 25,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 2,
                OpCode::AssignAdd.as_byte(),
                OpCode::Jump.as_byte(), 0, 0, 0, 3,
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
                OpCode::Constant.as_byte(), 0,
                OpCode::JumpIfFalse.as_byte(), 0, 0, 0, 10,
                OpCode::Constant.as_byte(), 1,
                OpCode::Return.as_byte(),
                OpCode::Constant.as_byte(), 2,
                OpCode::Return.as_byte(),
                OpCode::Constant.as_byte(), 3,
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
                OpCode::Constant.as_byte(), 0,
                OpCode::MemoryStore.as_byte(),
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 1,
                OpCode::Lt.as_byte(),
                OpCode::JumpIfFalse.as_byte(), 0, 0, 0, 29,
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Return.as_byte(),
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::Constant.as_byte(), 2,
                OpCode::AssignAdd.as_byte(),
                OpCode::Jump.as_byte(), 0, 0, 0, 3,
                OpCode::Constant.as_byte(), 2,
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
                OpCode::Constant.as_byte(), 0,
                OpCode::Constant.as_byte(), 1,
                OpCode::NewStruct.as_byte(), 0, 0,
                OpCode::MemoryStore.as_byte(),
                OpCode::MemoryLoad.as_byte(), 0, 0,
                OpCode::SubLoad.as_byte(), 0, 0,
                OpCode::Return.as_byte()
            ]
        );
    }
}