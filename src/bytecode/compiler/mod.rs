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

    // Compile the expression
    fn compile_expr(&mut self, chunk: &mut Chunk, expr: &Expression) {
        match expr {
            Expression::Value(v) => {
                // Compile the value
                let index = self.module.add_constant(v.clone());
                chunk.emit_opcode(OpCode::Constant);
                chunk.write_u8(index as u8);
            },
            Expression::Path(left, right) => {
                // Compile the path
                self.compile_expr(chunk, left);
                chunk.emit_opcode(OpCode::SubLoad);
                self.compile_expr(chunk, right);
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
            Expression::Operator(op, left, right) => {
                self.compile_expr(chunk, left);
                self.compile_expr(chunk, right);
                let opcode = match op {
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
                    // Operator::BitwiseAnd => OpCode::BitAnd,
                    // Operator::BitwiseOr => OpCode::BitOr,
                    Operator::Equals => OpCode::Eq,
                    Operator::NotEquals => OpCode::Neg,
                    Operator::GreaterThan => OpCode::Gt,
                    Operator::GreaterOrEqual => OpCode::Gte,
                    Operator::LessThan => OpCode::Lt,
                    Operator::LessOrEqual => OpCode::Lte,
                    _ => {
                        panic!("Operator {:?} not implemented", op);
                    }
                };

                chunk.emit_opcode(opcode);
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
                    // chunk.emit_opcode(OpCode::MemoryStore);
                    // chunk.write_u16(*id);
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
    fn prepare_program(code: &str) -> Program {
        let tokens = Lexer::new(code).get().unwrap();
        let environment = EnvironmentBuilder::new();
        let (program, _) = Parser::new(tokens, &environment).parse().unwrap();
        program
    }

    #[test]
    fn test_simple_program() {
        let program = prepare_program("func main() {}");
        let environment = Environment::new();
        let compiler = Compiler::new(&program, &environment);
        let module = compiler.compile().unwrap();
        assert_eq!(module.chunks().len(), 1);
        assert!(!module.is_entry_chunk(0));
        assert_eq!(module.constants().len(), 0);
    }

    #[test]
    fn test_entry_program() {
        let program = prepare_program("entry main() { return 0 }");
        let environment = Environment::new();
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
}