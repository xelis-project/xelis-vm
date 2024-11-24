mod stack;
mod state;
mod error;

use xelis_environment::{Environment, NativeFunction};
use xelis_types::{
    IdentifierType,
    NoHashMap,
    Path,
    Type,
    Value,
};
use xelis_ast::{
    Expression, FunctionType, Operator, Parameter, Program, Statement
};
use stack::Stack;

pub use state::State;
pub use error::InterpreterError;

enum StatementResult<'a> {
    Return(Option<Path<'a>>),
    Break,
    Continue,
    None
}

macro_rules! op {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (Value::U8(a), Value::U8(b)) => Value::U8(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::U16(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::U32(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::U64(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::U128(a $op b),
            _ => return Err(InterpreterError::OperationNotNumberType)
        }
    }};
}

macro_rules! op_div {
    ($t: ident, $a: expr, $b: expr) => {
        {
            if *$b == 0 {
                return Err(InterpreterError::DivByZero)
            }
    
            Value::$t($a / $b)
        }
    };
    ($a: expr, $b: expr) => {
        match ($a, $b) {
            (Value::U8(a), Value::U8(b)) => op_div!(U8, a, b),
            (Value::U16(a), Value::U16(b)) => op_div!(U16, a, b),
            (Value::U32(a), Value::U32(b)) => op_div!(U32, a, b),
            (Value::U64(a), Value::U64(b)) => op_div!(U64, a, b),
            (Value::U128(a), Value::U128(b)) => op_div!(U128, a, b),
            _ => return Err(InterpreterError::OperationNotNumberType)
        }
    };
}

macro_rules! op_bool {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a $op b),
            (Value::U8(a), Value::U8(b)) => Value::Boolean(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::Boolean(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::Boolean(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::Boolean(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::Boolean(a $op b),
            _ => return Err(InterpreterError::OperationNotBooleanType)
        }
    }};
}

macro_rules! op_num_with_bool {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (Value::Boolean(a), Value::Boolean(b)) => Value::Boolean(a $op b),
            (Value::U8(a), Value::U8(b)) => Value::U8(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::U16(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::U32(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::U64(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::U128(a $op b),
            _ => return Err(InterpreterError::OperationNotBooleanType)
        }
    }};
}

macro_rules! execute_foreach {
    ($self: expr, $statements: expr, $var: expr, $val: expr, $stack: expr, $state: expr) => {
        $stack.register_variable($var, $val)?;
        match $self.execute_statements($statements, $stack, $state)? {
            StatementResult::Return(v) => return Ok(StatementResult::Return(v)),
            StatementResult::Break => {
                break;
            },
            _ => {}
        };
    };
}

#[derive(Debug)]
enum ExprHelper<'a> {
    Expr(&'a Expression),
    // Index
    ArrayCall(&'a Expression),
    // FunctionCall {
    //     args: usize,
    //     on_value: bool,
    //     name: &'a IdentifierType
    // },
    // Cast(&'a Type),
    // Not,
    // Operator(&'a Operator),
    // OpAnd(&'a Expression),
    // OpOr(&'a Expression),
    // Ternary {
    //     left: &'a Expression,
    //     right: &'a Expression
    // },
    // Array(usize)
}

enum Function<'a> {
    Native(&'a NativeFunction),
    Program(&'a FunctionType)
}

impl<'a> Function<'a> {
    fn is_entry(&self) -> bool {
        match self {
            Function::Program(f) => f.is_entry(),
            _ => false
        }
    }
}

// The interpreter structure can be reused to execute multiple times the program
pub struct Interpreter<'a> {
    // Program to execute
    program: &'a Program,
    // Environment linked to execute the program
    env: &'a Environment,
    // All the constants defined in the program
    constants: Option<NoHashMap<Value>>
}

impl<'a> Interpreter<'a> {
    pub fn new(program: &'a Program, env: &'a Environment) -> Result<Self, InterpreterError> {
        Ok(Self {
            program,
            env,
            constants: None
        })
    }

    // Get the function from the environment or the program based on the index
    fn get_function(&self, name: &IdentifierType) -> Result<Function, InterpreterError> {
        let index = *name as usize;
        let len = self.env.get_functions().len();
        if index < len {
            Ok(Function::Native(&self.env.get_functions()[index]))
        } else {
            self.program.functions().get(index - len)
                .map(|v | Function::Program(v))
                .ok_or(InterpreterError::NoMatchingFunction)
        }
    }

    // Get a mutable reference to a value so we can update its content
    fn get_from_path(&'a self, path: &'a Expression, stack: &mut Stack<'a>, state: &mut State) -> Result<Path<'a>, InterpreterError> {
        // Fast path on no-depth expressions       
        match path {
            Expression::Variable(name) => return stack.get_variable_path(name),
            Expression::Constant(v) => return Ok(Path::Borrowed(v)),
            Expression::FunctionCall(_, _, _) => return self.execute_expression_and_expect_value(path, stack, state),
            _ => ()
        };

        let mut local_stack: Vec<ExprHelper<'a>> = vec![ExprHelper::Expr(path)];
        let mut local_result: Vec<Path<'a>> = Vec::with_capacity(4);

        while let Some(h) = local_stack.pop() {
            match h {
                ExprHelper::Expr(expr) => match expr {
                    Expression::Path(left, right) => {
                        local_stack.push(ExprHelper::Expr(right));
                        local_stack.push(ExprHelper::Expr(left));
                    },
                    Expression::ArrayCall(expr, expr_index) => {
                        local_stack.push(ExprHelper::ArrayCall(expr_index));
                        local_stack.push(ExprHelper::Expr(expr));
                    },
                    Expression::Variable(name) => {
                        let inner_value = match local_result.pop() {
                            Some(v) => v.get_sub_variable(*name as usize)?,
                            None => stack.get_variable_path(name)?
                        };
    
                        local_result.push(inner_value);
                    },
                    Expression::Constant(v) => {
                        local_result.push(Path::Borrowed(v));
                    },
                    Expression::FunctionCall(_, _, _) => {
                        let value = self.execute_expression_and_expect_value(expr, stack, state)?;
                        local_result.push(value);
                    },
                    e => return Err(InterpreterError::ExpectedPath(e.clone()))
                },
                ExprHelper::ArrayCall(index) => {
                    let index = self.execute_expression_and_expect_value(index, stack, state)?
                        .as_u32()?;

                    let on_value = local_result.pop().ok_or(InterpreterError::MissingValueOnStack)?;
                    local_result.push(on_value.get_sub_variable(index as usize)?);
                }
            }
        }

        local_result.pop().ok_or(InterpreterError::MissingValueOnStack)
    }

    // Execute the selected operator
    fn execute_operator(&self, op: &Operator, left: &Value, right: &Value, _: &mut State) -> Result<Value, InterpreterError> {
        match op {
            Operator::Equals => Ok(Value::Boolean(left ==right)),
            Operator::NotEquals => Ok(Value::Boolean(left != right)),
            Operator::Plus => {
                if left.is_string() || right.is_string() {
                    Ok(Value::String(format!("{}{}", left, right)))
                } else {
                    Ok(op!(left, right, +))
                }
            },
            Operator::Minus => Ok(op!(left, right, -)),
            Operator::Divide => Ok(op_div!(left, right)),
            Operator::Multiply => Ok(op!(left, right, *)),
            Operator::Rem => Ok(op!(left, right, %)),
            Operator::BitwiseXor => Ok(op!(left, right, ^)),
            Operator::BitwiseAnd => Ok(op_num_with_bool!(left, right, &)),
            Operator::BitwiseOr => Ok(op_num_with_bool!(left, right, |)),
            Operator::BitwiseLeft => Ok(op!(left, right, <<)),
            Operator::BitwiseRight => Ok(op!(left, right, >>)),
            Operator::GreaterOrEqual => Ok(op_bool!(left, right, >=)),
            Operator::GreaterThan => Ok(op_bool!(left, right, >)),
            Operator::LessOrEqual => Ok(op_bool!(left, right, <=)),
            Operator::LessThan => Ok(op_bool!(left, right, <)),
            // Those are handled in the execute_expression function
            Operator::And | Operator::Or | Operator::Assign(_) => return Err(InterpreterError::UnexpectedOperator)
        }
    }

    #[inline(always)]
    fn execute_expression_and_expect_value(&'a self, expr: &'a Expression, stack: &mut Stack<'a>, state: &mut State) -> Result<Path<'a>, InterpreterError> {
        match self.execute_expression(expr, stack, state)? {
            Some(val) => Ok(val),
            None => Err(InterpreterError::ExpectedValue)
        }
    }

    fn execute_expression(&'a self, expr: &'a Expression, stack: &mut Stack<'a>, state: &mut State) -> Result<Option<Path<'a>>, InterpreterError> {
        state.increase_expressions_executed()?;
        match expr {
            Expression::FunctionCall(path, name, parameters) => {
                let mut values = Vec::with_capacity(parameters.len());
                let on_value = match path {
                    Some(path) => Some(self.get_from_path(path, stack, state)?),
                    None => None,
                };

                for param in parameters {
                    values.push(self.execute_expression_and_expect_value(param, stack, state)?);
                }

                state.increase_recursive_depth()?;

                let f = self.get_function(name)?;
                let res = self.execute_function(f, on_value, values, state)?;

                state.decrease_recursive_depth();

                Ok(res)
            },
            Expression::ArrayConstructor(expressions) => {
                let mut values = Vec::with_capacity(expressions.len());
                for expr in expressions {
                    let value = self.execute_expression_and_expect_value(&expr, stack, state)?;
                    values.push(value.into_pointer());
                }

                Ok(Some(Path::Owned(Value::Array(values))))
            },
            Expression::StructConstructor(expr_fields, _type) => {
                let mut fields = Vec::with_capacity(expr_fields.len());
                for expr in expr_fields {
                    let value = self.execute_expression_and_expect_value(&expr, stack, state)?;
                    fields.push(value.into_pointer());
                }

                Ok(Some(Path::Owned(Value::Struct(fields, _type.clone()))))
            },
            Expression::IsNot(expr) => {
                let val = self.execute_expression_and_expect_value(&expr, stack, state)?.as_bool()?;
                Ok(Some(Path::Owned(Value::Boolean(!val))))
            }
            Expression::SubExpression(expr) => self.execute_expression(expr, stack, state),
            Expression::Ternary(condition, left, right) => {
                if self.execute_expression_and_expect_value(&condition, stack, state)?.as_bool()? {
                    Ok(Some(self.execute_expression_and_expect_value(&left, stack, state)?))
                } else {
                    Ok(Some(self.execute_expression_and_expect_value(&right, stack, state)?))
                }
            }
            Expression::Constant(v) => Ok(Some(Path::Borrowed(v))),
            Expression::Operator(op, expr_left, expr_right) => {
                match op {
                    Operator::Assign(op) => {
                        let value = self.execute_expression_and_expect_value(expr_right, stack, state)?;
                        let mut path = self.get_from_path(expr_left, stack, state)?;

                        if let Some(op) = op {
                            let result = self.execute_operator(&op, &path.as_ref(), &value.as_ref(), state)?;
                            *path.as_mut() = result;
                        } else {
                            *path.as_mut() = value.into_owned();
                        }
                        Ok(None)
                    },
                    Operator::And => Ok(Some(Path::Owned(Value::Boolean({
                        let left = self.execute_expression_and_expect_value(&expr_left, stack, state)?.as_bool()?;
                        if !left {
                            false
                        } else {
                            let right = self.execute_expression_and_expect_value(&expr_right, stack, state)?;
                            right.as_bool()?
                        }
                    })))),
                    Operator::Or => Ok(Some(Path::Owned(Value::Boolean({
                        let left = self.execute_expression_and_expect_value(&expr_left, stack, state)?.as_bool()?;
                        if !left {
                            let right = self.execute_expression_and_expect_value(&expr_right, stack, state)?;
                            right.as_bool()?
                        } else {
                            true
                        }
                    })))),
                    _ => {
                        let left = self.execute_expression_and_expect_value(&expr_left, stack, state)?;
                        let left_handle = left.as_ref();
                        let right = self.execute_expression_and_expect_value(&expr_right, stack, state)?;
                        let right_handle = right.as_ref();
                        self.execute_operator(op, &left_handle, &right_handle, state).map(Path::Owned).map(Some)
                    }
                }
            },
            Expression::Cast(expr, cast_type) => {
                let value = self.execute_expression_and_expect_value(expr, stack, state)?.into_owned();
                Ok(Some(Path::Owned(match cast_type {
                    Type::U8 => Value::U8(value.cast_to_u8()?),
                    Type::U16 => Value::U16(value.cast_to_u16()?),
                    Type::U32 => Value::U32(value.cast_to_u32()?),
                    Type::U64 => Value::U64(value.cast_to_u64()?),
                    Type::U128 => Value::U128(value.cast_to_u128()?),
                    Type::U256 => Value::U256(value.cast_to_u256()?),
                    Type::String => Value::String(value.cast_to_string()?),
                    _ => return Err(InterpreterError::InvalidType(cast_type.clone()))
                })))
            },
            expr => Ok(Some(self.get_from_path(expr, stack, state)?)),
        }
    }

    fn execute_statements<'b>(&'b self, statements: &'b [Statement], stack: &mut Stack<'b>, state: &mut State) -> Result<StatementResult<'b>, InterpreterError> {
        for statement in statements {
            // Increase the number of executed expressions
            state.increase_expressions_executed()?;

            match statement {
                Statement::Break => {
                    return Ok(StatementResult::Break);
                },
                Statement::Continue => {
                    return Ok(StatementResult::Continue);
                },
                Statement::Variable(var) => {
                    let value = self.execute_expression_and_expect_value(&var.value, stack, state)?;
                    stack.register_variable(var.id.clone(), value)?;
                },
                Statement::If(condition, statements, else_statements) => {
                    let statements = if self.execute_expression_and_expect_value(&condition, stack, state)?.as_bool()? {
                        Some(statements)
                    } else if let Some(statements) = else_statements {
                        Some(statements)
                    } else {
                        None
                    };

                    if let Some(statements) = statements {
                        match self.execute_statements(&statements, stack, state)? {
                            StatementResult::Return(v) => return Ok(StatementResult::Return(v)),
                            StatementResult::Break => return Ok(StatementResult::Break),
                            StatementResult::Continue => return Ok(StatementResult::Continue),
                            _ => {}
                        };
                    }
                },
                Statement::For(var, condition, increment, statements) => {
                    // register the variable
                    let value = self.execute_expression_and_expect_value(&var.value, stack, state)?;
                    stack.register_variable(var.id.clone(), value)?;

                    loop {
                        // check the condition
                        if !self.execute_expression_and_expect_value(condition, stack, state)?.as_bool()? {
                            break;
                        }

                        // execute the statements
                        match self.execute_statements(&statements, stack, state)? {
                            StatementResult::Return(v) => return Ok(StatementResult::Return(v)),
                            StatementResult::Break => break,
                            StatementResult::Continue => continue,
                            _ => {}
                        }

                        // increment once the iteration is done
                        self.execute_expression(increment, stack, state)?;

                        // We clear the last scope to avoid keeping the variables
                        // as we may register them again in next iteration
                    }

                    // Before ending the loop, we remove the variable
                    // Because we registered outside of the newly created scope
                    // as we clear this scope each time
                    stack.remove_variable(&var.id)?;
                },
                Statement::ForEach(var, expr, statements) => {
                    let v = self.execute_expression_and_expect_value(expr, stack, state)?;
                    match v {
                        Path::Owned(v) => {
                            for value in v.to_vec()? {
                                execute_foreach!(self, statements, var.clone(), Path::Wrapper(value), stack, state);
                            }
                        },
                        Path::Borrowed(v) => {
                            for value in v.as_vec()? {
                                execute_foreach!(self, statements, var.clone(), Path::Wrapper(value.clone()), stack, state);
                            }
                        },
                        Path::Wrapper(v) => {
                            let v = v.borrow();
                            for value in v.as_vec()? {
                                execute_foreach!(self, statements, var.clone(), Path::Wrapper(value.clone()), stack, state);
                            }
                        }
                    }
                },
                Statement::While(condition, statements) => {
                    while self.execute_expression_and_expect_value(&condition, stack, state)?.as_bool()? {
                        match self.execute_statements(&statements, stack, state)? {
                            StatementResult::Return(v) => return Ok(StatementResult::Return(v)),
                            StatementResult::Break => break,
                            StatementResult::Continue => continue,
                            _ => {}
                        };
                    }
                },
                Statement::Return(opt) => {
                    return Ok(StatementResult::Return(match opt {
                        Some(v) => Some(self.execute_expression_and_expect_value(&v, stack, state)?),
                        None => None
                    }))
                },
                Statement::Scope(statements) => {
                    match self.execute_statements(&statements, stack, state)? {
                        StatementResult::Return(v) => return Ok(StatementResult::Return(v)),
                        StatementResult::Break => return Ok(StatementResult::Break),
                        StatementResult::Continue => return Ok(StatementResult::Continue),
                        _ => {}
                    };
                },
                Statement::Expression(expr) => {
                    self.execute_expression(&expr, stack, state)?;
                }
            };
        }
        Ok(StatementResult::None)
    }

    fn execute_function_internal(&'a self, type_instance: Option<(Path<'a>, IdentifierType)>, parameters: &'a Vec<Parameter>, values: Vec<Path<'a>>, statements: &'a Vec<Statement>, variables_count: u16, state: &mut State) -> Result<Option<Path<'a>>, InterpreterError> {
        let mut stack = Stack::new(variables_count);
        if let Some((instance, instance_name)) = type_instance {
            stack.register_variable(instance_name, instance)?;
        }

        for (param, value) in parameters.iter().zip(values.into_iter()) {
            stack.register_variable(param.get_name().clone(), value)?;
        }

        match self.execute_statements(statements, &mut stack, state)? {
            StatementResult::Return(v) => Ok(v),
            _ => Ok(None)
        }
    }

    // Execute the selected function
    fn execute_function(&'a self, f: Function<'a>, type_instance: Option<Path<'a>>, values: Vec<Path<'a>>, state: &mut State) -> Result<Option<Path<'a>>, InterpreterError> {
        match f {
            Function::Native(f) => {
                state.increase_gas_usage(f.get_cost())?;
                match type_instance {
                    Some(mut v) => {
                        let mut instance = v.as_mut();
                        f.call_function(Some(instance.as_mut()), values, state.context_mut())
                    },
                    None => f.call_function(None, values, state.context_mut()),
                }.map(|v| v.map(Path::Owned))
                .map_err(InterpreterError::EnvironmentError)
            },
            Function::Program(f) => {
                let instance = match (type_instance, f.get_instance_name()) {
                    (Some(v), Some(n)) => Some((v, *n)),
                    (None, None) => None,
                    _ => return Err(InterpreterError::NativeFunctionExpectedInstance)
                };
                self.execute_function_internal(instance, &f.get_parameters(), values, &f.get_statements(), f.get_variables_count(), state)
            },
        }
    }

    // Compute the constants defined in the program
    pub fn compute_constants(&mut self, state: &mut State) -> Result<(), InterpreterError> {
        if self.constants.is_none() {
            let mut constants = NoHashMap::default();
            let mut stack = Stack::new(self.program.constants().len() as u16);

            for constant in self.program.constants().iter() {
                let value = self.execute_expression_and_expect_value(&constant.value, &mut stack, state)?
                    .into_owned();
                constants.insert(constant.id.clone(), value);
            }

            self.constants = Some(constants);
        }

        Ok(())
    }

    // Execute the program by calling an available entry function
    pub fn call_entry_function(&'a self, function_name: &IdentifierType, parameters: Vec<Path<'a>>, state: &mut State) -> Result<u64, InterpreterError> {
        let f = self.get_function(function_name)?;

        // only function marked as entry can be called from external
        if !f.is_entry() {
            return Err(InterpreterError::FunctionEntry(true, false))
        }

        match self.execute_function(f, None, parameters, state)? {
            Some(val) => Ok(val.as_u64()?),
            None => return Err(InterpreterError::NoExitCode)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use xelis_ast::Signature;
    use xelis_lexer::Lexer;
    use xelis_parser::Parser;
    use xelis_builder::EnvironmentBuilder;

    #[track_caller]
    fn test_code_expect_value(key: &Signature, code: &str) -> Value {
        test_code_expect_value_with_env(EnvironmentBuilder::default(), key, code)
    }

    #[track_caller]
    fn test_code_expect_value_with_env(builder: EnvironmentBuilder, key: &Signature, code: &str) -> Value {
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        let parser = Parser::new(tokens, &builder);
        let (program, mapper) = parser.parse().unwrap();

        let mut state = State::new(None, None, None);
        let interpreter = Interpreter::new(&program, builder.environment()).unwrap();

        let mapped_name = mapper.get(&key).unwrap();
        let f = interpreter.get_function(&mapped_name).unwrap();
        let result = interpreter.execute_function(f, None, Vec::new(), &mut state).unwrap();
        result.unwrap().into_owned()
    }

    #[track_caller]
    fn test_code_expect_return(code: &str, expected: u64) {
        assert_eq!(test_code_expect_value(&Signature::new("main".to_string(), None, Vec::new()), code).to_u64().unwrap(), expected);
    }

    #[track_caller]
    fn test_code_expect_return_with_env(code: &str, expected: u64, env: EnvironmentBuilder) {
        assert_eq!(test_code_expect_value_with_env(env, &Signature::new("main".to_string(), None, Vec::new()), code).to_u64().unwrap(), expected);
    }

    #[test]
    fn test_no_stackoverflow() {
        let code = "entry main() { let a: u64 = 0; for i: u64 = 0; i < 100000; i += 1 { a += i; } return a; }";
        test_code_expect_return(code, 4999950000);

        // TODO
        let mut code = "entry main() { let a: u64 = 1; return ".to_string();
        code.push_str("a + a + ".repeat(10000).as_str());
        code.push_str("a; }");

        todo!("FIX ME");
        // test_code_expect_return(code.as_str(), 4999950000);
    }

    #[test]
    fn test_self_assign() {
        // For mutability check, we must be sure to be able to use the same variable
        test_code_expect_return("entry main() { let a: u64 = 10; a = a; return a; }", 10);
        test_code_expect_return("entry main() { let a: u64 = 10; a = a + a; return a; }", 20);
    }

    #[test]
    fn test_op_assignation() {
        test_code_expect_return("entry main() { let a: u64 = 10; a += 10; return a; }", 20);
        test_code_expect_return("entry main() { let a: u64 = 10; a -= 10; return a; }", 0);
        test_code_expect_return("entry main() { let a: u64 = 10; a *= 10; return a; }", 100);
        test_code_expect_return("entry main() { let a: u64 = 10; a /= 10; return a; }", 1);
        test_code_expect_return("entry main() { let a: u64 = 10; a %= 10; return a; }", 0);
        test_code_expect_return("entry main() { let a: u64 = 10; a &= 10; return a; }", 10);
        test_code_expect_return("entry main() { let a: u64 = 10; a |= 10; return a; }", 10);
        test_code_expect_return("entry main() { let a: u64 = 10; a ^= 10; return a; }", 0);
        test_code_expect_return("entry main() { let a: u64 = 10; a <<= 10; return a; }", 10240);
        test_code_expect_return("entry main() { let a: u64 = 10; a >>= 10; return a; }", 0);
    }

    #[test]
    fn test_op_bool_assignation() {
        test_code_expect_return("entry main() { let a: bool = true; a = a && true; return a as u64; }", 1);
        test_code_expect_return("entry main() { let a: bool = true; a = a && false; return a as u64; }", 0);
        test_code_expect_return("entry main() { let a: bool = true; a = a || false; return a as u64; }", 1);
        test_code_expect_return("entry main() { let a: bool = true; a = a || true; return a as u64; }", 1);
        // |=
        test_code_expect_return("entry main() { let a: bool = false; a |= true; return a as u64; }", 1);
        test_code_expect_return("entry main() { let a: bool = false; a |= false; return a as u64; }", 0);
        // &=
        test_code_expect_return("entry main() { let a: bool = true; a &= true; return a as u64; }", 1);
        test_code_expect_return("entry main() { let a: bool = true; a &= false; return a as u64; }", 0);
    }

    #[test]
    fn test_op_and() {
        // No call shouldn't be called
        test_code_expect_return("fn no_call() -> bool { return panic('should not call') } entry main() { return (false && no_call()) as u64; }", 0);
        // Both should be called
        test_code_expect_return("entry main() { return (true && true) as u64; }", 1);
        test_code_expect_return("entry main() { return (false && false) as u64; }", 0);
    }

    #[test]
    fn test_op_or() {
        // No call shouldn't be called
        test_code_expect_return("fn no_call() -> bool { return panic('should not call') } entry main() { return (true || no_call()) as u64; }", 1);
        // Both are called
        test_code_expect_return("entry main() { return (false || true) as u64; }", 1);
        // Both are called but none are true
        test_code_expect_return("entry main() { return (false || false) as u64; }", 0);
    }

    #[test]
    fn test_optional() {
        test_code_expect_return("entry main() { let a: u64[] = []; return a.first().unwrap_or(777); }", 777);
    }

    #[test]
    fn test_number_operations() {
        test_code_expect_return("entry main() { return 10; }", 10);
        test_code_expect_return("entry main() { return 10 + 10; }", 20);
        test_code_expect_return("entry main() { return 10 - 10; }", 0);
        test_code_expect_return("entry main() { return 10 * 10; }", 100);
        test_code_expect_return("entry main() { return 10 / 10; }", 1);
        test_code_expect_return("entry main() { return 10 % 10; }", 0);
        test_code_expect_return("entry main() { return (10 == 10) as u64; }", 1);
        test_code_expect_return("entry main() { return (10 != 10) as u64; }", 0);
        test_code_expect_return("entry main() { return (10 > 10) as u64; }", 0);
        test_code_expect_return("entry main() { return (10 >= 10) as u64; }", 1);
        test_code_expect_return("entry main() { return (10 < 10) as u64; }", 0);
        test_code_expect_return("entry main() { return (10 <= 10) as u64; }", 1);
        test_code_expect_return("entry main() { return 10 & 10; }", 10);
        test_code_expect_return("entry main() { return 10 | 10; }", 10);
        test_code_expect_return("entry main() { return 10 ^ 10; }", 0);
        test_code_expect_return("entry main() { return 10 << 10; }", 10240);
        test_code_expect_return("entry main() { return 10 >> 10; }", 0);

        test_code_expect_return("entry main() { return 10 + 10 * 10; }", 110);
        test_code_expect_return("entry main() { return (10 + 10) * 10; }", 200);
    }

    #[test]
    fn test_u128() {
        test_code_expect_return("entry main() { let j: u128 = 10; j = 2 + j; return j as u64; }", 12);
        test_code_expect_return("entry main() { let j: u128 = 10; j = ((2 + j) * (3 + j) * (4 + j)); return j as u64; }", 2184);
    }

    #[test]
    fn test_array() {
        test_code_expect_return("entry main() { let a: u64[] = [1]; let b: u32 = 0; return a[b]; }", 1);
        test_code_expect_return("fn test() -> u64[] { return [0, 1, 2]; } entry main() { let b: u32 = 0; return test()[b]; }", 0);

        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; return a[0]; }", 1);
        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; return a[1]; }", 2);
        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; return a[2]; }", 3);

        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; return a[0] + a[1] + a[2]; }", 6);
        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; a[0] = 10; return a[0]; }", 10);
        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; a[0] = 10; return a[1]; }", 2);
        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; a[0] = 10; return a[2]; }", 3);
        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; a[0] = 10; return a[0] + a[1] + a[2]; }", 15);

        // Push
        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; a.push(10); return a[3]; }", 10);
        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; let v: u64 = 10; a.push(v); return a[0] + a[1] + a[2] + a[3]; }", 16);
        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; let b: u64[] = []; let v: u64 = 10; b.push(10); a.push(b[0]); return a[0] + a[1] + a[2] + a[3]; }", 16);

        // Pop
        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; a.pop(); return a.len() as u64; }", 2);
    }

    #[test]
    fn test_number_operations_priority() {
        test_code_expect_return("entry main() { return 10 + 10 * 10; }", 110);
        test_code_expect_return("entry main() { return (10 + 10) * 10; }", 200);

        test_code_expect_return("entry main() { return 10 + 10 / 5 + 3; }", 15);
        test_code_expect_return("entry main() { return 10 + 10 / 5 * 3; }", 16);
        test_code_expect_return("entry main() { return 10 + 10 / 5 + 3 * 10; }", 42);
    }

    #[test]
    fn test_basic_function_call() {
        test_code_expect_return("fn add(a: u64, b: u64) -> u64 { return a + b; } entry main() { return add(10, 10); }", 20);
        test_code_expect_return("fn add(a: u64, b: u64) -> u64 { return a + b; } entry main() { return add(10, add(10, 10)); }", 30);

        // With variable
        test_code_expect_return("fn add(a: u64, b: u64) -> u64 { return a + b; } entry main() { let a: u64 = 10; return add(a, 10); }", 20);
        test_code_expect_return("fn add(a: u64, b: u64) -> u64 { return a + b; } entry main() { let a: u64 = 10; return add(a, add(10, 10)); }", 30);
    }

    #[test]
    fn test_function_call_on_value() {
        test_code_expect_return("struct Test { a: u64 } fn (v Test) add(b: u64) -> u64 { return v.a + b; } entry main() { let t: Test = Test {a: 10}; return t.add(10); }", 20);
        test_code_expect_return("struct Test { a: u64 } fn (v Test) add(b: u64) -> u64 { return v.a + b; } entry main() { let t: Test = Test {a: 10}; return t.add(t.add(10)); }", 30);
        test_code_expect_return("struct Test { a: u64 } fn (v Test) add(b: u64) { v.a += b; } entry main() { let t: Test = Test {a: 10}; t.add(10); return t.a }", 20);
    }

    #[test]
    fn test_casting() {
        let key = Signature::new("main".to_string(), None, Vec::new());

        // Auto casting
        assert_eq!(Value::U8(10), test_code_expect_value(&key, "fn main() -> u8 { return 10; }"));
        assert_eq!(Value::U16(10), test_code_expect_value(&key, "fn main() -> u16 { return 10; }"));
        assert_eq!(Value::U32(10), test_code_expect_value(&key, "fn main() -> u32 { return 10; }"));
        assert_eq!(Value::U64(10), test_code_expect_value(&key, "fn main() -> u64 { return 10; }"));
        assert_eq!(Value::U128(10), test_code_expect_value(&key, "fn main() -> u128 { return 10u128; }"));

        // Explicit casting
        assert_eq!(Value::U8(10), test_code_expect_value(&key, "fn main() -> u8 { let a: u64 = 10; return a as u8; }"));
        assert_eq!(Value::U16(10), test_code_expect_value(&key, "fn main() -> u16 { let a: u64 = 10; return a as u16; }"));
        assert_eq!(Value::U32(10), test_code_expect_value(&key, "fn main() -> u32 { let a: u64 = 10; return a as u32; }"));
        assert_eq!(Value::U64(10), test_code_expect_value(&key, "fn main() -> u64 { let a: u32 = 10; return a as u64; }"));
        assert_eq!(Value::U128(10), test_code_expect_value(&key, "fn main() -> u128 { let a: u64 = 10; return a as u128; }"));

        let code = "
            fn add(left: u64, right: u64) -> u64 {
                return left + right;
            }

            entry main() {
                let a: u8 = 10;
                let b: u8 = 20;
                return add(a as u64, b as u64);
            }
        ";

        assert_eq!(Value::U64(30), test_code_expect_value(&key, code));

        let code = "entry main() {
            let a: u8 = 10;
            let b: u8 = 20;
            return a as u64 + b as u64;
        }";

        assert_eq!(Value::U64(30), test_code_expect_value(&key, code));
    }

    #[test]
    fn test_string_number_concatenation() {
        let key = Signature::new("main".to_string(), None, Vec::new());
        assert_eq!(Value::String("hello world10".to_string()), test_code_expect_value(&key, "fn main() -> string { return (\"hello world\" + 10); }"));
        assert_eq!(Value::String("10hello world".to_string()), test_code_expect_value(&key, "fn main() -> string { return (10 + \"hello world\"); }"));
        assert_eq!(Value::String("10hello world10".to_string()), test_code_expect_value(&key, "fn main() -> string { return (10 + \"hello world\" + 10); }"));

        // With variables
        assert_eq!(Value::String("hello world10".to_string()), test_code_expect_value(&key, "fn main() -> string { let a: u64 = 10; return (\"hello world\" + a); }"));
        assert_eq!(Value::String("10hello world".to_string()), test_code_expect_value(&key, "fn main() -> string { let a: u64 = 10; return (a + \"hello world\"); }"));
        assert_eq!(Value::String("10hello world10".to_string()), test_code_expect_value(&key, "fn main() -> string { let a: u64 = 10; return (a + \"hello world\" + a); }"));
    }

    #[test]
    fn test_negative_bool() {
        let key = Signature::new("main".to_string(), None, Vec::new());
        assert_eq!(Value::Boolean(true), test_code_expect_value(&key, "fn main() -> bool { return !false; }"));
        assert_eq!(Value::Boolean(false), test_code_expect_value(&key, "fn main() -> bool { return !true; }"));
        assert_eq!(Value::Boolean(false), test_code_expect_value(&key, "fn main() -> bool { let add: bool = true; add = !add; return add; }"));
    }

    #[test]
    fn test_foreach() {
        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; let sum: u64 = 0; foreach i in a { sum += i; } return sum; }", 6);
    }

    #[test]
    fn test_while() {
        test_code_expect_return("entry main() { let a: u64 = 0; while a < 10 { a += 1; } return a; }", 10);
    }

    #[test]
    fn test_for() {
        test_code_expect_return("entry main() { let a: u64 = 1; for i: u64 = 0; i < 10; i += 1 { a *= 2; } return a; }", 1024);
    }

    #[test]
    fn test_break() {
        test_code_expect_return("entry main() { let a: u64 = 0; while a < 10 { a += 1; if a == 5 { break; } } return a; }", 5);
    }

    #[test]
    fn test_continue() {
        test_code_expect_return("entry main() { let i: u64 = 0; let a: u64 = 1; while i < 10 { i += 1; if i == 5 { continue; } a *= 2; } return a; }", 512);
    }

    #[test]
    fn test_string_equals() {
        let key = &&Signature::new("main".to_string(), None, Vec::new());
        assert_eq!(Value::Boolean(true), test_code_expect_value(key, "fn main() -> bool { return \"test\" == 'test'; }"));
        assert_eq!(Value::Boolean(false), test_code_expect_value(key, "fn main() -> bool { return \"test\" == \"test2\"; }"));
    }

    #[test]
    fn test_ternary() {
        test_code_expect_return("entry main() { let a: u64 = 10; return a == 10 ? 0 : 1; }", 0);
        test_code_expect_return("entry main() { let a: u64 = 0; return (a == 10) ? 1 : 0; }", 0);
    }

    #[test]
    fn test_if() {
        test_code_expect_return("entry main() { let a: u64 = 10; if a == 10 { return 0; } else { return 1; } }", 0);
        test_code_expect_return("entry main() { let a: u64 = 10; if a == 0 { return 1; } else { return 0; } }", 0);
    }

    #[test]
    fn test_nested_if() {
        test_code_expect_return("entry main() { let a: u64 = 10; if a > 0 { if a == 10 { return 10; } else { return 0; } } else { return 0; } }", 10);
        test_code_expect_return("entry main() { let a: u64 = 10; if a != 0 { if a == 10 { return 0; } else { return 11; } } return 999; }", 0);
    }

    #[test]
    fn test_else_if() {
        test_code_expect_return("entry main() { let a: u64 = 10; if a == 10 { return 10; } else if a == 0 { return 0; } else { return 1; } }", 10);
        test_code_expect_return("entry main() { let a: u64 = 0; if a == 10 { return 10; } else if a == 0 { return 0; } else { return 1; } }", 0);
        test_code_expect_return("entry main() { let a: u64 = 1; if a == 10 { return 10; } else if a == 0 { return 0; } else { return 1; } }", 1);
    }

    #[test]
    fn test_struct_from_env() {
        let mut env = EnvironmentBuilder::default();
        env.register_structure("Test", vec![("a", Type::U64)]);
        test_code_expect_return_with_env("entry main() { let t: Test = Test { a: 10 }; return t.a; }", 10, env);
    }

    #[test]
    fn test_struct() {
        test_code_expect_return("struct Test { a: u64 } entry main() { let t: Test = Test { a: 10 }; return t.a; }", 10);
        test_code_expect_return("struct Test { a: u64 } entry main() { let t: Test = Test { a: 10 }; t.a = 20; return t.a; }", 20);
    }
}