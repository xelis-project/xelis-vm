mod context;
mod state;

use crate::{
    environment::Environment,
    ast::{
        Expression,
        Operator,
        Statement,
        Parameter,
        FunctionType
    },
    variable::Path,
    parser::Program,
    types::*,
    values::Value,
    IdentifierType,
    NoHashMap,
    NoOpHasher
};
use context::Context;
use itertools::Either;
pub use state::State;
use std::{cell::RefCell, hash::BuildHasherDefault, rc::Rc};

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

#[derive(Debug)]
pub enum InterpreterError {
    Unknown,
    NoReturnValue,
    OptionalIsNull,
    NoMatchingFunction,
    TypeNotFound(Value),
    FunctionEntry(bool, bool), // expected, got
    LimitReached,
    NotImplemented,
    NoExitCode,
    ExpectedValue,
    InvalidNativeFunctionCall,
    ExpectedPath(Expression),
    UnexpectedInstanceType,
    ExpectedInstanceType,
    UnexpectedOperator,
    ExpectedStructType,
    NativeFunctionExpectedInstance,
    OverflowOccured,
    DivByZero,
    StructureNotFound(IdentifierType),
    StructureFieldNotFound(IdentifierType, IdentifierType),
    ExpectedValueType(Type),
    InvalidType(Type),
    OutOfBounds(usize, usize),
    InvalidRange(u64, u64),
    NoValueFoundAtIndex(u64),
    MissingValueForFunctionCall,
    InvalidStructValue(Value),
    InvalidValue(Value, Type), // got value, but expected type
    VariableNotFound(IdentifierType),
    VariableAlreadyExists(IdentifierType),
    NoScopeFound,
    ExpectedAssignOperator,
    OperationNotNumberType,
    OperationNotBooleanType,
    CastNumberError,
    RecursiveLimitReached,
    InvalidCastType(Type),
}

// The interpreter structure can be reused to execute multiple times the program
pub struct Interpreter<'a> {
    // Program to execute
    program: &'a Program,
    // Environment linked to execute the program
    env: &'a Environment,
    ref_structures: RefMap<'a, IdentifierType, Struct, BuildHasherDefault<NoOpHasher>>,
    // All the constants defined in the program
    constants: Option<NoHashMap<Value>>
}

impl<'a> Interpreter<'a> {
    pub fn new(program: &'a Program, env: &'a Environment) -> Result<Self, InterpreterError> {
        let mut interpreter = Self {
            program,
            env,
            ref_structures: RefMap::new(),
            constants: None
        };

        interpreter.ref_structures.link_maps(vec![interpreter.env.get_structures(), &interpreter.program.structures]);
        Ok(interpreter)
    }

    fn get_function(&self, name: &IdentifierType) -> Result<&FunctionType, InterpreterError> {
        match self.env.get_functions().get(name) {
            Some(func) => Ok(func),
            None => self.program.functions.get(name).ok_or_else(|| InterpreterError::NoMatchingFunction)
        }
    }

    // Get a mutable reference to a value so we can update its content
    fn get_from_path(&'a self, ref_value: Option<Path<'a>>, path: &'a Expression, context: &mut Context<'a>, state: &mut State) -> Result<Path<'a>, InterpreterError> {
        match path {
            Expression::ArrayCall(expr, expr_index) => {
                let v = self.execute_expression_and_expect_value(expr_index, context, state)?;
                let index = v.as_u64()? as usize;
                let array = self.get_from_path(ref_value, expr, context, state)?;
                array.get_index_at(index)
            },
            Expression::Path(left, right) => {
                let left_value = self.get_from_path(ref_value, left, context, state)?;
                self.get_from_path(Some(left_value), right, context, state)
            },
            Expression::Variable(name) => Ok(match ref_value {
                Some(v) => v.get_sub_variable(name)?,
                None => context.get_variable_reference(name)?
            }),
            e => Err(InterpreterError::ExpectedPath(e.clone()))
        }
    }

    // Execute the selected operator
    fn execute_operator(&self, op: &Operator, left: &Value, right: &Value) -> Result<Value, InterpreterError> {
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
            Operator::BitwiseAnd => Ok(op!(left, right, &)),
            Operator::BitwiseOr => Ok(op!(left, right, |)),
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

    fn execute_expression_and_expect_value(&'a self, expr: &'a Expression, context: &mut Context<'a>, state: &mut State) -> Result<Path<'a>, InterpreterError> {
        match self.execute_expression(expr, context, state)? {
            Some(val) => Ok(val),
            None => Err(InterpreterError::ExpectedValue)
        }
    }

    // Execute the selected expression
    // Do not give any mutable value, on reference or owned
    fn execute_expression(&'a self, expr: &'a Expression, context: &mut Context<'a>, state: &mut State) -> Result<Option<Path<'a>>, InterpreterError> {
        state.increase_expressions_executed()?;
        match expr {
            Expression::FunctionCall(path, name, parameters) => {
                let mut values = Vec::with_capacity(parameters.len());
                let on_value = match path {
                    Some(path) => Some(self.get_from_path(None, path, context, state)?),
                    None => None,
                };

                for param in parameters {
                    values.push(self.execute_expression_and_expect_value(param, context, state)?);
                }

                state.increase_recursive_depth()?;

                let func = self.get_function(name)?;
                let res = self.execute_function(&func, on_value, values, state)?;

                state.decrease_recursive_depth();

                Ok(res)
            },
            Expression::ArrayConstructor(expressions) => {
                let mut values = Vec::with_capacity(expressions.len());
                for expr in expressions {
                    let value = self.execute_expression_and_expect_value(&expr, context, state)?;
                    values.push(Rc::new(RefCell::new(value.into_owned())));
                }

                Ok(Some(Path::Owned(Value::Array(values))))
            },
            Expression::StructConstructor(struct_name, expr_fields) => {
                let mut fields = NoHashMap::with_capacity_and_hasher(expr_fields.len(), Default::default());
                for (name, expr) in expr_fields {
                    let value = self.execute_expression_and_expect_value(&expr, context, state)?;
                    fields.insert(name.clone(),Rc::new(RefCell::new(value.into_owned())));
                }

                Ok(Some(Path::Owned(Value::Struct(struct_name.clone(), fields))))
            },
            Expression::IsNot(expr) => {
                let val = self.execute_expression_and_expect_value(&expr, context, state)?.as_bool()?;
                Ok(Some(Path::Owned(Value::Boolean(!val))))
            }
            Expression::SubExpression(expr) => self.execute_expression(expr, context, state),
            Expression::Ternary(condition, left, right) => {
                if self.execute_expression_and_expect_value(&condition, context, state)?.as_bool()? {
                    Ok(Some(self.execute_expression_and_expect_value(&left, context, state)?))
                } else {
                    Ok(Some(self.execute_expression_and_expect_value(&right, context, state)?))
                }
            }
            Expression::Value(v) => Ok(Some(Path::Borrowed(v))),
            Expression::Operator(op, expr_left, expr_right) => {
                if op.is_assignation() {
                    let value = self.execute_expression_and_expect_value(expr_right, context, state)?;
                    let mut path = self.get_from_path(None, expr_left, context, state)?;

                    match op {
                        Operator::Assign(op) => {
                            if let Some(op) = op {
                                let result = self.execute_operator(&op, &path.as_ref(), &value.as_ref())?;
                                *path.as_mut() = result;
                            } else {
                                *path.as_mut() = value.into_owned();
                            }
                        },
                        _ => return Err(InterpreterError::NotImplemented) 
                    };
                    Ok(None)
                } else {
                    let left = self.execute_expression_and_expect_value(&expr_left, context, state)?;

                    if op.is_and_or_or() {
                        match op {
                            Operator::And => Ok(Some(Path::Owned(Value::Boolean({
                                let left = left.as_bool()?;
                                if !left {
                                    false
                                } else {
                                    let right = self.execute_expression_and_expect_value(&expr_right, context, state)?;
                                    right.as_bool()?
                                }
                            })))),
                            Operator::Or => Ok(Some(Path::Owned(Value::Boolean({
                                let left = left.as_bool()?;
                                if !left {
                                    let right = self.execute_expression_and_expect_value(&expr_right, context, state)?;
                                    right.as_bool()?
                                } else {
                                    true
                                }
                            })))),
                            _ => return Err(InterpreterError::UnexpectedOperator)
                        }
                    } else {
                        let right = self.execute_expression_and_expect_value(&expr_right, context, state)?;
                        let handle = right.as_ref();
                        self.execute_operator(op, &left.as_ref(), &handle).map(Path::Owned).map(Some)
                    }
                }
            },
            Expression::Cast(expr, cast_type) => {
                let value = self.execute_expression_and_expect_value(expr, context, state)?.into_owned();
                Ok(Some(Path::Owned(match cast_type {
                    Type::U8 => Value::U8(value.cast_to_u8()?),
                    Type::U16 => Value::U16(value.cast_to_u16()?),
                    Type::U32 => Value::U32(value.cast_to_u32()?),
                    Type::U64 => Value::U64(value.cast_to_u64()?),
                    Type::U128 => Value::U128(value.cast_to_u128()?),
                    Type::String => Value::String(value.cast_to_string()?),
                    _ => return Err(InterpreterError::InvalidType(cast_type.clone()))
                })))
            },
            expr => Ok(Some(self.get_from_path(None, expr, context, state)?)),
        }
    }

    fn map_to_cow_vec<'b>(v: Path<'b>) -> Result<impl Iterator<Item=Path<'b>>, InterpreterError> {
        Ok(match v {
            Path::Borrowed(v) => {
                Either::Left(Either::Left(v.as_vec()?.iter().map(|val| Path::Wrapper(val.clone()))))
            }
            Path::Owned(v) => {
                Either::Left(Either::Right(v.to_vec()?.into_iter().map(|val| Path::Wrapper(val))))
            },
            Path::Wrapper(v) => {
                let v = v.borrow();
                Either::Right(v.as_vec()?.clone().into_iter().map(|val| Path::Wrapper(val.clone())))
            }
        })
    }

    fn execute_statements<'b>(&'b self, statements: &'b Vec<Statement>, context: &mut Context<'b>, state: &mut State) -> Result<Option<Path<'b>>, InterpreterError> {
        for statement in statements {
            // In case some inner statement has a break or continue, we stop the loop
            if context.get_loop_break() || context.get_loop_continue() {
                break;
            }

            // Increase the number of executed expressions
            state.increase_expressions_executed()?;

            match statement {
                Statement::Break => {
                    context.set_loop_break(true);
                    break;
                },
                Statement::Continue => {
                    context.set_loop_continue(true);
                    break;
                },
                Statement::Variable(var) => {
                    let value = self.execute_expression_and_expect_value(&var.value, context, state)?;
                    context.register_variable(var.id.clone(), value)?;
                },
                Statement::If(condition, statements, else_statements) => {
                    let statements = if self.execute_expression_and_expect_value(&condition, context, state)?.as_bool()? {
                        Some(statements)
                    } else if let Some(statements) = else_statements {
                        Some(statements)
                    } else {
                        None
                    };

                    if let Some(statements) = statements {
                        match self.execute_statements(&statements, context, state)? {
                            Some(v) => return Ok(Some(v)),
                            None => {}
                        };
                    }
                },
                Statement::For(var, condition, increment, statements) => {
                    // register the variable
                    let value = self.execute_expression_and_expect_value(&var.value, context, state)?;
                    context.register_variable(var.id.clone(), value)?;

                    context.begin_scope();
                    loop {
                        // check the condition
                        if !self.execute_expression_and_expect_value(condition, context, state)?.as_bool()? {
                            break;
                        }

                        // execute the statements
                        match self.execute_statements(&statements, context, state)? {
                            Some(v) => {
                                context.end_scope()?;
                                return Ok(Some(v))
                            },
                            None => {}
                        };

                        if context.get_loop_break() {
                            context.set_loop_break(false);
                            break;
                        }

                        if context.get_loop_continue() {
                            context.set_loop_continue(false);
                        }

                        // increment once the iteration is done
                        self.execute_expression(increment, context, state)?;

                        // We clear the last scope to avoid keeping the variables
                        // as we may register them again in next iteration
                        context.clear_last_scope()?;
                    }

                    // Before ending the loop, we remove the variable
                    // Because we registered outside of the newly created scope
                    // as we clear this scope each time
                    context.remove_variable(&var.id)?;

                    context.end_scope()?;
                },
                Statement::ForEach(var, expr, statements) => {
                    let v = self.execute_expression_and_expect_value(expr, context, state)?;
                    let values = Self::map_to_cow_vec(v)?;
                    context.begin_scope();
                    for val in values {
                        context.register_variable(var.clone(), val)?;
                        match self.execute_statements(&statements, context, state)? {
                            Some(v) => {
                                context.end_scope()?;
                                return Ok(Some(v))
                            },
                            None => {}
                        };

                        if context.get_loop_break() {
                            context.set_loop_break(false);
                            break;
                        }

                        if context.get_loop_continue() {
                            context.set_loop_continue(false);
                        }

                        context.clear_last_scope()?;
                    }
                    context.end_scope()?;
                },
                Statement::While(condition, statements) => {
                    context.begin_scope();
                    while self.execute_expression_and_expect_value(&condition, context, state)?.as_bool()? {
                        match self.execute_statements(&statements, context, state)? {
                            Some(v) => {
                                context.end_scope()?;
                                return Ok(Some(v))
                            },
                            None => {}
                        };

                        if context.get_loop_break() {
                            context.set_loop_break(false);
                            break;
                        }

                        if context.get_loop_continue() {
                            context.set_loop_continue(false);
                        }
                        context.clear_last_scope()?;
                    }
                    context.end_scope()?;
                },
                Statement::Return(opt) => {
                    return Ok(match opt {
                        Some(v) => Some(self.execute_expression_and_expect_value(&v, context, state)?),
                        None => None
                    })
                },
                Statement::Scope(statements) => {
                    context.begin_scope();
                    match self.execute_statements(&statements, context, state)? {
                        Some(v) => {
                            context.end_scope()?;
                            return Ok(Some(v))
                        },
                        None => {
                            context.end_scope()?;
                        }
                    };
                },
                Statement::Expression(expr) => {
                    self.execute_expression(&expr, context, state)?;
                }
            };
        }
        Ok(None)
    }

    fn execute_function_internal(&'a self, type_instance: Option<(Path<'a>, IdentifierType)>, parameters: &'a Vec<Parameter>, values: Vec<Path<'a>>, statements: &'a Vec<Statement>, state: &mut State) -> Result<Option<Path<'a>>, InterpreterError> {
        let mut context = Context::new();
        context.begin_scope();
        if let Some((instance, instance_name)) = type_instance {
            context.register_variable(instance_name, instance)?;
        }

        for (param, value) in parameters.iter().zip(values.into_iter()) {
            context.register_variable(param.get_name().clone(), value)?;
        }

        self.execute_statements(statements, &mut context, state)
    }

    // Execute the selected function
    fn execute_function(&'a self, func: &'a FunctionType, type_instance: Option<Path<'a>>, values: Vec<Path<'a>>, state: &mut State) -> Result<Option<Path<'a>>, InterpreterError> {
        match func {
            FunctionType::Native(ref f) => {
                match type_instance {
                    Some(mut v) => {
                        let mut instance = v.as_mut();
                        f.call_function(Some(instance.as_mut()), values, state).map(|v| v.map(Path::Owned))
                    },
                    None => {
                        f.call_function(None, values, state).map(|v| v.map(Path::Owned))
                    }
                }
            },
            FunctionType::Declared(ref f) => {
                let instance = match (type_instance, f.get_instance_name()) {
                    (Some(v), Some(n)) => Some((v, *n)),
                    (None, None) => None,
                    _ => return Err(InterpreterError::NativeFunctionExpectedInstance)
                };
                self.execute_function_internal(instance, &f.get_parameters(), values, &f.get_statements(), state)
            },
            FunctionType::Entry(ref f) => self.execute_function_internal(None, &f.get_parameters(), values, &f.get_statements(), state)
        }
    }

    // Compute the constants defined in the program
    pub fn compute_constants(&mut self, state: &mut State) -> Result<(), InterpreterError> {
        if self.constants.is_none() {
            let mut constants = NoHashMap::default();
            let mut context = Context::new();

            for constant in self.program.constants.iter() {
                let value = self.execute_expression_and_expect_value(&constant.value, &mut context, state)?
                    .into_owned();
                constants.insert(constant.id.clone(), value);
            }

            self.constants = Some(constants);
        }

        Ok(())
    }

    // Execute the program by calling an available entry function
    pub fn call_entry_function(&'a self, function_name: &IdentifierType, parameters: Vec<Path<'a>>, state: &mut State) -> Result<u64, InterpreterError> {
        let func = self.get_function(function_name)?;

        // only function marked as entry can be called from external
        if !func.is_entry() {
            return Err(InterpreterError::FunctionEntry(true, false))
        }

        match self.execute_function(func, None, parameters, state)? {
            Some(val) => Ok(val.as_u64()?),
            None => return Err(InterpreterError::NoExitCode)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ast::Signature, lexer::Lexer, parser::Parser, EnvironmentBuilder};

    #[track_caller]
    fn test_code_expect_value(key: &Signature, code: &str) -> Value {
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();
        let builder = EnvironmentBuilder::default();
        let parser = Parser::new(None, tokens, &builder);
        let (program, mapper) = parser.parse().unwrap();

        let mut state = State::new(None, None);
        let interpreter = Interpreter::new(&program, builder.environment()).unwrap();

        let mapped_name = mapper.get(&key).unwrap();
        let func = interpreter.get_function(&mapped_name).unwrap();
        let result = interpreter.execute_function(func, None, Vec::new(), &mut state).unwrap();
        result.unwrap().into_owned()
    }

    #[track_caller]
    fn test_code_expect_return(code: &str, expected: u64) {
        assert_eq!(test_code_expect_value(&Signature::new("main".to_string(), None, Vec::new()), code).to_u64().unwrap(), expected);
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
        test_code_expect_return("entry main() { let a: u64[] = [1, 2, 3]; a.pop(); return a.len(); }", 2);
    }

    #[test]
    fn test_number_operations_priority() {
        // test_code_expect_return("entry main() { return 10 + 10 * 10; }", 110);
        // test_code_expect_return("entry main() { return (10 + 10) * 10; }", 200);

        // test_code_expect_return("entry main() { return 10 + 10 / 5 + 3; }", 15);
    }

    #[test]
    fn test_basic_function_call() {
        test_code_expect_return("func add(a: u64, b: u64): u64 { return a + b; } entry main() { return add(10, 10); }", 20);
        test_code_expect_return("func add(a: u64, b: u64): u64 { return a + b; } entry main() { return add(10, add(10, 10)); }", 30);

        // With variable
        test_code_expect_return("func add(a: u64, b: u64): u64 { return a + b; } entry main() { let a: u64 = 10; return add(a, 10); }", 20);
        test_code_expect_return("func add(a: u64, b: u64): u64 { return a + b; } entry main() { let a: u64 = 10; return add(a, add(10, 10)); }", 30);
    }

    #[test]
    fn test_function_call_on_value() {
        test_code_expect_return("struct Test { a: u64 } func (v Test) add(b: u64): u64 { return v.a + b; } entry main() { let t: Test = Test {a: 10}; return t.add(10); }", 20);
        test_code_expect_return("struct Test { a: u64 } func (v Test) add(b: u64): u64 { return v.a + b; } entry main() { let t: Test = Test {a: 10}; return t.add(t.add(10)); }", 30);
        test_code_expect_return("struct Test { a: u64 } func (v Test) add(b: u64) { v.a += b; } entry main() { let t: Test = Test {a: 10}; t.add(10); return t.a }", 20);
    }

    #[test]
    fn test_casting() {
        let key = Signature::new("main".to_string(), None, Vec::new());

        // Auto casting
        assert_eq!(Value::U8(10), test_code_expect_value(&key, "func main(): u8 { return 10; }"));
        assert_eq!(Value::U16(10), test_code_expect_value(&key, "func main(): u16 { return 10; }"));
        assert_eq!(Value::U32(10), test_code_expect_value(&key, "func main(): u32 { return 10; }"));
        assert_eq!(Value::U64(10), test_code_expect_value(&key, "func main(): u64 { return 10; }"));
        assert_eq!(Value::U128(10), test_code_expect_value(&key, "func main(): u128 { return 10L; }"));

        // Explicit casting
        assert_eq!(Value::U8(10), test_code_expect_value(&key, "func main(): u8 { let a: u64 = 10; return a as u8; }"));
        assert_eq!(Value::U16(10), test_code_expect_value(&key, "func main(): u16 { let a: u64 = 10; return a as u16; }"));
        assert_eq!(Value::U32(10), test_code_expect_value(&key, "func main(): u32 { let a: u64 = 10; return a as u32; }"));
        assert_eq!(Value::U64(10), test_code_expect_value(&key, "func main(): u64 { let a: u32 = 10; return a as u64; }"));
        assert_eq!(Value::U128(10), test_code_expect_value(&key, "func main(): u128 { let a: u64 = 10; return a as u128; }"));

        let code = "
            func add(left: u64, right: u64): u64 {
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
        assert_eq!(Value::String("hello world10".to_string()), test_code_expect_value(&key, "func main(): string { return (\"hello world\" + 10); }"));
        assert_eq!(Value::String("10hello world".to_string()), test_code_expect_value(&key, "func main(): string { return (10 + \"hello world\"); }"));
        assert_eq!(Value::String("10hello world10".to_string()), test_code_expect_value(&key, "func main(): string { return (10 + \"hello world\" + 10); }"));

        // With variables
        assert_eq!(Value::String("hello world10".to_string()), test_code_expect_value(&key, "func main(): string { let a: u64 = 10; return (\"hello world\" + a); }"));
        assert_eq!(Value::String("10hello world".to_string()), test_code_expect_value(&key, "func main(): string { let a: u64 = 10; return (a + \"hello world\"); }"));
        assert_eq!(Value::String("10hello world10".to_string()), test_code_expect_value(&key, "func main(): string { let a: u64 = 10; return (a + \"hello world\" + a); }"));
    }

    #[test]
    fn test_negative_bool() {
        let key = Signature::new("main".to_string(), None, Vec::new());
        assert_eq!(Value::Boolean(true), test_code_expect_value(&key, "func main(): bool { return !false; }"));
        assert_eq!(Value::Boolean(false), test_code_expect_value(&key, "func main(): bool { return !true; }"));
        assert_eq!(Value::Boolean(false), test_code_expect_value(&key, "func main(): bool { let add: bool = true; add = !add; return add; }"));
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
        assert_eq!(Value::Boolean(true), test_code_expect_value(key, "func main(): bool { return \"test\" == 'test'; }"));
        assert_eq!(Value::Boolean(false), test_code_expect_value(key, "func main(): bool { return \"test\" == \"test2\"; }"));
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
}