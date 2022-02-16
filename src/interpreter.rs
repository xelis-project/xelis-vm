use crate::functions::{FunctionType};
use crate::expressions::{Statement, Expression};
use crate::parser::Program;
use crate::types::*;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug)]
pub enum InterpreterError {
    FunctionNotFound(String, Vec<Type>),
    TypeNotFound(Value),
    FunctionEntry(bool, bool), // expected, got
    NotImplemented,
    ExpectedValue,
    InvalidNativeFunctionCall,
    NoInstanceType,
    NoTypeFound,
    ExpectedValueType(Type),
    OutOfBounds(usize, usize),
    InvalidStructValue(Value),
    InvalidValue(Value, Type), // got value, but expected type
    VariableNotFound(String),
    VariableAlreadyExists(String),
    NoScopeFound,
    ExpectedAssignOperator
}

struct Variable {
    value: Value,
    value_type: Type
}

struct Context {
    variables: Vec<HashMap<String, Variable>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            variables: Vec::new()
        }
    }

    fn get_last_scope(&mut self) -> Result<&mut HashMap<String, Variable>, InterpreterError> {
        let size = self.variables.len();
        if size == 0 {
            return Err(InterpreterError::NoScopeFound)
        }

        match self.variables.get_mut(size - 1) {
            Some(scope) => Ok(scope),
            None => Err(InterpreterError::NoScopeFound)
        }
    }

    pub fn create_scope(&mut self) {
        self.variables.push(HashMap::new());
    }

    pub fn remove_last_scope(&mut self) -> Result<(), InterpreterError> {
        let size = self.variables.len();
        if size == 0 {
            return Err(InterpreterError::NoScopeFound)
        }

        self.variables.remove(size - 1);
        Ok(())
    }

    pub fn get_variable(&self, name: &String) -> Result<&Variable, InterpreterError> {
        for vars in &self.variables {
            match vars.get(name) {
                Some(ref var) => return Ok(var),
                None => {}
            };
        }

        Err(InterpreterError::VariableNotFound(name.clone()))
    }

    pub fn get_mut_variable(&mut self, name: &String) -> Result<&mut Variable, InterpreterError> {
        for vars in &mut self.variables {
            match vars.get_mut(name) {
                Some(var) => return Ok(var),
                None => {}
            };
        }

        Err(InterpreterError::VariableNotFound(name.clone()))
    }

    pub fn set_variable_value(&mut self, name: &String, value: Value, structures: &HashMap<String, Struct>) -> Result<(), InterpreterError> {
        let var = self.get_mut_variable(name)?;
        match Type::from_value(&value, structures) {
            Some(t) => {
                if var.value_type != t {
                    return Err(InterpreterError::ExpectedValueType(var.value_type.clone()))
                }
            },
            None => return Err(InterpreterError::ExpectedValueType(var.value_type.clone()))
        }
        var.value = value;
        Ok(())
    }

    pub fn has_variable(&self, name: &String) -> bool {
        self.get_variable(name).is_ok()
    }

    pub fn register_variable(&mut self, name: String, variable: Variable) -> Result<(), InterpreterError> {
        if self.has_variable(&name) {
            return Err(InterpreterError::VariableAlreadyExists(name))
        }
        let scope = self.get_last_scope()?;
        scope.insert(name, variable);

        Ok(())
    }
}

pub struct Interpreter {
    program: Program,
    constants: Context,
    count_expr: RefCell<usize>
}

impl Interpreter {
    pub fn new(program: Program) -> Result<Self, InterpreterError> {
        let mut interpreter = Self {
            program,
            constants: Context::new(),
            count_expr: RefCell::new(0)
        };

        // Setup constants scope
        interpreter.constants.create_scope();
        while interpreter.program.constants.len() > 0 { // consume constants without "borrow partial move" error
            let constant = interpreter.program.constants.remove(0); // TODO prevent shifting all values
            let value = interpreter.execute_expression_and_expect_value(&constant.value)?;
            let variable = Variable {
                value,
                value_type: constant.value_type
            };
            interpreter.constants.register_variable(constant.name, variable)?;
        }

        Ok(interpreter)
    }

    fn get_types_from_values(&self, values: &Vec<Value>) -> Result<Vec<Type>, InterpreterError> {
        let mut types: Vec<Type> = Vec::new();
        for value in values {
            match Type::from_value(value, &self.program.structures) {
                Some(v) => {
                    types.push(v);
                }
                None => return Err(InterpreterError::TypeNotFound(value.clone()))
            };
        }

        Ok(types)
    }

    fn get_compatible_function(&self, name: &String, for_type: Option<Type>, values: &Vec<Value>) -> Result<&FunctionType, InterpreterError> {
        self.get_function(name, for_type, &self.get_types_from_values(values)?)
    }

    fn get_function(&self, name: &String, for_type: Option<Type>, parameters: &Vec<Type>) -> Result<&FunctionType, InterpreterError> {
        'funcs: for f in &self.program.functions {
            if *f.get_name() == *name && for_type == *f.for_type() && f.get_parameters_count() == parameters.len() {
                let f_types = f.get_parameters_types();
                for i in 0..f_types.len() {
                    if *f_types[i] != parameters[i] {
                        continue 'funcs;
                    }
                }
                return Ok(f)
            }
        }

        return Err(InterpreterError::FunctionNotFound(name.clone(), parameters.clone()))
    }

    fn execute_expression_and_expect_value(&self, expr: &Expression) -> Result<Value, InterpreterError> {
        match self.execute_expression(expr)? {
            Some(val) => Ok(val),
            None => Err(InterpreterError::ExpectedValue)
        }
    }

    fn execute_expression(&self, expr: &Expression) -> Result<Option<Value>, InterpreterError> {
        *self.count_expr.borrow_mut() += 1;
        match expr {
            Expression::FunctionCall(name, parameters) => {
                let mut values: Vec<Value> = Vec::new();
                for param in parameters {
                    match self.execute_expression(param)? {
                        Some(v) => values.push(v),
                        None => return Err(InterpreterError::ExpectedValue)
                    }
                }

                let func = self.get_function(name, None, &self.get_types_from_values(&values)?)?;
                self.execute_function(&func, None, values)?;
            }
            _ => {}
        }
        return Err(InterpreterError::NotImplemented)
    }

    fn execute_statements(&self, statements: &Vec<Statement>, context: &mut Context) -> Result<Option<Value>, InterpreterError> {
        let mut accept_else = false;
        for statement in statements {
            *self.count_expr.borrow_mut() += 1;
            match statement {
                Statement::Break => break,
                Statement::Continue => break,
                Statement::Variable(var) => {
                    let variable = Variable {
                        value: self.execute_expression_and_expect_value(&var.value)?,
                        value_type: var.value_type.clone()
                    };
                    context.register_variable(var.name.clone(), variable)?;
                },
                Statement::If(condition, statements) => {
                    if self.execute_expression_and_expect_value(&condition)?.to_bool()? {
                        context.create_scope();
                        match self.execute_statements(&statements, context)? {
                            Some(v) => {
                                context.remove_last_scope()?;
                                return Ok(Some(v))
                            },
                            None => {
                                context.remove_last_scope()?;
                            }
                        };
                    } else {
                        accept_else = true;
                    }
                },
                Statement::ElseIf(condition, statements) => if accept_else {
                    if self.execute_expression_and_expect_value(&condition)?.to_bool()? {
                        context.create_scope();
                        match self.execute_statements(&statements, context)? {
                            Some(v) => {
                                context.remove_last_scope()?;
                                return Ok(Some(v))
                            },
                            None => {
                                context.remove_last_scope()?;
                            }
                        };
                    } else {
                        accept_else = true;
                    }
                },
                Statement::Else(statements) => if accept_else {
                    context.create_scope();
                    match self.execute_statements(&statements, context)? {
                        Some(v) => {
                            context.remove_last_scope()?;
                            return Ok(Some(v))
                        },
                        None => {
                            context.remove_last_scope()?;
                        }
                    };
                }
                Statement::For(var, condition, increment, statements) => {
                    context.create_scope();
                    let variable = Variable {
                        value: self.execute_expression_and_expect_value(&var.value)?,
                        value_type: var.value_type.clone()
                    };
                    context.register_variable(var.name.clone(), variable)?;
                    loop {
                        if !self.execute_expression_and_expect_value(condition)?.to_bool()? {
                            break;
                        }

                        if self.execute_expression(increment)?.is_some() { // assign operator don't return values
                            return Err(InterpreterError::ExpectedAssignOperator);
                        }

                        match self.execute_statements(&statements, context)? {
                            Some(v) => {
                                context.remove_last_scope()?;
                                return Ok(Some(v))
                            },
                            None => {}
                        };
                    }
                    context.remove_last_scope()?;
                },
                Statement::ForEach(var, expr, statements) => {
                    let values = self.execute_expression_and_expect_value(expr)?.to_vec()?;
                    if values.len() != 0 {
                        context.create_scope();
                        let value_type = match Type::from_value(&values[0], &self.program.structures) {
                            Some(v) => {
                                v
                            },
                            None => return Err(InterpreterError::NoTypeFound)
                        };
                        let variable = Variable {
                            value: Value::Null,
                            value_type
                        };

                        context.register_variable(var.clone(), variable)?;
                        for val in values {
                            context.set_variable_value(var, val, &self.program.structures)?;
                            match self.execute_statements(&statements, context)? {
                                Some(v) => {
                                    context.remove_last_scope()?;
                                    return Ok(Some(v))
                                },
                                None => {}
                            };
                        }
                        context.remove_last_scope()?;
                    }
                },
                Statement::While(condition, statements) => {
                    context.create_scope();
                    while self.execute_expression_and_expect_value(&condition)?.to_bool()? {
                        match self.execute_statements(&statements, context)? {
                            Some(v) => {
                                context.remove_last_scope()?;
                                return Ok(Some(v))
                            },
                            None => {}
                        };
                    }
                    context.remove_last_scope()?;
                },
                Statement::Return(opt) => {
                    return Ok(match opt {
                        Some(v) => Some(self.execute_expression_and_expect_value(&v)?),
                        None => None
                    })
                },
                Statement::Scope(statements) => {
                    context.create_scope();
                    match self.execute_statements(&statements, context)? {
                        Some(v) => {
                            context.remove_last_scope()?;
                            return Ok(Some(v))
                        },
                        None => {
                            context.remove_last_scope()?;
                        }
                    };
                },
                Statement::Expression(expr) => {
                    self.execute_expression(&expr)?;
                }
            };

            match statement {
                Statement::If(_, _) | Statement::ElseIf(_, _) => {},
                _ => {
                    accept_else = false;
                }
            };
        }
        Ok(None)
    }

    fn execute_function(&self, func: &FunctionType, type_instance: Option<&mut Value>, mut values: Vec<Value>) -> Result<Option<Value>, InterpreterError> {
        match func {
            FunctionType::Native(ref f) => {
                Err(InterpreterError::NotImplemented)
                //f.call_function(current_value: &mut Value, parameters: Vec<Value>)
            },
            FunctionType::Custom(ref f) => {
                let mut context = Context::new();
                context.create_scope();
                for param in f.get_parameters() {
                    let variable = Variable {
                        value: values.remove(0),
                        value_type: param.get_type().clone()
                    };

                    context.register_variable(param.get_name().clone(), variable)?;
                }
                let result = self.execute_statements(f.get_statements(), &mut context);
                context.remove_last_scope()?;

                result
            }
        }
    }

    pub fn call_entry_function(&self, function_name: &String, parameters: Vec<Value>) -> Result<u64, InterpreterError> {
        let func = self.get_compatible_function(function_name, None, &parameters)?;
        if !func.is_entry() { // only function marked as entry can be called from external
            return Err(InterpreterError::FunctionEntry(true, false))
        }

        self.execute_function(func, None, parameters)?;
        Ok(0)
    }
}