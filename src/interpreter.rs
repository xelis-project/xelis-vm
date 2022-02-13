use crate::functions::{FunctionType};
use crate::expressions::{Statement, Expression};
use crate::parser::Program;
use crate::types::*;
use std::cell::RefCell;

#[derive(Debug)]
pub enum InterpreterError {
    FunctionNotFound(String, Vec<Type>),
    TypeNotFound(Value),
    FunctionEntry(bool, bool), // expected, got
    NotImplemented,
    ExpectedValue,
    InvalidNativeFunctionCall,
    NoInstanceType,
    OutOfBounds(usize, usize),
    InvalidValue(Value, Type) // got value, but expected type
}

pub struct Interpreter {
    program: Program,
    count_expr: RefCell<usize>
}

impl Interpreter {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            count_expr: RefCell::new(0)
        }
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

    fn execute_statements(&self, statements: &Vec<Statement>) -> Result<Option<Value>, InterpreterError> {
        for statement in statements {
            *self.count_expr.borrow_mut() += 1;

            match statement {
                Statement::Break => break,
                Statement::Continue => break,
                Statement::Variable(var) => {},
                Statement::If(condition, statements) => {},
                Statement::ElseIf(condition, statements) => {},
                Statement::Else(statements) => {},
                Statement::For(var, condition, increment, statements) => {},
                Statement::ForEach(var, values, statements) => {},
                Statement::While(condition, statements) => {},
                Statement::Return(opt) => {},
                Statement::Scope(statements) => {},
                Statement::Expression(expr) => {}
            };
        }
        return Err(InterpreterError::NotImplemented)
    }

    fn execute_function(&self, func: &FunctionType, type_instance: Option<&mut Value>, values: Vec<Value>) -> Result<Option<Value>, InterpreterError> {
        match func {
            FunctionType::Native(ref f) => {
                // f.call_function(current_value: &mut Value, parameters: Vec<Value>)
            },
            FunctionType::Custom(ref f) => {
                self.execute_statements(f.get_statements());
            }
        }

        return Err(InterpreterError::NotImplemented)
    }

    pub fn call_entry_function(&self, function_name: &String, parameters: Vec<Value>) -> Result<u64, InterpreterError> {
        let func = self.get_function(function_name, None, &self.get_types_from_values(&parameters)?)?;
        if !func.is_entry() { // only function marked as entry can be called from external
            return Err(InterpreterError::FunctionEntry(true, false))
        }

        self.execute_function(func, None, parameters)?;

        Ok(0)
    }
}