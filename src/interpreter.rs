use crate::functions::{FunctionType};
use crate::expressions::{Statement, Expression, Operator};
use crate::parser::Program;
use crate::types::*;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug)]
pub enum InterpreterError {
    StructureNotFound(String),
    FunctionNotFound(String, Vec<Type>),
    TypeNotFound(Value),
    FunctionEntry(bool, bool), // expected, got
    NotImplemented,
    NoExitCode,
    ExpectedValue,
    InvalidNativeFunctionCall,
    NoInstanceType,
    ExpectedPath,
    ExpectedValueType(Type),
    InvalidType(Type),
    OutOfBounds(usize, usize),
    InvalidStructValue(Value),
    InvalidValue(Value, Type), // got value, but expected type
    VariableNotFound(String),
    VariableAlreadyExists(String),
    NoScopeFound,
    ExpectedAssignOperator,
    OperationNotNumberType
}

struct Variable {
    value: Value,
    value_type: Type
}

struct Context {
    variables: Vec<HashMap<String, Variable>>
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

    pub fn push_scope(&mut self) {
        self.variables.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) -> Result<(), InterpreterError> {
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
        interpreter.constants.push_scope();
        while interpreter.program.constants.len() > 0 { // consume constants without "borrow partial move" error
            let constant = interpreter.program.constants.remove(0); // TODO prevent shifting all values
            // let mut clone = interpreter.constants.clone(); TODO
            let value = interpreter.execute_expression_and_expect_value(&constant.value, &mut Context::new())?;
            let variable = Variable {
                value,
                value_type: constant.value_type
            };
            interpreter.constants.register_variable(constant.name, variable)?;
        }

        Ok(interpreter)
    }

    fn is_same_value(&self, value_type: &Type, left: &Value, right: &Value) -> Result<bool, InterpreterError> {
        Ok(match value_type {
            Type::Null => left.is_null() && right.is_null(),
            Type::Any => return Err(InterpreterError::InvalidType(value_type.clone())),
            Type::Byte => *left.as_byte()? == *right.as_byte()?,
            Type::Short => *left.as_short()? == *right.as_short()?,
            Type::Int => *left.as_int()? == *right.as_int()?,
            Type::Long => *left.as_long()? == *right.as_long()?,
            Type::Boolean => *left.as_bool()? == *right.as_bool()?,
            Type::String => *left.as_string()? == *right.as_string()?,
            Type::Struct(structure) => {
                let left_map = left.as_map()?;
                let right_map = right.as_map()?;

                if left_map.len() == right_map.len() {
                    let mut equal = true;
                    for (k, v) in left_map {
                        if !match right_map.get(k) {
                            Some(r_v) => {
                                let field_type = match structure.fields.get(k) {
                                    Some(field) => field,
                                    None => return Err(InterpreterError::InvalidStructValue(v.clone()))
                                };
                                self.is_same_value(field_type, v, r_v)?
                            },
                            None => false
                        } {
                            equal = false;
                            break;
                        }
                    }
                    equal
                } else {
                    false
                }
            },
            Type::Array(sub_type) => {
                let left_vec = left.as_vec()?;
                let right_vec = right.as_vec()?;
                if left_vec.len() == right_vec.len() {
                    let mut equal = true;
                    for i in 0..left_vec.len() - 1 {
                        if !self.is_same_value(sub_type, &left_vec[i], &right_vec[i])? {
                            equal = false;
                            break;
                        }
                    }
                    equal
                } else {
                    false
                }
            }
        })
    }

    fn get_type_from_value(&self, value: &Value) -> Result<Type, InterpreterError> {
        match Type::from_value(value, &self.program.structures) {
            Some(v) => Ok(v),
            None => Err(InterpreterError::TypeNotFound(value.clone()))
        }
    }

    fn get_types_from_values(&self, values: &Vec<Value>) -> Result<Vec<Type>, InterpreterError> {
        let mut types: Vec<Type> = Vec::new();
        for value in values {
            types.push(self.get_type_from_value(&value)?);
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

    fn get_from_path<'a>(&self, path: &Expression, context: &'a mut Context) -> Result<(&'a mut Value, &'a Type), InterpreterError> {
        match path {
            Expression::ArrayCall(expr, expr_index) => {
                let index = self.execute_expression_and_expect_value(expr_index, context)?.to_int()? as usize;
                let (array, array_type) = self.get_from_path(expr, context)?;
                let values: &mut Vec<Value> = array.as_mut_vec()?;
                let size = values.len();
                match values.get_mut(index as usize) {
                    Some(v) => Ok((v, array_type.get_array_type())),
                    None => return Err(InterpreterError::OutOfBounds(size, index))
                }
            }
            Expression::Path(left, right) => {
                let (left_value, left_type) = self.get_from_path(left, context)?;
                // TODO set left_value as instance ?
                self.execute_expression(right, context)?;
                panic!("")
            },
            Expression::Variable(name) => {
                let var = context.get_mut_variable(name)?;
                Ok((&mut var.value, &var.value_type))
            }
            _ => Err(InterpreterError::ExpectedPath)
        }
    }

    fn execute_expression_and_expect_value(&self, expr: &Expression, context: &mut Context) -> Result<Value, InterpreterError> {
        match self.execute_expression(expr, context)? {
            Some(val) => Ok(val),
            None => Err(InterpreterError::ExpectedValue)
        }
    }

    fn execute_expression(&self, expr: &Expression, context: &mut Context) -> Result<Option<Value>, InterpreterError> {
        *self.count_expr.borrow_mut() += 1;
        match expr {
            Expression::FunctionCall(name, parameters) => {
                let mut values: Vec<Value> = Vec::new();
                for param in parameters {
                    values.push(self.execute_expression_and_expect_value(param, context)?);
                }

                let func = self.get_function(name, None, &self.get_types_from_values(&values)?)?;
                self.execute_function(&func, None, values)
            },
            Expression::ArrayConstructor(expressions) => {
                let mut values = vec![];
                for expr in expressions {
                    let value = self.execute_expression_and_expect_value(&expr, context)?;
                    values.push(value);
                }

                Ok(Some(Value::Array(values)))
            },
            Expression::StructConstructor(name, expr_fields) => {
                match self.program.structures.get(name) {
                    Some(_) => { // TODO check validity ?
                        let mut fields = HashMap::new();
                        for (name, expr) in expr_fields {
                            fields.insert(name.clone(), self.execute_expression_and_expect_value(&expr, context)?);
                        }
                        Ok(Some(Value::Struct(name.clone(), fields)))
                    },
                    None => Err(InterpreterError::StructureNotFound(name.clone()))
                }
            },
            Expression::ArrayCall(expr, expr_index) => {
                let values = self.execute_expression_and_expect_value(&expr, context)?.to_vec()?;
                let index = self.execute_expression_and_expect_value(&expr_index, context)?.to_int()? as usize;

                Ok(match values.get(index) {
                    Some(v) => Some(v.clone()),
                    None => return Err(InterpreterError::OutOfBounds(values.len(), index))
                })
            },
            Expression::IsNot(expr) => {
                let val = self.execute_expression_and_expect_value(&expr, context)?.to_bool()?;
                Ok(Some(Value::Boolean(!val)))
            }
            Expression::SubExpression(expr) => self.execute_expression(expr, context),
            Expression::Ternary(condition, left, right) => {
                if self.execute_expression_and_expect_value(&condition, context)?.to_bool()? {
                    Ok(Some(self.execute_expression_and_expect_value(&left, context)?))
                } else {
                    Ok(Some(self.execute_expression_and_expect_value(&right, context)?))
                }
            }
            Expression::Value(v) => Ok(Some(v.clone())),
            Expression::Variable(var) => {
                match context.get_variable(var) {
                    Ok(v) => Ok(Some(v.value.clone())),
                    Err(_) => Ok(Some(self.constants.get_variable(var)?.value.clone()))
                }
            }
            Expression::Operator(op, expr_left, expr_right) => {
                if op.is_assignation() {
                    let value = self.execute_expression_and_expect_value(expr_right, context)?;
                    let (path_value, path_type) = self.get_from_path(expr_left, context)?;
                    let value_type = self.get_type_from_value(&value)?;

                    if (!path_value.is_number() || !value.is_number() || *path_type != value_type) && op.is_number_operator() {
                        return Err(InterpreterError::OperationNotNumberType)
                    }

                    match op {
                        Operator::Assign => {
                            *path_value = value;
                        },
                        Operator::AssignPlus => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(path_value.as_byte()? + value.to_byte()?),
                                Type::Short => Value::Short(path_value.as_short()? + value.to_short()?),
                                Type::Int => Value::Int(path_value.as_int()? + value.to_int()?),
                                Type::Long => Value::Long(path_value.as_long()? + value.to_long()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignMinus => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(path_value.as_byte()? - value.to_byte()?),
                                Type::Short => Value::Short(path_value.as_short()? - value.to_short()?),
                                Type::Int => Value::Int(path_value.as_int()? - value.to_int()?),
                                Type::Long => Value::Long(path_value.as_long()? - value.to_long()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignDivide => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(path_value.as_byte()? / value.to_byte()?),
                                Type::Short => Value::Short(path_value.as_short()? / value.to_short()?),
                                Type::Int => Value::Int(path_value.as_int()? / value.to_int()?),
                                Type::Long => Value::Long(path_value.as_long()? / value.to_long()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignMultiply => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(path_value.as_byte()? * value.to_byte()?),
                                Type::Short => Value::Short(path_value.as_short()? * value.to_short()?),
                                Type::Int => Value::Int(path_value.as_int()? * value.to_int()?),
                                Type::Long => Value::Long(path_value.as_long()? * value.to_long()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        }, 
                        _ => return Err(InterpreterError::NotImplemented) 
                    };
                    Ok(None)
                } else {
                    let left = self.execute_expression_and_expect_value(&expr_left, context)?;
                    let right = self.execute_expression_and_expect_value(&expr_right, context)?;
                    let left_type = self.get_type_from_value(&left)?;
                    let right_type = self.get_type_from_value(&right)?;
                    if (!left.is_number() || !right.is_number() || right_type != left_type) && op.is_number_operator() {
                        return Err(InterpreterError::OperationNotNumberType)
                    }
    
                    match op {
                        Operator::Equals => Ok(Some(Value::Boolean(left_type == right_type && self.is_same_value(&left_type, &left, &right)?))),
                        Operator::NotEquals => Ok(Some(Value::Boolean(left_type != right_type || !self.is_same_value(&left_type, &left, &right)?))),
                        Operator::And => Ok(Some(Value::Boolean(left.to_bool()? && right.to_bool()?))),
                        Operator::Or => Ok(Some(Value::Boolean(left.to_bool()? || right.to_bool()?))),
                        Operator::Plus => {
                            if left_type == Type::String || right_type == Type::String {
                                return Ok(Some(Value::String(format!("{}{}", left, right))))
                            } else {
                                Ok(Some(match left_type {
                                    Type::Byte => Value::Byte(left.to_byte()? + right.to_byte()?),
                                    Type::Short => Value::Short(left.to_short()? + right.to_short()?),
                                    Type::Int => Value::Int(left.to_int()? + right.to_int()?),
                                    Type::Long => Value::Long(left.to_long()? + right.to_long()?),
                                    _ => return Err(InterpreterError::OperationNotNumberType)
                                }))
                            }
                        },
                        Operator::Minus => Ok(Some(match left_type {
                            Type::Byte => Value::Byte(left.to_byte()? - right.to_byte()?),
                            Type::Short => Value::Short(left.to_short()? - right.to_short()?),
                            Type::Int => Value::Int(left.to_int()? - right.to_int()?),
                            Type::Long => Value::Long(left.to_long()? - right.to_long()?),
                            _ => return Err(InterpreterError::OperationNotNumberType)
                        })),
                        Operator::Divide => Ok(Some(match left_type {
                            Type::Byte => Value::Byte(left.to_byte()? / right.to_byte()?),
                            Type::Short => Value::Short(left.to_short()? / right.to_short()?),
                            Type::Int => Value::Int(left.to_int()? / right.to_int()?),
                            Type::Long => Value::Long(left.to_long()? / right.to_long()?),
                            _ => return Err(InterpreterError::OperationNotNumberType)
                        })),
                        Operator::Multiply => Ok(Some(match left_type {
                            Type::Byte => Value::Byte(left.to_byte()? * right.to_byte()?),
                            Type::Short => Value::Short(left.to_short()? * right.to_short()?),
                            Type::Int => Value::Int(left.to_int()? * right.to_int()?),
                            Type::Long => Value::Long(left.to_long()? * right.to_long()?),
                            _ => return Err(InterpreterError::OperationNotNumberType)
                        })),
                        Operator::Modulo => Ok(Some(match left_type {
                            Type::Byte => Value::Byte(left.to_byte()? % right.to_byte()?),
                            Type::Short => Value::Short(left.to_short()? % right.to_short()?),
                            Type::Int => Value::Int(left.to_int()? % right.to_int()?),
                            Type::Long => Value::Long(left.to_long()? % right.to_long()?),
                            _ => return Err(InterpreterError::OperationNotNumberType)
                        })),
                        Operator::BitwiseLeft => Ok(Some(match left_type {
                            Type::Byte => Value::Byte(left.to_byte()? << right.to_byte()?),
                            Type::Short => Value::Short(left.to_short()? << right.to_short()?),
                            Type::Int => Value::Int(left.to_int()? << right.to_int()?),
                            Type::Long => Value::Long(left.to_long()? << right.to_long()?),
                            _ => return Err(InterpreterError::OperationNotNumberType)
                        })),
                        Operator::BitwiseRight => Ok(Some(match left_type {
                            Type::Byte => Value::Byte(left.to_byte()? >> right.to_byte()?),
                            Type::Short => Value::Short(left.to_short()? >> right.to_short()?),
                            Type::Int => Value::Int(left.to_int()? >> right.to_int()?),
                            Type::Long => Value::Long(left.to_long()? >> right.to_long()?),
                            _ => return Err(InterpreterError::OperationNotNumberType)
                        })),
                        Operator::GreaterOrEqual => Ok(Some(match left_type {
                            Type::Byte => Value::Boolean(left.to_byte()? >= right.to_byte()?),
                            Type::Short => Value::Boolean(left.to_short()? >= right.to_short()?),
                            Type::Int => Value::Boolean(left.to_int()? >= right.to_int()?),
                            Type::Long => Value::Boolean(left.to_long()? >= right.to_long()?),
                            _ => return Err(InterpreterError::OperationNotNumberType)
                        })),
                        Operator::GreaterThan => Ok(Some(match left_type {
                            Type::Byte => Value::Boolean(left.to_byte()? > right.to_byte()?),
                            Type::Short => Value::Boolean(left.to_short()? > right.to_short()?),
                            Type::Int => Value::Boolean(left.to_int()? > right.to_int()?),
                            Type::Long => Value::Boolean(left.to_long()? > right.to_long()?),
                            _ => return Err(InterpreterError::OperationNotNumberType)
                        })),
                        Operator::LessOrEqual => Ok(Some(match left_type {
                            Type::Byte => Value::Boolean(left.to_byte()? <= right.to_byte()?),
                            Type::Short => Value::Boolean(left.to_short()? <= right.to_short()?),
                            Type::Int => Value::Boolean(left.to_int()? <= right.to_int()?),
                            Type::Long => Value::Boolean(left.to_long()? <= right.to_long()?),
                            _ => return Err(InterpreterError::OperationNotNumberType)
                        })),
                        Operator::LessThan => Ok(Some(match left_type {
                            Type::Byte => Value::Boolean(left.to_byte()? < right.to_byte()?),
                            Type::Short => Value::Boolean(left.to_short()? < right.to_short()?),
                            Type::Int => Value::Boolean(left.to_int()? < right.to_int()?),
                            Type::Long => Value::Boolean(left.to_long()? < right.to_long()?),
                            _ => return Err(InterpreterError::OperationNotNumberType)
                        })),
                        _ => return Err(InterpreterError::NotImplemented)
                    }
                }
            },
            Expression::Path(left, right) => {
                let (left_value, left_type) = self.get_from_path(left, context)?;
                // TODO
                self.execute_expression(right, context)
            }
        }
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
                        value: self.execute_expression_and_expect_value(&var.value, context)?,
                        value_type: var.value_type.clone()
                    };
                    context.register_variable(var.name.clone(), variable)?;
                },
                Statement::If(condition, statements) => {
                    if self.execute_expression_and_expect_value(&condition, context)?.to_bool()? {
                        context.push_scope();
                        match self.execute_statements(&statements, context)? {
                            Some(v) => {
                                context.pop_scope()?;
                                return Ok(Some(v))
                            },
                            None => {
                                context.pop_scope()?;
                            }
                        };
                    } else {
                        accept_else = true;
                    }
                },
                Statement::ElseIf(condition, statements) => if accept_else {
                    if self.execute_expression_and_expect_value(&condition, context)?.to_bool()? {
                        context.push_scope();
                        match self.execute_statements(&statements, context)? {
                            Some(v) => {
                                context.pop_scope()?;
                                return Ok(Some(v))
                            },
                            None => {
                                context.pop_scope()?;
                            }
                        };
                    } else {
                        accept_else = true;
                    }
                },
                Statement::Else(statements) => if accept_else {
                    context.push_scope();
                    match self.execute_statements(&statements, context)? {
                        Some(v) => {
                            context.pop_scope()?;
                            return Ok(Some(v))
                        },
                        None => {
                            context.pop_scope()?;
                        }
                    };
                }
                Statement::For(var, condition, increment, statements) => {
                    context.push_scope();
                    let variable = Variable {
                        value: self.execute_expression_and_expect_value(&var.value, context)?,
                        value_type: var.value_type.clone()
                    };
                    context.register_variable(var.name.clone(), variable)?;
                    loop {
                        if !self.execute_expression_and_expect_value(condition, context)?.to_bool()? {
                            break;
                        }

                        if self.execute_expression(increment, context)?.is_some() { // assign operator don't return values
                            return Err(InterpreterError::ExpectedAssignOperator);
                        }

                        match self.execute_statements(&statements, context)? {
                            Some(v) => {
                                context.pop_scope()?;
                                return Ok(Some(v))
                            },
                            None => {}
                        };
                    }
                    context.pop_scope()?;
                },
                Statement::ForEach(var, expr, statements) => {
                    let values = self.execute_expression_and_expect_value(expr, context)?.to_vec()?;
                    if values.len() != 0 {
                        context.push_scope();
                        let value_type = self.get_type_from_value(&values[0])?;
                        let variable = Variable {
                            value: Value::Null,
                            value_type
                        };

                        context.register_variable(var.clone(), variable)?;
                        for val in values {
                            context.set_variable_value(var, val, &self.program.structures)?;
                            match self.execute_statements(&statements, context)? {
                                Some(v) => {
                                    context.pop_scope()?;
                                    return Ok(Some(v))
                                },
                                None => {}
                            };
                        }
                        context.pop_scope()?;
                    }
                },
                Statement::While(condition, statements) => {
                    context.push_scope();
                    while self.execute_expression_and_expect_value(&condition, context)?.to_bool()? {
                        match self.execute_statements(&statements, context)? {
                            Some(v) => {
                                context.pop_scope()?;
                                return Ok(Some(v))
                            },
                            None => {}
                        };
                    }
                    context.pop_scope()?;
                },
                Statement::Return(opt) => {
                    return Ok(match opt {
                        Some(v) => Some(self.execute_expression_and_expect_value(&v, context)?),
                        None => None
                    })
                },
                Statement::Scope(statements) => {
                    context.push_scope();
                    match self.execute_statements(&statements, context)? {
                        Some(v) => {
                            context.pop_scope()?;
                            return Ok(Some(v))
                        },
                        None => {
                            context.pop_scope()?;
                        }
                    };
                },
                Statement::Expression(expr) => {
                    self.execute_expression(&expr, context)?;
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
                context.push_scope();
                for param in f.get_parameters() {
                    let variable = Variable {
                        value: values.remove(0),
                        value_type: param.get_type().clone()
                    };

                    context.register_variable(param.get_name().clone(), variable)?;
                }
                let result = self.execute_statements(f.get_statements(), &mut context);
                context.pop_scope()?;

                result
            }
        }
    }

    pub fn call_entry_function(&self, function_name: &String, parameters: Vec<Value>) -> Result<u64, InterpreterError> {
        let func = self.get_compatible_function(function_name, None, &parameters)?;
        if !func.is_entry() { // only function marked as entry can be called from external
            return Err(InterpreterError::FunctionEntry(true, false))
        }

        match self.execute_function(func, None, parameters)? {
            Some(val) => Ok(val.to_int()?),
            None => return Err(InterpreterError::NoExitCode)
        }
    }
}