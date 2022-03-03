use crate::expressions::{Statement, Expression, Operator};
use crate::functions::{FunctionType};
use crate::environment::Environment;
use crate::parser::Program;
use crate::types::*;
use std::collections::HashMap;
use std::cell::RefCell;

#[derive(Debug)]
pub enum InterpreterError {
    StructureNotFound(String),
    FunctionNotFound(String, Vec<Type>),
    TypeNotFound(Value),
    FunctionEntry(bool, bool), // expected, got
    LimitReached,
    NotImplemented,
    NoExitCode,
    ExpectedValue,
    InvalidNativeFunctionCall,
    NoInstanceType,
    ExpectedPath,
    UnexpectedInstanceType,
    ExpectedStructType,
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

type Scope = HashMap<String, Variable>;

struct Variable {
    value: Value,
    value_type: Type
}

impl Variable {
    pub fn new(value: Value, value_type: Type) -> Self {
        Self {
            value: value,
            value_type
        }
    }

    pub fn get_value(&self) -> &Value {
        &self.value
    }

    pub fn get_mut_value(&mut self) -> &mut Value {
        &mut self.value
    }

    pub fn get_type(&self) -> &Type {
        &self.value_type
    }
}

struct Context {
    variables: Vec<Scope>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            variables: Vec::new()
        }
    }

    fn get_last_scope(&mut self) -> Result<&mut Scope, InterpreterError> {
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

    pub fn set_variable_value(&mut self, name: &String, value: Value, structures: &RefMap<String, Struct>) -> Result<(), InterpreterError> {
        let var = self.get_mut_variable(name)?;
        match Type::from_value(&value, structures) {
            Some(t) => {
                if *var.get_type() != t {
                    Err(InterpreterError::ExpectedValueType(var.value_type.clone()))
                } else {
                    *var.get_mut_value() = value;
                    Ok(())
                }
            },
            None => Err(InterpreterError::ExpectedValueType(var.value_type.clone()))
        }
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

pub struct Interpreter<'a> {
    program: &'a Program,
    max_expr: u64,
    count_expr: RefCell<u64>,
    constants: Option<Context>,
    env: &'a Environment,
    ref_structures: RefMap<'a, String, Struct>
}

impl<'a> Interpreter<'a> {
    pub fn new(program: &'a Program, max_expr: u64, env: &'a Environment) -> Result<Self, InterpreterError> {
        let mut interpreter = Self {
            program,
            max_expr,
            count_expr: RefCell::new(0),
            constants: None,
            env,
            ref_structures: RefMap::new()
        };

        interpreter.ref_structures.link_maps(vec![interpreter.env.get_structures(), &interpreter.program.structures]);

        // register constants
        if interpreter.program.constants.len() > 0 {
            let mut context = Context::new();
            context.push_scope();
            for constant in &interpreter.program.constants {
                let value = interpreter.execute_expression_and_expect_value(&constant.value, &mut context)?;
                let variable = Variable::new(value, constant.value_type.clone());
                context.register_variable(constant.name.clone(), variable)?;
            }
            interpreter.constants = Some(context);
        }

        Ok(interpreter)
    }

    fn get_constant_variable(&self, name: &String) -> Result<&Variable, InterpreterError> {
        match &self.constants {
            Some(constants) => constants.get_variable(name),
            None => Err(InterpreterError::VariableNotFound(name.clone()))
        }
    }

    fn increment_expr(&self) -> Result<(), InterpreterError> {
        *self.count_expr.borrow_mut() += 1;
        if self.max_expr != 0 && *self.count_expr.borrow() >= self.max_expr {
            return Err(InterpreterError::LimitReached)
        }

        Ok(())
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
        match Type::from_value(value, &self.ref_structures) {
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

    fn get_compatible_function(&self, name: &String, for_type: Option<&Type>, values: &Vec<Value>) -> Result<&FunctionType, InterpreterError> {
        self.get_function(name, for_type, &self.get_types_from_values(values)?)
    }

    fn get_function(&self, name: &String, for_type: Option<&Type>, parameters: &Vec<Type>) -> Result<&FunctionType, InterpreterError> {
        let mut functions: Vec<&FunctionType> = self.program.functions.iter().map(|v| v).collect(); // merge two in one
        for f in self.env.get_functions() {
            functions.push(f);
        }

        'funcs: for f in &functions {
            if *f.get_name() == *name && for_type == f.for_type().as_ref() && f.get_parameters_count() == parameters.len() {
                let f_types = f.get_parameters_types();
                for i in 0..f_types.len() {
                    if *f_types[i] != Type::Any && *f_types[i] != parameters[i] {
                        continue 'funcs;
                    }
                }
                return Ok(f)
            }
        }

        return Err(InterpreterError::FunctionNotFound(name.clone(), parameters.clone()))
    }

    fn get_from_path(&self, on_value: Option<&mut Value>, path: &Expression, context: &mut Context) -> Result<&mut Value, InterpreterError> {
        match path {
            Expression::ArrayCall(expr, expr_index) => {
                let index = self.execute_expression_and_expect_value(expr_index, context)?.to_int()? as usize;
                let array = self.get_from_path(on_value, expr, context)?;
                let values: &mut Vec<Value> = array.as_mut_vec()?;
                let size = values.len();
                match values.get_mut(index as usize) {
                    Some(v) => Ok(v),
                    None => return Err(InterpreterError::OutOfBounds(size, index))
                }
            }
            Expression::Path(left, right) => {
                let left_value = self.get_from_path(on_value, left, context)?;
                let res = self.get_from_path(Some(left_value), right, context);
                res
            },
            Expression::Variable(name) => match on_value {
                Some(v) => match v.as_mut_map()?.get_mut(name) {
                    Some(value) => {
                        Ok(value)
                    },
                    None => Err(InterpreterError::VariableNotFound(name.clone()))
                },
                None => {
                    Ok(context.get_mut_variable(name)?.get_mut_value())
                }
            }
            _ => Err(InterpreterError::ExpectedPath)
        }
    }

    fn execute_expression_and_expect_value(&self, expr: &Expression, context: &mut Context) -> Result<Value, InterpreterError> {
        match self.execute_expression(None, expr, context)? {
            Some(val) => Ok(val),
            None => Err(InterpreterError::ExpectedValue)
        }
    }

    fn execute_expression(&self, on_value: Option<&mut Value>, expr: &Expression, context: &mut Context) -> Result<Option<Value>, InterpreterError> {
        self.increment_expr()?;
        match expr {
            Expression::FunctionCall(name, parameters) => {
                let mut values: Vec<Value> = Vec::new();
                for param in parameters {
                    values.push(self.execute_expression_and_expect_value(param, context)?);
                }

                match on_value {
                    Some(v) => {
                        let func = self.get_function(name, Some(&self.get_type_from_value(&v)?), &self.get_types_from_values(&values)?)?;
                        self.execute_function(&func, Some(v), values)
                    },
                    None => {
                        let func = self.get_function(name, None, &self.get_types_from_values(&values)?)?;
                        self.execute_function(&func, None, values)
                    }
                }
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
                let mut fields = HashMap::new();
                for (name, expr) in expr_fields {
                    fields.insert(name.clone(), self.execute_expression_and_expect_value(&expr, context)?);
                }
                Ok(Some(Value::Struct(name.clone(), fields)))
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
            Expression::SubExpression(expr) => self.execute_expression(None, expr, context),
            Expression::Ternary(condition, left, right) => {
                if self.execute_expression_and_expect_value(&condition, context)?.to_bool()? {
                    Ok(Some(self.execute_expression_and_expect_value(&left, context)?))
                } else {
                    Ok(Some(self.execute_expression_and_expect_value(&right, context)?))
                }
            }
            Expression::Value(v) => Ok(Some(v.clone())),
            Expression::Variable(var) =>  match context.get_variable(var) {
                Ok(v) => Ok(Some(v.get_value().clone())),
                Err(_) => Ok(Some(self.get_constant_variable(var)?.get_value().clone()))
            },
            Expression::Operator(op, expr_left, expr_right) => {
                if op.is_assignation() {
                    let value = self.execute_expression_and_expect_value(expr_right, context)?;
                    let path_value = self.get_from_path(on_value, expr_left, context)?;
                    let path_type = self.get_type_from_value(&path_value)?;
                    let value_type = self.get_type_from_value(&value)?;

                    if (!path_value.is_number() || !value.is_number() || path_type != value_type) && op.is_number_operator() {
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
            e => { // Path
                let left_value = self.get_from_path(on_value, e, context)?;
                Ok(Some(left_value.clone()))
            }
        }
    }

    fn execute_statements(&self, statements: &Vec<Statement>, context: &mut Context) -> Result<Option<Value>, InterpreterError> {
        let mut accept_else = false;
        for statement in statements {
            self.increment_expr()?;
            match statement {
                Statement::Break => break,
                Statement::Continue => break,
                Statement::Variable(var) => {
                    let variable = Variable::new(self.execute_expression_and_expect_value(&var.value, context)?, var.value_type.clone());
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
                    let variable = Variable::new(self.execute_expression_and_expect_value(&var.value, context)?, var.value_type.clone());
                    context.register_variable(var.name.clone(), variable)?;
                    loop {
                        if !self.execute_expression_and_expect_value(condition, context)?.to_bool()? {
                            break;
                        }

                        if self.execute_expression(None, increment, context)?.is_some() { // assign operator don't return values
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
                        let variable = Variable::new(Value::Null, value_type);
                        context.register_variable(var.clone(), variable)?;
                        for val in values {
                            context.set_variable_value(var, val, &self.ref_structures)?;
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
                    self.execute_expression(None, &expr, context)?;
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
        if func.for_type().is_some() != type_instance.is_some() {
            return Err(InterpreterError::UnexpectedInstanceType)
        }

        match func {
            FunctionType::Native(ref f) => {
                f.call_function(type_instance, values)
            },
            FunctionType::Custom(ref f) => {
                let mut context = Context::new();
                context.push_scope();
                match &f.get_instance_name() {
                    Some(name) => match type_instance {
                        Some(instance) => {
                            let var = Variable::new(instance.clone(), func.for_type().clone().unwrap());
                            context.register_variable(name.clone(), var)?;
                        },
                        None => return Err(InterpreterError::UnexpectedInstanceType)
                    },
                    None => {}
                };

                for param in f.get_parameters() {
                    let variable = Variable::new(values.remove(0), param.get_type().clone());
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

    pub fn get_count_expr(&self) -> u64 {
        *self.count_expr.borrow()
    }
}