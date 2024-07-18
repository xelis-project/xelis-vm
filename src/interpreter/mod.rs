mod context;
mod state;

use crate::{
    expressions::{Expression, Operator, Statement},
    environment::Environment,
    functions::FunctionType,
    parser::Program,
    IdentifierType,
    types::*
};
use context::Context;
pub use state::State;

use std::{
    collections::{HashMap, VecDeque},
    convert::TryInto
};

macro_rules! exec {
    ($func: ident, $a: expr, $b: expr) => {{
        let (v, overflow) = $a.$func($b);
        if overflow {
            return Err(InterpreterError::OverflowOccured)
        }
        v
    }};
}

macro_rules! add {
    ($a: expr, $b: expr) => {{
        exec!(overflowing_add, $a, $b)
    }};
}

macro_rules! sub {
    ($a: expr, $b: expr) => {{
        exec!(overflowing_sub, $a, $b)
    }};
}

macro_rules! mul {
    ($a: expr, $b: expr) => {{
        exec!(overflowing_mul, $a, $b)
    }};
}

macro_rules! div {
    ($a: expr, $b: expr) => {{
        let v = $b;
        if v == 0 {
            return Err(InterpreterError::DivByZero)
        }

        exec!(overflowing_div, $a, v)
    }};
}

macro_rules! convert {
    ($a: expr) => {{
        match $a.try_into() {
            Ok(v) => v,
            Err(_) => return Err(InterpreterError::CastNumberError)
        }
    }};
}

macro_rules! shl {
    ($a: expr, $b: expr) => {{
        exec!(overflowing_shl, $a, convert!($b))
    }};
}

macro_rules! shr {
    ($a: expr, $b: expr) => {{
        exec!(overflowing_shr, $a, convert!($b))
    }};
}

#[derive(Debug)]
pub enum InterpreterError {
    NoMatchingFunction,
    TypeNotFound(Value),
    FunctionEntry(bool, bool), // expected, got
    LimitReached,
    NotImplemented,
    NoExitCode,
    ExpectedValue,
    InvalidNativeFunctionCall,
    ExpectedPath,
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
    CastNumberError,
    RecursiveLimitReached,
    InvalidCastType(Type),
}

trait CopyRef<T> {
    fn copy_ref(&mut self) -> Option<&mut T>;
}

impl<T> CopyRef<T> for Option<&mut T> {
    fn copy_ref(&mut self) -> Option<&mut T> {
        match self {
            Some(ref mut x) => Some(x),
            None => None,
        }
    }
}

// The interpreter structure can be reused to execute multiple times the program
pub struct Interpreter<'a> {
    // Program to execute
    program: &'a Program,
    // Environment linked to execute the program
    env: &'a Environment,
    ref_structures: RefMap<'a, IdentifierType, Struct>
}

impl<'a> Interpreter<'a> {
    pub fn new(program: &'a Program, env: &'a Environment) -> Result<Self, InterpreterError> {
        let mut interpreter = Self {
            program,
            env,
            ref_structures: RefMap::new()
        };

        interpreter.ref_structures.link_maps(vec![interpreter.env.get_structures(), &interpreter.program.structures]);
        Ok(interpreter)
    }

    fn is_same_value(&self, value_type: &Type, left: &Value, right: &Value) -> Result<bool, InterpreterError> {
        Ok(match value_type {
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
                                let field_type = match self.ref_structures.get(structure) {
                                    Some(structure) => match structure.fields.get(k) {
                                        Some(field) => field,
                                        None => return Err(InterpreterError::InvalidStructValue(v.clone()))
                                    },
                                    None => return Err(InterpreterError::StructureNotFound(structure.clone()))
                                };
                                self.is_same_value(field_type, &v, &r_v)?
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
                    for i in 0..left_vec.len() {
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

    pub fn get_type_from_value(&self, value: &Value) -> Result<Type, InterpreterError> {
        match Type::from_value(value, &self.ref_structures) {
            Some(v) => Ok(v),
            None => Err(InterpreterError::TypeNotFound(value.clone()))
        }
    }

    fn get_types_from_values<'b, I: Iterator<Item = &'b Value>>(&self, values: I) -> Result<Vec<Type>, InterpreterError> {
        let mut types: Vec<Type> = Vec::new();
        for value in values {
            types.push(self.get_type_from_value(&value)?);
        }

        Ok(types)
    }

    fn get_compatible_function<'b, I: Iterator<Item = &'b Value>>(&self, name: &IdentifierType, for_type: Option<&Type>, values: I) -> Result<&FunctionType, InterpreterError> {
        let types = self.get_types_from_values(values)?;
        self.get_function(name, for_type, types.iter())
    }

    fn get_function<'b, I: Iterator<Item = &'b Type> + Clone + ExactSizeIterator>(&self, name: &IdentifierType, for_type: Option<&Type>, parameters: I) -> Result<&FunctionType, InterpreterError> {
        'funcs: for f in self.program.functions.iter().chain(self.env.get_functions()) {
            if *f.get_name() == *name && f.get_parameters_count() == parameters.len() {
                let same_type: bool = if let Some(type_a) = for_type {
                    if let Some(type_b) = f.for_type() {
                        type_a.is_compatible_with(type_b)
                    } else {
                        false
                    }
                } else {
                    for_type == f.for_type().as_ref()
                };

                if same_type {
                    let f_types = f.get_parameters_types();
                    for (f_type, param_type) in f_types.into_iter().zip(parameters.clone()) {
                        if *f_type != Type::Any && f_type != param_type {
                            continue 'funcs;
                        }
                    }
                    return Ok(f)
                }
            }
        }

        Err(InterpreterError::NoMatchingFunction)
    }

    fn get_from_path<'b>(&self, ref_value: Option<&'b mut Value>, path: &Expression, mut context: Option<&'b mut Context>, state: &mut State) -> Result<&'b mut Value, InterpreterError> {
        match path {
            Expression::ArrayCall(expr, expr_index) => {
                let index = self.execute_expression_and_expect_value(None, expr_index, context.copy_ref(), state)?.to_int()? as usize;
                let array = self.get_from_path(ref_value, expr, context, state)?;
                let values = array.as_mut_vec()?;
                let size = values.len();
                match values.get_mut(index as usize) {
                    Some(v) => Ok(v),
                    None => return Err(InterpreterError::OutOfBounds(size, index))
                }
            }
            Expression::Path(left, right) => {
                let left_value = self.get_from_path(ref_value, left, context, state)?;
                self.get_from_path(Some(left_value), right, None, state)
            },
            Expression::Variable(name) => {
                Ok(match ref_value {
                    Some(v) => {
                        match v.as_mut_map()?.get_mut(name) {
                            Some(value) => value,
                            None => return Err(InterpreterError::VariableNotFound(name.clone()))
                        }
                    },
                    None => match context {
                        Some(context) => context.get_mut_variable(name)?,
                        None => return Err(InterpreterError::ExpectedPath)
                    }
                })
            }
            _ => Err(InterpreterError::ExpectedPath)
        }
    }

    fn execute_expression_and_expect_value<'b>(&self, on_value: Option<&'b mut Value>, expr: &Expression, context: Option<&mut Context>, state: &mut State) -> Result<Value, InterpreterError> {
        match self.execute_expression(on_value, expr, context, state)? {
            Some(val) => Ok(val),
            None => Err(InterpreterError::ExpectedValue)
        }
    }

    fn execute_expression(&self, on_value: Option<&mut Value>, expr: &Expression, mut context: Option<&mut Context>, state: &mut State) -> Result<Option<Value>, InterpreterError> {
        state.increase_expressions_executed()?;
        match expr {
            Expression::FunctionCall(name, parameters) => {
                let mut values: Vec<Value> = Vec::new();
                for param in parameters {
                    values.push(self.execute_expression_and_expect_value(None, param, context.copy_ref(), state)?);
                }

                state.increase_recursive_depth()?;

                let params = self.get_types_from_values(values.iter())?;
                let res = match on_value {
                    Some(v) => {
                        let func = self.get_function(name, Some(&self.get_type_from_value(&v)?), params.iter())?;
                        self.execute_function(&func, Some(v), VecDeque::from(values), state)
                    },
                    None => {
                        let func = self.get_function(name, None, params.iter())?;
                        self.execute_function(&func, None, VecDeque::from(values), state)
                    }
                };

                state.decrease_recursive_depth();

                res
            },
            Expression::ArrayConstructor(expressions) => {
                let mut values = vec![];
                for expr in expressions {
                    let value = self.execute_expression_and_expect_value(None, &expr, context.copy_ref(), state)?;
                    values.push(value);
                }

                Ok(Some(Value::Array(values)))
            },
            Expression::StructConstructor(struct_name, expr_fields) => {
                let s = self.ref_structures.get(struct_name).ok_or_else(|| InterpreterError::StructureNotFound(struct_name.clone()))?;
                let mut fields = HashMap::new();
                for (name, expr) in expr_fields {
                    let value = self.execute_expression_and_expect_value(None, &expr, context.copy_ref(), state)?;
                    let value_type = self.get_type_from_value(&value)?;

                    let expected_type = s.fields.get(name).ok_or_else(|| InterpreterError::StructureFieldNotFound(struct_name.clone(), name.clone()))?;
                    if *expected_type != value_type {
                        return Err(InterpreterError::InvalidType(value_type))
                    }

                    fields.insert(name.clone(), value);
                }
                Ok(Some(Value::Struct(struct_name.clone(), fields)))
            },
            Expression::ArrayCall(expr, expr_index) => {
                let values = self.execute_expression_and_expect_value(on_value, &expr, context.copy_ref(), state)?.to_vec()?;
                let index = self.execute_expression_and_expect_value(None, &expr_index, context.copy_ref(), state)?.to_int()? as usize;

                Ok(match values.get(index) {
                    Some(v) => Some(v.clone()),
                    None => return Err(InterpreterError::OutOfBounds(values.len(), index))
                })
            },
            Expression::IsNot(expr) => {
                let val = self.execute_expression_and_expect_value(None, &expr, context, state)?.to_bool()?;
                Ok(Some(Value::Boolean(!val)))
            }
            Expression::SubExpression(expr) => self.execute_expression(None, expr, context, state),
            Expression::Ternary(condition, left, right) => {
                if self.execute_expression_and_expect_value(None, &condition, context.copy_ref(), state)?.to_bool()? {
                    Ok(Some(self.execute_expression_and_expect_value(None, &left, context.copy_ref(), state)?))
                } else {
                    Ok(Some(self.execute_expression_and_expect_value(None, &right, context.copy_ref(), state)?))
                }
            }
            Expression::Value(v) => Ok(Some(v.clone())),
            Expression::Variable(var) =>  match on_value {
                Some(instance) => {
                    match instance.as_map()?.get(var) {
                        Some(value) => Ok(Some(value.clone())),
                        None => return Err(InterpreterError::VariableNotFound(var.clone()))
                    }
                },
                None => match context {
                    Some(context) => match context.get_variable(var) {
                        Ok(v) => Ok(Some(v.clone())),
                        Err(_) => Ok(match state.get_constant_value(var) {
                            Some(v) => Some(v.clone()),
                            None => return Err(InterpreterError::VariableNotFound(var.clone()))
                        })
                    },
                    None => return Err(InterpreterError::ExpectedPath)
                }
            },
            Expression::Operator(op, expr_left, expr_right) => {
                if op.is_assignation() {
                    let value = self.execute_expression_and_expect_value(None, expr_right, context.copy_ref(), state)?;
                    let path_value = self.get_from_path(None, expr_left, context.copy_ref(), state)?;
                    let path_type = self.get_type_from_value(&path_value)?;
                    let value_type = self.get_type_from_value(&value)?;

                    if (!path_value.is_number() || !value.is_number() || path_type != value_type) && op.is_number_operator() && !(*op == Operator::AssignPlus && path_type == Type::String) {
                        return Err(InterpreterError::OperationNotNumberType)
                    }

                    match op {
                        Operator::Assign => {
                            *path_value = value;
                        },
                        Operator::AssignPlus => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(add!(path_value.as_byte()?, value.to_byte()?)),
                                Type::Short => Value::Short(add!(path_value.as_short()?, value.to_short()?)),
                                Type::Int => Value::Int(add!(path_value.as_int()?, value.to_int()?)),
                                Type::Long => Value::Long(add!(path_value.as_long()?,  value.to_long()?)),
                                Type::String => Value::String(format!("{}{}", path_value.as_string()?, value.to_string()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignMinus => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(sub!(path_value.as_byte()?, value.to_byte()?)),
                                Type::Short => Value::Short(sub!(path_value.as_short()?, value.to_short()?)),
                                Type::Int => Value::Int(sub!(path_value.as_int()?, value.to_int()?)),
                                Type::Long => Value::Long(sub!(path_value.as_long()?, value.to_long()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignDivide => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(div!(path_value.as_byte()?, value.to_byte()?)),
                                Type::Short => Value::Short(div!(path_value.as_short()?, value.to_short()?)),
                                Type::Int => Value::Int(div!(path_value.as_int()?, value.to_int()?)),
                                Type::Long => Value::Long(div!(path_value.as_long()?, value.to_long()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignMultiply => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(mul!(path_value.as_byte()?, value.to_byte()?)),
                                Type::Short => Value::Short(mul!(path_value.as_short()?, value.to_short()?)),
                                Type::Int => Value::Int(mul!(path_value.as_int()?, value.to_int()?)),
                                Type::Long => Value::Long(mul!(path_value.as_long()?, value.to_long()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        }, 
                        Operator::AssignModulo => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(path_value.as_byte()? % value.to_byte()?),
                                Type::Short => Value::Short(path_value.as_short()? % value.to_short()?),
                                Type::Int => Value::Int(path_value.as_int()? % value.to_int()?),
                                Type::Long => Value::Long(path_value.as_long()? % value.to_long()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignBitwiseXor => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(path_value.as_byte()? ^ value.to_byte()?),
                                Type::Short => Value::Short(path_value.as_short()? ^ value.to_short()?),
                                Type::Int => Value::Int(path_value.as_int()? ^ value.to_int()?),
                                Type::Long => Value::Long(path_value.as_long()? ^ value.to_long()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignBitwiseAnd => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(path_value.as_byte()? & value.to_byte()?),
                                Type::Short => Value::Short(path_value.as_short()? & value.to_short()?),
                                Type::Int => Value::Int(path_value.as_int()? & value.to_int()?),
                                Type::Long => Value::Long(path_value.as_long()? & value.to_long()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignBitwiseOr => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(path_value.as_byte()? | value.to_byte()?),
                                Type::Short => Value::Short(path_value.as_short()? | value.to_short()?),
                                Type::Int => Value::Int(path_value.as_int()? | value.to_int()?),
                                Type::Long => Value::Long(path_value.as_long()? | value.to_long()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignBitwiseLeft => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(shl!(path_value.as_byte()?, value.to_byte()?)),
                                Type::Short => Value::Short(shl!(path_value.as_short()?, value.to_short()?)),
                                Type::Int => Value::Int(shl!(path_value.as_int()?, value.to_int()?)),
                                Type::Long => Value::Long(shl!(path_value.as_long()?, value.to_long()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignBitwiseRight => {
                            *path_value = match path_type {
                                Type::Byte => Value::Byte(shr!(path_value.as_byte()?, value.to_byte()?)),
                                Type::Short => Value::Short(shr!(path_value.as_short()?, value.to_short()?)),
                                Type::Int => Value::Int(shr!(path_value.as_int()?, value.to_int()?)),
                                Type::Long => Value::Long(shr!(path_value.as_long()?, value.to_long()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        _ => return Err(InterpreterError::NotImplemented) 
                    };
                    Ok(None)
                } else {
                    let left = self.execute_expression_and_expect_value(None, &expr_left, context.copy_ref(), state)?;
                    let left_type = self.get_type_from_value(&left)?;

                    if op.is_and_or_or() {
                        match op {
                            Operator::And => Ok(Some(Value::Boolean({
                                let left = left.to_bool()?;
                                if !left {
                                    false
                                } else {
                                    let right = self.execute_expression_and_expect_value(None, &expr_right, context, state)?;
                                    right.to_bool()?
                                }
                            }))),
                            Operator::Or => Ok(Some(Value::Boolean({
                                let left = left.to_bool()?;
                                if !left {
                                    let right = self.execute_expression_and_expect_value(None, &expr_right, context, state)?;
                                    right.to_bool()?
                                } else {
                                    true
                                }
                            }))),
                            _ => return Err(InterpreterError::UnexpectedOperator)
                        }
                    } else {
                        let right = self.execute_expression_and_expect_value(None, &expr_right, context.copy_ref(), state)?;
                        let right_type = self.get_type_from_value(&right)?;
                        if (!left.is_number() || !right.is_number() || right_type != left_type) && op.is_number_operator() {
                            return Err(InterpreterError::OperationNotNumberType)
                        }
        
                        match op {
                            Operator::Equals => Ok(Some(Value::Boolean(left_type == right_type && self.is_same_value(&left_type, &left, &right)?))),
                            Operator::NotEquals => Ok(Some(Value::Boolean(left_type != right_type || !self.is_same_value(&left_type, &left, &right)?))),
                            Operator::Plus => {
                                if left_type == Type::String || right_type == Type::String {
                                    Ok(Some(Value::String(format!("{}{}", left, right))))
                                } else {
                                    Ok(Some(match left_type {
                                        Type::Byte => Value::Byte(add!(left.to_byte()?, right.to_byte()?)),
                                        Type::Short => Value::Short(add!(left.to_short()?, right.to_short()?)),
                                        Type::Int => Value::Int(add!(left.to_int()?, right.to_int()?)),
                                        Type::Long => Value::Long(add!(left.to_long()?, right.to_long()?)),
                                        _ => return Err(InterpreterError::OperationNotNumberType)
                                    }))
                                }
                            },
                            Operator::Minus => Ok(Some(match left_type {
                                Type::Byte => Value::Byte(sub!(left.to_byte()?, right.to_byte()?)),
                                Type::Short => Value::Short(sub!(left.to_short()?, right.to_short()?)),
                                Type::Int => Value::Int(sub!(left.to_int()?, right.to_int()?)),
                                Type::Long => Value::Long(sub!(left.to_long()?, right.to_long()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::Divide => Ok(Some(match left_type {
                                Type::Byte => Value::Byte(div!(left.to_byte()?, right.to_byte()?)),
                                Type::Short => Value::Short(div!(left.to_short()?, right.to_short()?)),
                                Type::Int => Value::Int(div!(left.to_int()?, right.to_int()?)),
                                Type::Long => Value::Long(div!(left.to_long()?, right.to_long()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::Multiply => Ok(Some(match left_type {
                                Type::Byte => Value::Byte(mul!(left.to_byte()?, right.to_byte()?)),
                                Type::Short => Value::Short(mul!(left.to_short()?, right.to_short()?)),
                                Type::Int => Value::Int(mul!(left.to_int()?, right.to_int()?)),
                                Type::Long => Value::Long(mul!(left.to_long()?, right.to_long()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::Modulo => Ok(Some(match left_type {
                                Type::Byte => Value::Byte(left.to_byte()? % right.to_byte()?),
                                Type::Short => Value::Short(left.to_short()? % right.to_short()?),
                                Type::Int => Value::Int(left.to_int()? % right.to_int()?),
                                Type::Long => Value::Long(left.to_long()? % right.to_long()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::BitwiseXor => Ok(Some(match left_type {
                                Type::Byte => Value::Byte(left.to_byte()? ^ right.to_byte()?),
                                Type::Short => Value::Short(left.to_short()? ^ right.to_short()?),
                                Type::Int => Value::Int(left.to_int()? ^ right.to_int()?),
                                Type::Long => Value::Long(left.to_long()? ^ right.to_long()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::BitwiseAnd => Ok(Some(match left_type {
                                Type::Byte => Value::Byte(left.to_byte()? & right.to_byte()?),
                                Type::Short => Value::Short(left.to_short()? & right.to_short()?),
                                Type::Int => Value::Int(left.to_int()? & right.to_int()?),
                                Type::Long => Value::Long(left.to_long()? & right.to_long()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::BitwiseOr => Ok(Some(match left_type {
                                Type::Byte => Value::Byte(left.to_byte()? | right.to_byte()?),
                                Type::Short => Value::Short(left.to_short()? | right.to_short()?),
                                Type::Int => Value::Int(left.to_int()? | right.to_int()?),
                                Type::Long => Value::Long(left.to_long()? | right.to_long()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::BitwiseLeft => Ok(Some(match left_type {
                                Type::Byte => Value::Byte(shl!(left.to_byte()?, right.to_byte()?)),
                                Type::Short => Value::Short(shl!(left.to_short()?, right.to_short()?)),
                                Type::Int => Value::Int(shl!(left.to_int()?, right.to_int()?)),
                                Type::Long => Value::Long(shl!(left.to_long()?, right.to_long()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::BitwiseRight => Ok(Some(match left_type {
                                Type::Byte => Value::Byte(shr!(left.to_byte()?, right.to_byte()?)),
                                Type::Short => Value::Short(shr!(left.to_short()?, right.to_short()?)),
                                Type::Int => Value::Int(shr!(left.to_int()?, right.to_int()?)),
                                Type::Long => Value::Long(shr!(left.to_long()?, right.to_long()?)),
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
                            _ => return Err(InterpreterError::UnexpectedOperator)
                        }
                    }
                }
            },
            Expression::Path(left, right) => {
                let value = self.get_from_path(on_value, left, context, state)?;
                self.execute_expression(Some(value), right, None, state)
            },
            Expression::Cast(expr, cast_type) => {
                let value = self.execute_expression_and_expect_value(on_value, expr, context, state)?;
                match cast_type {
                    Type::Byte => Ok(Some(Value::Byte(value.cast_to_byte()?))),
                    Type::Short => Ok(Some(Value::Short(value.cast_to_short()?))),
                    Type::Int => Ok(Some(Value::Int(value.cast_to_int()?))),
                    Type::Long => Ok(Some(Value::Long(value.cast_to_long()?))),
                    Type::String => Ok(Some(Value::String(value.cast_to_string()?))),
                    _ => Err(InterpreterError::InvalidType(cast_type.clone()))
                }
            }
        }
    }

    fn execute_statements(&self, statements: &Vec<Statement>, context: &mut Context, state: &mut State) -> Result<Option<Value>, InterpreterError> {
        let mut accept_else = false;
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
                    let value = self.execute_expression_and_expect_value(None, &var.value, Some(context), state)?;
                    context.register_variable(var.id.clone(), value)?;
                },
                Statement::If(condition, statements) => {
                    if self.execute_expression_and_expect_value(None, &condition, Some(context), state)?.to_bool()? {
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
                    } else {
                        accept_else = true;
                    }
                },
                Statement::ElseIf(condition, statements) => if accept_else {
                    if self.execute_expression_and_expect_value(None, &condition, Some(context), state)?.to_bool()? {
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
                    } else {
                        accept_else = true;
                    }
                },
                Statement::Else(statements) => if accept_else {
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
                }
                Statement::For(var, condition, increment, statements) => {
                    context.begin_scope();
                    let value = self.execute_expression_and_expect_value(None, &var.value, Some(context), state)?;
                    context.register_variable(var.id.clone(), value)?;
                    loop {
                        if !self.execute_expression_and_expect_value(None, condition, Some(context), state)?.to_bool()? {
                            break;
                        }

                        // assign operator don't return values
                        if self.execute_expression(None, increment, Some(context), state)?.is_some() {
                            return Err(InterpreterError::ExpectedAssignOperator);
                        }

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
                    }
                    context.end_scope()?;
                },
                Statement::ForEach(var, expr, statements) => {
                    let values = self.execute_expression_and_expect_value(None, expr, Some(context), state)?.to_vec()?;
                    if !values.is_empty() {
                        context.begin_scope();
                        context.register_variable(var.clone(), Value::Null)?;
                        for val in values {
                            context.set_variable_value(var, val)?;
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
                        }
                        context.end_scope()?;
                    }
                },
                Statement::While(condition, statements) => {
                    context.begin_scope();
                    while self.execute_expression_and_expect_value(None, &condition, Some(context), state)?.to_bool()? {
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
                    }
                    context.end_scope()?;
                },
                Statement::Return(opt) => {
                    return Ok(match opt {
                        Some(v) => Some(self.execute_expression_and_expect_value(None, &v, Some(context), state)?),
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
                    self.execute_expression(None, &expr, Some(context), state)?;
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

    // Execute the selected function
    fn execute_function(&self, func: &FunctionType, type_instance: Option<&mut Value>, mut values: VecDeque<Value>, state: &mut State) -> Result<Option<Value>, InterpreterError> {
        match func {
            FunctionType::Native(ref f) => f.call_function(type_instance, values, state),
            FunctionType::Custom(ref f) => {
                let mut context = Context::new();
                context.begin_scope();
                match &f.get_instance_name() {
                    Some(name) => match type_instance {
                        Some(instance) => {
                            if func.for_type().is_none() {
                                return Err(InterpreterError::ExpectedInstanceType)
                            }

                            context.register_variable(name.clone(), instance.clone())?;
                        },
                        None => return Err(InterpreterError::UnexpectedInstanceType)
                    },
                    None => {}
                };

                for param in f.get_parameters().iter() {
                    let value = values.pop_front().ok_or(InterpreterError::MissingValueForFunctionCall)?;
                    context.register_variable(param.get_name().clone(), value)?;
                }
                let result = self.execute_statements(f.get_statements(), &mut context, state);
                context.end_scope()?;

                result
            }
        }
    }

    // Execute the program by calling an available entry function
    pub fn call_entry_function<I: Into<VecDeque<Value>>>(&self, function_name: &IdentifierType, parameters: I, state: &mut State) -> Result<u64, InterpreterError> {
        let params = parameters.into();
        let func = self.get_compatible_function(function_name, None, params.iter())?;

        // initialize constants
        if !self.program.constants.is_empty() && !state.has_cache_initilized() {
            let mut context = Context::new();
            context.begin_scope();
            for c in &self.program.constants {
                let value = self.execute_expression_and_expect_value(None, &c.value, Some(&mut context), state)?;
                context.register_variable(c.id.clone(), value)?;
            }

            state.set_constants_cache(context.remove_scope()?);
        }

        // only function marked as entry can be called from external
        if !func.is_entry() {
            return Err(InterpreterError::FunctionEntry(true, false))
        }

        match self.execute_function(func, None, params, state)? {
            Some(val) => Ok(val.to_int()?),
            None => return Err(InterpreterError::NoExitCode)
        }
    }
}