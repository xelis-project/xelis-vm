mod context;
mod state;

use crate::{
    environment::Environment,
    expressions::{Expression, Operator, Parameter, Statement},
    functions::FunctionType,
    parser::Program,
    types::*,
    values::{SharableValue, Value, ValueVariant},
    IdentifierType
};
use context::Context;
pub use state::State;

use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    convert::TryInto,
    rc::Rc
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
    OptionalIsNull,
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
            Type::U8 => *left.as_byte()? == *right.as_byte()?,
            Type::U16 => *left.as_u16()? == *right.as_u16()?,
            Type::U64 => *left.as_u64()? == *right.as_u64()?,
            Type::U128 => *left.as_u128()? == *right.as_u128()?,
            Type::Bool => *left.as_bool()? == *right.as_bool()?,
            Type::String => *left.as_string()? == *right.as_string()?,
            Type::Optional(sub_type) => {
                let opt_left = left.as_optional(&sub_type)?;
                let opt_right = right.as_optional(&sub_type)?;
                if let (Some(left), Some(right)) = (opt_left, opt_right) {
                    self.is_same_value(sub_type, left, right)?
                } else {
                    opt_left.is_some() == opt_right.is_some()
                }
            },
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
                                        None => return Err(InterpreterError::InvalidStructValue(v.clone_value()))
                                    },
                                    None => return Err(InterpreterError::StructureNotFound(structure.clone()))
                                };
                                self.is_same_value(field_type, &v.get_value(), &r_v.get_value())?
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
                    for (left, right) in left_vec.iter().zip(right_vec.iter()) {
                        if !self.is_same_value(sub_type, &left.get_value(), &right.get_value())? {
                            equal = false;
                            break;
                        }
                    }
                    equal
                } else {
                    false
                }
            },
            _ => return Err(InterpreterError::InvalidType(value_type.clone()))
        })
    }

    pub fn get_type_from_value(&self, value: &Value) -> Result<Type, InterpreterError> {
        match Type::from_value(value, &self.ref_structures) {
            Some(v) => Ok(v),
            None => Err(InterpreterError::TypeNotFound(value.clone()))
        }
    }

    fn get_function(&self, name: &IdentifierType) -> Result<&FunctionType, InterpreterError> {
        match self.env.get_functions().get(name) {
            Some(func) => Ok(func),
            None => self.program.functions.get(name).ok_or_else(|| InterpreterError::NoMatchingFunction)
        }
    }

    fn get_from_path<'b>(&self, ref_value: Option<SharableValue>, path: &Expression, mut context: Option<&mut Context>, state: &mut State) -> Result<SharableValue, InterpreterError> {
        match path {
            Expression::ArrayCall(expr, expr_index) => {
                let index = self.execute_expression_and_expect_value(None, expr_index, context.copy_ref(), state)?.to_u64()? as usize;
                let array = self.get_from_path(ref_value, expr, context, state)?;
                let mut a = array.borrow_mut();
                let values = a.as_mut_vec()?;
                let size = values.len();
                match values.get_mut(index as usize) {
                    Some(v) => Ok(v.get_sharable()),
                    None => return Err(InterpreterError::OutOfBounds(size, index))
                }
            },
            Expression::Path(left, right) => {
                let left_value = self.get_from_path(ref_value, left, context.copy_ref(), state)?;
                self.get_from_path(Some(left_value), right, context, state)
            },
            Expression::Variable(name) => {
                Ok(match ref_value {
                    Some(v) => {
                        match v.borrow_mut().as_mut_map()?.get_mut(name) {
                            Some(value) => value.get_sharable(),
                            None => return Err(InterpreterError::VariableNotFound(name.clone()))
                        }
                    },
                    None => match context {
                        Some(context) => context.get_sharable_value(name)?,
                        None => return Err(InterpreterError::ExpectedPath)
                    },
                })
            }
            _ => Err(InterpreterError::ExpectedPath)
        }
    }

    fn execute_expression_and_expect_value(&self, on_value: Option<SharableValue>, expr: &Expression, context: Option<&mut Context>, state: &mut State) -> Result<Value, InterpreterError> {
        match self.execute_expression(on_value, expr, context, state)? {
            Some(val) => Ok(val),
            None => Err(InterpreterError::ExpectedValue)
        }
    }

    fn execute_expression(&self, on_value: Option<SharableValue>, expr: &Expression, mut context: Option<&mut Context>, state: &mut State) -> Result<Option<Value>, InterpreterError> {
        state.increase_expressions_executed()?;
        match expr {
            Expression::FunctionCall(name, parameters) => {
                let mut values: Vec<Value> = Vec::with_capacity(parameters.len());
                for param in parameters {
                    values.push(self.execute_expression_and_expect_value(None, param, context.copy_ref(), state)?);
                }

                state.increase_recursive_depth()?;

                let res = match on_value {
                    Some(v) => {
                        let func = self.get_function(name)?;
                        self.execute_function(&func, Some(&mut v.borrow_mut()), VecDeque::from(values), state)
                    },
                    None => {
                        let func = self.get_function(name)?;
                        self.execute_function(&func, None, VecDeque::from(values), state)
                    }
                };

                state.decrease_recursive_depth();

                res
            },
            Expression::ArrayConstructor(expressions) => {
                let mut values = Vec::with_capacity(expressions.len());
                for expr in expressions {
                    let value = self.execute_expression_and_expect_value(None, &expr, context.copy_ref(), state)?;
                    values.push(ValueVariant::Value(value));
                }

                Ok(Some(Value::Array(values)))
            },
            Expression::StructConstructor(struct_name, expr_fields) => {
                let s = self.ref_structures.get(struct_name).ok_or_else(|| InterpreterError::StructureNotFound(struct_name.clone()))?;
                let mut fields = HashMap::with_capacity(expr_fields.len());
                for (name, expr) in expr_fields {
                    let value = self.execute_expression_and_expect_value(None, &expr, context.copy_ref(), state)?;
                    let value_type = self.get_type_from_value(&value)?;

                    let expected_type = s.fields.get(name).ok_or_else(|| InterpreterError::StructureFieldNotFound(struct_name.clone(), name.clone()))?;
                    if *expected_type != value_type {
                        return Err(InterpreterError::InvalidType(value_type))
                    }

                    fields.insert(name.clone(), ValueVariant::Value(value));
                }
                Ok(Some(Value::Struct(struct_name.clone(), fields)))
            },
            Expression::ArrayCall(expr, expr_index) => {
                let values = self.execute_expression_and_expect_value(on_value, &expr, context.copy_ref(), state)?.to_vec()?;
                let index = self.execute_expression_and_expect_value(None, &expr_index, context.copy_ref(), state)?.to_u64()? as usize;

                Ok(match values.get(index) {
                    Some(v) => Some(v.clone_value()),
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
                    match instance.borrow_mut().as_map()?.get(var) {
                        Some(value) => Ok(Some(value.clone_value())),
                        None => return Err(InterpreterError::VariableNotFound(var.clone()))
                    }
                },
                None => match context {
                    Some(context) => match context.get_variable(var) {
                        Ok(v) => Ok(Some(v.clone_value())),
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
                    let path = self.get_from_path(None, expr_left, context.copy_ref(), state)?;
                    let mut path_value = path.borrow_mut();
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
                                Type::U8 => Value::U8(add!(path_value.as_byte()?, value.to_u8()?)),
                                Type::U16 => Value::U16(add!(path_value.as_u16()?, value.to_u16()?)),
                                Type::U32 => Value::U32(add!(path_value.as_u32()?, value.to_u32()?)),
                                Type::U64 => Value::U64(add!(path_value.as_u64()?, value.to_u64()?)),
                                Type::U128 => Value::U128(add!(path_value.as_u128()?,  value.to_u128()?)),
                                Type::String => Value::String(format!("{}{}", path_value.as_string()?, value.to_string()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignMinus => {
                            *path_value = match path_type {
                                Type::U8 => Value::U8(sub!(path_value.as_byte()?, value.to_u8()?)),
                                Type::U16 => Value::U16(sub!(path_value.as_u16()?, value.to_u16()?)),
                                Type::U32 => Value::U32(sub!(path_value.as_u32()?, value.to_u32()?)),
                                Type::U64 => Value::U64(sub!(path_value.as_u64()?, value.to_u64()?)),
                                Type::U128 => Value::U128(sub!(path_value.as_u128()?, value.to_u128()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignDivide => {
                            *path_value = match path_type {
                                Type::U8 => Value::U8(div!(path_value.as_byte()?, value.to_u8()?)),
                                Type::U16 => Value::U16(div!(path_value.as_u16()?, value.to_u16()?)),
                                Type::U32 => Value::U32(div!(path_value.as_u32()?, value.to_u32()?)),
                                Type::U64 => Value::U64(div!(path_value.as_u64()?, value.to_u64()?)),
                                Type::U128 => Value::U128(div!(path_value.as_u128()?, value.to_u128()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignMultiply => {
                            *path_value = match path_type {
                                Type::U8 => Value::U8(mul!(path_value.as_byte()?, value.to_u8()?)),
                                Type::U16 => Value::U16(mul!(path_value.as_u16()?, value.to_u16()?)),
                                Type::U32 => Value::U32(mul!(path_value.as_u32()?, value.to_u32()?)),
                                Type::U64 => Value::U64(mul!(path_value.as_u64()?, value.to_u64()?)),
                                Type::U128 => Value::U128(mul!(path_value.as_u128()?, value.to_u128()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        }, 
                        Operator::AssignModulo => {
                            *path_value = match path_type {
                                Type::U8 => Value::U8(path_value.as_byte()? % value.to_u8()?),
                                Type::U16 => Value::U16(path_value.as_u16()? % value.to_u16()?),
                                Type::U32 => Value::U32(path_value.as_u32()? % value.to_u32()?),
                                Type::U64 => Value::U64(path_value.as_u64()? % value.to_u64()?),
                                Type::U128 => Value::U128(path_value.as_u128()? % value.to_u128()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignBitwiseXor => {
                            *path_value = match path_type {
                                Type::U8 => Value::U8(path_value.as_byte()? ^ value.to_u8()?),
                                Type::U16 => Value::U16(path_value.as_u16()? ^ value.to_u16()?),
                                Type::U32 => Value::U32(path_value.as_u32()? ^ value.to_u32()?),
                                Type::U64 => Value::U64(path_value.as_u64()? ^ value.to_u64()?),
                                Type::U128 => Value::U128(path_value.as_u128()? ^ value.to_u128()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignBitwiseAnd => {
                            *path_value = match path_type {
                                Type::U8 => Value::U8(path_value.as_byte()? & value.to_u8()?),
                                Type::U16 => Value::U16(path_value.as_u16()? & value.to_u16()?),
                                Type::U32 => Value::U32(path_value.as_u32()? & value.to_u32()?),
                                Type::U64 => Value::U64(path_value.as_u64()? & value.to_u64()?),
                                Type::U128 => Value::U128(path_value.as_u128()? & value.to_u128()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignBitwiseOr => {
                            *path_value = match path_type {
                                Type::U8 => Value::U8(path_value.as_byte()? | value.to_u8()?),
                                Type::U16 => Value::U16(path_value.as_u16()? | value.to_u16()?),
                                Type::U32 => Value::U32(path_value.as_u32()? | value.to_u32()?),
                                Type::U64 => Value::U64(path_value.as_u64()? | value.to_u64()?),
                                Type::U128 => Value::U128(path_value.as_u128()? | value.to_u128()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignBitwiseLeft => {
                            *path_value = match path_type {
                                Type::U8 => Value::U8(shl!(path_value.as_byte()?, value.to_u8()?)),
                                Type::U16 => Value::U16(shl!(path_value.as_u16()?, value.to_u16()?)),
                                Type::U32 => Value::U32(shl!(path_value.as_u32()?, value.to_u32()?)),
                                Type::U64 => Value::U64(shl!(path_value.as_u64()?, value.to_u64()?)),
                                Type::U128 => Value::U128(shl!(path_value.as_u128()?, value.to_u128()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            };
                        },
                        Operator::AssignBitwiseRight => {
                            *path_value = match path_type {
                                Type::U8 => Value::U8(shr!(path_value.as_byte()?, value.to_u8()?)),
                                Type::U16 => Value::U16(shr!(path_value.as_u16()?, value.to_u16()?)),
                                Type::U32 => Value::U32(shr!(path_value.as_u32()?, value.to_u32()?)),
                                Type::U64 => Value::U64(shr!(path_value.as_u64()?, value.to_u64()?)),
                                Type::U128 => Value::U128(shr!(path_value.as_u128()?, value.to_u128()?)),
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
                                        Type::U8 => Value::U8(add!(left.to_u8()?, right.to_u8()?)),
                                        Type::U16 => Value::U16(add!(left.to_u16()?, right.to_u16()?)),
                                        Type::U32 => Value::U32(add!(left.to_u32()?, right.to_u32()?)),
                                        Type::U64 => Value::U64(add!(left.to_u64()?, right.to_u64()?)),
                                        Type::U128 => Value::U128(add!(left.to_u128()?, right.to_u128()?)),
                                        _ => return Err(InterpreterError::OperationNotNumberType)
                                    }))
                                }
                            },
                            Operator::Minus => Ok(Some(match left_type {
                                Type::U8 => Value::U8(sub!(left.to_u8()?, right.to_u8()?)),
                                Type::U16 => Value::U16(sub!(left.to_u16()?, right.to_u16()?)),
                                Type::U32 => Value::U32(sub!(left.to_u32()?, right.to_u32()?)),
                                Type::U64 => Value::U64(sub!(left.to_u64()?, right.to_u64()?)),
                                Type::U128 => Value::U128(sub!(left.to_u128()?, right.to_u128()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::Divide => Ok(Some(match left_type {
                                Type::U8 => Value::U8(div!(left.to_u8()?, right.to_u8()?)),
                                Type::U16 => Value::U16(div!(left.to_u16()?, right.to_u16()?)),
                                Type::U32 => Value::U32(div!(left.to_u32()?, right.to_u32()?)),
                                Type::U64 => Value::U64(div!(left.to_u64()?, right.to_u64()?)),
                                Type::U128 => Value::U128(div!(left.to_u128()?, right.to_u128()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::Multiply => Ok(Some(match left_type {
                                Type::U8 => Value::U8(mul!(left.to_u8()?, right.to_u8()?)),
                                Type::U16 => Value::U16(mul!(left.to_u16()?, right.to_u16()?)),
                                Type::U32 => Value::U32(mul!(left.to_u32()?, right.to_u32()?)),
                                Type::U64 => Value::U64(mul!(left.to_u64()?, right.to_u64()?)),
                                Type::U128 => Value::U128(mul!(left.to_u128()?, right.to_u128()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::Modulo => Ok(Some(match left_type {
                                Type::U8 => Value::U8(left.to_u8()? % right.to_u8()?),
                                Type::U16 => Value::U16(left.to_u16()? % right.to_u16()?),
                                Type::U32 => Value::U32(left.to_u32()? % right.to_u32()?),
                                Type::U64 => Value::U64(left.to_u64()? % right.to_u64()?),
                                Type::U128 => Value::U128(left.to_u128()? % right.to_u128()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::BitwiseXor => Ok(Some(match left_type {
                                Type::U8 => Value::U8(left.to_u8()? ^ right.to_u8()?),
                                Type::U16 => Value::U16(left.to_u16()? ^ right.to_u16()?),
                                Type::U32 => Value::U32(left.to_u32()? ^ right.to_u32()?),
                                Type::U64 => Value::U64(left.to_u64()? ^ right.to_u64()?),
                                Type::U128 => Value::U128(left.to_u128()? ^ right.to_u128()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::BitwiseAnd => Ok(Some(match left_type {
                                Type::U8 => Value::U8(left.to_u8()? & right.to_u8()?),
                                Type::U16 => Value::U16(left.to_u16()? & right.to_u16()?),
                                Type::U32 => Value::U32(left.to_u32()? & right.to_u32()?),
                                Type::U64 => Value::U64(left.to_u64()? & right.to_u64()?),
                                Type::U128 => Value::U128(left.to_u128()? & right.to_u128()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::BitwiseOr => Ok(Some(match left_type {
                                Type::U8 => Value::U8(left.to_u8()? | right.to_u8()?),
                                Type::U16 => Value::U16(left.to_u16()? | right.to_u16()?),
                                Type::U32 => Value::U32(left.to_u32()? | right.to_u32()?),
                                Type::U64 => Value::U64(left.to_u64()? | right.to_u64()?),
                                Type::U128 => Value::U128(left.to_u128()? | right.to_u128()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::BitwiseLeft => Ok(Some(match left_type {
                                Type::U8 => Value::U8(shl!(left.to_u8()?, right.to_u8()?)),
                                Type::U16 => Value::U16(shl!(left.to_u16()?, right.to_u16()?)),
                                Type::U32 => Value::U32(shl!(left.to_u32()?, right.to_u32()?)),
                                Type::U64 => Value::U64(shl!(left.to_u64()?, right.to_u64()?)),
                                Type::U128 => Value::U128(shl!(left.to_u128()?, right.to_u128()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::BitwiseRight => Ok(Some(match left_type {
                                Type::U8 => Value::U8(shr!(left.to_u8()?, right.to_u8()?)),
                                Type::U16 => Value::U16(shr!(left.to_u16()?, right.to_u16()?)),
                                Type::U32 => Value::U32(shr!(left.to_u32()?, right.to_u32()?)),
                                Type::U64 => Value::U64(shr!(left.to_u64()?, right.to_u64()?)),
                                Type::U128 => Value::U128(shr!(left.to_u128()?, right.to_u128()?)),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::GreaterOrEqual => Ok(Some(match left_type {
                                Type::U8 => Value::Boolean(left.to_u8()? >= right.to_u8()?),
                                Type::U16 => Value::Boolean(left.to_u16()? >= right.to_u16()?),
                                Type::U32 => Value::Boolean(left.to_u32()? >= right.to_u32()?),
                                Type::U64 => Value::Boolean(left.to_u64()? >= right.to_u64()?),
                                Type::U128 => Value::Boolean(left.to_u128()? >= right.to_u128()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::GreaterThan => Ok(Some(match left_type {
                                Type::U8 => Value::Boolean(left.to_u8()? > right.to_u8()?),
                                Type::U16 => Value::Boolean(left.to_u16()? > right.to_u16()?),
                                Type::U32 => Value::Boolean(left.to_u32()? > right.to_u32()?),
                                Type::U64 => Value::Boolean(left.to_u64()? > right.to_u64()?),
                                Type::U128 => Value::Boolean(left.to_u128()? > right.to_u128()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::LessOrEqual => Ok(Some(match left_type {
                                Type::U8 => Value::Boolean(left.to_u8()? <= right.to_u8()?),
                                Type::U16 => Value::Boolean(left.to_u16()? <= right.to_u16()?),
                                Type::U32 => Value::Boolean(left.to_u32()? <= right.to_u32()?),
                                Type::U64 => Value::Boolean(left.to_u64()? <= right.to_u64()?),
                                Type::U128 => Value::Boolean(left.to_u128()? <= right.to_u128()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            Operator::LessThan => Ok(Some(match left_type {
                                Type::U8 => Value::Boolean(left.to_u8()? < right.to_u8()?),
                                Type::U16 => Value::Boolean(left.to_u16()? < right.to_u16()?),
                                Type::U32 => Value::Boolean(left.to_u32()? < right.to_u32()?),
                                Type::U64 => Value::Boolean(left.to_u64()? < right.to_u64()?),
                                Type::U128 => Value::Boolean(left.to_u128()? < right.to_u128()?),
                                _ => return Err(InterpreterError::OperationNotNumberType)
                            })),
                            _ => return Err(InterpreterError::UnexpectedOperator)
                        }
                    }
                }
            },
            Expression::Path(left, right) => {
                let path = self.get_from_path(on_value, left, context.copy_ref(), state)?;
                self.execute_expression(Some(path), right, context, state)
            },
            Expression::Cast(expr, cast_type) => {
                let value = self.execute_expression_and_expect_value(on_value, expr, context, state)?;
                match cast_type {
                    Type::U8 => Ok(Some(Value::U8(value.cast_to_u8()?))),
                    Type::U16 => Ok(Some(Value::U16(value.cast_to_u16()?))),
                    Type::U32 => Ok(Some(Value::U32(value.cast_to_u32()?))),
                    Type::U64 => Ok(Some(Value::U64(value.cast_to_u64()?))),
                    Type::U128 => Ok(Some(Value::U128(value.cast_to_u128()?))),
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

    fn execute_function_internal(&self, type_instance: Option<(&mut Value, IdentifierType)>, parameters: &Vec<Parameter>, values: VecDeque<Value>, statements: &Vec<Statement>, state: &mut State) -> Result<Option<Value>, InterpreterError> {
        let mut context = Context::new();
        context.begin_scope();
        if let Some((instance, instance_name)) = type_instance.as_ref() {
            // TODO no clone, otherwise our changes will not be reflected
            let val = Rc::new(RefCell::new((*instance).clone()));
            context.register_variable(instance_name.clone(), val)?;
        }

        for (param, value) in parameters.iter().zip(values.into_iter()) {
            context.register_variable(param.get_name().clone(), value)?;
        }

        let result = self.execute_statements(statements, &mut context, state);

        let mut scope = context.remove_scope()?;
        if let Some((instance, name)) = type_instance {
            let expected = scope.remove(&name)
                .ok_or_else(|| InterpreterError::VariableNotFound(name.clone()))?;

            *instance = expected.into_value();
        }

        result
    }

    // Execute the selected function
    fn execute_function(&self, func: &FunctionType, type_instance: Option<&mut Value>, values: VecDeque<Value>, state: &mut State) -> Result<Option<Value>, InterpreterError> {
        match func {
            FunctionType::Native(ref f) => f.call_function(type_instance, values, state),
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

    // Execute the program by calling an available entry function
    pub fn call_entry_function<I: Into<VecDeque<Value>>>(&self, function_name: &IdentifierType, parameters: I, state: &mut State) -> Result<u64, InterpreterError> {
        let params = parameters.into();
        let func = self.get_function(function_name)?;

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
            Some(val) => Ok(val.to_u64()?),
            None => return Err(InterpreterError::NoExitCode)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser::Parser, lexer::Lexer};

    fn test_code_expect_value(key: &(String, Option<Type>, Vec<Type>), code: &str) -> Value {
        let lexer = Lexer::new(code);
        let tokens = lexer.get().unwrap();

        let (e, mut mapper) = Environment::new();
        let parser = Parser::new(tokens, &e);
        let program = parser.parse(&mut mapper).unwrap();

        let mut state = State::new(None, None);
        let interpreter = Interpreter::new(&program, &e).unwrap();

        let mapped_name = mapper.get(&key).unwrap();
        let func = interpreter.get_function(&mapped_name).unwrap();
        let result = interpreter.execute_function(func, None, VecDeque::new(), &mut state).unwrap();
        result.unwrap()
    }

    fn test_code_expect_return(code: &str, expected: u64) {
        assert_eq!(test_code_expect_value(&("main".to_owned(), None, Vec::new()), code).to_u64().unwrap(), expected);
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

        // TODO
        // test_code_expect_return("entry main() { return 10 + 10 * 10 + 10; }", 120);
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
        let key = &(String::from("main"), None, Vec::new());

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
        let key = &(String::from("main"), None, Vec::new());
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
        test_code_expect_return("entry main() { let a: u64 = 10; if a == 0 { if a == 10 { return 10; } else { return 0; } } else { return 0; } }", 0);
    }

    #[test]
    fn test_else_if() {
        test_code_expect_return("entry main() { let a: u64 = 10; if a == 10 { return 10; } else if a == 0 { return 0; } else { return 1; } }", 10);
        test_code_expect_return("entry main() { let a: u64 = 0; if a == 10 { return 10; } else if a == 0 { return 0; } else { return 1; } }", 0);
        test_code_expect_return("entry main() { let a: u64 = 1; if a == 10 { return 10; } else if a == 0 { return 0; } else { return 1; } }", 1);
    }
}