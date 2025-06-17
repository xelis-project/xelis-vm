mod context;
mod error;
mod mapper;

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet, VecDeque},
    mem
};
use error::ParserErrorKind;
use indexmap::IndexMap;
use log::trace;
use mapper::GlobalMapper;
use xelis_builder::{Builder, EnvironmentBuilder};
use xelis_ast::*;
use xelis_environment::NativeFunction;
use xelis_types::*;
use context::Context;

pub use error::ParserError;

#[derive(Debug, Clone)]
pub enum QueueItem {
  Operator(Operator),         // For operators, identifiers, or literals
  Separator,
  Expression(Expression),  // For finalized expressions or sub-expressions
}

macro_rules! err {
    ($self: expr, $kind: expr) => {
        ParserError {
            line: $self.line,
            column_start: $self.column_start,
            column_end: $self.column_end,
            kind: $kind
        }
    };
}


macro_rules! op {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (Primitive::U8(a), Primitive::U8(b)) => Primitive::U8(a $op b),
            (Primitive::U16(a), Primitive::U16(b)) => Primitive::U16(a $op b),
            (Primitive::U32(a), Primitive::U32(b)) => Primitive::U32(a $op b),
            (Primitive::U64(a), Primitive::U64(b)) => Primitive::U64(a $op b),
            (Primitive::U128(a), Primitive::U128(b)) => Primitive::U128(a $op b),
            _ => return None
        }
    }};
}

macro_rules! op_div {
    ($t: ident, $a: expr, $b: expr) => {
        {
            if *$b == 0 {
                return None
            }
    
            Primitive::$t($a / $b)
        }
    };
    ($a: expr, $b: expr) => {
        match ($a, $b) {
            (Primitive::U8(a), Primitive::U8(b)) => op_div!(U8, a, b),
            (Primitive::U16(a), Primitive::U16(b)) => op_div!(U16, a, b),
            (Primitive::U32(a), Primitive::U32(b)) => op_div!(U32, a, b),
            (Primitive::U64(a), Primitive::U64(b)) => op_div!(U64, a, b),
            (Primitive::U128(a), Primitive::U128(b)) => op_div!(U128, a, b),
            _ => return None
        }
    };
}

macro_rules! op_bool {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (Primitive::Boolean(a), Primitive::Boolean(b)) => Primitive::Boolean(a $op b),
            (Primitive::U8(a), Primitive::U8(b)) => Primitive::Boolean(a $op b),
            (Primitive::U16(a), Primitive::U16(b)) => Primitive::Boolean(a $op b),
            (Primitive::U32(a), Primitive::U32(b)) => Primitive::Boolean(a $op b),
            (Primitive::U64(a), Primitive::U64(b)) => Primitive::Boolean(a $op b),
            (Primitive::U128(a), Primitive::U128(b)) => Primitive::Boolean(a $op b),
            _ => return None
        }
    }};
}

macro_rules! op_num_with_bool {
    ($a: expr, $b: expr, $op: tt) => {{
        match ($a, $b) {
            (Primitive::Boolean(a), Primitive::Boolean(b)) => Primitive::Boolean(a $op b),
            (Primitive::U8(a), Primitive::U8(b)) => Primitive::U8(a $op b),
            (Primitive::U16(a), Primitive::U16(b)) => Primitive::U16(a $op b),
            (Primitive::U32(a), Primitive::U32(b)) => Primitive::U32(a $op b),
            (Primitive::U64(a), Primitive::U64(b)) => Primitive::U64(a $op b),
            (Primitive::U128(a), Primitive::U128(b)) => Primitive::U128(a $op b),
            _ => return None
        }
    }};
}

enum Function<'a> {
    Native(&'a NativeFunction),
    Program(&'a FunctionType)
}

impl<'a> Function<'a> {
    fn return_type(&self) -> &Option<Type> {
        match self {
            Function::Native(f) => f.return_type(),
            Function::Program(f) => f.return_type()
        }
    }

    fn is_normal(&self) -> bool {
        match self {
            Function::Program(f) => f.kind().is_normal(),
            Function::Native(_) => true,
        }
    }
}

// Character to use to ignore a variable
// its content will be dropped directly by VM
const IGNORE_VARIABLE: &str = "_";

pub struct Parser<'a> {
    // Tokens to process
    tokens: VecDeque<TokenResult<'a>>,
    // All constants declared
    constants: HashMap<&'a str, ConstantDeclaration>,
    // All functions registered by the program
    functions: Vec<FunctionType>,
    global_mapper: GlobalMapper<'a>,
    // Environment contains all the library linked to the program
    environment: &'a EnvironmentBuilder<'a>,
    // Disable upgrading values to consts
    disable_const_upgrading: bool,
    // Disable shadowing variables
    // This check that the variable name is not already declared
    // in the same scope or by a parent scope
    disable_shadowing_variables: bool,
    // force the behavior of entry having a return type or not
    // This let the user customize its return
    // If None, return type can be anything
    entry_forced_return_type: Option<Option<Type>>,
    // TODO: Path to use to import files
    // _path: Option<&'a str>
    // Used for errors, we track the line and column
    line: usize,
    column_start: usize,
    column_end: usize
}

#[derive(Debug)]
enum TuplePattern<'a> {
    Ignore,
    Variable(&'a str),
    Tuple(Vec<TuplePattern<'a>>),
}

impl<'a> Parser<'a> {
    // Compatibility purpose: Create a new parser with a list of tokens only
    pub fn new<I: IntoIterator<Item = Token<'a>>>(tokens: I, environment: &'a EnvironmentBuilder) -> Self {
        Self::with(tokens.into_iter().map(|v| TokenResult {
            token: v,
            line: 0,
            column_start: 0,
            column_end: 0
        }), environment)
    }

    // Create a new parser with a list of tokens and the environment
    pub fn with<I: Iterator<Item = TokenResult<'a>>>(tokens: I, environment: &'a EnvironmentBuilder) -> Self {
        Self {
            tokens: tokens.collect(),
            constants: HashMap::new(),
            functions: Vec::new(),
            global_mapper: GlobalMapper::with(environment),
            environment,
            disable_const_upgrading: false,
            disable_shadowing_variables: false,
            entry_forced_return_type: Some(Some(Type::U64)),
            line: 0,
            column_start: 0,
            column_end: 0,
        }
    }

    // Set the const upgrading to true or false
    pub fn set_const_upgrading_disabled(&mut self, value: bool) {
        self.disable_const_upgrading = value;
    }

    // Set the shadowing variables to true or false
    pub fn set_shadowing_variables(&mut self, value: bool) {
        self.disable_shadowing_variables = value;
    }

    // Set the entry function return type forced to u64
    // Set None to make it fully free for the user to select whatever he want to return
    // Some(None) to enforce no return type
    // Some(Some(T)) for enforced return type
    pub fn set_entry_forced_return_type(&mut self, value: Option<Option<Type>>) {
        self.entry_forced_return_type = value;
    }

    // Consume the next token
    #[inline(always)]
    fn advance(&mut self) -> Result<Token<'a>, ParserError<'a>> {
        self.next().ok_or(err!(self, ParserErrorKind::ExpectedToken))
    }

    // Consume the next token without error
    #[inline(always)]
    fn next(&mut self) -> Option<Token<'a>> {
        self.tokens.pop_front().map(|v| {
            self.line = v.line;
            self.column_start = v.column_start;
            self.column_end = v.column_end;
            v.token
        })
    }

    // Push back a token
    fn push_back(&mut self, token: Token<'a>) {
        self.tokens.push_front(TokenResult {
            token,
            line: self.line,
            column_start: self.column_start,
            column_end: self.column_end
        });
    }

    // Peek the next token without consuming it
    #[inline(always)]
    fn peek(&self) -> Result<&Token<'a>, ParserError<'a>> {
        self.tokens.front()
            .map(|t| &t.token)
            .ok_or(err!(self, ParserErrorKind::ExpectedToken))
    }

    // Peek the next token without consuming it
    #[inline(always)]
    fn peek_n(&self, n: usize) -> Result<&Token<'a>, ParserError<'a>> {
        self.tokens.get(n)
            .map(|t| &t.token)
            .ok_or(err!(self, ParserErrorKind::ExpectedToken))
    }

    // Limited to 32 characters
    #[inline(always)]
    fn next_identifier(&mut self) -> Result<&'a str, ParserError<'a>> {
        match self.advance()? {
            Token::Identifier(id) => Ok(id),
            token => Err(err!(self, ParserErrorKind::ExpectedIdentifierToken(token)))
        }
    }

    // Check if the next token is a specific token
    #[inline(always)]
    fn peek_is(&self, token: Token<'a>) -> bool {
        self.tokens.front().filter(|t| t.token == token).is_some()
    }

    // Check if the next token is not a specific token
    #[inline(always)]
    fn peek_is_not(&self, token: Token<'a>) -> bool {
        !self.peek_is(token)
    }

    // Check if the next token is an identifier
    #[inline(always)]
    fn peek_is_identifier(&self) -> bool {
        self.tokens.front().filter(|t| match t.token {
            Token::Identifier(_) => true,
            _ => false
        }).is_some()
    }

    // Require a specific token
    fn expect_token(&mut self, expected: Token<'a>) -> Result<(), ParserError<'a>> {
        let token = self.advance()?;
        if token != expected {
            return Err(err!(self, ParserErrorKind::InvalidToken(token, expected)))
        }
        Ok(())
    }

    fn get_generic_type(&mut self) -> Result<Type, ParserError<'a>> {
        trace!("Get generic type");
        self.expect_token(Token::OperatorLessThan)?;
        let token = self.advance()?;
        let inner = self.get_type_from_token(token)?;
        Ok(inner)
    }

    fn get_single_inner_type(&mut self) -> Result<Type, ParserError<'a>> {
        trace!("Get single inner type");
        let inner = self.get_generic_type()?;
        self.expect_token(Token::OperatorGreaterThan)?;
        Ok(inner)
    }

    fn get_type_from_token(&mut self, token: Token<'a>) -> Result<Type, ParserError<'a>> {
        trace!("Get type from token: {:?}", token);
        Ok(match token {
            Token::Number(inner) => match inner {
                NumberType::U8 => Type::U8,
                NumberType::U16 => Type::U16,
                NumberType::U32 => Type::U32,
                NumberType::U64 => Type::U64,
                NumberType::U128 => Type::U128,
                NumberType::U256 => Type::U256,
            },
            Token::String => Type::String,
            Token::Bool => Type::Bool,
            Token::Optional => Type::Optional(Box::new(self.get_single_inner_type()?)),
            Token::Range => Type::Range(Box::new(self.get_single_inner_type()?)),
            Token::Bytes => Type::Bytes,
            Token::Map => {
                let key = self.get_generic_type()?;
                if key.is_map() {
                    return Err(err!(self, ParserErrorKind::InvalidMapKeyType))
                }

                self.expect_token(Token::Comma)?;
                let token = self.advance()?;
                let value = self.get_type_from_token(token)?;
                self.expect_token(Token::OperatorGreaterThan)?;

                Type::Map(Box::new(key), Box::new(value))
            },
            Token::Identifier(id) => {
                if let Ok(builder) = self.global_mapper.structs().get_by_name(id) {
                    Type::Struct(builder.get_type().clone())
                } else if let Ok(builder) = self.global_mapper.enums().get_by_name(id) {
                    Type::Enum(builder.get_type().clone())
                } else if let Some(ty) = self.environment.get_opaque_by_name(id) {
                    Type::Opaque(ty.clone())
                } else {
                    return Err(err!(self, ParserErrorKind::TypeNameNotFound(id)))
                }
            },
            Token::ParenthesisOpen => {
                // Most likely tuples
                let mut tuples = Vec::new();
                while self.peek_is_not(Token::ParenthesisClose) {
                    let token = self.advance()?;
                    let inner = self.get_type_from_token(token)?;
                    tuples.push(inner);

                    if self.peek_is(Token::Comma) {
                        self.expect_token(Token::Comma)?;
                    }
                }

                if tuples.len() == 1 {
                    return Err(err!(self, ParserErrorKind::InvalidTupleType))
                }

                self.expect_token(Token::ParenthesisClose)?;

                Type::Tuples(tuples)
            },
            token => return Err(err!(self, ParserErrorKind::UnexpectedToken(token)))
        })
    }

    /**
     * Example: let message: string[] = ["hello", "world", "!"];
     * Types: (unsigned)
     * - u8
     * - u16
     * - u64
     * - u128
     * - u256
     * - string
     * - bool
     * - Struct (Structure with name that starts with a uppercase letter)
     * - T[] (where T is any above Type)
     */
    fn read_type(&mut self) -> Result<Type, ParserError<'a>> {
        trace!("Read type");
        let token = self.advance()?;
        let mut _type = self.get_type_from_token(token)?;

        // support multi dimensional arrays
        loop {
            if !self.peek_is(Token::BracketOpen) {
                break;
            }

            self.expect_token(Token::BracketOpen)?;
            self.expect_token(Token::BracketClose)?;
            _type = Type::Array(Box::new(_type));
        }

        Ok(_type)
    }

    // get the type of an expression
    fn get_type_from_expression<'b>(&'b self, on_type: Option<&Type>, expression: &'b Expression, context: &'b Context<'a>) -> Result<Cow<'b, Type>, ParserError<'a>> {
        match self.get_type_from_expression_internal(on_type, expression, context)? {
            Some(v) => Ok(v),
            None => Err(err!(self, ParserErrorKind::EmptyValue(expression.clone())))
        }
    }

    fn get_from_generic_type(&self, on_type: Option<&Type>, _type: &Type, path: Option<&Expression>, context: &Context<'a>) -> Result<Type, ParserError<'a>> {
        trace!("Get from generic type: {:?}", _type);
        Ok(match _type {
            Type::T(id) => match on_type {
                Some(t) if id.is_some() => t.get_generic_type(id.unwrap())
                    .cloned()
                    .ok_or_else(|| err!(self, ParserErrorKind::InvalidTypeT))?,
                Some(t) => t.clone(),
                None => match path {
                    Some(v) => {
                        let on_type = self.get_type_from_expression(on_type, v, context)?;
                        self.get_from_generic_type(Some(&on_type), _type, path, context)?
                    },
                    None => return Err(err!(self, ParserErrorKind::NoValueType))
                }
            },
            Type::Optional(inner) => Type::Optional(Box::new(self.get_from_generic_type(on_type, inner, path, context)?)),
            _ => _type.clone()
        })
    }

    // this function don't verify, but only returns the type of an expression
    // all tests should be done when constructing an expression, not here
    fn get_type_from_expression_internal<'b>(&'b self, on_type: Option<&Type>, expression: &'b Expression, context: &'b Context<'a>) -> Result<Option<Cow<'b, Type>>, ParserError<'a>> {
        trace!("Get type from expression: {:?}", expression);
        let _type: Cow<'b, Type> = match expression {
            Expression::ArrayConstructor(ref values) => match values.first() {
                Some(v) => Cow::Owned(Type::Array(Box::new(self.get_type_from_expression(on_type, v, context)?.into_owned()))),
                None => return Err(err!(self, ParserErrorKind::EmptyArrayConstructor)) // cannot determine type from empty array
            },
            Expression::TuplesConstructor(ref values) => {
                let mut types = Vec::with_capacity(values.len());
                for v in values.iter() {
                    types.push(self.get_type_from_expression(on_type, v, context)?.into_owned());
                }
                Cow::Owned(Type::Tuples(types))
            },
            Expression::ForceType(_, ty) => Cow::Borrowed(ty),
            Expression::MapConstructor(_, key_type, value_type) => Cow::Owned(Type::Map(Box::new(key_type.clone()), Box::new(value_type.clone()))),
            Expression::EnumConstructor(_, _type) => Cow::Owned(Type::Enum(_type.enum_type().clone())),
            Expression::Deconstruction(_, ty) => Cow::Borrowed(ty),
            Expression::Variable(ref var_name) => match on_type {
                Some(t) => match t {
                    Type::Struct(_type) => {
                        let index = *var_name as usize;
                        if let Some(field_type) = _type.fields().get(index) {
                            Cow::Owned(field_type.clone())
                        } else {
                            return Err(err!(self, ParserErrorKind::UnexpectedMappedVariableId(var_name.clone())))
                        }
                    },
                    Type::Tuples(types) => {
                        let index = *var_name as usize;
                        if let Some(field_type) = types.get(index) {
                            Cow::Owned(field_type.clone())
                        } else {
                            return Err(err!(self, ParserErrorKind::UnexpectedMappedVariableId(var_name.clone())))
                        }
                    },
                    _ => return Err(err!(self, ParserErrorKind::UnexpectedMappedVariableId(var_name.clone())))
                },
                None => Cow::Borrowed(context.get_type_of_variable(var_name).ok_or_else(|| err!(self, ParserErrorKind::UnexpectedMappedVariableId(*var_name)))?),
            },
            Expression::FunctionCall(path, name, _) => {
                let f = self.get_function(*name)?;
                let return_type = f.return_type();
                match return_type {
                    Some(ref v) => Cow::Owned(self.get_from_generic_type(on_type, v, path.as_deref(), context)?),
                    None => return Err(err!(self, ParserErrorKind::FunctionNoReturnType))
                }
            },
            // we have to clone everything due to this
            Expression::Constant(ref val) => match Type::from_value_type(val) {
                Some(v) => Cow::Owned(v),
                None => return Ok(None)
            },
            Expression::ArrayCall(path, _) => {
                match self.get_type_from_expression(on_type, path, context)?.into_owned() {
                    Type::Array(_type) => Cow::Owned(*_type),
                    Type::Bytes => Cow::Owned(Type::U8),
                    _ => return Err(err!(self, ParserErrorKind::InvalidArrayCall))
                }
            },
            Expression::SubExpression(expr) => self.get_type_from_expression(on_type, expr, context)?,
            Expression::StructConstructor(_, _type) => Cow::Owned(Type::Struct(_type.clone())),
            Expression::Path(left, right) => {
                let var_type = self.get_type_from_expression(on_type, left, context)?;
                self.get_type_from_expression(Some(&var_type), right, context)?
            },
            // Compatibility checks are done when constructing the expression
            Expression::Operator(op, left, right) => match op {
                // Condition operators
                Operator::Or
                | Operator::Eq
                | Operator::Neq
                | Operator::Gte
                | Operator::Gt
                | Operator::Lte
                | Operator::Lt
                | Operator::And => Cow::Owned(Type::Bool),
                // Assign operators
                Operator::Assign(_) => return Err(err!(self, ParserErrorKind::AssignReturnNothing)),
                // String compatible operators
                Operator::Add => {
                    let left_type = self.get_type_from_expression(on_type, left, context)?;
                    let right_type = self.get_type_from_expression(on_type, right, context)?;

                    if *left_type == Type::String || *right_type == Type::String {
                        Cow::Owned(Type::String)
                    } else {
                        left_type
                    }
                },
                // Number only operators
                Operator::Sub
                | Operator::Mul
                | Operator::Div
                | Operator::Mod
                | Operator::BitwiseXor
                | Operator::BitwiseAnd
                | Operator::BitwiseOr
                | Operator::BitwiseShl
                | Operator::BitwiseShr => {
                    let left_type = self.get_type_from_expression(on_type, left, context)?;
                    let right_type = self.get_type_from_expression(on_type, right, context)?;

                    if !left_type.is_number() || !right_type.is_number() || left_type != right_type {
                        return Err(err!(self, ParserErrorKind::InvalidOperationNotSameType(left_type.into_owned(), right_type.into_owned())))
                    }
                    left_type
                },
                Operator::Pow => {
                    let left_type = self.get_type_from_expression(on_type, left, context)?;
                    let right_type = self.get_type_from_expression(on_type, right, context)?;

                    if !left_type.is_number() || !right_type.is_number() {
                        return Err(err!(self, ParserErrorKind::InvalidOperationNotSameType(left_type.into_owned(), right_type.into_owned())))
                    }

                    if *right_type != Type::U32 {
                        return Err(err!(self, ParserErrorKind::InvalidOperationNotSameType(Type::U32, right_type.into_owned())))
                    }

                    left_type
                }
            },
            Expression::IsNot(_) => Cow::Owned(Type::Bool),
            Expression::Ternary(_, expr, _) => self.get_type_from_expression(on_type, expr, context)?,
            Expression::Cast(_, _type) => Cow::Borrowed(_type),
            Expression::RangeConstructor(start, _) => Cow::Owned(Type::Range(Box::new(self.get_type_from_expression(on_type, start, context)?.into_owned()))),
        };

        Ok(Some(_type))
    }

    // Read a function call with the following syntax:
    // function_name(param1, param2, ...)
    fn read_function_params(&mut self, context: &mut Context<'a>) -> Result<(Vec<Expression>, Vec<Option<Type>>), ParserError<'a>> {
        trace!("Read function params");
        // we remove the token from the list
        self.expect_token(Token::ParenthesisOpen)?;
        let mut parameters: Vec<Expression> = Vec::new();
        let mut types: Vec<Option<Type>> = Vec::new();
        while self.peek_is_not(Token::ParenthesisClose) {
            let expr = self.read_expression_delimited(&Token::Comma, context)?;
            // We are forced to clone the type because we can't borrow it from the expression
            // I prefer to do this than doing an iteration below
            let t = self.get_type_from_expression_internal(None, &expr, context)?
                .map(|v| v.into_owned());

            types.push(t);
            parameters.push(expr);

            if self.peek_is(Token::Comma) {
                self.expect_token(Token::Comma)?;
            }
        }

        self.expect_token(Token::ParenthesisClose)?;
        Ok((parameters, types))
    }

    // Read a function call with the following syntax:
    // function_name(param1, param2, ...)
    fn read_function_call(&mut self, path: Option<Expression>, instance: bool, on_type: Option<&Type>, name: &str, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        trace!("read function call {}", name);
        let (mut parameters, types) = self.read_function_params(context)?;

        let id = self.global_mapper
            .functions()
            .get_compatible(name, on_type, instance, &types, &mut parameters)
            .map_err(|e| err!(self, e.into()))?;

        // Entry are only callable by external
        let f = self.get_function(id)?;
        if !f.is_normal() {
            return Err(err!(self, ParserErrorKind::FunctionIsNotCallable))
        }

        Ok(Expression::FunctionCall(path.map(Box::new), id, parameters))
    }

    // Read fields of a constructor with the following syntax:
    // { field1, field2, ... }
    // or with values
    // { field1: value1, field2: value2, ... }
    fn read_constructor_fields(&mut self, context: &mut Context<'a>, expected_types: &[Type]) -> Result<(Vec<(&'a str, Expression)>, bool), ParserError<'a>> {
        trace!("Read constructor fields");

        // bool to determine if we can register the variable
        let in_match = context.is_in_a_match();
        let mut registered_as_var = false;
        let mut fields = Vec::with_capacity(expected_types.len());
        for ty in expected_types {
            let field_name = self.next_identifier()?;
            trace!("field name: {}", field_name);
            let expr = match self.peek()? {
                Token::Comma | Token::BraceClose => {
                    // If we are in a match, register the variable
                    // So we can use it in the body
                    let id = if in_match {
                        registered_as_var = true;
                        self.register_variable(context, field_name, ty.clone())?
                    } else {
                        context.get_variable_id(field_name)
                            .ok_or_else(|| err!(self, ParserErrorKind::UnexpectedVariable(field_name)))?
                    };

                    Expression::Variable(id)
                },
                Token::Colon => {
                    self.expect_token(Token::Colon)?;
                    self.read_expression_expected_type(context, ty)?
                },
                token => return Err(err!(self, ParserErrorKind::UnexpectedToken(token.clone())))
            };

            fields.push((field_name, expr));

            if self.peek_is(Token::Comma) {
                self.expect_token(Token::Comma)?;
            }
        }
        self.expect_token(Token::BraceClose)?;

        Ok((fields, registered_as_var))
    }

    // Verify the type of an expression, if not the same, try to cast it with no loss
    fn verify_type_of(&self, expr: &mut Expression, expected_type: &Type, context: &Context<'a>, is_assign: bool) -> Result<(), ParserError<'a>> {
        let ty = self.get_type_from_expression_internal(None, &expr, context)?
            .map(Cow::into_owned);
        self.verify_type_compatibility(expr, ty.as_ref(), expected_type, is_assign)
    }

    fn verify_type_compatibility(&self, expr: &mut Expression, got: Option<&Type>, expected_type: &Type, is_assign: bool) -> Result<(), ParserError<'a>> {
        let _type = match got {
            Some(v) => v,
            None => {
                // Map is empty, so any type can be enforced
                if expected_type.allow_null() || expected_type.is_map() {
                    return Ok(())
                }

                return Err(err!(self, ParserErrorKind::NullNotAllowed(expected_type.clone())))
            }
        };

        let is_valid = if is_assign {
            expected_type.is_assign_compatible_with(_type)
        } else {
            expected_type.is_compatible_with(_type)
        };

        if !is_valid {
            match expr {
                Expression::Constant(v) if _type.is_castable_to(expected_type) => v.mut_checked_cast_to_primitive_type(expected_type)
                    .map_err(|e| err!(self, e.into()))?,
                _ => return Err(err!(self, ParserErrorKind::InvalidValueType(_type.clone(), expected_type.clone())))
            }
        }

        Ok(())
    }

    // Read a struct constructor with the following syntax:
    // struct_name { field_name: value1, field2: value2 }
    // If we have a field that has the same name as a variable we can pass it as following:
    // Example: struct_name { field_name, field2: value2 }
    fn read_struct_constructor(&mut self, struct_type: StructType, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        trace!("Read struct constructor: {:?}", struct_type);
        self.expect_token(Token::BraceOpen)?;
        let (fields, deconstruct) = self.read_constructor_fields(context, struct_type.fields())?;

        if struct_type.fields().len() != fields.len() {
            return Err(err!(self, ParserErrorKind::InvalidFieldCount))
        }

        // Now verify that it match our struct
        let builder = self.global_mapper.structs().get_by_ref(&struct_type)
            .map_err(|e| err!(self, e.into()))?;
        let mut fields_expressions = Vec::with_capacity(fields.len());
        let iter = fields.into_iter()
            .zip(
                struct_type.fields()
                    .iter()
                    .zip(builder.names())
            );

        for ((field_name, mut field_expr), (field_type, field_name_expected)) in iter {
            if field_name != *field_name_expected {
                return Err(err!(self, ParserErrorKind::InvalidFieldName(field_name, field_name_expected)))
            }

            self.verify_type_of(&mut field_expr, field_type, context, true)?;

            fields_expressions.push(field_expr);
        }

        Ok(if deconstruct {
            Expression::Deconstruction(fields_expressions, Type::Struct(struct_type))
        } else {
            Expression::StructConstructor(fields_expressions, struct_type)
        })
    }

    // Read an enum variant constructor with the following syntax:
    // enum_name::variant_name { field1: value1, field2: value2 }
    // Or if no fields: enum_name::variant_name
    fn read_enum_variant_constructor(&mut self, enum_type: EnumType, variant_name: &'a str, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        trace!("Read enum variant constructor: {:?}::{}", enum_type, variant_name);

        let (variant_id, fields) = {
            let builder = self.global_mapper.enums()
                .get_by_ref(&enum_type)
                .map_err(|e| err!(self, e.into()))?;
            let (variant_id, variant_fields) = builder.get_variant_by_name(&variant_name)
                .ok_or_else(|| err!(self, ParserErrorKind::EnumVariantNotFound(variant_name)))?;

            (variant_id, variant_fields.iter().map(|(_, ty)| ty.clone()).collect::<Vec<_>>())
        };

        // If its an enum variant with fields
        let (exprs, deconstruct) = if !fields.is_empty() {
            self.expect_token(Token::BraceOpen)?;
            let (fields, deconstruct) = self.read_constructor_fields(context, &fields)?;

            // Now we verify that we have all fields needed
            let builder = self.global_mapper.enums()
                .get_by_ref(&enum_type)
                .map_err(|e| err!(self, e.into()))?;

            let variant = builder.get_variant_by_id(variant_id)
                .ok_or_else(|| err!(self, ParserErrorKind::EnumVariantNotFound(variant_name)))?;

            if variant.len() != fields.len() {
                return Err(err!(self, ParserErrorKind::InvalidFieldCount))
            }

            let mut fields_expressions = Vec::with_capacity(variant.len());
            for ((field_name, mut field_expr), (expected_name, expected_type)) in fields.into_iter().zip(variant) {
                if field_name != *expected_name {
                    return Err(err!(self, ParserErrorKind::InvalidEnumFieldName(field_name)))
                }

                self.verify_type_of(&mut field_expr, expected_type, context, true)?;

                fields_expressions.push(field_expr);
            }

            (fields_expressions, deconstruct)
        } else {
            (Vec::new(), false)
        };

        Ok(if deconstruct {
            Expression::Deconstruction(exprs, Type::Enum(enum_type))
        } else {
            Expression::EnumConstructor(exprs, EnumValueType::new(enum_type, variant_id))
        })
    }

    // Read a constant from the environment
    fn read_type_constant(&mut self, token: Token<'a>, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        trace!("Read type constant: {:?}", token);
        let _type = self.get_type_from_token(token)?;
        self.expect_token(Token::Colon)?;
        self.expect_token(Token::Colon)?;

        let constant_name = self.next_identifier()?;

        trace!("Read type constant: {:?}::{}", _type, constant_name);
        // check if its a constant value
        if let Some(expr) = self.environment.get_constant_by_name(&_type, &constant_name)
            .map(|v| Expression::Constant(v.clone())) {
            Ok(expr)
        // Check if its a preprocessor constant function
        } else if let Some(const_fn) = self.environment.get_const_fn(&_type, constant_name) {
            let (mut parameters, _) = self.read_function_params(context)?;
            let mut constants = Vec::with_capacity(parameters.len());
            for param in parameters.iter_mut() {
                let constant = self.try_convert_expr_to_value(param)
                    .ok_or_else(|| err!(self, ParserErrorKind::ConstantNotFound(_type.clone(), constant_name)))?;
                constants.push(constant);
            }

            const_fn.call(constants)
                .map(|v| Expression::Constant(v))
                .map_err(|e| err!(self, e.into()))
        } else if self.peek_is(Token::ParenthesisOpen) {
            // Try to read a static (on type) function call from it
            self.read_function_call(None, false, Some(&_type), constant_name, context)
        // If its a enum, it may be a variant constructor
        } else if let Type::Enum(enum_type) = _type {
            self.read_enum_variant_constructor(enum_type, constant_name, context)
        } else {
            Err(err!(self, ParserErrorKind::ConstantNotFound(_type, constant_name)))
        }
    }

    // Execute the selected operator
    fn execute_operator(&self, op: &Operator, left: &Primitive, right: &Primitive) -> Option<Primitive> {
        Some(match op {
            Operator::Add => {
                if left.is_string() || right.is_string() {
                    Primitive::String(format!("{}{}", left, right))
                } else {
                    op!(left, right, +)
                }
            },
            Operator::Sub => op!(left, right, -),
            Operator::Div => op_div!(left, right),
            Operator::Mul => op!(left, right, *),
            Operator::Mod => op!(left, right, %),
            Operator::Pow => {
                let pow_n = right.as_u32().ok()?;
                match left {
                    Primitive::U8(v) => Primitive::U8(v.pow(pow_n)),
                    Primitive::U16(v) => Primitive::U16(v.pow(pow_n)),
                    Primitive::U32(v) => Primitive::U32(v.pow(pow_n)),
                    Primitive::U64(v) => Primitive::U64(v.pow(pow_n)),
                    Primitive::U128(v) => Primitive::U128(v.pow(pow_n)),
                    _ => return None
                }
            },

            Operator::BitwiseXor => op!(left, right, ^),
            Operator::BitwiseAnd => op_num_with_bool!(left, right, &),
            Operator::BitwiseOr => op_num_with_bool!(left, right, |),
            Operator::BitwiseShl => op!(left, right, <<),
            Operator::BitwiseShr => op!(left, right, >>),

            Operator::Eq => Primitive::Boolean(left == right),
            Operator::Neq => Primitive::Boolean(left != right),
            Operator::Gte => op_bool!(left, right, >=),
            Operator::Gt => op_bool!(left, right, >),
            Operator::Lte => op_bool!(left, right, <=),
            Operator::Lt => op_bool!(left, right, <),
            Operator::And => Primitive::Boolean(left.as_bool().ok()? && right.as_bool().ok()?),
            Operator::Or => Primitive::Boolean(left.as_bool().ok()? || right.as_bool().ok()?),
            Operator::Assign(_) => return None,
        })
    }

    // Try to convert an expression to a value
    // By converting an expression to a constant value, we earn in performance as we have less operations to execute
    // If it can't fully convert the expression to a value, it will still try to change some parts of the expression to a value
    // Example: 5 + 5 = 10 -> we only write 10
    // if we have a if true { 5 } else { 10 } -> we write 5
    fn try_convert_expr_to_value(&self, expr: &mut Expression) -> Option<Constant> {
        if self.disable_const_upgrading {
            return None
        }

        Some(match expr {
            Expression::Constant(v) => v.clone(),
            Expression::ArrayConstructor(values) => {
                let len = values.len();
                let mut new_values = Vec::with_capacity(len);
                for value in values {
                    if let Some(v) = self.try_convert_expr_to_value(value) {
                        *value = Expression::Constant(v.clone());
                        new_values.push(v);
                    }
                }

                // We can't fully upgrade it to const
                if len != new_values.len() {
                    return None
                }

                Constant::Array(new_values)
            },
            Expression::RangeConstructor(min, max) => {
                let min_value = self.try_convert_expr_to_value(min);
                let max_value = self.try_convert_expr_to_value(max);

                if min_value.is_none() || max_value.is_none() {
                    if let Some(v) = &min_value {
                        *min.as_mut() = Expression::Constant(v.clone());
                    }

                    if let Some(v) = &max_value {
                        *max.as_mut() = Expression::Constant(v.clone());
                    }
                }

                let min = min_value?.into_value().ok()?;
                let max = max_value?.into_value().ok()?;

                Constant::Default(Primitive::Range(Box::new((min, max))))
            },
            Expression::StructConstructor(fields, struct_type) => {
                let len = fields.len();
                let mut new_fields = Vec::with_capacity(len);
                for field in fields {
                    if let Some(v) = self.try_convert_expr_to_value(field) {
                        *field = Expression::Constant(v.clone());
                        new_fields.push(v);
                    }
                }

                if len != new_fields.len() {
                    return None
                }

                Constant::Typed(new_fields, DefinedType::Struct(struct_type.clone()))
            },
            Expression::EnumConstructor(fields, enum_type) => {
                let mut new_fields = Vec::with_capacity(fields.len());
                for field in fields {
                    let v = self.try_convert_expr_to_value(field)?;
                    *field = Expression::Constant(v.clone());
                    new_fields.push(v);
                }
                Constant::Typed(new_fields, DefinedType::Enum(enum_type.clone()))
            },
            Expression::MapConstructor(entries, _, _) => {
                let mut new_entries = IndexMap::with_capacity(entries.len());
                for (key, value) in entries {
                    let k = self.try_convert_expr_to_value(key);
                    let v = self.try_convert_expr_to_value(value);

                    if let Some(k) = &k {
                        *key = Expression::Constant(k.clone());
                    }

                    if let Some(v) = &v {
                        *value = Expression::Constant(v.clone());
                    }

                    new_entries.insert(k?, v?);
                }
                Constant::Map(new_entries)
            },
            Expression::Cast(expr, _type) => {
                let v = self.try_convert_expr_to_value(expr)?;
                v.checked_cast_to_primitive_type(_type).ok()?
            },
            Expression::ArrayCall(array_expr, value) => {
                let array = self.try_convert_expr_to_value(array_expr);
                let index = self.try_convert_expr_to_value(value);

                if let Some(array) = &array {
                    *array_expr.as_mut() = Expression::Constant(array.clone());
                }

                if let Some(index) = &index {
                    *value.as_mut() = Expression::Constant(index.clone());
                }

                let index = index?.checked_cast_to_u32().ok()?;
                let array = array?;
                let values = array.as_vec().ok()?;
                values.get(index as usize)?.clone()
            },
            Expression::IsNot(expr) => {
                let v = self.try_convert_expr_to_value(expr)?;
                Constant::Default(Primitive::Boolean(!v.to_bool().ok()?))
            },
            Expression::Operator(op, left, right) => {
                let l = self.try_convert_expr_to_value(left);
                let r = self.try_convert_expr_to_value(right);
            
                if let Some(l) = &l {
                    *left.as_mut() = Expression::Constant(l.clone());
                }

                if let Some(r) = &r {
                    *right.as_mut() = Expression::Constant(r.clone());
                }

                Constant::Default(self.execute_operator(op, l?.as_value().ok()?, r?.as_value().ok()?)?)
            },
            Expression::SubExpression(expr) => self.try_convert_expr_to_value(expr)?,
            Expression::Ternary(condition, left, right) => {
                let c = self.try_convert_expr_to_value(condition);

                if let Some(c) = &c {
                    *condition.as_mut() = Expression::Constant(c.clone());
                }

                let l = self.try_convert_expr_to_value(left);
                let r = self.try_convert_expr_to_value(right);

                if let Some(l) = &l {
                    *left.as_mut() = Expression::Constant(l.clone());
                }

                if let Some(r) = &r {
                    *right.as_mut() = Expression::Constant(r.clone());
                }

                if c?.to_bool().ok()? {
                    l?
                } else {
                    r?
                }
            },
            Expression::Path(left, right) => {
                let l = self.try_convert_expr_to_value(left);
                let r = self.try_convert_expr_to_value(right);

                if let Some(l) = &l {
                    *left.as_mut() = Expression::Constant(l.clone());
                }

                if let Some(r) = &r {
                    *right.as_mut() = Expression::Constant(r.clone());
                }

                // TODO: find a way to get the value of the path
                return None
            },
            _ => return None
        })
    }

    fn try_postfix_collapse(
        &self,
        output_queue: impl Iterator<Item = QueueItem>, 
        context: &mut Context<'a>,
    ) -> Result<Expression, ParserError<'a>> {
        let mut collapse_queue = Vec::new();
        for item in output_queue {
            match item {
                QueueItem::Expression(expr) => {
                    collapse_queue.push(expr);
                }
                QueueItem::Operator(op) => {
                    let (mut right, mut left) = match (collapse_queue.pop(), collapse_queue.pop()) {
                        (Some(right), Some(left)) => (right, left),
                        _ => return Err(err!(self, ParserErrorKind::InvalidExpression)),
                    };

                    let left_type = self.get_type_from_expression(None, &left, context)?
                        .into_owned();
                    let right_type = self.get_type_from_expression_internal(None, &right, context)?;
                    if right_type.is_none() && !left_type.allow_null() {
                        return Err(err!(self, ParserErrorKind::EmptyValue(right)));
                    }

                    if let Some(right_type) = right_type {
                        self.verify_operator(&op, left_type, right_type.into_owned(), &mut left, &mut right)?;
                    }

                    collapse_queue.push(Expression::Operator(op, Box::new(left), Box::new(right)));
                },
                _ => {}
            }
        }

        Ok(collapse_queue.remove(0))
    }

    // Read an expression with default parameters
    fn read_expression(&mut self, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        self.read_expr(None, None, true, true, None, context)
    }

    // Read an expression with default parameters
    fn read_expression_expected_type(&mut self, context: &mut Context<'a>, ty: &Type) -> Result<Expression, ParserError<'a>> {
        self.read_expr(None, None, true, true, Some(ty), context)
    }

    // Read an expression with a delimiter
    fn read_expression_delimited(&mut self, delimiter: &Token, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        self.read_expr(Some(delimiter), None, true, true, None, context)
    }

    // Read an expression with the possibility to accept operators
    // number_type is used to force the type of a number
    fn read_expr(
        &mut self,
        delimiter: Option<&Token>, 
        on_type: Option<&Type>, 
        allow_ternary: bool, 
        accept_operator: bool, 
        expected_type: Option<&Type>, 
        context: &mut Context<'a>
    ) -> Result<Expression, ParserError<'a>> {
        trace!("Read expression with peek {:?}", self.peek());
        // All expressions parsed
        let mut operator_stack: Vec<Operator> = Vec::new();
        // Queued items, draining the above two vectors
        let mut queue: Vec<QueueItem> = Vec::new();

        let mut last_assign_queue_size: usize = 0;
        let mut last_assign_operator_stack_size: usize = 0;

        let mut required_operator = false;
        while self.peek()
            .ok()
            .filter(|peek| {
                if delimiter == Some(peek) {
                    return false
                }

                if !allow_ternary && **peek == Token::OperatorTernary {
                    return false
                }

                if !accept_operator && required_operator {
                    return false
                }

                required_operator == peek.is_operator() 
                    || (**peek == Token::BracketOpen && queue.is_empty())

            }).is_some()
        {
            let token = self.advance()?;
            trace!("token: {:?}", token);

            let expr = match token {
                Token::BracketOpen => {
                    match queue.pop() {
                        Some(QueueItem::Expression(v)) => {
                            if !self.get_type_from_expression(on_type, &v, context)?.support_array_call() {
                                return Err(err!(self, ParserErrorKind::InvalidArrayCall))
                            }

                            let mut expr = v;
                            loop {
                                // Index must be of type u32
                                let index = self.read_expr(delimiter, None, true, true, Some(&Type::U32), context)?;
                                let index_type = self.get_type_from_expression(None, &index, context)?;
                                if *index_type != Type::U32 {
                                    return Err(err!(self, ParserErrorKind::InvalidArrayCallIndexType(index_type.into_owned())))
                                }
                                expr = Expression::ArrayCall(Box::new(expr), Box::new(index));

                                self.expect_token(Token::BracketClose)?;

                                if self.peek_is(Token::BraceOpen) {
                                    self.expect_token(Token::BracketOpen)?;
                                } else {
                                    break;
                                }
                            }

                            required_operator = !required_operator;
                            expr
                        },
                        None => {
                            // require at least one value in a array constructor
                            let mut elements: Vec<Expression> = Vec::new();
                            let mut array_type: Option<Type> = None;
                            while self.peek_is_not(Token::BracketClose) {
                                let expr = self.read_expr(None, on_type, true, true, expected_type.map(|t| t.get_inner_type()), context)?;
                                match &array_type { // array values must have the same type
                                    Some(t) => {
                                        let _type = self.get_type_from_expression(on_type, &expr, context)?;
                                        if *_type != *t {
                                            return Err(err!(self, ParserErrorKind::InvalidTypeInArray(_type.into_owned(), t.clone())))
                                        }
                                    },
                                    None => { // first value rules the type of array
                                        array_type = Some(self.get_type_from_expression(on_type, &expr, context)?.into_owned());
                                    }
                                };
                                elements.push(expr);

                                if self.peek_is(Token::Comma) {
                                    self.expect_token(Token::Comma)?;
                                }
                            }

                            self.expect_token(Token::BracketClose)?;
                            Expression::ArrayConstructor(elements)
                        },
                        _ => return Err(err!(self, ParserErrorKind::InvalidArrayCall))
                    }
                },
                Token::ParenthesisOpen => {
                    // Empty tuple
                    if self.peek_is(Token::ParenthesisClose) {
                        trace!("empty tuple");
                        self.expect_token(Token::ParenthesisClose)?;
                        Expression::Constant(
                            Constant::Typed(
                                Vec::new(),
                                DefinedType::Tuples(Vec::new())
                            )
                        )
                    } else {
                        trace!("read parenthesis");
                        match expected_type {
                            Some(Type::Tuples(tuples)) => {
                                let mut elements = Vec::with_capacity(tuples.len());
                                for tu in tuples {
                                    let expr = self.read_expr(Some(&Token::ParenthesisClose), None, true, true, Some(tu), context)?;
                                    elements.push(expr);

                                    if self.peek_is(Token::Comma) {
                                        self.expect_token(Token::Comma)?;
                                    }
                                }

                                self.expect_token(Token::ParenthesisClose)?;
                                Expression::TuplesConstructor(elements)
                            },
                            _ => {
                                let expr = self.read_expr(Some(&Token::ParenthesisClose), None, true, true, expected_type, context)?;
                                self.expect_token(Token::ParenthesisClose)?;
                               Expression::SubExpression(Box::new(expr))
                            }
                        }
                    }
                },
                Token::ParenthesisClose => {
                    if delimiter == Some(&Token::Comma) {
                        break;
                    }
                    continue;
                },
                Token::Identifier(id) => {
                    match self.peek() {
                        // function call
                        Ok(Token::ParenthesisOpen) => {
                            let prev_expr = match queue.pop() {
                                Some(QueueItem::Expression(v)) => Some(v),
                                None | Some(QueueItem::Separator) => None,
                                _ => return Err(err!(self, ParserErrorKind::InvalidOperation))
                            };
                            self.read_function_call(prev_expr, on_type.is_some(), on_type, id, context)?
                        },
                        Ok(Token::Colon) if matches!(self.peek_n(1), Ok(Token::Colon)) => self.read_type_constant(Token::Identifier(id), context)?,
                        _ => {
                            match on_type {
                                // mostly an access to a struct field
                                Some(t) => match t {
                                    Type::Struct(_type) => {
                                        let builder = self.global_mapper.structs()
                                            .get_by_ref(_type)
                                            .map_err(|e| err!(self, e.into()))?;
                                        let id = builder.get_id_for_field(id)
                                            .ok_or_else(|| err!(self, ParserErrorKind::UnexpectedAttributeOnType(id, t.clone())))?;

                                        Expression::Variable(id)
                                    },
                                    _ => return Err(err!(self, ParserErrorKind::UnexpectedType(t.clone())))
                                },
                                None => {
                                    if let Some(num_id) = context.get_variable_id(id) {
                                        Expression::Variable(num_id)
                                    } else if let Some(constant) = self.constants.get(id) {
                                        Expression::Constant(constant.value.clone())
                                    } else if let Ok(builder) = self.global_mapper.structs().get_by_name(&id) {
                                        self.read_struct_constructor(builder.get_type().clone(), context)?
                                    } else if let Ok(builder) = self.global_mapper.enums().get_by_name(&id) {
                                        self.read_enum_variant_constructor(builder.get_type().clone(), id, context)?
                                    } else if let Some(ty) = context.get_match_on_type() {
                                        let id = self.register_variable(context, id, ty)?;
                                        Expression::Variable(id)
                                    } else {
                                        return Err(err!(self, ParserErrorKind::UnexpectedVariable(id)))
                                    }
                                }
                            }
                        }
                    }
                },
                Token::Value(value) => match (on_type, value) {
                     (Some(Type::Tuples(tuples)), Literal::Number(n)) => {
                        let id = n as usize;
                        if id < tuples.len() {
                            Expression::Variable(id as _)
                        } else {
                            return Err(err!(self, ParserErrorKind::InvalidTupleIndex(id)))
                        }
                     },
                     (_, value) => Expression::Constant(
                        Constant::Default(match value {
                            Literal::U8(n) => Primitive::U8(n),
                            Literal::U16(n) => Primitive::U16(n),
                            Literal::U32(n) => Primitive::U32(n),
                            Literal::U64(n) => Primitive::U64(n),
                            Literal::U128(n) => Primitive::U128(n),
                            Literal::U256(n) => Primitive::U256(n),
                            Literal::Number(n) => match expected_type {
                                Some(Type::U8) => Primitive::U8(n.try_into().map_err(|_| err!(self, ParserErrorKind::NumberTooBigForType(Type::U8)))?),
                                Some(Type::U16) => Primitive::U16(n.try_into().map_err(|_| err!(self, ParserErrorKind::NumberTooBigForType(Type::U16)))?),
                                Some(Type::U32) => Primitive::U32(n.try_into().map_err(|_| err!(self, ParserErrorKind::NumberTooBigForType(Type::U32)))?),
                                Some(Type::U64) => Primitive::U64(n),
                                Some(Type::U128) => Primitive::U128(n as u128),
                                Some(Type::U256) => Primitive::U256(U256::from(n)),
                                _ => Primitive::U64(n)
                            },
                            Literal::String(s) => Primitive::String(s.into_owned()),
                            Literal::Bool(b) => Primitive::Boolean(b),
                            Literal::Null => Primitive::Null
                        })
                    )
                }
                Token::Dot => {
                    match queue.pop() {
                        Some(QueueItem::Expression(value)) => {
                            let _type = self.get_type_from_expression(on_type, &value, context)?.into_owned();
                            // If we have .. that is mostly a range

                            // because we read operator DOT + right expression
                            required_operator = !required_operator;

                            // Read a range
                            if self.peek_is(Token::Dot) {
                                self.expect_token(Token::Dot)?;
                                let end_expr = self.read_expr(delimiter, Some(&_type), false, false, expected_type, context)?;
                                let end_type = self.get_type_from_expression(on_type, &end_expr, context)?;
                                if _type != *end_type {
                                    return Err(err!(self, ParserErrorKind::InvalidRangeType(_type, end_type.into_owned())))
                                }

                                if !_type.is_primitive() {
                                    return Err(err!(self, ParserErrorKind::InvalidRangeTypePrimitive(_type)))
                                }

                                Expression::RangeConstructor(Box::new(value), Box::new(end_expr))
                            } else {
                                // Read a variable access OR a function call
                                let right_expr = self.read_expr(delimiter, Some(&_type), false, false, expected_type, context)?;
                                if let Expression::FunctionCall(path, name, params) = right_expr {
                                    if path.is_some() {
                                        return Err(err!(self, ParserErrorKind::UnexpectedPathInFunctionCall))
                                    }

                                    Expression::FunctionCall(Some(Box::new(value)), name, params)
                                } else {
                                    Expression::Path(Box::new(value), Box::new(right_expr))
                                }
                            }
                        },
                        _ => return Err(err!(self, ParserErrorKind::UnexpectedToken(Token::Dot)))
                    }
                },
                Token::IsNot => { // it's an operator, but not declared as
                    let expr = self.read_expression(context)?;
                    let expr_type = self.get_type_from_expression(on_type, &expr, context)?;
                    if *expr_type != Type::Bool {
                        return Err(err!(self, ParserErrorKind::InvalidValueType(expr_type.into_owned(), Type::Bool)))
                    }

                    Expression::IsNot(Box::new(expr))
                },
                Token::OperatorTernary => {
                    if queue.is_empty() {
                        return Err(err!(self, ParserErrorKind::InvalidTernaryNoPreviousExpression));
                    }

                    let ternary_queue = queue.drain(last_assign_queue_size..)
                        .chain(operator_stack.drain(last_assign_operator_stack_size..)
                            .rev()
                            .map(|v| QueueItem::Operator(v))
                        );

                    let collapsed_expr = self.try_postfix_collapse(
                        ternary_queue,
                        context,
                    )?;

                    if *self.get_type_from_expression(on_type, &collapsed_expr, context)? != Type::Bool {
                        return Err(err!(self, ParserErrorKind::InvalidCondition(Type::Bool, collapsed_expr)))
                    }

                    let valid_expr = self.read_expr(Some(&Token::Colon), None, true, true, expected_type, context)?;
                    let first_type = self.get_type_from_expression(None, &valid_expr, context)?.into_owned();

                    self.expect_token(Token::Colon)?;
                    let else_expr = self.read_expr(None, on_type, true, true, expected_type, context)?;
                    let else_type = self.get_type_from_expression(None, &else_expr, context)?;
                    
                    if !first_type.is_compatible_with(&else_type) { // both expr should have the SAME type.
                        return Err(err!(self, ParserErrorKind::InvalidValueType(else_type.into_owned(), first_type)))
                    }
                    required_operator = !required_operator;

                    Expression::Ternary(Box::new(collapsed_expr), Box::new(valid_expr), Box::new(else_expr))
                },
                Token::As => {
                    let prev_expr = match queue.pop() {
                        Some(QueueItem::Expression(v)) => v,
                        _ => return Err(err!(self, ParserErrorKind::InvalidOperation))
                    };

                    let left_type = self.get_type_from_expression(on_type, &prev_expr, context)?.into_owned();
                    let right_type = self.read_type()?;

                    required_operator = !required_operator;

                    if left_type.is_any() {
                        Expression::ForceType(Box::new(prev_expr), right_type)
                    } else {
                        if !left_type.is_castable_to(&right_type) {
                            return Err(err!(self, ParserErrorKind::CastError(left_type, right_type)))
                        }
    
                        if !right_type.is_primitive() {
                            return Err(err!(self, ParserErrorKind::CastPrimitiveError(left_type, right_type)))
                        }
        
                        Expression::Cast(Box::new(prev_expr), right_type)
                    }
                },
                Token::SemiColon => { // Force the parser to recognize a valid semicolon placement, or cut its losses and return an error
                    if !queue.is_empty() {
                        break;
                    } else {
                        return Err(err!(self, ParserErrorKind::UnexpectedToken(Token::SemiColon)))
                    }
                },
                token => {
                    if token.is_type() {
                        self.read_type_constant(token, context)?
                    } else if token == Token::BraceOpen {
                        let (key, value) = if let Some(Type::Map(key, value)) = expected_type {
                            (Some(*key.clone()), Some(*value.clone()))
                        } else {
                            // If its an assignation
                            // variable path is on first
                            match queue.first() {
                                Some(QueueItem::Expression(expr)) => match self.get_type_from_expression_internal(None, expr, context)?
                                    .map(|v| v.into_owned()) {
                                        Some(Type::Map(key, value)) => (Some(*key), Some(*value)),
                                        _ => (None, None)
                                }
                                _ => (None, None)
                            }
                        };

                        self.read_map_constructor(key, value, context)?
                    } else {
                        let op = match Operator::value_of(&token) {
                            Some(op) => op,
                            None => {
                                return Err(err!(self, ParserErrorKind::OperatorNotFound(token)))
                            }
                        };

                        while let Some(top_op) = operator_stack.pop() {
                            if top_op.precedence().0 > op.precedence().0 || (top_op.precedence().0 == op.precedence().0 && op.is_left_to_right()) {
                                queue.push(QueueItem::Operator(top_op));
                            } else {
                                operator_stack.push(top_op);
                                break;
                            }
                        }

                        operator_stack.push(op);

                        if token == Token::OperatorAssign {
                            last_assign_queue_size = queue.len();
                            last_assign_operator_stack_size = operator_stack.len();
                        }

                        queue.push(QueueItem::Separator);
                        required_operator = false;
                        continue;
                    }
                }
            };
            queue.push(QueueItem::Expression(expr));
            required_operator = !required_operator;
        }

        trace!("pre-final queue {:?}", queue);
        let queue = queue.into_iter()
            .chain(
                operator_stack.drain(..)
                .rev()
                .map(|v| QueueItem::Operator(v))
            );

        // Process the postfix arithmetic that was generated
        let mut collapsed_expr = self.try_postfix_collapse(
            queue,
            context,
        )?;

        // Discard any subsequent semicolons directly adjacent to the current token
        // This is a valid parsing procedure because this point is only reached if
        // There is already enough data to safely parse a completed expression.
        //
        // Semicolons will never have unhandled data that is relevant left
        while self.peek_is(Token::SemiColon) {
            self.advance()?;
        };

        Ok(self.try_convert_expr_to_value(&mut collapsed_expr)
            .map(|constant| Expression::Constant(constant))
            .unwrap_or(collapsed_expr))
    }

    fn try_map_expr_to_type(&self, expr: &mut Expression, expected_type: &Type) -> Result<bool, ParserError<'a>> {
        if expected_type.is_generic() {
            return Ok(true)
        }

        if let Expression::Constant(v) = expr {
            let taken = mem::take(v); 
            *v = taken.checked_cast_to_primitive_type(expected_type)
                .map_err(|e| err!(self, e.into()))?;

            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn verify_operator(&self, op: &Operator, left_type: Type, right_type: Type, left_expr: &mut Expression, right_expr: &mut Expression) -> Result<(), ParserError<'a>> {
        match op {
            Operator::Sub
            | Operator::Mod
            | Operator::Div
            | Operator::Mul

            | Operator::BitwiseXor
            | Operator::BitwiseAnd
            | Operator::BitwiseOr
            | Operator::BitwiseShl
            | Operator::BitwiseShr

            | Operator::Gt
            | Operator::Lt
            | Operator::Lte
            | Operator::Gte => {
                if left_type != right_type {
                    let throw = !self.try_map_expr_to_type(left_expr, &right_type)?
                        && !self.try_map_expr_to_type(right_expr, &left_type)?;

                    if throw {
                        return Err(err!(self, ParserErrorKind::InvalidOperationNotSameType(left_type, right_type)))
                    }
                }
            }
            Operator::Add => {
                if left_type != right_type && !(left_type == Type::String || right_type == Type::String) {
                    let throw = !self.try_map_expr_to_type(left_expr, &right_type)?
                        && !self.try_map_expr_to_type(right_expr, &left_type)?;

                    if throw {
                        return Err(err!(self, ParserErrorKind::InvalidOperationNotSameType(left_type, right_type)))
                    }
                }
            },
            Operator::And | Operator::Or => {
                if left_type != Type::Bool {
                    return Err(err!(self, ParserErrorKind::InvalidOperationNotSameType(left_type, Type::Bool)))
                }

                if right_type != Type::Bool {
                    return Err(err!(self, ParserErrorKind::InvalidOperationNotSameType(right_type, Type::Bool)))
                }
            },
            Operator::Pow => {
                if !left_type.is_number() || !right_type.is_number() {
                    return Err(err!(self, ParserErrorKind::InvalidOperationNotSameType(left_type, right_type)))
                }

                if right_type != Type::U32 {
                    // Try to convert the right expression to a u32
                    if !self.try_map_expr_to_type(right_expr, &Type::U32)? {
                        return Err(err!(self, ParserErrorKind::InvalidOperationNotSameType(left_type, right_type)))
                    }
                }
            },
            Operator::Eq
            | Operator::Neq => {
                if left_type != right_type {
                    let throw = !self.try_map_expr_to_type(left_expr, &right_type)?
                        && !self.try_map_expr_to_type(right_expr, &left_type)?;

                    if throw {
                        return Err(err!(self, ParserErrorKind::InvalidOperationNotSameType(left_type, right_type)))
                    }
                }
            },
            Operator::Assign(None) => {
                if !left_type.is_assign_compatible_with(&right_type) {
                    let throw = !self.try_map_expr_to_type(left_expr, &right_type)?
                        && !self.try_map_expr_to_type(right_expr, &left_type)?;

                    if throw {
                        return Err(err!(self, ParserErrorKind::InvalidOperationNotSameType(left_type, right_type)))
                    }
                }
            },
            Operator::Assign(Some(inner)) => self.verify_operator(inner, left_type, right_type, left_expr, right_expr)?,
        };

        Ok(())
    }

    // Read a map constructor with the following syntax:
    // { key1: value1, key2: value2 }
    // The keys must be of the same type and the values must be of the same type
    // you can use direct values like { "hello": "world" }
    fn read_map_constructor(&mut self, mut key_type: Option<Type>, mut value_type: Option<Type>, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        trace!("Read map constructor {:?} {:?}", key_type, value_type);

        let mut expressions: Vec<(Expression, Expression)> = Vec::new();
        while self.peek_is_not(Token::BraceClose) {
            let mut key = self.read_expr(Some(&Token::Colon), None, true, true, key_type.as_ref(), context)?;
            let expr_type = self.get_type_from_expression_internal(None, &key, context)?
                .map(Cow::into_owned);

            if let Some(ty) = &key_type {
                self.verify_type_compatibility(&mut key, expr_type.as_ref(), ty, true)?;
            } else {
                key_type = expr_type;
            }

            self.expect_token(Token::Colon)?;
            let mut value = self.read_expr(None, None, true, true, value_type.as_ref(), context)?;
            let expr_type = self.get_type_from_expression_internal(None, &value, context)?
                .map(Cow::into_owned);

            if let Some(ty) = &value_type {
                self.verify_type_compatibility(&mut value, expr_type.as_ref(), ty, true)?;
            } else {
                value_type = expr_type;
            }

            expressions.push((key, value));

            if self.peek_is(Token::Comma) {
                self.expect_token(Token::Comma)?;
            }
        }

        let (key, value) = match (key_type, value_type) {
            (Some(k), Some(v)) => (k, v),
            _ => return Err(err!(self, ParserErrorKind::NoValueType))
        };

        self.expect_token(Token::BraceClose)?;
        Ok(Expression::MapConstructor(expressions, key, value))
    }

    /**
     * {
     *     ...
     * }
     */
    fn read_body(&mut self, context: &mut Context<'a>, return_type: &Option<Type>) -> Result<Vec<Statement>, ParserError<'a>> {
        context.begin_scope();
        let statements = self.read_statements(context, return_type)?;
        context.end_scope();
        Ok(statements)
    }

    // Read a variable declaration
    fn read_variable_internal(&mut self, context: &mut Context<'a>, is_const: bool) -> Result<(&'a str, Type, Expression, bool), ParserError<'a>> {
        let name: &'a str = self.next_identifier()?;
        trace!("Read variable: {}", name);

        // Constants must be uppercase
        if is_const && name.to_uppercase() != name {
            return Err(err!(self, ParserErrorKind::ConstantNameNotUppercase(name)))
        }

        let ignored = name == IGNORE_VARIABLE;

        // Variable name must start with a alphabetic character
        if !ignored && !name.starts_with(char::is_alphabetic) {
            return Err(err!(self, ParserErrorKind::VariableMustStartWithAlphabetic(name)))
        }

        if ignored && is_const {
            return Err(err!(self, ParserErrorKind::InvalidConstName(name)))
        }

        self.expect_token(Token::Colon)?;
        let value_type = self.read_type()?;
        let value: Expression = if self.peek_is(Token::OperatorAssign) {
            self.expect_token(Token::OperatorAssign)?;
            let expr = self.read_expr(None, None, true, true, Some(&value_type), context)?;

            let expr_type = match self.get_type_from_expression_internal(None, &expr, context) {
                Ok(opt_type) => match opt_type {
                    Some(v) => v,
                    None => if value_type.contains_sub_type() {
                        Cow::Owned(value_type.clone())
                    } else {
                        return Err(err!(self, ParserErrorKind::NoValueType))
                    }
                },
                Err(e) => match e.kind { // support empty array declaration
                    ParserErrorKind::EmptyArrayConstructor if value_type.support_array_call() => Cow::Owned(value_type.clone()),
                    _ => return Err(e)
                }
            };

            // Don't allow the reverse to prevent that example:
            // let _: bool = <optional bool>
            if !value_type.is_assign_compatible_with(&expr_type) {
                return Err(err!(self, ParserErrorKind::InvalidValueType(expr_type.into_owned(), value_type)))
            }

            expr
        } else if value_type.is_optional() {
            Expression::Constant(Constant::Default(Primitive::Null))
        } else {
            return Err(err!(self, ParserErrorKind::NoValueForVariable(name)))
        };

        Ok((name, value_type, value, ignored))
    }

    fn read_tuple_pattern(&mut self, variables: &mut HashSet<&'a str>) -> Result<TuplePattern<'a>, ParserError<'a>> {
        if self.peek_is(Token::ParenthesisOpen) {
            self.expect_token(Token::ParenthesisOpen)?;
            let mut elements = Vec::new();
            loop {
                elements.push(self.read_tuple_pattern(variables)?);
    
                if self.peek_is(Token::ParenthesisClose) {
                    break;
                } else {
                    self.expect_token(Token::Comma)?;
                }
            }

            self.expect_token(Token::ParenthesisClose)?;
            Ok(TuplePattern::Tuple(elements))
        } else {
            let name = self.next_identifier()?;
            if name == IGNORE_VARIABLE {
                Ok(TuplePattern::Ignore)
            } else {
                if !name.starts_with(char::is_alphabetic) {
                    return Err(err!(self, ParserErrorKind::VariableMustStartWithAlphabetic(name)));
                }

                if !variables.insert(name) {
                    return Err(err!(self, ParserErrorKind::VariableNameAlreadyUsed(name)));
                }

                Ok(TuplePattern::Variable(name))
            }
        }
    }

    fn match_pattern_with_type(
        &self,
        pattern: &TuplePattern<'a>,
        ty: &Type,
        context: &mut Context<'a>,
        statements: &mut Vec<TupleStatement>,
    ) -> Result<(), ParserError<'a>> {
        match (pattern, ty) {
            (TuplePattern::Ignore, _) => {
                statements.push(TupleStatement::Deconstruct(TupleDeconstruction { id: None, value_type: ty.clone() }));
                Ok(())
            }
            (TuplePattern::Variable(name), _) => {
                let id = if self.disable_shadowing_variables {
                    context.register_variable(name, ty.clone())
                        .ok_or_else(|| err!(self, ParserErrorKind::VariableNameAlreadyUsed(name)))?
                } else {
                    context.register_variable_unchecked(name, ty.clone())
                };
                statements.push(TupleStatement::Deconstruct(TupleDeconstruction { id: Some(id), value_type: ty.clone() }));
                Ok(())
            }
            (TuplePattern::Tuple(sub_patterns), Type::Tuples(sub_types)) => {
                if sub_patterns.len() != sub_types.len() {
                    return Err(err!(self, ParserErrorKind::InvalidTupleDeconstruction));
                }

                statements.push(TupleStatement::Depth);
                for (p, t) in sub_patterns.iter().zip(sub_types).rev() {
                    self.match_pattern_with_type(p, t, context, statements)?;
                }
                Ok(())
            }
            _ => Err(err!(self, ParserErrorKind::InvalidTupleType)),
        }
    }
    
    /*
     * Read a tuple deconstruction
     * Example: let (a, b): (u64, u64) = (1, 2);
     */
    fn read_tuples_deconstruction(&mut self, context: &mut Context<'a>) -> Result<Statement, ParserError<'a>> {
        let pattern = self.read_tuple_pattern(&mut HashSet::new())?;
        self.expect_token(Token::Colon)?;
        let expected_type = self.read_type()?;

        self.expect_token(Token::OperatorAssign)?;
        let value = self.read_expr(None, None, true, true, Some(&expected_type), context)?;
        let value_type = self.get_type_from_expression(None, &value, context)?;

        if *value_type != expected_type {
            return Err(err!(self, ParserErrorKind::InvalidValueType(value_type.into_owned(), expected_type)));
        }

        let mut statements = Vec::new();

        self.match_pattern_with_type(&pattern, &expected_type, context, &mut statements)?;

        Ok(Statement::TuplesDeconstruction(value, statements))
    }

    // Register a variable based on the shadow config
    fn register_variable(&self, context: &mut Context<'a>, name: &'a str, value_type: Type) -> Result<IdentifierType, ParserError<'a>> {
        if self.disable_shadowing_variables {
            context.register_variable(name, value_type)
                .ok_or_else(|| err!(self, ParserErrorKind::VariableNameAlreadyUsed(name)))
        } else {
            Ok(context.register_variable_unchecked(name, value_type))
        }
    }

    /**
     * Example: let hello: string = "hello";
     * Rules:
     * - Every variable must be declared with 'let' keyword
     * - Variable name must be alphanumeric characters
     * - Must provide a value type
     * - If no value is set, Null is set by default
     */
    fn read_variable(&mut self, context: &mut Context<'a>) -> Result<DeclarationStatement, ParserError<'a>> {
        let (name, value_type, value, ignored) = self.read_variable_internal(context, false)?;
        let id = if !ignored {
            Some(self.register_variable(context, name, value_type.clone())?)
        } else {
            None
        };

        Ok(DeclarationStatement {
            id,
            value_type,
            value
        })
    }

    // Read a constant declaration
    fn read_const(&mut self, context: &mut Context<'a>) -> Result<(), ParserError<'a>> {
        let (name, value_type, mut value, _) = self.read_variable_internal(context, true)?;

        let const_value = self.try_convert_expr_to_value(&mut value)
                .ok_or(err!(self, ParserErrorKind::InvalidConstantValue))?;

        self.constants.insert(name, ConstantDeclaration {
            value: const_value,
            value_type
        });

        Ok(())
    }

    fn read_loop_body(&mut self, context: &mut Context<'a>, return_type: &Option<Type>) -> Result<Vec<Statement>, ParserError<'a>> {
        // support nested loop
        let old_value = context.is_in_a_loop();
        context.set_in_a_loop(true);
        self.expect_token(Token::BraceOpen)?;
        let statements = self.read_body(context, return_type)?;
        context.set_in_a_loop(old_value);

        Ok(statements)
    }

    // Read a single statement
    fn read_statement(&mut self, context: &mut Context<'a>, return_type: &Option<Type>) -> Result<Option<Statement>, ParserError<'a>> {
        if let Some(token) = self.next() {
            trace!("statement token: {:?}", token);
            let statement: Statement = match token {
                Token::BraceClose => return Ok(None),
                Token::For => { // Example: for i: u64 = 0; i < 10; i += 1 {}
                    context.begin_scope();
                    let var = self.read_variable(context)?;
                    if var.id.is_none() {
                        return Err(err!(self, ParserErrorKind::ExpectedVariableDeclaration))
                    }

                    let condition = self.read_expression_delimited(&Token::SemiColon, context)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if  *condition_type != Type::Bool {
                        return Err(err!(self, ParserErrorKind::InvalidCondition(condition_type.into_owned(), condition)))
                    }

                    let increment = self.read_expression(context)?;
                    match &increment { // allow only assignations on this expr
                        Expression::Operator(op, _, _) if op.is_assignation() => {},
                        _ => return Err(err!(self, ParserErrorKind::InvalidForExpression(increment)))
                    };

                    let statements = self.read_loop_body(context, return_type)?;
                    context.end_scope();

                    Statement::For(var, condition, increment, statements)
                }
                Token::ForEach => { // Example: foreach a in array {}
                    context.begin_scope();
                    let variable = self.next_identifier()?;
                    self.expect_token(Token::In)?;
                    let expr = self.read_expression(context)?;
                    let expr_type = self.get_type_from_expression(None, &expr, context)?;

                    // verify that we can iter on it
                    if !expr_type.is_iterable() {
                        return Err(err!(self, ParserErrorKind::NotIterable(expr_type.into_owned())))
                    }

                    let id = self.register_variable(context, variable, expr_type.get_inner_type().clone())?;
                    let statements = self.read_loop_body(context, return_type)?;
                    context.end_scope();

                    Statement::ForEach(id, expr, statements)
                },
                Token::While => { // Example: while i < 10 {}
                    let condition = self.read_expression_delimited(&Token::BraceOpen, context)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if  *condition_type != Type::Bool {
                        return Err(err!(self, ParserErrorKind::InvalidCondition(condition_type.into_owned(), condition)))
                    }

                    let statements = self.read_loop_body(context, return_type)?;

                    Statement::While(condition, statements)
                },
                Token::If => {
                    let condition = self.read_expression(context)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if *condition_type != Type::Bool {
                        return Err(err!(self, ParserErrorKind::InvalidCondition(condition_type.into_owned(), condition)))
                    }

                    self.expect_token(Token::BraceOpen)?;
                    let body = self.read_body(context, return_type)?;
                    let else_statement = if self.peek_is(Token::Else) {
                        self.advance()?;
                        Some(if self.peek_is(Token::If) {
                            let statement = self.read_statement(context, return_type)?;
                            vec![statement.ok_or(err!(self, ParserErrorKind::UnexpectedToken(Token::If)))?]
                        } else {
                            self.expect_token(Token::BraceOpen)?;
                            self.read_body(context, return_type)?
                        })
                    } else {
                        None
                    };

                    Statement::If(condition, body, else_statement)
                },
                Token::BraceOpen => Statement::Scope(self.read_body(context, return_type)?),
                Token::Let => {
                    if self.peek_is(Token::ParenthesisOpen) {
                        self.read_tuples_deconstruction(context)?
                    } else {
                        Statement::Variable(self.read_variable(context)?)
                    }
                }
                Token::Return => {
                    let opt: Option<Expression> = if let Some(return_type) = return_type {
                        let expr = self.read_expr(None, None, true, true, Some(return_type), context)?;
                        if let Some(expr_type) = self.get_type_from_expression_internal(None, &expr, context)? {
                            // Support the optional<T> by returning T
                            if !return_type.is_assign_compatible_with(&expr_type) {
                                return Err(err!(self, ParserErrorKind::InvalidValueType(expr_type.into_owned(), return_type.clone())))
                            }
                        }

                        Some(expr)
                    } else {
                        None
                    };

                    // we can't have anything after a return
                    if self.peek_is_not(Token::BraceClose) && self.peek_is_not(Token::Comma) {
                        return Err(err!(self, ParserErrorKind::NoCodeAfterReturn));
                    }

                    Statement::Return(opt)
                }
                Token::Continue => {
                    if !context.is_in_a_loop() {
                        return Err(err!(self, ParserErrorKind::UnexpectedToken(Token::Continue)));
                    }

                    // we can't have anything after a continue
                    if self.peek_is_not(Token::BraceClose) && self.peek_is_not(Token::Comma) {
                        return Err(err!(self, ParserErrorKind::NoCodeAfterContinue));
                    }

                    Statement::Continue
                },
                Token::Break => {
                    if !context.is_in_a_loop() {
                        return Err(err!(self, ParserErrorKind::UnexpectedToken(Token::Break)));
                    }

                    // we can't have anything after a break
                    if self.peek_is_not(Token::BraceClose) && self.peek_is_not(Token::Comma) {
                        return Err(err!(self, ParserErrorKind::NoCodeAfterBreak));
                    }

                    Statement::Break
                },
                Token::Match => {
                    context.begin_scope();
                    let expr = self.read_expression(context)?;
                    let expr_ty = self.get_type_from_expression(None, &expr, context)?
                        .into_owned();

                    if !expr_ty.is_enum() && !expr_ty.is_primitive() {
                        return Err(err!(self, ParserErrorKind::InvalidTypeMatch))
                    }

                    self.expect_token(Token::BraceOpen)?;
                    let mut unique_patterns = HashSet::new();
                    let mut patterns = Vec::new();
                    let mut default_case = None;
                    loop {
                        if default_case.is_none() {
                            context.set_in_a_match(Some(expr_ty.clone()));
                        }

                        let pattern = self.read_expression_expected_type(context, &expr_ty)?;

                        let is_consumed = default_case.is_none() && context.get_match_on_type().is_none();
                        if !is_consumed {
                            context.set_in_a_match(None);
                        }

                        let ty = self.get_type_from_expression(None, &pattern, context)?;
                        if *ty != expr_ty {
                            if let Type::Range(inner) = ty.as_ref() {
                                if **inner != expr_ty {
                                    return Err(err!(self, ParserErrorKind::ExpectedMatchingType))        
                                }
                            } else {
                                return Err(err!(self, ParserErrorKind::ExpectedMatchingType))
                            }
                        }

                        self.expect_token(Token::FatArrow)?;
                        let body = self.read_statement(context, &return_type)?
                            .ok_or_else(|| err!(self, ParserErrorKind::ExpectedBodyPatternMatch))?;

                        if is_consumed {
                            default_case = Some(Box::new(body));
                        } else {
                            patterns.push((pattern.clone(), body));
                            if !unique_patterns.insert(pattern) {
                                return Err(err!(self, ParserErrorKind::MatchPatternDuplicated))
                            }
                        }

                        if self.peek_is(Token::Comma) {
                            self.expect_token(Token::Comma)?;
                        } else {
                            break;
                        }
                    }

                    self.expect_token(Token::BraceClose)?;

                    context.end_scope();

                    Statement::Match(Box::new(expr), patterns, default_case, None)
                },
                token => {
                    self.push_back(token);
                    Statement::Expression(self.read_expression(context)?)
                }
            };
            Ok(Some(statement))
        } else {
            Ok(None)
        }

    }

    // Read all statements in a block
    // return type is used to verify that the last statement is a return with a valid value type
    // consume_brace is used to know if we should consume the open brace
    fn read_statements(&mut self, context: &mut Context<'a>, return_type: &Option<Type>) -> Result<Vec<Statement>, ParserError<'a>> {
        trace!("Read statements");
        let mut statements: Vec<Statement> = Vec::new();
        while let Some(statement) = self.read_statement(context, return_type)? {
            trace!("statement: {:?}", statement);
            statements.push(statement);
        }

        Ok(statements)
    }

    // Read the parameters for a function
    fn read_parameters(&mut self) -> Result<Vec<(&'a str, Type)>, ParserError<'a>> {
        let mut parameters = Vec::new();
        while self.peek_is_identifier() {
            let name = self.next_identifier()?;
            self.expect_token(Token::Colon)?;
            let value_type = self.read_type()?;

            trace!("Read parameter: `{}: {:?}`", name, value_type);
            parameters.push((name, value_type));

            if self.peek_is_not(Token::Comma) {
                break;
            }

            self.expect_token(Token::Comma)?;
        }

        Ok(parameters)
    }

    // Verify that the last statement is a return
    // We don't check the last statement directly has it would allow
    // to have dead code after a return
    fn ends_with_return(statements: &Vec<Statement>) -> Result<bool, ParserError<'a>> {
        let mut ok = false;
        if let Some(statement) = statements.last() {
            match statement {
                Statement::If(_, statements, else_statements) => {
                    // if its the last statement
                    ok = Self::ends_with_return(&statements)?;
                    // if it ends with a return, else must also end with a return
                    if let Some(statements) = else_statements.as_ref().filter(|_| ok) {
                        ok = Self::ends_with_return(statements)?;
                    } else {
                        ok = false;
                    }
                }
                Statement::Return(Some(_)) => {
                    ok = true;
                },
                _ => {}
            }
        }

        Ok(ok)
    }

    // Verify if we allow function declaration on a type
    fn allow_fn_declaration_on_type(&self, ty: &Type) -> bool {
        match ty {
            Type::Struct(ty) => self.global_mapper.structs().is_defined(ty),
            Type::Enum(ty) => self.global_mapper.enums().is_defined(ty),
            _ => false
        }
    }

    /**
     * Examples:
     * - entry foo() { ... }
     * - fn foo() { ... }
     * - fn foo() -> u64 { ... }
     * - fn foo(a: u64, b: u64) { ... }
     * - fn (f Foo) bar() { ... }
     * Rules:
     * - Signature is based on function name, and parameters
     * - Entry function is a "public callable" function and must return a u64 value
     */
    fn read_function(&mut self, kind: FunctionKind, context: &mut Context<'a>) -> Result<(), ParserError<'a>> {
        trace!("Read function");
        context.begin_scope();

        let token = self.advance()?;
        let (instance_name, for_type, name) = if kind.is_normal() && token == Token::ParenthesisOpen {
            let instance_name = self.next_identifier()?;
            let for_type = self.read_type()?;

            if !self.allow_fn_declaration_on_type(&for_type) {
                return Err(err!(self, ParserErrorKind::InvalidFunctionType(for_type)))
            }

            let id = context.register_variable(instance_name, for_type.clone())
                .ok_or_else(|| err!(self, ParserErrorKind::VariableNameAlreadyUsed(instance_name)))?;

            self.expect_token(Token::ParenthesisClose)?;

            (Some(id), Some(for_type), self.next_identifier()?)
        } else {
            let Token::Identifier(name) = token else {
                return Err(err!(self, ParserErrorKind::ExpectedIdentifierToken(token)))
            };
            (None, None, name)
        };

        self.expect_token(Token::ParenthesisOpen)?;
        let parameters = self.read_parameters()?;
        self.expect_token(Token::ParenthesisClose)?;

        if parameters.len() + for_type.is_some() as usize > u8::MAX as usize {
            return Err(err!(self, ParserErrorKind::TooManyParameters))
        }

        // an entrypoint cannot be a method
        if kind.is_entry() && for_type.is_some() {
            return Err(err!(self, ParserErrorKind::EntryFunctionCannotHaveForType))
        }

        // all entries must return a u64 value without being specified
        let return_type: Option<Type> = if let Some(forced_type) = self.entry_forced_return_type.clone().filter(|_| kind.is_entry()) {
            forced_type
        } else if self.peek_is(Token::ReturnType) { // read returned type
            self.advance()?;
            Some(self.read_type()?)
        } else {
            None
        };

        // Check that the function name is a known hook
        let mut hook_id = None;
        if kind.is_hook() {
            let hook = self.environment.get_hooks().get(name)
                .ok_or_else(|| err!(self, ParserErrorKind::UnknownHook(name)))?;

            // Verify that the params len are correct
            if parameters.len() != hook.parameters.len() {
                return Err(err!(self, ParserErrorKind::InvalidHookParameters(name, parameters.len(), hook.parameters.len())));
            }

            // Verify their types
            for ((_, got), (_, expected)) in parameters.iter().zip(hook.parameters.iter()) {
                if got != expected {
                    return Err(err!(self, ParserErrorKind::InvalidHookParameter(name, got.clone(), expected.clone())));
                }
            }

            // Verify the return type
            if return_type != hook.return_type {
                return Err(err!(self, ParserErrorKind::InvalidHookReturnType(name, return_type, hook.return_type.clone())));
            }

            // Check that this hook isn't already registered
            for f in self.functions.iter() {
                if let FunctionType::Hook(h) = f {
                    if h.hook_id() == hook.hook_id {
                        return Err(err!(self, ParserErrorKind::DuplicatedHook(name, hook.hook_id)))
                    }
                }
            }

            hook_id = Some(hook.hook_id);
        }

        let id = self.global_mapper
            .functions_mut()
            .register(name, for_type.clone(), instance_name.is_some(), parameters.clone(), return_type.clone())
            .map_err(|e| err!(self, e.into()))?;

        if self.has_function(id) {
            return Err(err!(self, ParserErrorKind::FunctionSignatureAlreadyExist)) 
        }

        let has_return_type = return_type.is_some();
        let mut new_params = Vec::with_capacity(parameters.len());
        for (name, param_type) in parameters {
            let id = context.register_variable(name, param_type.clone())
                .ok_or_else(|| err!(self, ParserErrorKind::VariableNameAlreadyUsed(name)))?;
            new_params.push(Parameter::new(id, param_type));
        }

        let function = match kind {
            FunctionKind::Entry => FunctionType::Entry(EntryFunction::new(new_params, Vec::new(), context.max_variables_count() as u16)),
            FunctionKind::Declared => FunctionType::Declared(DeclaredFunction::new(
                for_type,
                instance_name,
                new_params,
                Vec::new(),
                return_type.clone(),
                0
            )),
            FunctionKind::Hook => FunctionType::Hook(HookFunction::new(
                new_params,
                Vec::new(),
                return_type.clone(),
                0,
                hook_id.unwrap()
            ))
        };

        // push function before reading statements to allow recursive calls
        self.functions.push(function);

        self.expect_token(Token::BraceOpen)?;
        let statements = self.read_body(context, &return_type)?;
        context.end_scope();

        // verify that the function ends with a return
        if has_return_type && !Self::ends_with_return(&statements)? {
            return Err(err!(self, ParserErrorKind::NoReturnFound))
        }

        let last = self.functions
            .last_mut()
            .ok_or(err!(self, ParserErrorKind::UnknownError))?;

        last.set_statements(statements);
        last.set_max_variables_count(context.max_variables_count() as u16);

        Ok(())
    }

    // Read a type with the following syntax:
    // import "filename.xel";
    // or with an alias:
    // import "filename.xel" as alias;
    fn read_import(&mut self) -> Result<(), ParserError<'a>> {
        trace!("Read import");

        let path = self.advance()?;

        let Token::Value(Literal::String(path)) = path else {
            return Err(err!(self, ParserErrorKind::InvalidImport))
        };

        // We don't allow absolute path or path that contains ".."
        if path.starts_with("/") || path.contains("..") {
            return Err(err!(self, ParserErrorKind::InvalidImportPath(path)))
        }

        // If its a local import, we will import its content directly
        let is_local = path.ends_with(".xel");
        if !is_local {
            return Err(err!(self, ParserErrorKind::NotImplemented))
        }

        Ok(())
    }

    // check if a function with the same signature exists
    fn has_function(&self, id: u16) -> bool {
        self.get_function(id).is_ok()
    }

    // get a function using its identifier
    fn get_function<'b>(&'b self, id: u16) -> Result<Function<'b>, ParserError<'a>> {
        // the id is the index of the function in the functions array
        let index = id as usize;
        let len = self.environment.get_functions().len();
        if index < len {
            Ok(Function::Native(&self.environment.get_functions()[index]))
        } else {
            match self.functions.get(index - len) {
                Some(func) => Ok(Function::Program(func)),
                None => Err(err!(self, ParserErrorKind::FunctionNotFound))
            }
        }
    }

    // Verify that a type name is not already used
    fn is_name_available(&self, name: &str) -> bool {
        trace!("Check if name is available: {}", name);
        self.global_mapper.structs().get_by_name(name).is_err()
            && self.global_mapper.enums().get_by_name(name).is_err()
    }

    /**
     * Example: Message { message_id: u64, message: string }
     * Rules:
     * - Structure name should start with a uppercase (ascii alphabet) character
     */
    fn read_struct(&mut self) -> Result<(), ParserError<'a>> {
        let name = self.next_identifier()?;
        trace!("Read struct: {}", name);

        // Verify that we don't have a type with the same name
        if !self.is_name_available(name) {
            return Err(err!(self, ParserErrorKind::TypeNameAlreadyUsed(name)))
        }

        // check if the first letter is in uppercase
        match name.chars().nth(0) {
            Some(v) => {
                if !v.is_ascii_alphabetic() || !v.is_uppercase() {
                    return Err(err!(self, ParserErrorKind::InvalidStructureName(name)))
                }
            },
            None => return Err(err!(self, ParserErrorKind::EmptyStructName))
        };

        self.expect_token(Token::BraceOpen)?;
        let params = self.read_parameters()?;
        if params.len() > u8::MAX as usize {
            return Err(err!(self, ParserErrorKind::TooManyParameters))
        }

        let mut fields = Vec::with_capacity(params.len());
        for (name, param_type) in params {
            fields.push((name, param_type));
        }

        self.expect_token(Token::BraceClose)?;

        self.global_mapper
            .structs_mut()
            .add(Cow::Borrowed(name), fields)
            .map_err(|e| err!(self, e.into()))?;

        Ok(())
    }

    /**
     * Example: enum Message { HELLO, WORLD { a: u64 } }
     * Rules:
     * - Enum name should start with a uppercase character
     * - each variant name should start with a uppercase character
     * - each variant has a unique name
     */
    fn read_enum(&mut self) -> Result<(), ParserError<'a>> {
        let name = self.next_identifier()?;
        trace!("Read enum: {}", name);

        // Verify that we don't have a type with the same name
        if !self.is_name_available(name) {
            return Err(err!(self, ParserErrorKind::TypeNameAlreadyUsed(name)))
        }

        // check if the first letter is in uppercase
        match name.chars().nth(0) {
            Some(v) => {
                if !v.is_ascii_alphabetic() || !v.is_uppercase() {
                    return Err(err!(self, ParserErrorKind::InvalidEnumName(name)))
                }
            },
            None => return Err(err!(self, ParserErrorKind::EmptyEnumName))
        };

        self.expect_token(Token::BraceOpen)?;
        let mut variants = Vec::new();
        while self.peek_is_identifier() {
            let variant_name = self.next_identifier()?;

            // Variant name should be unique
            if variants.iter().any(|(name, _)| *name == variant_name) {
                return Err(err!(self, ParserErrorKind::EnumVariantAlreadyUsed(variant_name)))
            }

            let fields = if self.peek_is(Token::BraceOpen) {
                self.expect_token(Token::BraceOpen)?;
                let fields = self.read_parameters()?;
                if fields.len() > u8::MAX as usize {
                    return Err(err!(self, ParserErrorKind::TooManyParameters))
                }

                self.expect_token(Token::BraceClose)?;
                fields
            } else {
                Vec::new()
            };

            variants.push((variant_name, fields));

            if self.peek_is(Token::Comma) {
                self.expect_token(Token::Comma)?;
            }
        }

        if variants.len() > u8::MAX as usize {
            return Err(err!(self, ParserErrorKind::TooManyVariants))
        }

        self.expect_token(Token::BraceClose)?;

        self.global_mapper
            .enums_mut()
            .add(Cow::Borrowed(name), variants)
            .map_err(|e| err!(self, e.into()))?;

        Ok(())
    }

    // Parse the tokens and return a Program
    // The function mapper is also returned for external calls
    pub fn parse(mut self) -> Result<(Program, GlobalMapper<'a>), ParserError<'a>> {
        let mut context: Context = Context::new();
        while let Some(token) = self.next() {
            match token {
                Token::Import => {
                    self.read_import()?;
                    continue;
                }
                Token::Const => self.read_const(&mut context)?,
                Token::Function => self.read_function(FunctionKind::Declared, &mut context)?,
                Token::Entry => self.read_function(FunctionKind::Entry, &mut context)?,
                Token::Hook => self.read_function(FunctionKind::Hook, &mut context)?,
                Token::Struct => self.read_struct()?,
                Token::Enum => self.read_enum()?,
                token => return Err(err!(self, ParserErrorKind::UnexpectedToken(token)))
            };
        }

        let program = Program::with(self.constants.into_iter().map(|(_, v)| v).collect(), self.global_mapper.structs().finalize(), self.global_mapper.enums().finalize(), self.functions);
        Ok((program, self.global_mapper))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[track_caller]
    fn test_parser(tokens: Vec<Token>) -> Program {
        let env = EnvironmentBuilder::default();
        test_parser_with_env(tokens, &env)
    }

    #[track_caller]
    fn test_parser_with_env(tokens: Vec<Token>, env: &EnvironmentBuilder) -> Program {
        let parser = Parser::new(tokens, env);
        let (program, _) = parser.parse().unwrap();
        program
    }

    #[track_caller]
    fn test_parser_statement(tokens: Vec<Token>, variables: Vec<(&str, Type)>) -> Vec<Statement> {
        let env = EnvironmentBuilder::new();
        test_parser_statement_with(tokens, variables, &None, &env)
    }

    #[track_caller]
    fn test_parser_statement_with(tokens: Vec<Token>, variables: Vec<(&str, Type)>, return_type: &Option<Type>, env: &EnvironmentBuilder) -> Vec<Statement> {
        let mut parser = Parser::new(VecDeque::from(tokens), &env);
        let mut context = Context::new();
        context.begin_scope();
        for (name, t) in variables {
            context.register_variable(name, t).unwrap();
        }

        parser.read_statements(&mut context, return_type).unwrap()
    }

    #[track_caller]
    fn test_parser_statement_with_return_type(tokens: Vec<Token>, variables: Vec<(&str, Type)>, return_type: Type) -> Vec<Statement> {
        let env = EnvironmentBuilder::new();
        test_parser_statement_with(tokens, variables, &Some(return_type), &env)
    }

    #[test]
    fn test_array_upgraded_to_constant() {
        let tokens = vec![
            Token::Let,
            Token::Identifier("a"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::BracketOpen,
            Token::BracketClose,
            Token::OperatorAssign,
            Token::BracketOpen,
            Token::Value(Literal::U64(1)),
            Token::Comma,
            Token::Value(Literal::U64(2)),
            Token::BracketClose,
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert!(statements.len() == 1);
        let Statement::Variable(DeclarationStatement { value, .. }) = &statements[0] else {
            panic!("Expected a variable statement")
        };
        assert_eq!(
            *value,
            Expression::Constant(Constant::Array(vec![Primitive::U64(1).into(), Primitive::U64(2).into()]))
        )
    }

    #[test]
    fn test_shunting_yard_error() {
        // Test 3 + - 4 * 6 - * - (4 / 2)
        let tokens = vec![
            Token::Value(Literal::U64(3)),
            Token::OperatorPlus,
            Token::OperatorMinus,
            Token::Value(Literal::U64(4)),
            Token::OperatorMultiply,
            Token::Value(Literal::U64(6)),
            Token::OperatorMinus,
            Token::OperatorPlus,
            Token::OperatorMinus,
            Token::ParenthesisOpen,
            Token::Value(Literal::U64(4)),
            Token::OperatorDivide,
            Token::Value(Literal::U64(2)),
            Token::ParenthesisClose,
        ];
    
        let result = std::panic::catch_unwind(|| test_parser_statement(tokens, Vec::new()));
        assert!(result.is_err(), "Expected parsing to fail, but it succeeded.");
    }

    #[test]
    fn test_shunting_yard() {
        // Test 3 + 4 * 6 - (4 / 2)
        let tokens = vec![
            Token::Value(Literal::U64(3)),
            Token::OperatorPlus,
            Token::Value(Literal::U64(4)),
            Token::OperatorMultiply,
            Token::Value(Literal::U64(6)),
            Token::OperatorMinus,
            Token::ParenthesisOpen,
            Token::Value(Literal::U64(4)),
            Token::OperatorDivide,
            Token::Value(Literal::U64(2)),
            Token::ParenthesisClose,
        ];
    
        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(statements.len(), 1);
    
        // Build the expected AST
        let expected_ast = Statement::Expression(
            Expression::Constant(Constant::Default(xelis_types::Primitive::U64(25)))
        );

        // Compare the parsed AST to the expected AST
        assert_eq!(statements[0], expected_ast);
    }

    #[test]
    fn test_shunting_yard_nested() {
        // Test 3 + 4 * 6 - ((4 / 2) + 10 - 2)
        let tokens = vec![
            Token::Value(Literal::U64(3)),
            Token::OperatorPlus,
            Token::Value(Literal::U64(4)),
            Token::OperatorMultiply,
            Token::Value(Literal::U64(6)),
            Token::OperatorMinus,
            Token::ParenthesisOpen,
            Token::ParenthesisOpen,
            Token::Value(Literal::U64(4)),
            Token::OperatorDivide,
            Token::Value(Literal::U64(2)),
            Token::ParenthesisClose,
            Token::OperatorPlus,
            Token::Value(Literal::U64(10)),
            Token::OperatorMinus,
            Token::Value(Literal::U64(2)),
            Token::ParenthesisClose,
        ];
    
        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(statements.len(), 1);
    
        // Build the expected AST
        let expected_ast = Statement::Expression(
            Expression::Constant(Constant::Default(xelis_types::Primitive::U64(25 - 8)))
        );

        // Compare the parsed AST to the expected AST
        assert_eq!(statements[0], expected_ast);
    }

    #[test]
    fn test_shunting_yard_constant() {
        // Test 3 + u8::MAX as u64 + 4 * 6 - (4 / 2)
        let tokens = vec![
            Token::Value(Literal::U64(3)),
            Token::OperatorPlus,
            Token::Number(NumberType::U8),
            Token::Colon,
            Token::Colon,
            Token::Identifier("MAX"),
            Token::As,
            Token::Number(NumberType::U64),
            Token::OperatorPlus,
            Token::Value(Literal::U64(4)),
            Token::OperatorMultiply,
            Token::Value(Literal::U64(6)),
            Token::OperatorMinus,
            Token::ParenthesisOpen,
            Token::Value(Literal::U64(4)),
            Token::OperatorDivide,
            Token::Value(Literal::U64(2)),
            Token::ParenthesisClose,
        ];
    
        let mut env = EnvironmentBuilder::new();
        env.register_constant(Type::U8, "MAX", Primitive::U8(u8::MAX).into());

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 1);
    
        // Build the expected AST
        let expected_ast = Statement::Expression(
            Expression::Constant(Constant::Default(xelis_types::Primitive::U64(25+255)))
        );

        // Compare the parsed AST to the expected AST
        assert_eq!(statements[0], expected_ast);
    }

    #[test]
    fn test_shunting_yard_loop() {
        // Test for i = 0; i < 10 + 3*2; i+= 1 {}
        let tokens = vec![
            Token::For,
            Token::Identifier("i"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::OperatorAssign,
            Token::Value(Literal::U64(0)),
            Token::SemiColon,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::OperatorPlus,
            Token::Value(Literal::U64(3)),
            Token::OperatorMultiply,
            Token::Value(Literal::U64(2)),
            Token::SemiColon,
            Token::Identifier("i"),
            Token::OperatorPlusAssign,
            Token::Value(Literal::U64(1)),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_shunting_yard_while() {
        // Test while i < 10 - 2 / 2 {}
        let tokens = vec![
            Token::While,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::OperatorMinus,
            Token::Value(Literal::U64(2)),
            Token::OperatorDivide,
            Token::Value(Literal::U64(2)),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_semicolon_optional() {
        // Test let blank: optional<bool> = null;
        let tokens = vec![
            Token::Let,
            Token::Identifier("blank"),
            Token::Colon,
            Token::Optional,
            Token::OperatorLessThan,
            Token::Bool,
            Token::OperatorGreaterThan,
            Token::OperatorAssign,
            Token::Value(Literal::Null),
            Token::SemiColon,
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(
            statements[0],
            Statement::Variable(
                DeclarationStatement {
                    id: Some(0),
                    value_type: Type::Optional(Box::new(Type::Bool)),
                    value: Expression::Constant(Primitive::Null.into())
                }
            )
        );
    }

    #[test]
    fn test_semicolon_ignore() {
        // Test let blank: optional<bool> = null;;
        let tokens = vec![
            Token::Let,
            Token::Identifier("blank"),
            Token::Colon,
            Token::Optional,
            Token::OperatorLessThan,
            Token::Bool,
            Token::OperatorGreaterThan,
            Token::OperatorAssign,
            Token::Value(Literal::Null),
            Token::SemiColon,
            Token::SemiColon,
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(
            statements[0],
            Statement::Variable(
                DeclarationStatement {
                    id: Some(0),
                    value_type: Type::Optional(Box::new(Type::Bool)),
                    value: Expression::Constant(Primitive::Null.into())
                }
            )
        );
    }

    #[test]
    fn test_constant() {
        let tokens = vec![
            Token::Const,
            Token::Identifier("A"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::OperatorAssign,
            Token::Value(Literal::U64(10)),
        ];

        let program = test_parser(tokens);
        let constants = program.constants();
        assert_eq!(constants.len(), 1);

        let constant = constants.iter().next().unwrap();

        assert_eq!(constant.value_type, Type::U64);
        assert_eq!(constant.value, Primitive::U64(10).into());
    }

    #[test]
    fn test_range() {
        let tokens = vec![
            Token::Let,
            Token::Identifier("a"),
            Token::Colon,

            Token::Range,
            Token::OperatorLessThan,
            Token::Number(NumberType::U64),
            Token::OperatorGreaterThan,
            Token::OperatorAssign,

            Token::Value(Literal::U64(0)),
            Token::Dot,
            Token::Dot,
            Token::Value(Literal::U64(10)),
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(
            statements[0],
            Statement::Variable(
                DeclarationStatement {
                    id: Some(0),
                    value_type: Type::Range(Box::new(Type::U64)),
                    value: Expression::Constant(
                        Primitive::Range(
                            Box::new((Primitive::U64(0), Primitive::U64(10)))
                        ).into()
                    )
                }
            )
        );
    }

    #[test]
    fn test_double_optional() {
        // let a: optional<optional<u64>> = null;
        let tokens = vec![
            Token::Let,
            Token::Identifier("a"),
            Token::Colon,
            Token::Optional,
            Token::OperatorLessThan,
            Token::Optional,
            Token::OperatorLessThan,
            Token::Number(NumberType::U64),
            Token::OperatorGreaterThan,
            Token::OperatorGreaterThan,
            Token::OperatorAssign,
            Token::Value(Literal::Null)
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(
            statements[0],
            Statement::Variable(
                DeclarationStatement {
                    id: Some(0),
                    value_type: Type::Optional(Box::new(Type::Optional(Box::new(Type::U64)))),
                    value: Expression::Constant(Primitive::Null.into())
                }
            )
        );
    }

    #[test]
    fn test_map() {
        // let a: map<u64, string> = {};
        let tokens = vec![
            Token::Let,
            Token::Identifier("a"),
            Token::Colon,
            Token::Map,
            Token::OperatorLessThan,
            Token::Number(NumberType::U64),
            Token::Comma,
            Token::String,
            Token::OperatorGreaterThan,
            Token::OperatorAssign,
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(
            statements[0],
            Statement::Variable(
                DeclarationStatement {
                    id: Some(0),
                    value_type: Type::Map(Box::new(Type::U64), Box::new(Type::String)),
                    value: Expression::Constant(Constant::Map(IndexMap::new()))
                }
            )
        );
    }

    #[test]
    fn test_map_with_values() {
        // let a: map<u64, string> = { 0: "hello" };
        let tokens = vec![
            Token::Let,
            Token::Identifier("a"),
            Token::Colon,
            Token::Map,
            Token::OperatorLessThan,
            Token::Number(NumberType::U64),
            Token::Comma,
            Token::String,
            Token::OperatorGreaterThan,
            Token::OperatorAssign,
            Token::BraceOpen,
            Token::Value(Literal::U64(0)),
            Token::Colon,
            Token::Value(Literal::String(Cow::Borrowed("hello"))),
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, Vec::new());

        let mut map = IndexMap::new();
        map.insert(Primitive::U64(0).into(), Primitive::String("hello".to_owned()).into());

        assert_eq!(
            statements[0],
            Statement::Variable(
                DeclarationStatement {
                    id: Some(0),
                    value_type: Type::Map(Box::new(Type::U64), Box::new(Type::String)),
                    value: Expression::Constant(Constant::Map(map))
                }
            )
        );
    }


    #[test]
    fn test_map_as_map_key() {
        // let a: map<map<u64, string>, string> = {};
        let tokens = vec![
            Token::Let,
            Token::Identifier("a"),
            Token::Colon,
            Token::Map,
            Token::OperatorLessThan,
            Token::Map,
            Token::OperatorLessThan,
            Token::Number(NumberType::U64),
            Token::Comma,
            Token::String,
            Token::OperatorGreaterThan,
            Token::Comma,
            Token::String,
            Token::OperatorGreaterThan,
            Token::OperatorAssign,
            Token::BraceOpen,
            Token::BraceClose
        ];

        let env = EnvironmentBuilder::new();
        let mut parser = Parser::new(VecDeque::from(tokens), &env);
        let mut context = Context::new();
        context.begin_scope();

        assert!(parser.read_statements(&mut context, &None).is_err());
    }

    #[test]
    fn test_double_depth_map() {
        // let a: map<u64, map<u64, string>> = {};
        let tokens = vec![
            Token::Let,
            Token::Identifier("a"),
            Token::Colon,
            Token::Map,
            Token::OperatorLessThan,
            Token::Number(NumberType::U64),
            Token::Comma,
            Token::Map,
            Token::OperatorLessThan,
            Token::Number(NumberType::U64),
            Token::Comma,
            Token::String,
            Token::OperatorGreaterThan,
            Token::OperatorGreaterThan,
            Token::OperatorAssign,
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(
            statements[0],
            Statement::Variable(
                DeclarationStatement {
                    id: Some(0),
                    value_type: Type::Map(Box::new(Type::U64), Box::new(Type::Map(Box::new(Type::U64), Box::new(Type::String)))),
                    value: Expression::Constant(Constant::Map(IndexMap::new()))
                }
            )
        );
    }

    #[test]
    fn test_for_each_range() {
        // foreach a in 0..10 {}
        let tokens = vec![
            Token::ForEach,
            Token::Identifier("a"),
            Token::In,
            Token::Value(Literal::U64(0)),
            Token::Dot,
            Token::Dot,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(statements.len(), 1);
        assert_eq!(
            statements[0],
            Statement::ForEach(
                0,
                Expression::Constant(
                    Primitive::Range(
                        Box::new((Primitive::U64(0), Primitive::U64(10)))
                    ).into()
                ),
                Vec::new()
            )
        );
    }

    #[test]
    fn test_function() {
        let tokens = vec![
            Token::Function,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::Return,
            Token::BraceClose
        ];

        let program = test_parser(tokens);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_function_call_in_expr() {
        /*
        function foo() -> bool {
            let array: u64[] = [1, 2, 3];
            return 0 > array.len()
        }
        */
        let tokens = vec![
            Token::Function,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::ReturnType,
            Token::Bool,
            Token::BraceOpen,
            Token::Let,
            Token::Identifier("array"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::BracketOpen,
            Token::BracketClose,
            Token::OperatorAssign,
            Token::BracketOpen,
            Token::Value(Literal::U64(1)),
            Token::Comma,
            Token::Value(Literal::U64(2)),
            Token::Comma,
            Token::Value(Literal::U64(3)),
            Token::BracketClose,
            Token::Return,
            Token::Value(Literal::U64(0)),
            Token::OperatorGreaterThan,
            Token::Identifier("array"),
            Token::Dot,
            Token::Identifier("len"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceClose
        ];

        let program = test_parser(tokens);
        let statements = vec![
            Statement::Variable(
                DeclarationStatement {
                    id: Some(0),
                    value_type: Type::Array(Box::new(Type::U64)),
                    value: Expression::Constant(
                        Constant::Array(
                            vec![
                                Primitive::U64(1).into(),
                                Primitive::U64(2).into(),
                                Primitive::U64(3).into()
                            ]
                        )
                    )
                }
            ),
            Statement::Return(Some(Expression::Operator(
                Operator::Gt,
                Box::new(Expression::Constant(Primitive::U32(0).into())),
                Box::new(Expression::FunctionCall(
                    Some(Box::new(Expression::Variable(0))),
                    0,
                    Vec::new()
                ))
            )))
        ];

        assert_eq!(
            *program.functions().get(0).unwrap(),
            FunctionType::Declared(
                DeclaredFunction::new(None, None, Vec::new(), statements, Some(Type::Bool), 1)
            )
        );
    }

    #[test]
    fn test_entry_function() {
        let tokens = vec![
            Token::Entry,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::Return,
            Token::Value(Literal::U64(0)),
            Token::BraceClose
        ];

        let program = test_parser(tokens);
        assert_eq!(program.functions().len(), 1);
    }


    #[test]
    fn test_hook_function() {
        let tokens = vec![
            Token::Hook,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::Identifier("data"),
            Token::Colon,
            Token::Number(NumberType::U8),
            Token::ParenthesisClose,
            Token::ReturnType,
            Token::Bool,
            Token::BraceOpen,
            Token::Return,
            Token::Value(Literal::Bool(false)),
            Token::BraceClose
        ];

        let mut env = EnvironmentBuilder::new();
        env.register_hook("foo", vec![("data", Type::U8)], Some(Type::Bool));

        let program = test_parser_with_env(tokens, &env);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_function_on_type_err() {
        // fn (f Foo) bar() {}
        let tokens = vec![
            Token::Function,
            Token::ParenthesisOpen,
            Token::Identifier("f"),
            Token::Identifier("Foo"),
            Token::ParenthesisClose,
            Token::Identifier("bar"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::BraceClose
        ];

        let mut env = EnvironmentBuilder::new();
        env.register_structure("Foo", Vec::new());
        let parser = Parser::new(tokens, &env);
        let err = parser.parse().unwrap_err();
        assert!(
            matches!(err.kind, ParserErrorKind::InvalidFunctionType(_))
        )
    }

    #[test]
    fn test_function_on_declared_type() {
        // struct Foo {}
        // fn (f Foo) bar() {}
        let tokens = vec![
            Token::Struct,
            Token::Identifier("Foo"),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Function,
            Token::ParenthesisOpen,
            Token::Identifier("f"),
            Token::Identifier("Foo"),
            Token::ParenthesisClose,
            Token::Identifier("bar"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::BraceClose
        ];

        let program = test_parser(tokens);
        assert_eq!(program.structures().len(), 1);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_function_with_parameters() {
        let tokens = vec![
            Token::Function,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::Identifier("a"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::Comma,
            Token::Identifier("b"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::ParenthesisClose,
            Token::BraceOpen,
            Token::Return,
            Token::BraceClose
        ];

        let program = test_parser(tokens);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_function_with_return_type() {
        let tokens = vec![
            Token::Function,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::ReturnType,
            Token::Number(NumberType::U64),
            Token::BraceOpen,
            Token::Return,
            Token::Value(Literal::U64(0)),
            Token::BraceClose
        ];

        let program = test_parser(tokens);
        assert_eq!(program.functions().len(), 1);
    }

    #[test]
    fn test_foreach() {
        // foreach a in array {}
        let tokens = vec![
            Token::ForEach,
            Token::Identifier("a"),
            Token::In,
            Token::Identifier("array"),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(
            tokens,
            vec![
                ("array", Type::Array(Box::new(Type::U64)))
            ]
        );

        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_for() {
        // for i: u64 = 0; i < 10; i += 1 {}
        let tokens = vec![
            Token::For,
            Token::Identifier("i"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::OperatorAssign,
            Token::Value(Literal::U64(0)),
            Token::SemiColon,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::SemiColon,
            Token::Identifier("i"),
            Token::OperatorPlusAssign,
            Token::Value(Literal::U64(1)),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_while() {
        // while i < 10 {}
        let tokens = vec![
            Token::While,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_if() {
        // if i < 10 {}
        let tokens = vec![
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_if_else() {
        // if i < 10 {} else {}
        let tokens = vec![
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Else,
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_if_else_if() {
        // if i < 10 {} else if i < 20 {}
        let tokens = vec![
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Else,
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(20)),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_if_return() {
        // if i < 10 { nothing } return 0
        let mut tokens = vec![
            Token::BraceOpen,
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Return,
            Token::BraceClose
        ];

        let statements = test_parser_statement(tokens.clone(), vec![("i", Type::U64)]);
        assert_eq!(statements, vec![
            Statement::Scope(vec![
                Statement::If(
                    Expression::Operator(
                        Operator::Lt,
                        Box::new(Expression::Variable(0)),
                        Box::new(Expression::Constant(Primitive::U64(10).into()))
                    ),
                    Vec::new(),
                    None
                ),
                Statement::Return(None)
            ])
        ]);

        // add a value after the return
        tokens.insert(tokens.len() - 1, Token::Value(Literal::U64(0)));


        let statements = test_parser_statement_with_return_type(tokens, vec![("i", Type::U64)], Type::U64);
        assert_eq!(statements, vec![
            Statement::Scope(vec![
                Statement::If(
                    Expression::Operator(
                        Operator::Lt,
                        Box::new(Expression::Variable(0)),
                        Box::new(Expression::Constant(Primitive::U64(10).into()))
                    ),
                    Vec::new(),
                    None
                ),
                Statement::Return(Some(Expression::Constant(Primitive::U64(0).into())))
            ])
        ]);
    }

    #[test]
    fn test_if_else_if_else_return() {
        // { if i < 10 {} else if i < 20 {} else {} return 0 }
        let tokens = vec![
            Token::BraceOpen,
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Else,
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(20)),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Else,
            Token::BraceOpen,
            Token::BraceClose,
            Token::Return,
            Token::Value(Literal::U64(0)),
            Token::BraceClose
        ];

        let statements = test_parser_statement_with_return_type(tokens, vec![("i", Type::U64)], Type::U64);
        assert_eq!(statements, vec![
            Statement::Scope(vec![
                Statement::If(
                    Expression::Operator(
                        Operator::Lt,
                        Box::new(Expression::Variable(0)),
                        Box::new(Expression::Constant(Primitive::U64(10).into()))
                    ),
                    Vec::new(),
                    Some(vec![
                        Statement::If(
                            Expression::Operator(
                                Operator::Lt,
                                Box::new(Expression::Variable(0)),
                                Box::new(Expression::Constant(Primitive::U64(20).into()))
                            ),
                            Vec::new(),
                            Some(Vec::new())
                        )
                    ])
                ),
                Statement::Return(Some(Expression::Constant(Primitive::U64(0).into())))
            ])                
        ]);
    }

    #[test]
    fn test_ends_with_return() {
        const RETURN: Statement = Statement::Return(Some(Expression::Constant(Constant::Default(Primitive::U64(0)))));
        let statements = vec![RETURN];
        assert!(Parser::ends_with_return(&statements).unwrap());

        let statements = vec![RETURN, Statement::Expression(Expression::Constant(Primitive::U64(0).into()))];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if ... return
        let statements = vec![Statement::If(Expression::Constant(Primitive::Boolean(true).into()), Vec::new(), None), RETURN];
        assert!(Parser::ends_with_return(&statements).unwrap());

        let statements = vec![Statement::If(Expression::Constant(Primitive::Boolean(true).into()), Vec::new(), None)];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if else
        let statements = vec![Statement::If(Expression::Constant(Primitive::Boolean(true).into()), Vec::new(), Some(Vec::new()))];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if return else return
        let statements = vec![
            Statement::If(Expression::Constant(Primitive::Boolean(true).into()), vec![RETURN], Some(vec![RETURN]))
        ];
        assert!(Parser::ends_with_return(&statements).unwrap());

        // if return else if return else no return
        let statements = vec![
            Statement::If(
                Expression::Constant(Primitive::Boolean(true).into()),
                vec![RETURN],
                Some(vec![
                    Statement::If(Expression::Constant(Primitive::Boolean(true).into()), vec![RETURN], None)
                ])
            )
        ];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if return else if return else return
        let statements = vec![
            Statement::If(
                Expression::Constant(Primitive::Boolean(true).into()),
                vec![RETURN],
                Some(vec![
                    Statement::If(Expression::Constant(Primitive::Boolean(true).into()), vec![RETURN], Some(vec![RETURN]))
                ])
            )
        ];
        assert!(Parser::ends_with_return(&statements).unwrap());
    }

    #[test]
    fn test_variable() {
        // let hello: string = "hello";
        let tokens = vec![
            Token::Let,
            Token::Identifier("hello"),
            Token::Colon,
            Token::String,
            Token::OperatorAssign,
            Token::Value(Literal::String(Cow::Borrowed("world"))),
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_ternary() {
        // i < 10 ? 1 : 0
        let tokens = vec![
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::OperatorTernary,
            Token::Value(Literal::U64(1)),
            Token::Colon,
            Token::Value(Literal::U64(0)),
        ];

        let statements = test_parser_statement(tokens, vec![("i", Type::U64)]);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_struct() {
        // Message { message_id: u64, message: string }
        let tokens = vec![
            Token::Struct,
            Token::Identifier("Message"),
            Token::BraceOpen,
            Token::Identifier("aaa_message_id"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::Comma,
            Token::Identifier("aaa_message"),
            Token::Colon,
            Token::String,
            Token::BraceClose
        ];

        let program = test_parser(tokens.clone());
        assert_eq!(program.structures().len(), 1);

        // Also test with a environment
        let mut env = EnvironmentBuilder::new();
        env.register_structure("Message", vec![
            ("message_id", Type::U8),
            ("message", Type::String)
        ]);

        // Create a struct instance
        // let msg: Message = Message { message_id: 0, message: "hello" };
        let tokens = vec![
            Token::Let,
            Token::Identifier("msg"),
            Token::Colon,
            Token::Identifier("Message"),
            Token::OperatorAssign,
            Token::Identifier("Message"),
            Token::BraceOpen,
            Token::Identifier("message_id"),
            Token::Colon,
            Token::Value(Literal::U64(0)),
            Token::Comma,
            Token::Identifier("message"),
            Token::Colon,
            Token::Value(Literal::String(Cow::Borrowed("hello"))),
            Token::BraceClose
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_struct_cast_error() {
        // Also test with a environment
        let mut env = EnvironmentBuilder::new();
        env.register_structure("Message", vec![
            ("message_id", Type::U8)
        ]);

        // Create a struct instance
        // let msg: Message = Message { message_id: 0 };
        let tokens = vec![
            Token::Let,
            Token::Identifier("msg"),
            Token::Colon,
            Token::Identifier("Message"),
            Token::OperatorAssign,
            Token::Identifier("Message"),
            Token::BraceOpen,
            Token::Identifier("message_id"),
            Token::Colon,
            Token::Value(Literal::U64(5000)),
            Token::BraceClose
        ];

        let mut parser = Parser::new(VecDeque::from(tokens), &env);
        let mut context = Context::new();
        context.begin_scope();

        assert!(parser.read_statements(&mut context, &None).is_err());
    }

    #[test]
    fn test_struct_optional() {
        // struct Message { message_id: u64 }
        let mut env = EnvironmentBuilder::default();
        env.register_structure("Message", vec![("message_id", Type::U64)]);

        // let msg: optional<Message> = null;
        // let id: u64 = msg.unwrap().message_id;
        let tokens = vec![
            Token::Let,
            Token::Identifier("msg"),
            Token::Colon,
            Token::Optional,
            Token::OperatorLessThan,
            Token::Identifier("Message"),
            Token::OperatorGreaterThan,
            Token::OperatorAssign,
            Token::Value(Literal::Null),

            Token::Let,
            Token::Identifier("id"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::OperatorAssign,
            Token::Identifier("msg"),
            Token::Dot,
            Token::Identifier("unwrap"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose,
            Token::Dot,
            Token::Identifier("message_id")
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 2);
    }

    #[test]
    fn test_type_constant() {
        // let test: u64 = u64::MAX;
        let tokens = vec![
            Token::Let,
            Token::Identifier("test"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::OperatorAssign,
            Token::Number(NumberType::U64),
            Token::Colon,
            Token::Colon,
            Token::Identifier("MAX"),
        ];

        let mut env = EnvironmentBuilder::new();
        env.register_constant(Type::U64, "MAX", Primitive::U64(u64::MAX).into());
        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_enum() {
        // enum Message { HELLO, WORLD { a: u64 } }
        let tokens = vec![
            Token::Enum,
            Token::Identifier("Message"),
            Token::BraceOpen,
            Token::Identifier("HELLO"),
            Token::Comma,
            Token::Identifier("WORLD"),
            Token::BraceOpen,
            Token::Identifier("a"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::BraceClose,
            Token::BraceClose
        ];

        let program = test_parser(tokens.clone());
        assert_eq!(program.enums().len(), 1);

        // Also test with a environment
        let mut env = EnvironmentBuilder::new();
        env.register_enum("Message", vec![
            ("HELLO", Vec::new()),
            ("WORLD", vec![("a", Type::U64)])
        ]);

        // Create an enum instance
        // let msg: Message = Message::HELLO;
        let tokens = vec![
            Token::Let,
            Token::Identifier("msg"),
            Token::Colon,
            Token::Identifier("Message"),
            Token::OperatorAssign,
            Token::Identifier("Message"),
            Token::Colon,
            Token::Colon,
            Token::Identifier("HELLO"),
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_enum_operation() {
        // enum Either { Left, Right }
        let mut env = EnvironmentBuilder::new();
        env.register_enum("Either", vec![
            ("Left", Vec::new()),
            ("Right", Vec::new())
        ]);

        // let value: Either = Either::Left;
        // if value == Either::Left {}
        let tokens = vec![
            Token::Let,
            Token::Identifier("value"),
            Token::Colon,
            Token::Identifier("Either"),
            Token::OperatorAssign,
            Token::Identifier("Either"),
            Token::Colon,
            Token::Colon,
            Token::Identifier("Left"),
            Token::If,
            Token::Identifier("value"),
            Token::OperatorEquals,
            Token::Identifier("Either"),
            Token::Colon,
            Token::Colon,
            Token::Identifier("Left"),
            Token::BraceOpen,
            Token::BraceClose
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 2);
    }

    #[test]
    fn test_enum_optional() {
        // enum Either { Left, Right }
        let mut env = EnvironmentBuilder::default();
        env.register_enum("Either", vec![
            ("Left", Vec::new()),
            ("Right", Vec::new())
        ]);

        // let value: optional<Either> = null;
        // let v: Either = value.unwrap();
        let tokens = vec![
            Token::Let,
            Token::Identifier("value"),
            Token::Colon,
            Token::Optional,
            Token::OperatorLessThan,
            Token::Identifier("Either"),
            Token::OperatorGreaterThan,
            Token::OperatorAssign,
            Token::Value(Literal::Null),
            Token::Let,
            Token::Identifier("v"),
            Token::Colon,
            Token::Identifier("Either"),
            Token::OperatorAssign,
            Token::Identifier("value"),
            Token::Dot,
            Token::Identifier("unwrap"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 2);
    }

    #[test]
    fn test_enum_with_fields() {
        // enum Message { HELLO { a: u64 }, WORLD { b: string } }
        let tokens = vec![
            Token::Enum,
            Token::Identifier("Message"),
            Token::BraceOpen,
            Token::Identifier("HELLO"),
            Token::BraceOpen,
            Token::Identifier("a"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::BraceClose,
            Token::Comma,
            Token::Identifier("WORLD"),
            Token::BraceOpen,
            Token::Identifier("b"),
            Token::Colon,
            Token::String,
            Token::BraceClose,
            Token::BraceClose
        ];

        let program = test_parser(tokens.clone());
        assert_eq!(program.enums().len(), 1);

        // Also test with a environment
        let mut env = EnvironmentBuilder::new();
        env.register_enum("Message", vec![
            ("HELLO", vec![("a", Type::U64)]),
            ("WORLD", vec![("b", Type::String)])
        ]);

        // Create an enum instance
        // let msg: Message = Message::HELLO { a: 0 };
        let tokens = vec![
            Token::Let,
            Token::Identifier("msg"),
            Token::Colon,
            Token::Identifier("Message"),
            Token::OperatorAssign,
            Token::Identifier("Message"),
            Token::Colon,
            Token::Colon,
            Token::Identifier("HELLO"),
            Token::BraceOpen,
            Token::Identifier("a"),
            Token::Colon,
            Token::Value(Literal::U64(0)),
            Token::BraceClose
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_const_fn_call() {
        let mut env = EnvironmentBuilder::new();
        env.register_const_function("test", Type::String, vec![("name", Type::String)], |params| {
            Ok(Constant::Default(Primitive::String(format!("hello {}", params[0].as_string()?))))
        });

        // let name: string = String::test("world");
        let tokens = vec![
            Token::Let,
            Token::Identifier("name"),
            Token::Colon,
            Token::String,
            Token::OperatorAssign,
            Token::String,
            Token::Colon,
            Token::Colon,
            Token::Identifier("test"),
            Token::ParenthesisOpen,
            Token::Value(Literal::String(Cow::Borrowed("world"))),
            Token::ParenthesisClose
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 1);
        let Statement::Variable(variable) = &statements[0] else {
            panic!("Expected a variable statement");
        };

        assert_eq!(variable.value_type, Type::String);
        assert_eq!(variable.value, Expression::Constant(Primitive::String("hello world".to_owned()).into()));
    }

    #[test]
    fn test_optional_fn_param() {
        let mut env = EnvironmentBuilder::new();
        env.register_native_function("test", None, vec![("input", Type::Optional(Box::new(Type::U8)))], |_, _, _| { Ok(None) }, 0, None);

        // test(null)
        let tokens = vec![
            Token::Identifier("test"),
            Token::ParenthesisOpen,
            Token::Value(Literal::Null),
            Token::ParenthesisClose
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 1);

        // test(10)
        let tokens = vec![
            Token::Identifier("test"),
            Token::ParenthesisOpen,
            Token::Value(Literal::U8(10)),
            Token::ParenthesisClose
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_type_t_same_as_instance() {
        // let a: optional<u64> = 10;
        // let b: u64 = a.unwrap_or(50);
        let tokens = vec![
            Token::Let,
            Token::Identifier("a"),
            Token::Colon,
            Token::Optional,
            Token::OperatorLessThan,
            Token::Number(NumberType::U64),
            Token::OperatorGreaterThan,
            Token::OperatorAssign,
            Token::Value(Literal::U64(10)),
            Token::Let,
            Token::Identifier("b"),
            Token::Colon,
            Token::Number(NumberType::U64),
            Token::OperatorAssign,
            Token::Identifier("a"),
            Token::Dot,
            Token::Identifier("unwrap_or"),
            Token::ParenthesisOpen,
            Token::Value(Literal::U64(50)),
            Token::ParenthesisClose
        ];

        let statements = test_parser_statement_with(tokens, vec![], &None, &EnvironmentBuilder::default());
        assert_eq!(statements.len(), 2);
    }

    #[test]
    fn test_static_function_on_type() {
        // register Foo in environment
        let mut env = EnvironmentBuilder::new();
        let ty = Type::Struct(env.register_structure("Foo", Vec::new()));
        // register a static function on it
        env.register_static_function("bar", ty, Vec::new(), |_, _, _| todo!(), 0, Some(Type::U64));

        // Foo::bar()
        let tokens = vec![
            Token::Identifier("Foo"),
            Token::Colon,
            Token::Colon,
            Token::Identifier("bar"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 1);

        // Try also on a enum type
        let ty = Type::Enum(env.register_enum("Bar", Vec::new()));
        // register a static function on it
        env.register_static_function("foo", ty, Vec::new(), |_, _, _| todo!(), 0, Some(Type::U64));

        // Bar::foo()
        let tokens = vec![
            Token::Identifier("Bar"),
            Token::Colon,
            Token::Colon,
            Token::Identifier("foo"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_assign_struct_field_null() {
        let mut env = EnvironmentBuilder::default();
        env.register_structure("Message", vec![("message_id", Type::Optional(Box::new(Type::U64)))]);

        let tokens = vec![
            Token::Identifier("Message"),
            Token::BraceOpen,
            Token::Identifier("message_id"),
            Token::Colon,
            Token::Value(Literal::Null),
            Token::BraceClose
        ];

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, &env);
        assert_eq!(statements.len(), 1);
    }

    #[test]
    fn test_assign_optional_to_type() {
        let mut env = EnvironmentBuilder::default();
        // env.register_structure("Message", vec![("message_id", Type::Optional(Box::new(Type::U64)))]);
        env.register_native_function("test", None, vec![], |_, _, _| todo!(), 0, Some(Type::Optional(Box::new(Type::Bool))));

        // let _: bool = test();
        let tokens = vec![
            Token::Let,
            Token::Identifier(IGNORE_VARIABLE),
            Token::Colon,
            Token::Bool,
            Token::OperatorAssign,
            Token::Identifier("test"),
            Token::ParenthesisOpen,
            Token::ParenthesisClose
        ];

        let mut parser = Parser::new(VecDeque::from(tokens), &env);
        let mut context = Context::new();
        context.begin_scope();

        assert!(parser.read_statements(&mut context, &None).is_err());
    }

    #[test]
    fn test_match() {
        let tokens = vec![
            Token::Match,
            Token::Value(Literal::Bool(true)),
            Token::BraceOpen,
            Token::Value(Literal::Bool(true)),
            Token::FatArrow,
            Token::BraceOpen,
            Token::BraceClose,
            Token::Comma,
            Token::Value(Literal::Bool(false)),
            Token::FatArrow,
            Token::BraceOpen,
            Token::BraceClose,
            Token::BraceClose,
        ];

        let statements = test_parser_statement(tokens, Vec::new());
        assert_eq!(statements.len(), 1);
    }
}