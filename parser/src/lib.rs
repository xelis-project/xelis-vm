mod context;
mod error;
mod mapper;

use std::{
    borrow::Cow,
    collections::{HashMap, VecDeque},
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

#[derive(Debug)]
pub enum QueueItem {
  Operator(Operator),         // For operators, identifiers, or literals
  Expression(Expression),  // For finalized expressions or sub-expressions
  Token(Token<'static>), // For operators
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
            (Value::U8(a), Value::U8(b)) => Value::U8(a $op b),
            (Value::U16(a), Value::U16(b)) => Value::U16(a $op b),
            (Value::U32(a), Value::U32(b)) => Value::U32(a $op b),
            (Value::U64(a), Value::U64(b)) => Value::U64(a $op b),
            (Value::U128(a), Value::U128(b)) => Value::U128(a $op b),
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
            _ => return None
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
            _ => return None
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

    fn is_entry(&self) -> bool {
        match self {
            Function::Program(f) => f.is_entry(),
            _ => false
        }
    }
}

fn trace_postfix(output_queue: &Vec<QueueItem>) {
  let postfix: Vec<String> = output_queue.iter().map(|item| match item {
      QueueItem::Expression(expr) => format!("{:?}", expr), // Customize the format for Expression
      QueueItem::Operator(op) => format!("{}", op.to_token()),       // Customize the format for Operator
      QueueItem::Token(token) => format!("{}", token),       // Customize the format for Token
  }).collect();
  trace!("Postfix Expression: {}", postfix.join(" "));
}

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
    // TODO: Path to use to import files
    // _path: Option<&'a str>
    // Used for errors, we track the line and column
    line: usize,
    column_start: usize,
    column_end: usize
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
            line: 0,
            column_start: 0,
            column_end: 0,
        }
    }

    pub fn set_disable_const_upgrading(&mut self, value: bool) {
        self.disable_const_upgrading = value;
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
        self.tokens.front().map(|t| &t.token)
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
            }
            Token::Identifier(id) => {
                if let Ok(builder) = self.global_mapper.structs().get_by_name(id) {
                    Type::Struct(builder.get_type().clone())
                } else if let Ok(builder) = self.global_mapper.enums().get_by_name(id) {
                    Type::Enum(builder.get_type().clone())
                } else {
                    return Err(err!(self, ParserErrorKind::TypeNameNotFound(id)))
                }
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
            None => Err(err!(self, ParserErrorKind::EmptyValue))
        }
    }

    fn get_from_generic_type<'b>(&'b self, on_type: Option<&Type>, _type: &'b Type, path: Option<&Expression>, context: &'b Context<'a>) -> Result<Type, ParserError<'a>> {
        trace!("Get from generic type: {:?}", _type);
        Ok(match _type {
            Type::T(id) => match on_type {
                Some(t) => t.get_generic_type(*id).ok_or(err!(self, ParserErrorKind::InvalidTypeT))?.clone(),
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
            Expression::MapConstructor(_, key_type, value_type) => Cow::Owned(Type::Map(Box::new(key_type.clone()), Box::new(value_type.clone()))),
            Expression::EnumConstructor(_, _type) => Cow::Owned(Type::Enum(_type.enum_type().clone())),
            Expression::Variable(ref var_name) => match on_type {
                Some(t) => {
                    if let Type::Struct(_type) = t {
                        let index = *var_name as usize;
                        if let Some(field_type) = _type.fields().get(index) {
                            Cow::Owned(field_type.clone())
                        } else {
                            return Err(err!(self, ParserErrorKind::UnexpectedMappedVariableId(var_name.clone())))
                        }
                    } else {
                        return Err(err!(self, ParserErrorKind::UnexpectedMappedVariableId(var_name.clone())))
                    }
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
    fn read_function_call(&mut self, path: Option<Expression>, on_type: Option<&Type>, name: &str, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        trace!("Read function call: {}", name);

        // we remove the token from the list
        self.expect_token(Token::ParenthesisOpen)?;
        let mut parameters: Vec<Expression> = Vec::new();
        let mut types: Vec<Type> = Vec::new();

        // read parameters for function call
        while self.peek_is_not(Token::ParenthesisClose) {
            let expr = self.read_expression(context)?;
            // We are forced to clone the type because we can't borrow it from the expression
            // I prefer to do this than doing an iteration below
            let t = self.get_type_from_expression(None, &expr, context)?.into_owned();
            types.push(t);
            parameters.push(expr);

            if self.peek_is(Token::Comma) {
                self.expect_token(Token::Comma)?;
            }
        }

        let id = self.global_mapper
            .functions()
            .get_compatible(Signature::new(name.to_owned(), on_type.cloned(), types), &mut parameters)
            .map_err(|e| err!(self, e.into()))?;

        // Entry are only callable by external
        let f = self.get_function(id)?;
        if f.is_entry() {
            return Err(err!(self, ParserErrorKind::FunctionIsEntry))
        }

        self.expect_token(Token::ParenthesisClose)?;
        Ok(Expression::FunctionCall(path.map(Box::new), id, parameters))
    }

    // Read fields of a constructor with the following syntax:
    // { field1, field2, ... }
    // or with values
    // { field1: value1, field2: value2, ... }
    fn read_constructor_fields(&mut self, context: &mut Context<'a>) -> Result<Vec<(&'a str, Expression)>, ParserError<'a>> {
        trace!("Read constructor fields");

        let mut fields = Vec::new();
        while self.peek_is_not(Token::BraceClose) {
            let field_name = self.next_identifier()?;
            let expr = match self.advance()? {
                Token::Comma | Token::BraceClose => Expression::Variable(context.get_variable_id(field_name).ok_or_else(|| err!(self, ParserErrorKind::UnexpectedVariable(field_name)))?),
                Token::Colon => self.read_expression(context)?,
                token => return Err(err!(self, ParserErrorKind::UnexpectedToken(token)))
            };

            fields.push((field_name, expr));

            if self.peek_is(Token::Comma) {
                self.expect_token(Token::Comma)?;
            }
        }
        self.expect_token(Token::BraceClose)?;

        Ok(fields)
    }

    // Verify the type of an expression, if not the same, try to cast it with no loss
    fn verify_type_of(&self, expr: &mut Expression, expected_type: &Type, context: &Context<'a>) -> Result<(), ParserError<'a>> {
        let _type = self.get_type_from_expression(None, &expr, context)?.into_owned();
        if _type != *expected_type {
            match expr {
                Expression::Constant(v) if _type.is_castable_to(expected_type) => v.mut_checked_cast_to_primitive_type(expected_type)
                    .map_err(|e| err!(self, e.into()))?,
                _ => return Err(err!(self, ParserErrorKind::InvalidValueType(_type, expected_type.clone())))
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
        let fields = self.read_constructor_fields(context)?;

        if struct_type.fields().len() != fields.len() {
            return Err(err!(self, ParserErrorKind::InvalidFieldCount))
        }

        // Now verify that it match our struct
        let builder = self.global_mapper.structs().get_by_ref(&struct_type)
            .map_err(|e| err!(self, e.into()))?;
        let mut fields_expressions = Vec::with_capacity(fields.len());
        for ((field_name, mut field_expr), (field_type, field_name_expected)) in fields.into_iter().zip(struct_type.fields().iter().zip(builder.names())) {
            if field_name != *field_name_expected {
                return Err(err!(self, ParserErrorKind::InvalidFieldName(field_name, field_name_expected)))
            }

            self.verify_type_of(&mut field_expr, field_type, context)?;

            fields_expressions.push(field_expr);
        }

        Ok(Expression::StructConstructor(fields_expressions, struct_type))
    }

    // Read an enum variant constructor with the following syntax:
    // enum_name::variant_name { field1: value1, field2: value2 }
    // Or if no fields: enum_name::variant_name
    fn read_enum_variant_constructor(&mut self, enum_type: EnumType, variant_name: &'a str, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        trace!("Read enum variant constructor: {:?}::{}", enum_type, variant_name);
        // If its an enum variant with fields
        let fields = if self.peek_is(Token::BraceOpen) {
            self.expect_token(Token::BraceOpen)?;
            self.read_constructor_fields(context)?
        } else {
            Vec::new()
        };

        let builder = self.global_mapper.enums()
            .get_by_ref(&enum_type)
            .map_err(|e| err!(self, e.into()))?;
        let (variant_id, variant_fields) = builder.get_variant_by_name(&variant_name)
            .ok_or_else(|| err!(self, ParserErrorKind::EnumVariantNotFound(variant_name)))?;

        if variant_fields.len() != fields.len() {
            return Err(err!(self, ParserErrorKind::InvalidFieldCount))
        }

        let mut fields_expressions = Vec::with_capacity(variant_fields.len());
        for ((field_name, mut field_expr), (expected_name, expected_type)) in fields.into_iter().zip(variant_fields) {
            if field_name != *expected_name {
                return Err(err!(self, ParserErrorKind::InvalidEnumFieldName(field_name)))
            }

            self.verify_type_of(&mut field_expr, expected_type, context)?;

            fields_expressions.push(field_expr);
        }

        Ok(Expression::EnumConstructor(fields_expressions, EnumValueType::new(enum_type, variant_id)))
    }

    // Read a constant from the environment
    fn read_type_constant(&mut self, token: Token<'a>, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        let _type = self.get_type_from_token(token)?;
        self.expect_token(Token::Colon)?;
        self.expect_token(Token::Colon)?;

        let constant_name = self.next_identifier()?;

        if let Type::Enum(enum_type) = _type {
            self.read_enum_variant_constructor(enum_type, constant_name, context)
        } else {
            trace!("Read type constant: {:?}::{}", _type, constant_name);
            self.environment.get_constant_by_name(&_type, &constant_name)
                .map(|v| Expression::Constant(v.clone()))
                .ok_or_else(|| err!(self, ParserErrorKind::ConstantNotFound(_type, constant_name)))
        }
    }

    // Execute the selected operator
    fn execute_operator(&self, op: &Operator, left: &Value, right: &Value) -> Option<Value> {
        Some(match op {
            Operator::Add => {
                if left.is_string() || right.is_string() {
                    Value::String(format!("{}{}", left, right))
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
                    Value::U8(v) => Value::U8(v.pow(pow_n)),
                    Value::U16(v) => Value::U16(v.pow(pow_n)),
                    Value::U32(v) => Value::U32(v.pow(pow_n)),
                    Value::U64(v) => Value::U64(v.pow(pow_n)),
                    Value::U128(v) => Value::U128(v.pow(pow_n)),
                    _ => return None
                }
            },

            Operator::BitwiseXor => op!(left, right, ^),
            Operator::BitwiseAnd => op_num_with_bool!(left, right, &),
            Operator::BitwiseOr => op_num_with_bool!(left, right, |),
            Operator::BitwiseShl => op!(left, right, <<),
            Operator::BitwiseShr => op!(left, right, >>),

            Operator::Eq => Value::Boolean(left == right),
            Operator::Neq => Value::Boolean(left != right),
            Operator::Gte => op_bool!(left, right, >=),
            Operator::Gt => op_bool!(left, right, >),
            Operator::Lte => op_bool!(left, right, <=),
            Operator::Lt => op_bool!(left, right, <),
            Operator::And => Value::Boolean(left.as_bool().ok()? && right.as_bool().ok()?),
            Operator::Or => Value::Boolean(left.as_bool().ok()? || right.as_bool().ok()?),
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
                let mut new_values = Vec::with_capacity(values.len());
                for value in values {
                    let v = self.try_convert_expr_to_value(value)?;
                    *value = Expression::Constant(v.clone());

                    new_values.push(v);
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

                let value_type = min.get_type().ok()?;
                Constant::Default(Value::Range(Box::new(min), Box::new(max), value_type))
            },
            Expression::StructConstructor(fields, struct_type) => {
                let mut new_fields = Vec::with_capacity(fields.len());
                for field in fields {
                    let v = self.try_convert_expr_to_value(field)?;
                    *field = Expression::Constant(v.clone());
                    new_fields.push(v);
                }
                Constant::Struct(new_fields, struct_type.clone())
            },
            Expression::EnumConstructor(fields, enum_type) => {
                let mut new_fields = Vec::with_capacity(fields.len());
                for field in fields {
                    let v = self.try_convert_expr_to_value(field)?;
                    *field = Expression::Constant(v.clone());
                    new_fields.push(v);
                }
                Constant::Enum(new_fields, enum_type.clone())
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
                Constant::Default(Value::Boolean(!v.to_bool().ok()?))
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
        output_queue: &[QueueItem], 
        on_type: Option<&Type>, 
        context: &mut Context<'a>,
    ) -> Result<Vec<QueueItem>, ParserError<'a>> {
        let mut collapse_queue: Vec<QueueItem> = Vec::new();
    
        output_queue.iter().try_for_each(|item| {
            match item {
                QueueItem::Expression(expr) => {
                    collapse_queue.push(QueueItem::Expression(expr.clone()));
                    Ok(())
                }
                QueueItem::Token(token) => {
                    if let Some(op) = Operator::value_of(token) {
                        if let Some(QueueItem::Expression(mut right)) = collapse_queue.pop() {
                            if let Some(QueueItem::Expression(mut left)) = collapse_queue.pop() {
                                let left_type = self.get_type_from_expression(on_type, &left, context)?.into_owned();
                                if let Some(right_type) = self.get_type_from_expression_internal(on_type, &right, context)? {
                                    self.verify_operator(&op, left_type, right_type.into_owned(), &mut left, &mut right)?;
                                    let mut result = Expression::Operator(op, Box::new(left), Box::new(right));
                                    
                                    let result_expr = self.try_convert_expr_to_value(&mut result)
                                        .map(Expression::Constant)
                                        .unwrap_or(result);
                                    collapse_queue.push(QueueItem::Expression(result_expr));
                                    Ok(())
                                } else {
                                    match op {
                                        Operator::Eq | Operator::Neq | Operator::Assign(None) if left_type.allow_null() => {
                                            let mut result = Expression::Operator(op, Box::new(left), Box::new(right));
                                            let result_expr = self.try_convert_expr_to_value(&mut result)
                                                .map(Expression::Constant)
                                                .unwrap_or(result);
                                            collapse_queue.push(QueueItem::Expression(result_expr));
                                            Ok(())
                                        }
                                        _ => Err(err!(self, ParserErrorKind::IncompatibleNullWith(left_type))),
                                    }
                                }
                            } else {
                                Err(err!(self, ParserErrorKind::InvalidOperation))
                            }
                        } else {
                            Err(err!(self, ParserErrorKind::InvalidOperation))
                        }
                    } else {
                        Err(err!(self, ParserErrorKind::InvalidOperation))
                    }
                }
                _ => Ok(()),
            }
        })?;
    
        // Return the fully collapsed queue
        Ok(collapse_queue)
    }

    // Read an expression with default parameters
    fn read_expression(&mut self, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        self.read_expr(None, None, true, true, None, context)
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
        trace!("Read expression");

        let mut operator_stack: Vec<Operator> = Vec::new(); // Stack for operators
        let mut output_queue: Vec<QueueItem> = Vec::new();   // Queue for operands and reordered tokens
        let mut collapse_queue: Vec<QueueItem> = Vec::new();   // Queue for operands and reordered tokens

        let mut required_operator = false;
        let mut last_expression: Option<Expression> = None;
        while self.peek()
            .ok()
            .filter(|peek| {
                if let Some(delimiter) = delimiter.as_ref() {
                  if **peek == **delimiter {
                    return false
                  }
                }

                if !allow_ternary && **peek == Token::OperatorTernary {
                    return false
                }
                
                required_operator == peek.is_operator() 
                    || (**peek == Token::BracketOpen && last_expression.is_none())

            }).is_some()
        {
            let token = self.advance()?;
            trace!("token: {:?}", token);

            let expr: Expression = match token {
                Token::BracketOpen => {
                    match last_expression {
                        Some(v) => {
                            if !self.get_type_from_expression(on_type, &v, context)?.is_array() {
                                return Err(err!(self, ParserErrorKind::InvalidArrayCall))
                            }

                            // Index must be of type u64
                            let index = self.read_expr(delimiter.clone(), on_type, true, true, Some(&Type::U32), context)?;
                            let index_type = self.get_type_from_expression(on_type, &index, context)?;
                            if *index_type != Type::U32 {
                                return Err(err!(self, ParserErrorKind::InvalidArrayCallIndexType(index_type.into_owned())))
                            }

                            self.expect_token(Token::BracketClose)?;
                            required_operator = !required_operator;
                            Expression::ArrayCall(Box::new(v), Box::new(index))
                        },
                        None => { // require at least one value in a array constructor
                            let mut expressions: Vec<Expression> = Vec::new();
                            let mut array_type: Option<Type> = None;
                            while self.peek_is_not(Token::BracketClose) {
                                let expr = self.read_expr(delimiter.clone(), on_type, true, true, expected_type.map(|t| t.get_inner_type()), context)?;
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
                                expressions.push(expr);

                                if self.peek_is(Token::Comma) {
                                    self.expect_token(Token::Comma)?;
                                }
                            }

                            self.expect_token(Token::BracketClose)?;
                            Expression::ArrayConstructor(expressions)
                        }
                    }
                },
                Token::ParenthesisOpen => {
                    let expr = self.read_expr(Some(&Token::ParenthesisClose), None, true, true, expected_type, context)?;
                    self.expect_token(Token::ParenthesisClose)?;
                    output_queue.push(QueueItem::Expression(expr.clone()));
                    Expression::SubExpression(Box::new(expr))
                },
                Token::Identifier(id) => {
                    trace!("identified {}", id);
                    match self.peek()? {
                        // function call
                        Token::ParenthesisOpen => {
                          let val = self.read_function_call(last_expression.take(), on_type, id, context)?;
                          output_queue.push(QueueItem::Expression(val.clone()));
                          val
                        },
                        Token::Colon => {
                          let val = self.read_type_constant(Token::Identifier(id), context)?;
                          output_queue.push(QueueItem::Expression(val.clone()));
                          val
                        },
                        _ => {
                            match on_type {
                                // mostly an access to a struct field
                                Some(t) => {
                                    if let Type::Struct(_type) = t {
                                        let builder = self.global_mapper.structs().get_by_ref(_type)
                                            .map_err(|e| err!(self, e.into()))?;
                                        match builder.get_id_for_field(id) {
                                            Some(v) => {
                                              let val = Expression::Variable(v);
                                              output_queue.push(QueueItem::Expression(val.clone()));
                                              val
                                            },
                                            None => return Err(err!(self, ParserErrorKind::UnexpectedVariable(id)))
                                        }
                                    } else {
                                        return Err(err!(self, ParserErrorKind::UnexpectedType(t.clone())))
                                    }
                                },
                                None => {
                                    if let Some(num_id) = context.get_variable_id(id) {
                                        let val = Expression::Variable(num_id);
                                        output_queue.push(QueueItem::Expression(val.clone()));
                                        val
                                    } else if let Some(constant) = self.constants.get(id) {
                                        let val = Expression::Constant(constant.value.clone());
                                        output_queue.push(QueueItem::Expression(val.clone()));
                                        val
                                    } else if let Ok(builder) = self.global_mapper.structs().get_by_name(&id) {
                                        let val = self.read_struct_constructor(builder.get_type().clone(), context)?;
                                        output_queue.push(QueueItem::Expression(val.clone()));
                                        val
                                    } else {
                                        return Err(err!(self, ParserErrorKind::UnexpectedVariable(id)))
                                    }
                                }
                            }
                        }
                    }
                },
                Token::Value(value) => {
                    let val = Expression::Constant(
                        Constant::Default(match value {
                            Literal::U8(n) => Value::U8(n),
                            Literal::U16(n) => Value::U16(n),
                            Literal::U32(n) => Value::U32(n),
                            Literal::U64(n) => Value::U64(n),
                            Literal::U128(n) => Value::U128(n),
                            Literal::U256(n) => Value::U256(n),
                            Literal::Number(n) => match expected_type {
                                Some(Type::U8) => Value::U8(n.try_into().map_err(|_| err!(self, ParserErrorKind::NumberTooBigForType(Type::U8)))?),
                                Some(Type::U16) => Value::U16(n.try_into().map_err(|_| err!(self, ParserErrorKind::NumberTooBigForType(Type::U16)))?),
                                Some(Type::U32) => Value::U32(n.try_into().map_err(|_| err!(self, ParserErrorKind::NumberTooBigForType(Type::U32)))?),
                                Some(Type::U64) => Value::U64(n),
                                Some(Type::U128) => Value::U128(n as u128),
                                Some(Type::U256) => Value::U256(U256::from(n)),
                                _ => Value::U64(n)
                            },
                            Literal::String(s) => Value::String(s.into_owned()),
                            Literal::Bool(b) => Value::Boolean(b),
                            Literal::Null => Value::Null
                        })
                    );

                    output_queue.push(QueueItem::Expression(val.clone()));
                    val
                },
                Token::Dot => {
                    match last_expression {
                        Some(value) => {
                            let _type = self.get_type_from_expression(on_type, &value, context)?.into_owned();
                            // If we have .. that is mostly a range

                            // because we read operator DOT + right expression
                            required_operator = !required_operator;

                            // Read a type constant
                            if self.peek_is(Token::Dot) {
                                self.expect_token(Token::Dot)?;
                                let end_expr = self.read_expr(delimiter.clone(), Some(&_type), false, false, expected_type, context)?;
                                let end_type = self.get_type_from_expression(on_type, &end_expr, context)?;
                                if _type != *end_type {
                                    return Err(err!(self, ParserErrorKind::InvalidRangeType(_type, end_type.into_owned())))
                                }

                                if !_type.is_primitive() {
                                    return Err(err!(self, ParserErrorKind::InvalidRangeTypePrimitive(_type)))
                                }

                                let range = Expression::RangeConstructor(Box::new(value), Box::new(end_expr));
                                output_queue.clear();
                                range
                            } else {
                                // Read a variable access OR a function call
                                let right_expr = self.read_expr(delimiter.clone(), Some(&_type), false, false, expected_type, context)?;
                                if let Expression::FunctionCall(path, name, params) = right_expr {
                                    if path.is_some() {
                                        return Err(err!(self, ParserErrorKind::UnexpectedPathInFunctionCall))
                                    }
                                    let val = Expression::FunctionCall(Some(Box::new(value)), name, params);
                                    output_queue.pop();
                                    output_queue.push(QueueItem::Expression(val.clone()));
                                    val
                                } else {
                                  let val = Expression::Path(Box::new(value), Box::new(right_expr));
                                  output_queue.pop();
                                  output_queue.push(QueueItem::Expression(val.clone()));
                                  val
                                }
                            }
                        },
                        None => return Err(err!(self, ParserErrorKind::UnexpectedToken(Token::Dot)))
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
                Token::OperatorTernary => match last_expression {
                    Some(expr) => {
                        trace!("reached ternary");

                        while let Some(top_op) = operator_stack.pop() {
                            output_queue.push(QueueItem::Token(top_op.to_token()));
                        }
                
                        trace_postfix(&output_queue);
                
                        let collapsed_queue = self.try_postfix_collapse(
                            &output_queue,
                            on_type,
                            context,
                        )?;

                        if let Some(QueueItem::Expression(shunt_expr)) = collapsed_queue.first() {
                            if *self.get_type_from_expression(on_type, &shunt_expr, context)? != Type::Bool {
                                return Err(err!(self, ParserErrorKind::InvalidCondition(Type::Bool, shunt_expr.clone())))
                            }
                        } else {
                            if *self.get_type_from_expression(on_type, &expr, context)? != Type::Bool {
                                return Err(err!(self, ParserErrorKind::InvalidCondition(Type::Bool, expr)))
                            }
                        }

                        let valid_expr = self.read_expr(Some(&Token::Colon), on_type, true, true, expected_type, context)?;
                        let first_type = self.get_type_from_expression(on_type, &valid_expr, context)?.into_owned();
                        trace!("left expr: {:?}", valid_expr.clone());

                        self.expect_token(Token::Colon)?;
                        let else_expr = self.read_expr(None, on_type, true, true, expected_type, context)?;
                        let else_type = self.get_type_from_expression(on_type, &else_expr, context)?;

                        trace!("right expr: {:?}", else_expr.clone());
                        
                        if first_type != *else_type { // both expr should have the SAME type.
                            return Err(err!(self, ParserErrorKind::InvalidValueType(else_type.into_owned(), first_type)))
                        }
                        required_operator = !required_operator;

                        let val = if let Some(QueueItem::Expression(shunt_expr)) = collapsed_queue.first() {
                          Expression::Ternary(Box::new(shunt_expr.clone()), Box::new(valid_expr), Box::new(else_expr))
                        } else {
                          Expression::Ternary(Box::new(expr), Box::new(valid_expr), Box::new(else_expr))
                        };

                        trace!("ternary result: {:?}", val);
                        output_queue.push(QueueItem::Expression(val.clone()));
                        val
                    },
                    None => return Err(err!(self, ParserErrorKind::InvalidTernaryNoPreviousExpression))
                },
                Token::As => {
                    if let Some(QueueItem::Expression(mut prev_expr)) = output_queue.pop() {
                        let left_type = self.get_type_from_expression(on_type, &prev_expr, context)?.into_owned();
                        let right_type = self.read_type()?;

                        if !left_type.is_castable_to(&right_type) {
                            return Err(err!(self, ParserErrorKind::CastError(left_type, right_type)))
                        }

                        if !right_type.is_primitive() {
                            return Err(err!(self, ParserErrorKind::CastPrimitiveError(left_type, right_type)))
                        }

                        required_operator = !required_operator;
                        let val = Expression::Cast(Box::new(prev_expr), right_type);
                        output_queue.push(QueueItem::Expression(val.clone()));
                        val
                    } else {
                        return Err(err!(self, ParserErrorKind::InvalidExpression))
                    }
                },
                token => {
                    let mut final_expr = Expression::Constant(Constant::Default(xelis_types::Value::U64(0)));
                    if token.is_type() {
                        let val = self.read_type_constant(token.clone(), context)?;
                        output_queue.push(QueueItem::Expression(val.clone()));
                        final_expr = val;
                    } else if token == Token::BraceOpen {
                        if operator_stack.len() + output_queue.len() == 0 {
                            let (key, value) = if let Some(Type::Map(key, value)) = expected_type {
                                (Some(*key.clone()), Some(*value.clone()))
                            } else {
                                (None, None)
                            };

                            let val = self.read_map_constructor(key, value, context)?;
                            final_expr = val;
                        } else {
                            return Err(err!(self, ParserErrorKind::InvalidOperation));
                        }
                    } else {
                        let op = match Operator::value_of(&token) {
                          Some(op) => op,
                            None => {
                                return Err(err!(self, ParserErrorKind::OperatorNotFound(token)))
                            }
                        };

                        while let Some(top_op) = operator_stack.last() {
                          if top_op.precedence().0 > op.precedence().0
                              || (top_op.precedence().0 == op.precedence().0 && op.is_left_to_right())
                          {
                              if let Some(pop_op) = operator_stack.pop() {
                                  output_queue.push(QueueItem::Token(pop_op.to_token()));
                              } else {
                                  return Err(err!(self, ParserErrorKind::InvalidOperation))
                              }
                          } else {
                              break;
                          }
                        }

                        operator_stack.push(op.clone());
                    }

                    final_expr
                }
            };

            last_expression = Some(expr);
            required_operator = !required_operator;
        }

        // Collapse remaining operators in the stack
        while let Some(top_op) = operator_stack.pop() {
            output_queue.push(QueueItem::Token(top_op.to_token()));
        }

        trace_postfix(&output_queue);

        // Process the postfix arithmetic that was generated
        let collapsed_queue = self.try_postfix_collapse(
            &output_queue,
            on_type,
            context,
        )?;

        if let Some(QueueItem::Expression(expr)) = collapsed_queue.first() {
            trace!("final shunted result: {:?}", expr);
            return Ok(expr.clone());
        } else {
            trace!("no result left in postfix process");
        }

        // fall back to the last stored expression if the shunting yard has no
        // postfix data to process, meaning the expression was not mathematical
        match last_expression {
            Some(mut v) => Ok(self.try_convert_expr_to_value(&mut v).map(Expression::Constant).unwrap_or(v)),
            None => Err(err!(self, ParserErrorKind::NotImplemented)), // No valid expression found
        }
    }

    fn try_map_expr_to_type(&self, expr: &mut Expression, expected_type: &Type) -> Result<bool, ParserError<'a>> {
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
                if left_type != right_type {
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
        trace!("Read map constructor");

        let mut expressions: Vec<(Expression, Expression)> = Vec::new();
        while self.peek_is_not(Token::BraceClose) {
            let key = self.read_expr(Some(&Token::Colon), None, true, true, key_type.as_ref(), context)?;
            let k_type = self.get_type_from_expression(None, &key, context)?;
            if let Some(t) = key_type.as_ref() {
                if *k_type != *t {
                    return Err(err!(self, ParserErrorKind::InvalidValueType(k_type.into_owned(), t.clone())))
                }
            } else {
                key_type = Some(k_type.into_owned());
            }

            self.expect_token(Token::Colon)?;
            let value = self.read_expr(None, None, true, true, value_type.as_ref(), context)?;
            let v_type = self.get_type_from_expression(None, &value, context)?;
            if let Some(t) = value_type.as_ref() {
                if *v_type != *t {
                    return Err(err!(self, ParserErrorKind::InvalidValueType(v_type.into_owned(), t.clone())))
                }
            } else {
                value_type = Some(v_type.into_owned());
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
    fn read_variable_internal(&mut self, context: &mut Context<'a>, is_const: bool) -> Result<(&'a str, Type, Expression), ParserError<'a>> {
        let name: &'a str = self.next_identifier()?;
        trace!("Read variable: {}", name);

        // Constants must be uppercase
        if is_const && name.to_uppercase() != name {
            return Err(err!(self, ParserErrorKind::ConstantNameNotUppercase(name)))
        }

        let ignored = name == "_";

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
            let mut expr = self.read_expr(None, None, true, true, Some(&value_type), context)?;

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
                    ParserErrorKind::EmptyArrayConstructor if value_type.is_array() => Cow::Owned(value_type.clone()),
                    _ => return Err(e)
                }
            };

            if !expr_type.is_compatible_with(&value_type) {
                // If its an optional type, we can assign a value of the inner type
                if let Type::Optional(inner) = &value_type {
                    if !expr_type.is_compatible_with(inner) {
                        return Err(err!(self, ParserErrorKind::InvalidValueType(expr_type.into_owned(), value_type)))
                    }

                    if let Expression::Constant(c) = expr {
                        expr = Expression::Constant(c.to_optional());
                    }
                } else {
                    return Err(err!(self, ParserErrorKind::InvalidValueType(expr_type.into_owned(), value_type)))
                }
            }

            expr
        } else if value_type.is_optional() {
            Expression::Constant(Constant::Default(Value::Null))
        } else {
            return Err(err!(self, ParserErrorKind::NoValueForVariable(name)))
        };

        Ok((name, value_type, value))
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
        let (name, value_type, value) = self.read_variable_internal(context, false)?;
        let id = if name != "_" {
            context.register_variable_unchecked(name, value_type.clone())
        } else {
            context.register_variable(name, value_type.clone())
                .ok_or_else(|| err!(self, ParserErrorKind::VariableNameAlreadyUsed(name)))?
        };

        Ok(DeclarationStatement {
            id,
            value_type,
            value
        })
    }


    fn read_const(&mut self, context: &mut Context<'a>) -> Result<(), ParserError<'a>> {
        let (name, value_type, mut value) = self.read_variable_internal(context, true)?;

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
            let statement: Statement = match token {
                Token::BraceClose => return Ok(None),
                Token::For => { // Example: for i: u64 = 0; i < 10; i += 1 {}
                    context.begin_scope();
                    let var = self.read_variable(context)?;
                    self.expect_token(Token::SemiColon)?;
                    
                    let condition = self.read_expression_delimited(&Token::SemiColon, context)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if  *condition_type != Type::Bool {
                        return Err(err!(self, ParserErrorKind::InvalidCondition(condition_type.into_owned(), condition)))
                    }
                    self.expect_token(Token::SemiColon)?;

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

                    let id = context.register_variable(variable, expr_type.get_inner_type().clone())
                        .ok_or_else(|| err!(self, ParserErrorKind::VariableNameAlreadyUsed(variable)))?;
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
                Token::Let => Statement::Variable(self.read_variable(context)?),
                Token::Return => {
                    let opt: Option<Expression> = if let Some(return_type) = return_type {
                        let expr = self.read_expr(None, None, true, true, Some(return_type), context)?;
                        let expr_type = self.get_type_from_expression(None, &expr, context)?;
                        if !expr_type.is_compatible_with(return_type) {
                            return Err(err!(self, ParserErrorKind::InvalidValueType(expr_type.into_owned(), return_type.clone())))
                        }
                        Some(expr)
                    } else {
                        None
                    };

                    // we can't have anything after a return
                    if self.peek_is_not(Token::BraceClose) {
                        return Err(err!(self, ParserErrorKind::DeadCodeNotAllowed));
                    }

                    Statement::Return(opt)
                }
                Token::Continue => {
                    if !context.is_in_a_loop() {
                        return Err(err!(self, ParserErrorKind::UnexpectedToken(Token::Continue)));
                    }

                    // we can't have anything after a continue
                    if self.peek_is_not(Token::BraceClose) {
                        return Err(err!(self, ParserErrorKind::DeadCodeNotAllowed));
                    }

                    Statement::Continue
                },
                Token::Break => {
                    if !context.is_in_a_loop() {
                        return Err(err!(self, ParserErrorKind::UnexpectedToken(Token::Break)));
                    }

                    // we can't have anything after a break
                    if self.peek_is_not(Token::BraceClose) {
                        return Err(err!(self, ParserErrorKind::DeadCodeNotAllowed));
                    }

                    Statement::Break
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
                    ok = Self::ends_with_return(statements)?;
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
    fn read_function(&mut self, entry: bool, context: &mut Context<'a>) -> Result<(), ParserError<'a>> {
        trace!("Read function");
        context.begin_scope();

        let token = self.advance()?;
        let (instance_name, for_type, name) = if !entry && token == Token::ParenthesisOpen {
            let instance_name = self.next_identifier()?;
            let for_type = self.read_type()?;
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

        // all entries must return a u64 value without being specified
        let return_type: Option<Type> = if entry {
            // an entrypoint cannot be a method
            if for_type.is_some() {
                return Err(err!(self, ParserErrorKind::EntryFunctionCannotHaveForType))
            }

            Some(Type::U64)
        } else if self.peek_is(Token::ReturnType) { // read returned type
            self.advance()?;
            Some(self.read_type()?)
        } else {
            None
        };

        let id = self.global_mapper.functions_mut().register(name, for_type.clone(), parameters.clone())
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


        let function = match entry {
            true => FunctionType::Entry(EntryFunction::new(new_params, Vec::new(), context.max_variables_count() as u16)),
            false => FunctionType::Declared(DeclaredFunction::new(
                for_type,
                instance_name,
                new_params,
                Vec::new(),
                return_type.clone(),
                0
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
                Token::Function => self.read_function(false, &mut context)?,
                Token::Entry => self.read_function(true, &mut context)?,
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
        test_parser_statement_with(tokens, variables, &None, env)
    }

    #[track_caller]
    fn test_parser_statement_with(tokens: Vec<Token>, variables: Vec<(&str, Type)>, return_type: &Option<Type>, env: EnvironmentBuilder) -> Vec<Statement> {
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
        test_parser_statement_with(tokens, variables, &Some(return_type), env)
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
            Expression::Constant(Constant::Array(vec![Value::U64(1).into(), Value::U64(2).into()]))
        )
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
            Expression::Constant(Constant::Default(xelis_types::Value::U64(25)))
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
        env.register_constant(Type::U8, "MAX", Value::U8(u8::MAX).into());

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, env);
        assert_eq!(statements.len(), 1);
    
        // Build the expected AST
        let expected_ast = Statement::Expression(
            Expression::Constant(Constant::Default(xelis_types::Value::U64(25+255)))
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
        assert_eq!(constant.value, Value::U64(10).into());
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
                    id: 0,
                    value_type: Type::Range(Box::new(Type::U64)),
                    value: Expression::Constant(
                        Value::Range(
                            Box::new(Value::U64(0)),
                            Box::new(Value::U64(10)),
                            Type::U64
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
                    id: 0,
                    value_type: Type::Optional(Box::new(Type::Optional(Box::new(Type::U64)))),
                    value: Expression::Constant(Value::Null.into())
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
                    id: 0,
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
        map.insert(Value::U64(0).into(), Value::String("hello".to_owned()).into());

        assert_eq!(
            statements[0],
            Statement::Variable(
                DeclarationStatement {
                    id: 0,
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
                    id: 0,
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
                    Value::Range(
                        Box::new(Value::U64(0)),
                        Box::new(Value::U64(10)),
                        Type::U64
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
                    id: 0,
                    value_type: Type::Array(Box::new(Type::U64)),
                    value: Expression::Constant(
                        Constant::Array(
                            vec![
                                Value::U64(1).into(),
                                Value::U64(2).into(),
                                Value::U64(3).into()
                            ]
                        )
                    )
                }
            ),
            Statement::Return(Some(Expression::Operator(
                Operator::Gt,
                Box::new(Expression::Constant(Value::U32(0).into())),
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
    fn test_function_on_type() {
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
        let program = test_parser_with_env(tokens, &env);
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
                        Box::new(Expression::Constant(Value::U64(10).into()))
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
                        Box::new(Expression::Constant(Value::U64(10).into()))
                    ),
                    Vec::new(),
                    None
                ),
                Statement::Return(Some(Expression::Constant(Value::U64(0).into())))
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
                        Box::new(Expression::Constant(Value::U64(10).into()))
                    ),
                    Vec::new(),
                    Some(vec![
                        Statement::If(
                            Expression::Operator(
                                Operator::Lt,
                                Box::new(Expression::Variable(0)),
                                Box::new(Expression::Constant(Value::U64(20).into()))
                            ),
                            Vec::new(),
                            Some(Vec::new())
                        )
                    ])
                ),
                Statement::Return(Some(Expression::Constant(Value::U64(0).into())))
            ])                
        ]);
    }

    #[test]
    fn test_ends_with_return() {
        const RETURN: Statement = Statement::Return(Some(Expression::Constant(Constant::Default(Value::U64(0)))));
        let statements = vec![RETURN];
        assert!(Parser::ends_with_return(&statements).unwrap());

        let statements = vec![RETURN, Statement::Expression(Expression::Constant(Value::U64(0).into()))];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if ... return
        let statements = vec![Statement::If(Expression::Constant(Value::Boolean(true).into()), Vec::new(), None), RETURN];
        assert!(Parser::ends_with_return(&statements).unwrap());

        let statements = vec![Statement::If(Expression::Constant(Value::Boolean(true).into()), Vec::new(), None)];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if else
        let statements = vec![Statement::If(Expression::Constant(Value::Boolean(true).into()), Vec::new(), Some(Vec::new()))];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if return else return
        let statements = vec![
            Statement::If(Expression::Constant(Value::Boolean(true).into()), vec![RETURN], Some(vec![RETURN]))
        ];
        assert!(Parser::ends_with_return(&statements).unwrap());

        // if return else if return else no return
        let statements = vec![
            Statement::If(
                Expression::Constant(Value::Boolean(true).into()),
                vec![RETURN],
                Some(vec![
                    Statement::If(Expression::Constant(Value::Boolean(true).into()), vec![RETURN], None)
                ])
            )
        ];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if return else if return else return
        let statements = vec![
            Statement::If(
                Expression::Constant(Value::Boolean(true).into()),
                vec![RETURN],
                Some(vec![
                    Statement::If(Expression::Constant(Value::Boolean(true).into()), vec![RETURN], Some(vec![RETURN]))
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

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, env);
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
        env.register_constant(Type::U64, "MAX", Value::U64(u64::MAX).into());
        let statements = test_parser_statement_with(tokens, Vec::new(), &None, env);
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

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, env);
        assert_eq!(statements.len(), 1);
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

        let statements = test_parser_statement_with(tokens, Vec::new(), &None, env);
        assert_eq!(statements.len(), 1);
    }
}