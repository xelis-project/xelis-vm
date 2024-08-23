mod scope;
mod context;
mod error;
mod struct_manager;

pub(crate) use struct_manager::{StructBuilder, StructManager};

pub use self::error::ParserError;
use self::context::Context;

use crate::{
    ast::*,
    mapper::{FunctionMapper, IdMapper, Mapper},
    types::{Struct, Type},
    values::Value,
    EnvironmentBuilder,
    IdentifierType,
    NoHashMap,
};
use std::{
    borrow::Cow,
    collections::{HashSet, VecDeque},
    convert::TryInto
};

macro_rules! convert {
    ($a: expr) => {{
        match $a.try_into() {
            Ok(v) => v,
            Err(_) => return Err(ParserError::InvalidNumberValueForType)
        }
    }};
}

#[derive(Debug)]
pub struct Program {
    // All constants declared
    pub constants: HashSet<DeclarationStatement>,
    // All structures declared
    pub structures: NoHashMap<Struct>,
    // All functions declared
    pub functions: NoHashMap<FunctionType>
}

pub struct Parser<'a> {
    // Tokens to process
    tokens: VecDeque<Token<'a>>,
    // All constants declared
    constants: HashSet<DeclarationStatement>,
    // All functions registered by the program
    functions: NoHashMap<FunctionType>,
    // Functions mapper
    functions_mapper: FunctionMapper<'a>,
    // Struct manager
    struct_manager: StructManager<'a>,
    // Environment contains all the library linked to the program
    env: &'a EnvironmentBuilder<'a>,
    // TODO: Path to use to import files
    _path: Option<&'a str>
}

impl<'a> Parser<'a> {
    pub fn new(path: Option<&'a str>, tokens: VecDeque<Token<'a>>, env: &'a EnvironmentBuilder) -> Self {
        let functions_mapper = FunctionMapper::with_parent(env.get_functions_mapper());

        Parser {
            tokens,
            constants: HashSet::new(),
            functions: NoHashMap::default(),
            functions_mapper,
            struct_manager: StructManager::new(),
            env,
            _path: path
        }
    }

    // Consume the next token
    fn advance(&mut self) -> Result<Token<'a>, ParserError<'a>> {
        self.tokens.pop_front().ok_or(ParserError::ExpectedToken)
    }

    // Consume the next token without error
    fn next(&mut self) -> Option<Token<'a>> {
        self.tokens.pop_front()
    }

    // Peek the next token without consuming it
    fn peek(&self) -> Result<&Token<'a>, ParserError<'a>> {
        self.tokens.front().ok_or(ParserError::ExpectedToken)
    }

    // Limited to 32 characters
    fn next_identifier(&mut self) -> Result<&'a str, ParserError<'a>> {
        match self.advance()? {
            Token::Identifier(id) => Ok(id),
            token => Err(ParserError::ExpectedIdentifierToken(token))
        }
    }

    // Check if the next token is an identifier
    fn peek_is_identifier(&self) -> bool {
        self.peek().ok().filter(|t| match t {
            Token::Identifier(_) => true,
            _ => false
        }).is_some()
    }

    // Require a specific token
    fn expect_token(&mut self, expected: Token<'a>) -> Result<(), ParserError<'a>> {
        let token = self.advance()?;
        if token != expected {
            return Err(ParserError::InvalidToken(token, expected)) 
        }
        Ok(())
    }

    /**
     * Example: let message: string[] = ["hello", "world", "!"];
     * Types: (unsigned)
     * - u8
     * - u16
     * - u64
     * - u128
     * - string
     * - bool
     * - Struct (Structure with name that starts with a uppercase letter)
     * - T[] (where T is any above Type)
     */
    fn read_type(&mut self) -> Result<Type, ParserError<'a>> {
        let token = self.advance()?;
        let mut _type = match Type::from_token(token, &self.struct_manager) {
            Some(v) => v,
            None => return Err(ParserError::TypeNotFound)
        };

        // support multi dimensional arrays
        loop {
            let token = self.advance()?;
            if token != Token::BracketOpen {
                // Push back
                // This allow us to economize one read per iteration on array type
                // by simply pushing back the token that we don't need
                self.tokens.push_front(token);
                break;
            }

            self.expect_token(Token::BracketClose)?;
            _type = Type::Array(Box::new(_type));
        }

        Ok(_type)
    }

    // get the type of an expression
    fn get_type_from_expression<'b>(&'b self, on_type: Option<&Type>, expression: &'b Expression, context: &'b Context) -> Result<Cow<'b, Type>, ParserError<'a>> {
        match self.get_type_from_expression_internal(on_type, expression, context)? {
            Some(v) => Ok(v),
            None => Err(ParserError::EmptyValue)
        }
    }

    // this function don't verify, but only returns the type of an expression
    // all tests should be done when constructing an expression, not here
    fn get_type_from_expression_internal<'b>(&'b self, on_type: Option<&Type>, expression: &'b Expression, context: &'b Context) -> Result<Option<Cow<'b, Type>>, ParserError<'a>> {
        let _type: Cow<'b, Type> = match expression {
            Expression::ArrayConstructor(ref values) => match values.first() {
                Some(v) => Cow::Owned(Type::Array(Box::new(self.get_type_from_expression(on_type, v, context)?.into_owned()))),
                None => return Err(ParserError::EmptyArrayConstructor) // cannot determine type from empty array
            },
            Expression::Variable(ref var_name) => match on_type {
                Some(t) => {
                    if let Type::Struct(struct_name) = t {
                        let structure = self.struct_manager.get(struct_name)?;
                        if structure.fields.contains_key(var_name) {
                            Cow::Borrowed(structure.fields.get(var_name).ok_or_else(|| ParserError::UnexpectedMappedVariableId(*var_name))?)
                        } else {
                            return Err(ParserError::UnexpectedMappedVariableId(var_name.clone()))
                        }
                    } else {
                        return Err(ParserError::UnexpectedMappedVariableId(var_name.clone()))
                    }
                },
                None => {
                    if context.has_variable(var_name) {
                        Cow::Borrowed(context.get_type_of_variable(var_name)?)
                    } else {
                        return Err(ParserError::UnexpectedMappedVariableId(var_name.clone()))
                    }
                }
            },
            Expression::FunctionCall(path, name, _) => {
                let func = self.get_function(name)?;
                match &func.return_type() {
                    Some(ref v) => match v {
                        Type::T => match on_type {
                            Some(t) => Cow::Owned(t.get_inner_type().clone()),
                            None => match path {
                                Some(p) => Cow::Owned(self.get_type_from_expression(on_type, p, context)?.get_inner_type().clone()),
                                None => return Err(ParserError::InvalidTypeT)
                            }
                        },
                        Type::Optional(inner) => match inner.as_ref() {
                            Type::T => match on_type {
                                Some(t) => Cow::Owned(Type::Optional(Box::new(t.get_inner_type().clone()))),
                                None => match path {
                                    Some(p) => Cow::Owned(Type::Optional(Box::new(self.get_type_from_expression(on_type, p, context)?.get_inner_type().clone()))),
                                    None => return Err(ParserError::InvalidTypeT)
                                }
                            },
                            _ => Cow::Borrowed(v)
                        },
                        _ => Cow::Borrowed(v)
                    },
                    None => return Err(ParserError::FunctionNoReturnType)
                }
            },
            // we have to clone everything due to this
            Expression::Value(ref val) => match Type::from_value(val, self.struct_manager.inner()) {
                Some(v) => Cow::Owned(v),
                None => return Ok(None)
            },
            Expression::ArrayCall(path, _) => {
                match self.get_type_from_expression(on_type, path, context)?.into_owned() {
                    Type::Array(_type) => Cow::Owned(*_type),
                    _ => return Err(ParserError::InvalidArrayCall)
                }
            },
            Expression::SubExpression(expr) => self.get_type_from_expression(on_type, expr, context)?,
            Expression::StructConstructor(name, _) => {
                if !self.struct_manager.has(name) {
                    return Err(ParserError::StructNotFound(name.clone()))
                }

                Cow::Owned(Type::Struct(name.clone()))
            }
            Expression::Path(left, right) => {
                let var_type = self.get_type_from_expression(on_type, left, context)?;
                self.get_type_from_expression(Some(&var_type), right, context)?
            },
            // Compatibility checks are done when constructing the expression
            Expression::Operator(op, left, right) => match op {
                // Condition operators
                Operator::Or
                | Operator::Equals
                | Operator::NotEquals
                | Operator::GreaterOrEqual
                | Operator::GreaterThan
                | Operator::LessOrEqual
                | Operator::LessThan
                | Operator::And => Cow::Owned(Type::Bool),
                // Assign operators
                Operator::Assign(_) => return Err(ParserError::AssignReturnNothing),
                // String compatible operators
                Operator::Plus | Operator::Minus => {
                    let left_type = self.get_type_from_expression(on_type, left, context)?;
                    let right_type = self.get_type_from_expression(on_type, right, context)?;

                    if *left_type == Type::String || *right_type == Type::String {
                        Cow::Owned(Type::String)
                    } else {
                        left_type
                    }
                },
                // Number only operators
                Operator::Multiply
                | Operator::Divide
                | Operator::BitwiseXor
                | Operator::BitwiseAnd
                | Operator::BitwiseOr
                | Operator::BitwiseLeft
                | Operator::BitwiseRight
                | Operator::Rem => {
                    let left_type = self.get_type_from_expression(on_type, left, context)?;
                    let right_type = self.get_type_from_expression(on_type, right, context)?;

                    if !left_type.is_number() || !right_type.is_number() || left_type != right_type {
                        return Err(ParserError::InvalidOperationNotSameType(left_type.into_owned(), right_type.into_owned()))
                    }
                    left_type
                }
            },
            Expression::IsNot(_) => Cow::Owned(Type::Bool),
            Expression::Ternary(_, expr, _) => self.get_type_from_expression(on_type, expr, context)?,
            Expression::Cast(_, _type) => Cow::Borrowed(_type)
        };

        Ok(Some(_type))
    }

    // Read a function call with the following syntax:
    // function_name(param1, param2, ...)
    fn read_function_call(&mut self, on_path: Option<Expression>, on_type: Option<&Type>, name: &str, context: &mut Context, mapper: &mut IdMapper) -> Result<Expression, ParserError<'a>> {
        // we remove the token from the list
        self.expect_token(Token::ParenthesisOpen)?;
        let mut parameters: Vec<Expression> = Vec::new();
        let mut types: Vec<Type> = Vec::new();

        // read parameters for function call
        while *self.peek()? != Token::ParenthesisClose {
            let expr = self.read_expression(context, mapper)?;
            // We are forced to clone the type because we can't borrow it from the expression
            // I prefer to do this than doing an iteration below
            types.push(self.get_type_from_expression(None, &expr, context)?.into_owned());
            parameters.push(expr);

            if *self.peek()? == Token::Comma {
                self.expect_token(Token::Comma)?;
            }
        }

        let id = self.functions_mapper.get_compatible(Signature::new(name.to_owned(), on_type.cloned(), types))?;
        let func = self.get_function(&id)?;
        // Entry are only callable by external
        if func.is_entry() {
            return Err(ParserError::FunctionNotFound(id))
        }

        self.expect_token(Token::ParenthesisClose)?;
        Ok(Expression::FunctionCall(on_path.map(Box::new), id, parameters))
    }

    // Read a struct constructor with the following syntax:
    // struct_name { field_name: value1, field2: value2 }
    // If we have a field that has the same name as a variable we can pass it as following:
    // Example: struct_name { field_name, field2: value2 }
    fn read_struct_constructor(&mut self, on_type: Option<&Type>, struct_name: IdentifierType, context: &mut Context, mapper: &mut IdMapper) -> Result<Expression, ParserError<'a>> {
        self.expect_token(Token::BraceOpen)?;
        let structure = self.struct_manager.get(&struct_name)?.clone();
        let mut fields = NoHashMap::default();
        for (id, t) in structure.fields.iter() {
            let field_name = self.next_identifier()?;
            let field_value = match self.advance()? {
                Token::Comma => {
                    if let Ok(id) = mapper.get(field_name) {
                        Expression::Variable(id)
                    } else {
                        return Err(ParserError::UnexpectedVariable(field_name.to_owned())) 
                    }
                }
                Token::Colon => {
                    let value = self.read_expr(on_type, true, Some(t), context, mapper)?;
                    if *self.peek()? == Token::Comma {
                        self.advance()?;
                    }
                    value
                }
                token => return Err(ParserError::UnexpectedToken(token))
            };

            let field_type = self.get_type_from_expression(on_type, &field_value, context)?;
            if *t != *field_type {
                return Err(ParserError::InvalidValueType(field_type.into_owned(), t.clone()))
            }

            fields.insert(id.clone(), field_value);
        }
        self.expect_token(Token::BraceClose)?;
        Ok(Expression::StructConstructor(struct_name, fields))
    }

    // Read an expression with default parameters
    fn read_expression(&mut self, context: &mut Context, mapper: &mut IdMapper) -> Result<Expression, ParserError<'a>> {
        self.read_expr(None, true, None, context, mapper)
    }

    // Read an expression with the possibility to accept operators
    // number_type is used to force the type of a number
    fn read_expr(&mut self, on_type: Option<&Type>, accept_operator: bool, expected_type: Option<&Type>, context: &mut Context, mapper: &mut IdMapper) -> Result<Expression, ParserError<'a>> {
        let mut required_operator = false;
        let mut last_expression: Option<Expression> = None;
        while {
            let peek = self.peek()?;
            !peek.should_stop() && ((required_operator == peek.is_operator()) || (*peek == Token::BracketOpen && last_expression.is_none()))
        } {
            if !accept_operator && required_operator {
                break;
            }

            let expr: Expression = match self.advance()? {
                Token::BracketOpen => {
                    match last_expression {
                        Some(v) => {
                            if !self.get_type_from_expression(on_type, &v, context)?.is_array() {
                                return Err(ParserError::InvalidArrayCall)
                            }

                            // Index must be of type u64
                            let index = self.read_expr(on_type, true, Some(&Type::U64), context, mapper)?;
                            let index_type = self.get_type_from_expression(on_type, &index, context)?;
                            if *index_type != Type::U64 {
                                return Err(ParserError::InvalidArrayCallIndexType(index_type.into_owned()))
                            }

                            self.expect_token(Token::BracketClose)?;
                            required_operator = !required_operator;
                            Expression::ArrayCall(Box::new(v), Box::new(index))
                        },
                        None => { // require at least one value in a array constructor
                            let mut expressions: Vec<Expression> = Vec::new();
                            let mut array_type: Option<Type> = None;
                            while *self.peek()? != Token::BracketClose {
                                let expr = self.read_expr(on_type, true, expected_type.map(|t| t.get_inner_type()), context, mapper)?;
                                match &array_type { // array values must have the same type
                                    Some(t) => {
                                        let _type = self.get_type_from_expression(on_type, &expr, context)?;
                                        if *_type != *t {
                                            return Err(ParserError::InvalidTypeInArray(_type.into_owned(), t.clone()))
                                        }
                                    },
                                    None => { // first value rules the type of array
                                        array_type = Some(self.get_type_from_expression(on_type, &expr, context)?.into_owned());
                                    }
                                };
                                expressions.push(expr);

                                if *self.peek()? == Token::Comma {
                                    self.expect_token(Token::Comma)?;
                                }
                            }

                            self.expect_token(Token::BracketClose)?;
                            Expression::ArrayConstructor(expressions)
                        }
                    }
                },
                Token::ParenthesisOpen => {
                    let expr = self.read_expr(None, true, expected_type, context, mapper)?;
                    self.expect_token(Token::ParenthesisClose)?;
                    Expression::SubExpression(Box::new(expr))
                },
                Token::Identifier(id) => {
                    match self.peek()? {
                        // function call
                        Token::ParenthesisOpen => self.read_function_call(last_expression.take(), on_type, id, context, mapper)?,
                        _ => {
                            match on_type {
                                // mostly an access to a struct field
                                Some(t) => {
                                    if let Type::Struct(struct_name) = t {
                                        let builder = self.struct_manager.get(struct_name)?;
                                        
                                        match builder.mapper.get(id).ok() {
                                            Some(v) => Expression::Variable(v),
                                            None => return Err(ParserError::UnexpectedVariable(id.to_owned()))
                                        }
                                    } else {
                                        return Err(ParserError::UnexpectedType(t.clone()))
                                    }
                                },
                                None => {
                                    if let Ok(id) = mapper.get(id) {
                                        Expression::Variable(id)
                                    } else if let Ok(id) = self.struct_manager.get_mapping(&id) {
                                        self.read_struct_constructor(on_type, id, context, mapper)?
                                    } else {
                                        return Err(ParserError::UnexpectedVariable(id.to_owned()))
                                    }
                                }
                            }
                        }
                    }
                },
                Token::U64Value(value) => Expression::Value(match expected_type {
                    Some(t) => match t {
                        Type::U8 => Value::U8(convert!(value)),
                        Type::U16 => Value::U16(convert!(value)),
                        Type::U32 => Value::U32(convert!(value)),
                        Type::U64 => Value::U64(value),
                        Type::U128 => Value::U128(convert!(value)),
                        Type::Optional(inner) => Value::Optional(Some(Box::new(match inner.as_ref() {
                            Type::U8 => Value::U8(convert!(value)),
                            Type::U16 => Value::U16(convert!(value)),
                            Type::U32 => Value::U32(convert!(value)),
                            Type::U64 => Value::U64(value),
                            Type::U128 => Value::U128(convert!(value)),
                            Type::String => Value::String(value.to_string()),
                            _ => return Err(ParserError::ExpectedNumberType)
                        }))),
                        Type::String => Value::String(value.to_string()),
                        _ => return Err(ParserError::ExpectedNumberType)
                    },
                    None => Value::U64(value)
                }),
                Token::U128Value(value) => Expression::Value(Value::U128(value)),
                Token::StringValue(value) => Expression::Value(Value::String(value.into_owned())),
                Token::True => Expression::Value(Value::Boolean(true)),
                Token::False => Expression::Value(Value::Boolean(false)),
                Token::Null => Expression::Value(Value::Null),
                Token::Dot => {
                    match last_expression {
                        Some(value) => {
                            let _type = self.get_type_from_expression(on_type, &value, context)?.into_owned();
                            let right_expr = self.read_expr(Some(&_type), false, expected_type, context, mapper)?;
                            // because we read operator DOT + right expression
                            required_operator = !required_operator;

                            if let Expression::FunctionCall(path, name, params) = right_expr {
                                assert!(path.is_none());
                                Expression::FunctionCall(Some(Box::new(value)), name, params)
                            } else {
                                Expression::Path(Box::new(value), Box::new(right_expr))
                            }
                        },
                        None => return Err(ParserError::UnexpectedToken(Token::Dot))
                    }
                },
                Token::IsNot => { // it's an operator, but not declared as
                    let expr = self.read_expression(context, mapper)?;
                    let expr_type = self.get_type_from_expression(on_type, &expr, context)?;
                    if *expr_type != Type::Bool {
                        return Err(ParserError::InvalidValueType(expr_type.into_owned(), Type::Bool))
                    }

                    Expression::IsNot(Box::new(expr))
                },
                Token::OperatorTernary => match last_expression { // condition ? expr : expr
                    Some(expr) => {
                        if *self.get_type_from_expression(on_type, &expr, context)? != Type::Bool {
                            return Err(ParserError::InvalidCondition(Type::Bool, expr))
                        }

                        let valid_expr = self.read_expr(on_type, true, expected_type, context, mapper)?;
                        let first_type = self.get_type_from_expression(on_type, &valid_expr, context)?.into_owned();
                        self.expect_token(Token::Colon)?;
                        let else_expr = self.read_expr(on_type, true, expected_type, context, mapper)?;
                        let else_type = self.get_type_from_expression(on_type, &else_expr, context)?;
                        
                        if first_type != *else_type { // both expr should have the SAME type.
                            return Err(ParserError::InvalidValueType(else_type.into_owned(), first_type))
                        }
                        required_operator = !required_operator;
                        Expression::Ternary(Box::new(expr), Box::new(valid_expr), Box::new(else_expr))
                    },
                    None => return Err(ParserError::InvalidTernaryNoPreviousExpression)
                },
                Token::As => {
                    let previous_expr = last_expression.ok_or_else(|| ParserError::InvalidOperation)?;
                    let left_type = self.get_type_from_expression(on_type, &previous_expr, context)?.into_owned();
                    let right_type = self.read_type()?;

                    if !left_type.is_castable_to(&right_type) {
                        return Err(ParserError::CastError(left_type, right_type))
                    }
                    required_operator = !required_operator;
                    Expression::Cast(Box::new(previous_expr), right_type)
                },
                token => {
                    match last_expression {
                        Some(previous_expr) => {
                            required_operator = !required_operator;

                            let left_type = self.get_type_from_expression(on_type, &previous_expr, context)?.into_owned();
                            // Parse the operator for this token
                            let op = match Operator::value_of(&token) {
                                Some(op) => op,
                                None => return Err(ParserError::OperatorNotFound(token))
                            };

                            let expr = self.read_expr(on_type, !op.is_bool_operator(), Some(&left_type), context, mapper)?;
                            if let Some(right_type) = self.get_type_from_expression_internal(on_type, &expr, context)? {
                                match &op {
                                    Operator::Minus | Operator::Rem | Operator::Divide | Operator::Multiply
                                    | Operator::Assign(_) | Operator::BitwiseLeft | Operator::BitwiseRight
                                    | Operator::GreaterThan | Operator::LessThan | Operator::LessOrEqual
                                    | Operator::GreaterOrEqual => {
                                        if left_type != *right_type {
                                            return Err(ParserError::InvalidOperationNotSameType(left_type, right_type.into_owned()))
                                        }
                                    },
                                    Operator::Plus => {
                                        if left_type != *right_type && !(left_type == Type::String || *right_type == Type::String) {
                                            return Err(ParserError::InvalidOperationNotSameType(left_type, right_type.into_owned()))
                                        }
                                    },
                                    Operator::And | Operator::Or => {
                                        if left_type != Type::Bool {
                                            return Err(ParserError::InvalidOperationNotSameType(left_type, Type::Bool))
                                        }

                                        if *right_type != Type::Bool {
                                            return Err(ParserError::InvalidOperationNotSameType(right_type.into_owned(), Type::Bool))
                                        }
                                    },
                                    _ => if left_type != *right_type {
                                        return Err(ParserError::InvalidOperationNotSameType(left_type, right_type.into_owned()))
                                    }
                                };

                                Expression::Operator(op, Box::new(previous_expr), Box::new(expr))
                            } else {
                                match op {
                                    Operator::Equals | Operator::NotEquals |
                                    Operator::Assign(None) if left_type.allow_null() => Expression::Operator(op, Box::new(previous_expr), Box::new(expr)),
                                    _ => return Err(ParserError::IncompatibleNullWith(left_type))
                                }
                            }
                        }
                        None => return Err(ParserError::InvalidOperation)
                    }
                }
            };

            last_expression = Some(expr);
            required_operator = !required_operator;
        }

        match last_expression {
            Some(v) => Ok(v),
            None => Err(ParserError::NotImplemented)
        }
    }

    /**
     * {
     *     ...
     * }
     */
    fn read_body(&mut self, context: &mut Context, return_type: &Option<Type>, consume_brace: bool, mapper: &mut IdMapper<'a>) -> Result<Vec<Statement>, ParserError<'a>> {
        context.begin_scope();
        let statements = self.read_statements(context, return_type, consume_brace, mapper)?;
        context.end_scope();
        Ok(statements)
    }

    /**
     * Example: let hello: string = "hello";
     * Rules:
     * - Every variable must be declared with 'let' keyword
     * - Variable name must be alphanumeric characters
     * - Must provide a value type
     * - If no value is set, Null is set by default
     */
    fn read_variable(&mut self, context: &mut Context, mapper: &mut IdMapper<'a>, is_const: bool) -> Result<DeclarationStatement, ParserError<'a>> {
        let name: &'a str = self.next_identifier()?;

        // Variable name must be unique
        // Shadowing is not allowed atm
        if mapper.has_variable(name) {
            return Err(ParserError::VariableNameAlreadyUsed(name.to_owned()))
        }

        // Constants must be uppercase
        if is_const && name.to_uppercase() != name {
            return Err(ParserError::ConstantNameNotUppercase(name.to_owned()))
        }

        // Variable name must start with a alphabetic character
        if !name.starts_with(char::is_alphabetic) {
            return Err(ParserError::VariableMustStartWithAlphabetic(name.to_owned()))
        }

        self.expect_token(Token::Colon)?;
        let value_type = self.read_type()?;
        let value: Expression = if *self.peek()? == Token::OperatorAssign {
            self.expect_token(Token::OperatorAssign)?;
            let expr = self.read_expr(None, true, Some(&value_type), context, mapper)?;

            let expr_type = match self.get_type_from_expression_internal(None, &expr, context) {
                Ok(opt_type) => match opt_type {
                    Some(v) => v,
                    None => if value_type.is_optional() {
                        Cow::Owned(value_type.clone())
                    } else {
                        return Err(ParserError::NoValueType)
                    }
                },
                Err(e) => match e { // support empty array declaration
                    ParserError::EmptyArrayConstructor if value_type.is_array() => Cow::Owned(value_type.clone()),
                    _ => return Err(e)
                }
            };

            if !expr_type.is_compatible_with(&value_type) {
                return Err(ParserError::InvalidValueType(expr_type.into_owned(), value_type))
            }

            expr
        } else {
            Expression::Value(Value::Null)
        };

        let id = mapper.register(Cow::Borrowed(name))?;
        context.register_variable(id, value_type.clone())?;

        Ok(DeclarationStatement {
            id,
            value_type,
            value
        })
    }

    fn read_loop_body(&mut self, context: &mut Context, return_type: &Option<Type>, mapper: &mut IdMapper<'a>) -> Result<Vec<Statement>, ParserError<'a>> {
        let old_value = context.is_in_a_loop(); // support loop in loop
        context.set_in_a_loop(true);
        let statements = self.read_body(context, return_type, true, mapper)?;
        context.set_in_a_loop(old_value);

        Ok(statements)
    }

    // Read all statements in a block
    // return type is used to verify that the last statement is a return with a valid value type
    // consume_brace is used to know if we should consume the open brace
    fn read_statements(&mut self, context: &mut Context, return_type: &Option<Type>, consume_brace: bool, mapper: &mut IdMapper<'a>) -> Result<Vec<Statement>, ParserError<'a>> {
        if consume_brace {
            self.expect_token(Token::BraceOpen)?;
        }

        let mut statements: Vec<Statement> = Vec::new();
        while let Some(token) = self.next() {
            let statement: Statement = match token {
                Token::BraceClose => break,
                Token::For => { // Example: for i: u64 = 0; i < 10; i += 1 {}
                    context.begin_scope();
                    let var = self.read_variable(context, mapper, false)?;
                    let condition = self.read_expression(context, mapper)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if  *condition_type != Type::Bool {
                        return Err(ParserError::InvalidCondition(condition_type.into_owned(), condition))
                    }

                    let increment = self.read_expression(context, mapper)?;
                    match &increment { // allow only assignations on this expr
                        Expression::Operator(op, _, _) if op.is_assignation() => {},
                        _ => return Err(ParserError::InvalidForExpression(increment))
                    };

                    let statements = self.read_loop_body(context, return_type, mapper)?;
                    context.end_scope();

                    Statement::For(var, condition, increment, statements)
                }
                Token::ForEach => { // Example: foreach a in array {}
                    context.begin_scope();
                    let variable = self.next_identifier()?;
                    self.expect_token(Token::In)?;
                    let expr = self.read_expression(context, mapper)?;
                    let expr_type = self.get_type_from_expression(None, &expr, context)?;

                    // verify that we can iter on it
                    if !expr_type.is_array() {
                        return Err(ParserError::ExpectedArrayType)
                    }

                    let id = mapper.register(Cow::Borrowed(variable))?;
                    context.register_variable(id, expr_type.get_inner_type().clone())?;
                    let statements = self.read_loop_body(context, return_type, mapper)?;
                    context.end_scope();

                    Statement::ForEach(id, expr, statements)
                },
                Token::While => { // Example: while i < 10 {}
                    let condition = self.read_expression(context, mapper)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if  *condition_type != Type::Bool {
                        return Err(ParserError::InvalidCondition(condition_type.into_owned(), condition))
                    }

                    let statements = self.read_loop_body(context, return_type, mapper)?;

                    Statement::While(condition, statements)
                },
                Token::If => {
                    let condition = self.read_expression(context, mapper)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if  *condition_type != Type::Bool {
                        return Err(ParserError::InvalidCondition(condition_type.into_owned(), condition))
                    }

                    let body = self.read_body(context, return_type, true, mapper)?;

                    let else_statement = if *self.peek()? == Token::Else {
                        self.advance()?;
                        Some(if *self.peek()? == Token::If {
                            self.read_statements(context, return_type, false, mapper)?
                        } else {
                            self.read_body(context, return_type, true, mapper)?
                        })
                    } else {
                        None
                    };

                    Statement::If(condition, body, else_statement)
                },
                Token::BraceOpen => Statement::Scope(self.read_body(context, return_type, false, mapper)?),
                Token::Let => Statement::Variable(self.read_variable(context, mapper, false)?),
                Token::Return => {
                    let opt: Option<Expression> = if let Some(return_type) = return_type {
                        let expr = self.read_expr(None, true, Some(return_type), context, mapper)?;
                        let expr_type = self.get_type_from_expression(None, &expr, context)?;
                        if *expr_type != *return_type {
                            return Err(ParserError::InvalidValueType(expr_type.into_owned(), return_type.clone()))
                        }
                        Some(expr)
                    } else {
                        None
                    };

                    // we can't have anything after a return
                    if *self.peek()? != Token::BraceClose {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Return(opt)
                }
                Token::Continue => {
                    if !context.is_in_a_loop() {
                        return Err(ParserError::UnexpectedToken(Token::Continue));
                    }

                    // we can't have anything after a continue
                    if *self.peek()? != Token::BraceClose {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Continue
                },
                Token::Break => {
                    if !context.is_in_a_loop() {
                        return Err(ParserError::UnexpectedToken(Token::Break));
                    }

                    // we can't have anything after a break
                    if *self.peek()? != Token::BraceClose {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Break
                },
                token => {
                    self.tokens.push_front(token);
                    Statement::Expression(self.read_expression(context, mapper)?)
                }
            };
            statements.push(statement);
        }

        Ok(statements)
    }

    // Read the parameters for a function
    fn read_parameters(&mut self, mapper: &mut IdMapper<'a>) -> Result<Vec<Parameter>, ParserError<'a>> {
        let mut parameters: Vec<Parameter> = Vec::new();
        while self.peek_is_identifier() {
            let name = self.next_identifier()?;
            self.expect_token(Token::Colon)?;
            let value_type = self.read_type()?;

            parameters.push(Parameter::new(mapper.register(Cow::Borrowed(name))?, value_type));

            if *self.peek()? != Token::Comma {
                break;
            }

            self.expect_token(Token::Comma)?;
        }

        Ok(parameters)
    }

    // Verify that the last statement is a return
    // We don't check the last statement directly has it would allow
    // to have dead code after a return
    fn ends_with_return(&self, statements: &Vec<Statement>) -> Result<bool, ParserError<'a>> {
        let mut ok = false;
        if let Some(statement) = statements.last() {
            match statement {
                Statement::If(_, statements, else_statements) => {
                    // if its the last statement
                    ok = self.ends_with_return(statements)?;
                    if let Some(statements) = else_statements.as_ref().filter(|_| ok) {
                        self.ends_with_return(statements)?;
                    }
                }
                Statement::Return(_) => {
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
     * - func foo() { ... }
     * - func foo(): u64 { ... }
     * - func foo(a: u64, b: u64) { ... }
     * - func (f Foo) bar() { ... }
     * Rules:
     * - Signature is based on function name, and parameters
     * - Entry function is a "public callable" function and must return a u64 value
     */
    fn read_function(&mut self, entry: bool, context: &mut Context, constants_mapper: &IdMapper<'a>) -> Result<(), ParserError<'a>> {
        context.begin_scope();

        // we need to clone the constants mapper to use it as our own local mapper
        let mut mapper = constants_mapper.clone();

        let token = self.advance()?;
        let (instance_name, for_type, name) = if !entry && token == Token::ParenthesisOpen {
            let instance_name = self.next_identifier()?;
            let for_type = self.read_type()?;

            // verify that the type is a struct
            if let Type::Struct(struct_name) = &for_type {
                // only types that are declared by the same program
                if !self.struct_manager.has(struct_name) {
                    return Err(ParserError::StructNotFound(struct_name.clone()))
                }
            } else {
                return Err(ParserError::InvalidFunctionType(for_type))
            }

            let id = mapper.register(Cow::Borrowed(instance_name))?;
            context.register_variable(id, for_type.clone())?;
            self.expect_token(Token::ParenthesisClose)?;

            (Some(id), Some(for_type), self.next_identifier()?)
        } else {
            let Token::Identifier(name) = token else {
                return Err(ParserError::ExpectedIdentifierToken(token))
            };
            (None, None, name)
        };

        self.expect_token(Token::ParenthesisOpen)?;
        let parameters = self.read_parameters(&mut mapper)?;
        self.expect_token(Token::ParenthesisClose)?;

        // all entries must return a u64 value without being specified
        let return_type: Option<Type> = if entry {
            // an entrypoint cannot be a method
            if for_type.is_some() {
                return Err(ParserError::EntryFunctionCannotHaveForType)
            }

            Some(Type::U64)
        } else if *self.peek()? == Token::Colon { // read returned type
            self.advance()?;
            Some(self.read_type()?)
        } else {
            None
        };

        let types: Vec<Type> = parameters.iter().map(|p| p.get_type().clone()).collect();
        let id = self.functions_mapper.register(Signature::new(name.to_owned(), for_type.clone(), types))?;
        if self.has_function(&id) {
            return Err(ParserError::FunctionSignatureAlreadyExist) 
        }

        let has_return_type = return_type.is_some();

        for param in parameters.iter() {
            context.register_variable(param.get_name().clone(), param.get_type().clone())?;
        }

        let statements = self.read_body(context, &return_type, true, &mut mapper)?;

        context.end_scope();

        // verify that the function ends with a return
        if has_return_type && !self.ends_with_return(&statements)? {
            return Err(ParserError::NoReturnFound)
        }

        let function = match entry {
            true => FunctionType::Entry(EntryFunction::new(parameters, statements)),
            false => FunctionType::Declared(DeclaredFunction::new(
                for_type,
                instance_name,
                parameters,
                statements,
                return_type,
            ))
        };

        // push function before reading statements to allow recursive calls
        self.functions.insert(id, function);

        Ok(())
    }

    // Read a type with the following syntax:
    // import "filename.xel";
    // or with an alias:
    // import "filename.xel" as alias;
    fn read_import(&mut self) -> Result<(), ParserError<'a>> {
        let path = self.advance()?;

        let Token::StringValue(path) = path else {
            return Err(ParserError::InvalidImport)
        };

        // We don't allow absolute path or path that contains ".."
        if path.starts_with("/") || path.contains("..") {
            return Err(ParserError::InvalidImportPath(path.into_owned()))
        }

        // If its a local import, we will import its content directly
        let is_local = path.ends_with(".xel");
        if !is_local {
            return Err(ParserError::NotImplemented)
        }

        // let content = fs::read_to_string(path.as_ref()).map_err(|e| ParserError::InvalidImportPath(e.to_string()))?;
        // let mut tokens = Lexer::new(&content).get().map_err(|e| ParserError::ImportLexerError(path.into_owned(), e))?;

        // if *self.peek()? == Token::As {
        //     self.expect_token(Token::As)?;
        //     todo!()
        // } else {
        //     // Append all the tokens parsed at the beginning
        //     std::mem::swap(&mut self.tokens, &mut tokens);
        //     self.tokens.append(&mut tokens);
        // }

        Ok(())
    }

    // check if a function with the same signature exists
    fn has_function(&self, name: &IdentifierType) -> bool {
        self.get_function(name).is_ok()
    }

    // get a function exist based on signature (name + params)
    fn get_function(&self, name: &IdentifierType) -> Result<&FunctionType, ParserError<'a>> {
        match self.env.get_functions().get(name) {
            Some(func) => Ok(func),
            None => self.functions.get(name).ok_or(ParserError::FunctionNotFound(name.clone()))
        }
    }

    /**
     * Example: Message { message_id: u64, message: string }
     * Rules:
     * - Structure name should start with a uppercase character
     * - only alphanumeric chars in name
     */
    fn read_struct(&mut self) -> Result<(&'a str, StructBuilder<'a>), ParserError<'a>> {
        let name = self.next_identifier()?;
        let mut chars = name.chars();
        if !chars.all(|c| c.is_ascii_alphanumeric()) {
            return Err(ParserError::InvalidStructureName(name.to_owned()))
        }

        // check if the first letter is in uppercase
        match name.chars().nth(0) {
            Some(v) => {
                if !v.is_ascii_alphabetic() || !v.is_uppercase() {
                    return Err(ParserError::InvalidStructureName(name.to_owned()))
                }
            },
            None => return Err(ParserError::EmptyStructName)
        };

        let mut mapper: Mapper<'a, Cow<str>> = IdMapper::new();
        self.expect_token(Token::BraceOpen)?;
        let mut fields: NoHashMap<Type> = NoHashMap::default();
        for param in self.read_parameters(&mut mapper)? {
            let (name, value_type) = param.consume();
            fields.insert(name, value_type);
        }

        self.expect_token(Token::BraceClose)?;

        Ok((name, StructBuilder {
            fields,
            mapper,
        }))
    }

    // Parse the tokens and return a Program
    // The function mapper is also returned for external calls
    pub fn parse(mut self) -> Result<(Program, FunctionMapper<'a>), ParserError<'a>> {
        let mut context: Context = Context::new();
        let mut constants_mapper: Mapper<'a, Cow<str>> = IdMapper::new();
        while let Some(token) = self.next() {
            match token {
                Token::Import => {
                    self.read_import()?;
                    continue;
                }
                Token::Const => {
                    let var = self.read_variable(&mut context, &mut constants_mapper, true)?;
                    let id = var.id;
                    if !self.constants.insert(var) {
                        return Err(ParserError::VariableIdAlreadyUsed(id))
                    }
                },
                Token::Function => self.read_function(false, &mut context, &constants_mapper)?,
                Token::Entry => self.read_function(true, &mut context, &constants_mapper)?,
                Token::Struct => {
                    let (name, builder) = self.read_struct()?;
                    self.struct_manager.add(name.to_owned(), builder)?;
                },
                token => return Err(ParserError::UnexpectedToken(token))
            };
        }

        Ok((Program {
            constants: self.constants,
            structures: self.struct_manager.finalize(),
            functions: self.functions,
        }, self.functions_mapper))
    }
}