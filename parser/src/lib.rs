mod context;
mod error;

use std::{
    borrow::Cow,
    collections::{HashSet, VecDeque}
};
use xelis_builder::{
    Builder,
    EnumManager,
    EnvironmentBuilder,
    FunctionMapper,
    StructManager
};
use xelis_ast::*;
use xelis_environment::NativeFunction;
use xelis_types::*;
use context::Context;

pub use error::ParserError;

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

pub struct Parser<'a> {
    // Tokens to process
    tokens: VecDeque<Token<'a>>,
    // All constants declared
    constants: HashSet<DeclarationStatement>,
    // All functions registered by the program
    functions: Vec<FunctionType>,
    // Functions mapper
    // It will contains all the functions declared in the program
    // with the matching id <-> function signature
    functions_mapper: FunctionMapper<'a>,
    // Struct manager
    struct_manager: StructManager<'a>,
    enum_manager: EnumManager<'a>,
    // Environment contains all the library linked to the program
    environment: &'a EnvironmentBuilder<'a>,
    // TODO: Path to use to import files
    // _path: Option<&'a str>
}

impl<'a> Parser<'a> {
    pub fn new(tokens: VecDeque<Token<'a>>, environment: &'a EnvironmentBuilder) -> Self {
        let functions_mapper = FunctionMapper::with_parent(environment.get_functions_mapper());

        Self {
            tokens,
            constants: HashSet::new(),
            functions: Vec::new(),
            functions_mapper,
            struct_manager: StructManager::with_parent(environment.get_struct_manager()),
            enum_manager: EnumManager::with_parent(environment.get_enum_manager()),
            environment
        }
    }

    // Consume the next token
    #[inline(always)]
    fn advance(&mut self) -> Result<Token<'a>, ParserError<'a>> {
        self.tokens.pop_front().ok_or(ParserError::ExpectedToken)
    }

    // Consume the next token without error
    #[inline(always)]
    fn next(&mut self) -> Option<Token<'a>> {
        self.tokens.pop_front()
    }

    // Peek the next token without consuming it
    #[inline(always)]
    fn peek(&self) -> Result<&Token<'a>, ParserError<'a>> {
        self.tokens.front().ok_or(ParserError::ExpectedToken)
    }

    // Limited to 32 characters
    #[inline(always)]
    fn next_identifier(&mut self) -> Result<&'a str, ParserError<'a>> {
        match self.advance()? {
            Token::Identifier(id) => Ok(id),
            token => Err(ParserError::ExpectedIdentifierToken(token))
        }
    }

    // Check if the next token is a specific token
    #[inline(always)]
    fn peek_is(&self, token: Token<'a>) -> bool {
        self.tokens.front().filter(|t| **t == token).is_some()
    }

    // Check if the next token is not a specific token
    #[inline(always)]
    fn peek_is_not(&self, token: Token<'a>) -> bool {
        self.tokens.front().filter(|t| **t != token).is_some()
    }

    // Check if the next token is an identifier
    #[inline(always)]
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

    fn get_generic_type(&mut self) -> Result<Type, ParserError<'a>> {
        self.expect_token(Token::OperatorLessThan)?;
        let token = self.advance()?;
        let inner = self.get_type_from_token(token)?;
        Ok(inner)
    }

    fn get_single_inner_type(&mut self) -> Result<Type, ParserError<'a>> {
        let inner = self.get_generic_type()?;
        self.expect_token(Token::OperatorGreaterThan)?;
        Ok(inner)
    }

    fn get_type_from_token(&mut self, token: Token<'a>) -> Result<Type, ParserError<'a>> {
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
                self.expect_token(Token::Comma)?;
                let token = self.advance()?;
                let value = self.get_type_from_token(token)?;
                self.expect_token(Token::OperatorGreaterThan)?;

                Type::Map(Box::new(key), Box::new(value))
            }
            Token::Identifier(id) => {
                if let Ok(builder) = self.struct_manager.get_by_name(id) {
                    Type::Struct(builder.get_type().clone())
                } else if let Ok(builder) = self.enum_manager.get_by_name(id) {
                    Type::Enum(builder.get_type().clone())
                } else {
                    return Err(ParserError::TypeNameNotFound(id))
                }
            },
            token => return Err(ParserError::UnexpectedToken(token))
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
        let token = self.advance()?;
        let mut _type = self.get_type_from_token(token)?;

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
    fn get_type_from_expression<'b>(&'b self, on_type: Option<&Type>, expression: &'b Expression, context: &'b Context<'a>) -> Result<Cow<'b, Type>, ParserError<'a>> {
        match self.get_type_from_expression_internal(on_type, expression, context)? {
            Some(v) => Ok(v),
            None => Err(ParserError::EmptyValue)
        }
    }

    fn get_from_generic_type<'b>(&'b self, on_type: Option<&Type>, _type: &'b Type, path: Option<&Expression>, context: &'b Context<'a>) -> Result<Type, ParserError<'a>> {
        Ok(match _type {
            Type::T(id) => match on_type {
                Some(t) => t.get_generic_type(*id).ok_or(ParserError::InvalidTypeT)?.clone(),
                None => match path {
                    Some(v) => {
                        let on_type = self.get_type_from_expression(on_type, v, context)?;
                        self.get_from_generic_type(Some(&on_type), _type, path, context)?
                    },
                    None => return Err(ParserError::NoValueType)
                }
            },
            Type::Optional(inner) => Type::Optional(Box::new(self.get_from_generic_type(on_type, inner, path, context)?)),
            _ => _type.clone()
        })
    }

    // this function don't verify, but only returns the type of an expression
    // all tests should be done when constructing an expression, not here
    fn get_type_from_expression_internal<'b>(&'b self, on_type: Option<&Type>, expression: &'b Expression, context: &'b Context<'a>) -> Result<Option<Cow<'b, Type>>, ParserError<'a>> {
        let _type: Cow<'b, Type> = match expression {
            Expression::ArrayConstructor(ref values) => match values.first() {
                Some(v) => Cow::Owned(Type::Array(Box::new(self.get_type_from_expression(on_type, v, context)?.into_owned()))),
                None => return Err(ParserError::EmptyArrayConstructor) // cannot determine type from empty array
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
                            return Err(ParserError::UnexpectedMappedVariableId(var_name.clone()))
                        }
                    } else {
                        return Err(ParserError::UnexpectedMappedVariableId(var_name.clone()))
                    }
                },
                None => Cow::Borrowed(context.get_type_of_variable(var_name)?),
            },
            Expression::FunctionCall(path, name, _) => {
                let f = self.get_function(*name)?;
                let return_type = f.return_type();
                match return_type {
                    Some(ref v) => Cow::Owned(self.get_from_generic_type(on_type, v, path.as_deref(), context)?),
                    None => return Err(ParserError::FunctionNoReturnType)
                }
            },
            // we have to clone everything due to this
            Expression::Value(ref val) => match Type::from_value(val) {
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
            Expression::StructConstructor(_, _type) => Cow::Owned(Type::Struct(_type.clone())),
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
            Expression::Cast(_, _type) => Cow::Borrowed(_type),
            Expression::RangeConstructor(start, _) => Cow::Owned(Type::Range(Box::new(self.get_type_from_expression(on_type, start, context)?.into_owned()))),
        };

        Ok(Some(_type))
    }

    // Read a function call with the following syntax:
    // function_name(param1, param2, ...)
    fn read_function_call(&mut self, path: Option<Expression>, on_type: Option<&Type>, name: &str, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
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

        let id = self.functions_mapper.get_compatible(Signature::new(name.to_owned(), on_type.cloned(), types), &mut parameters)?;

        // Entry are only callable by external
        let f = self.get_function(id)?;
        if f.is_entry() {
            return Err(ParserError::FunctionNotFound)
        }

        self.expect_token(Token::ParenthesisClose)?;
        Ok(Expression::FunctionCall(path.map(Box::new), id, parameters))
    }

    // Read fields of a constructor with the following syntax:
    // { field1, field2, ... }
    // or with values
    // { field1: value1, field2: value2, ... }
    fn read_constructor_fields(&mut self, context: &mut Context<'a>) -> Result<Vec<(&'a str, Expression)>, ParserError<'a>> {
        let mut fields = Vec::new();
        while self.peek_is_not(Token::BraceClose) {
            let field_name = self.next_identifier()?;
            let expr = match self.advance()? {
                Token::Comma | Token::BraceClose => Expression::Variable(context.get_variable_id(field_name).ok_or_else(|| ParserError::UnexpectedVariable(field_name.to_owned()))?),
                Token::Colon => self.read_expression(context)?,
                token => return Err(ParserError::UnexpectedToken(token))
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
                Expression::Value(v) if _type.is_castable_to(expected_type) => v.mut_checked_cast_to_primitive_type(expected_type)?,
                _ => return Err(ParserError::InvalidValueType(_type, expected_type.clone()))
            }
        }
        Ok(())
    }

    // Read a struct constructor with the following syntax:
    // struct_name { field_name: value1, field2: value2 }
    // If we have a field that has the same name as a variable we can pass it as following:
    // Example: struct_name { field_name, field2: value2 }
    fn read_struct_constructor(&mut self, struct_type: StructType, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        self.expect_token(Token::BraceOpen)?;
        let fields = self.read_constructor_fields(context)?;

        if struct_type.fields().len() != fields.len() {
            return Err(ParserError::InvalidFieldCount)
        }

        // Now verify that it match our struct
        let builder = self.struct_manager.get_by_ref(&struct_type)?;
        let mut fields_expressions = Vec::with_capacity(fields.len());
        for ((field_name, mut field_expr), (field_type, field_name_expected)) in fields.into_iter().zip(struct_type.fields().iter().zip(builder.names())) {
            if field_name != *field_name_expected {
                return Err(ParserError::InvalidFieldName(field_name, field_name_expected))
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
        // If its an enum variant with fields
        let fields = if self.peek_is(Token::BraceOpen) {
            self.expect_token(Token::BraceOpen)?;
            self.read_constructor_fields(context)?
        } else {
            Vec::new()
        };

        let builder = self.enum_manager.get_by_ref(&enum_type)?;
        let (variant_id, variant_fields) = builder.get_variant_by_name(&variant_name)
            .ok_or_else(|| ParserError::EnumVariantNotFound(variant_name))?;

        if variant_fields.len() != fields.len() {
            return Err(ParserError::InvalidFieldCount)
        }

        let mut fields_expressions = Vec::with_capacity(variant_fields.len());
        for ((field_name, mut field_expr), (expected_name, expected_type)) in fields.into_iter().zip(variant_fields) {
            if field_name != *expected_name {
                return Err(ParserError::InvalidEnumFieldName(field_name))
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
            self.environment.get_constant_by_name(&_type, &constant_name)
                .map(|v| Expression::Value(v.clone()))
                .ok_or_else(|| ParserError::ConstantNotFound(_type, constant_name))
        }
    }

    // Read an expression with default parameters
    fn read_expression(&mut self, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        self.read_expr(None, true, true, None, context)
    }

    // Read an expression with the possibility to accept operators
    // number_type is used to force the type of a number
    fn read_expr(&mut self, on_type: Option<&Type>, allow_ternary: bool, accept_operator: bool, expected_type: Option<&Type>, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        let mut required_operator = false;
        let mut last_expression: Option<Expression> = None;
        while self.peek()
            .ok()
            .filter(|peek| {
                if !allow_ternary && **peek == Token::OperatorTernary {
                    return false
                }

                if !accept_operator && required_operator {
                    return false
                }

                required_operator == peek.is_operator()
                    || (**peek == Token::BracketOpen && last_expression.is_none())
            }).is_some()
        {

            let token = self.advance()?;
            let expr: Expression = match token {
                Token::BracketOpen => {
                    match last_expression {
                        Some(v) => {
                            if !self.get_type_from_expression(on_type, &v, context)?.is_array() {
                                return Err(ParserError::InvalidArrayCall)
                            }

                            // Index must be of type u64
                            let index = self.read_expr(on_type, true, true, Some(&Type::U32), context)?;
                            let index_type = self.get_type_from_expression(on_type, &index, context)?;
                            if *index_type != Type::U32 {
                                return Err(ParserError::InvalidArrayCallIndexType(index_type.into_owned()))
                            }

                            self.expect_token(Token::BracketClose)?;
                            required_operator = !required_operator;
                            Expression::ArrayCall(Box::new(v), Box::new(index))
                        },
                        None => { // require at least one value in a array constructor
                            let mut expressions: Vec<Expression> = Vec::new();
                            let mut array_type: Option<Type> = None;
                            while self.peek_is_not(Token::BracketClose) {
                                let expr = self.read_expr(on_type, true, true, expected_type.map(|t| t.get_inner_type()), context)?;
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
                    let expr = self.read_expr(None, true, true, expected_type, context)?;
                    self.expect_token(Token::ParenthesisClose)?;
                    Expression::SubExpression(Box::new(expr))
                },
                Token::Identifier(id) => {
                    match self.peek()? {
                        // function call
                        Token::ParenthesisOpen => self.read_function_call(last_expression.take(), on_type, id, context)?,
                        Token::Colon => self.read_type_constant(Token::Identifier(id), context)?,
                        _ => {
                            match on_type {
                                // mostly an access to a struct field
                                Some(t) => {
                                    if let Type::Struct(_type) = t {
                                        let builder = self.struct_manager.get_by_ref(_type)?;
                                        match builder.get_id_for_field(id) {
                                            Some(v) => Expression::Variable(v),
                                            None => return Err(ParserError::UnexpectedVariable(id.to_owned()))
                                        }
                                    } else {
                                        return Err(ParserError::UnexpectedType(t.clone()))
                                    }
                                },
                                None => {
                                    if let Some(id) = context.get_variable_id(id) {
                                        Expression::Variable(id)
                                    } else if let Ok(builder) = self.struct_manager.get_by_name(&id) {
                                        self.read_struct_constructor(builder.get_type().clone(), context)?
                                    } else {
                                        return Err(ParserError::UnexpectedVariable(id.to_owned()))
                                    }
                                }
                            }
                        }
                    }
                },
                Token::Value(value) => {
                    Expression::Value(match value {
                        Literal::U8(n) => Value::U8(n),
                        Literal::U16(n) => Value::U16(n),
                        Literal::U32(n) => Value::U32(n),
                        Literal::U64(n) => Value::U64(n),
                        Literal::U128(n) => Value::U128(n),
                        Literal::U256(n) => Value::U256(n),
                        Literal::Number(n) => match expected_type {
                            Some(Type::U8) => Value::U8(n as u8),
                            Some(Type::U16) => Value::U16(n as u16),
                            Some(Type::U32) => Value::U32(n as u32),
                            Some(Type::U64) => Value::U64(n as u64),
                            Some(Type::U128) => Value::U128(n as u128),
                            Some(Type::U256) => Value::U256(U256::from(n)),
                            _ => Value::U64(n)
                        },
                        Literal::String(s) => Value::String(s.into_owned()),
                        Literal::Bool(b) => Value::Boolean(b),
                        Literal::Null => Value::Null
                    })
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
                                let end_expr = self.read_expr(Some(&_type), false, false, expected_type, context)?;
                                let end_type = self.get_type_from_expression(on_type, &end_expr, context)?;
                                if _type != *end_type {
                                    return Err(ParserError::InvalidRangeType(_type, end_type.into_owned()))
                                }

                                if !_type.is_primitive() {
                                    return Err(ParserError::InvalidRangeTypePrimitive(_type))
                                }

                                Expression::RangeConstructor(Box::new(value), Box::new(end_expr))
                            } else {
                                // Read a variable access OR a function call
                                let right_expr = self.read_expr(Some(&_type), false, false, expected_type, context)?;
                                if let Expression::FunctionCall(path, name, params) = right_expr {
                                    if path.is_some() {
                                        return Err(ParserError::UnexpectedPathInFunctionCall)
                                    }
                                    Expression::FunctionCall(Some(Box::new(value)), name, params)
                                } else {
                                    Expression::Path(Box::new(value), Box::new(right_expr))
                                }
                            }
                        },
                        None => return Err(ParserError::UnexpectedToken(Token::Dot))
                    }
                },
                Token::IsNot => { // it's an operator, but not declared as
                    let expr = self.read_expression(context)?;
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

                        let valid_expr = self.read_expr(on_type, true, true, expected_type, context)?;
                        let first_type = self.get_type_from_expression(on_type, &valid_expr, context)?.into_owned();
                        self.expect_token(Token::Colon)?;
                        let else_expr = self.read_expr(on_type, true, true, expected_type, context)?;
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

                    if !right_type.is_primitive() {
                        return Err(ParserError::CastPrimitiveError(left_type, right_type))
                    }

                    required_operator = !required_operator;
                    Expression::Cast(Box::new(previous_expr), right_type)
                },
                token => {
                    match last_expression {
                        Some(mut previous_expr) => {
                            required_operator = !required_operator;

                            let left_type = self.get_type_from_expression(on_type, &previous_expr, context)?.into_owned();
                            // Parse the operator for this token
                            let op = match Operator::value_of(&token) {
                                Some(op) => op,
                                None => return Err(ParserError::OperatorNotFound(token))
                            };

                            let mut expr = self.read_expr(on_type, false, true, Some(&left_type), context)?;
                            if let Some(right_type) = self.get_type_from_expression_internal(on_type, &expr, context)? {
                                match &op {
                                    Operator::Minus | Operator::Rem | Operator::Divide | Operator::Multiply
                                    | Operator::Assign(_) | Operator::BitwiseLeft | Operator::BitwiseRight
                                    | Operator::GreaterThan | Operator::LessThan | Operator::LessOrEqual
                                    | Operator::GreaterOrEqual => {
                                        if left_type != *right_type {
                                            // It is an hardcoded value, lets map it to the correct type
                                            if let Expression::Value(value) = previous_expr {
                                                previous_expr = Expression::Value(value.checked_cast_to_primitive_type(&right_type)?);
                                            } else if let Expression::Value(value) = expr {
                                                expr = Expression::Value(value.checked_cast_to_primitive_type(&left_type)?);
                                            } else {
                                                return Err(ParserError::InvalidOperationNotSameType(left_type, right_type.into_owned()))
                                            }
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
                        None => {
                            if token.is_type() {
                                self.read_type_constant(token, context)?
                            } else if token == Token::BraceOpen {
                                let (key, value) = if let Some(Type::Map(key, value)) = expected_type {
                                    (Some(*key.clone()), Some(*value.clone()))
                                } else {
                                    (None, None)
                                };

                                self.read_map_constructor(key, value, context)?
                            } else {
                                return Err(ParserError::InvalidOperation)
                            }
                        }
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

    // Read a map constructor with the following syntax:
    // { key1: value1, key2: value2 }
    // The keys must be of the same type and the values must be of the same type
    // you can use direct values like { "hello": "world" }
    fn read_map_constructor(&mut self, mut key_type: Option<Type>, mut value_type: Option<Type>, context: &mut Context<'a>) -> Result<Expression, ParserError<'a>> {
        let mut expressions: Vec<(Expression, Expression)> = Vec::new();
        while self.peek_is_not(Token::BraceClose) {
            let key = self.read_expr(None, true, true, key_type.as_ref(), context)?;
            let k_type = self.get_type_from_expression(None, &key, context)?;
            if let Some(t) = key_type.as_ref() {
                if *k_type != *t {
                    return Err(ParserError::InvalidValueType(k_type.into_owned(), t.clone()))
                }
            } else {
                key_type = Some(k_type.into_owned());
            }

            self.expect_token(Token::Colon)?;
            let value = self.read_expr(None, true, true, value_type.as_ref(), context)?;
            let v_type = self.get_type_from_expression(None, &value, context)?;
            if let Some(t) = value_type.as_ref() {
                if *v_type != *t {
                    return Err(ParserError::InvalidValueType(v_type.into_owned(), t.clone()))
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
            _ => return Err(ParserError::NoValueType)
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

    /**
     * Example: let hello: string = "hello";
     * Rules:
     * - Every variable must be declared with 'let' keyword
     * - Variable name must be alphanumeric characters
     * - Must provide a value type
     * - If no value is set, Null is set by default
     */
    fn read_variable(&mut self, context: &mut Context<'a>, is_const: bool) -> Result<DeclarationStatement, ParserError<'a>> {
        let name: &'a str = self.next_identifier()?;

        // Constants must be uppercase
        if is_const && name.to_uppercase() != name {
            return Err(ParserError::ConstantNameNotUppercase(name.to_owned()))
        }

        let ignored = name == "_";

        // Variable name must start with a alphabetic character
        if !ignored && !name.starts_with(char::is_alphabetic) {
            return Err(ParserError::VariableMustStartWithAlphabetic(name.to_owned()))
        }

        self.expect_token(Token::Colon)?;
        let value_type = self.read_type()?;
        let value: Expression = if self.peek_is(Token::OperatorAssign) {
            self.expect_token(Token::OperatorAssign)?;
            let expr = self.read_expr(None, true, true, Some(&value_type), context)?;

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

        let id = if ignored {
            context.register_variable_unchecked(name, value_type.clone())
        } else {
            context.register_variable(name, value_type.clone())?
        };

        Ok(DeclarationStatement {
            id,
            value_type,
            value
        })
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
                    let var = self.read_variable(context, false)?;
                    let condition = self.read_expression(context)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if  *condition_type != Type::Bool {
                        return Err(ParserError::InvalidCondition(condition_type.into_owned(), condition))
                    }

                    let increment = self.read_expression(context)?;
                    match &increment { // allow only assignations on this expr
                        Expression::Operator(op, _, _) if op.is_assignation() => {},
                        _ => return Err(ParserError::InvalidForExpression(increment))
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
                        return Err(ParserError::NotIterable(expr_type.into_owned()))
                    }

                    let id = context.register_variable(variable, expr_type.get_inner_type().clone())?;
                    let statements = self.read_loop_body(context, return_type)?;
                    context.end_scope();

                    Statement::ForEach(id, expr, statements)
                },
                Token::While => { // Example: while i < 10 {}
                    let condition = self.read_expression(context)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if  *condition_type != Type::Bool {
                        return Err(ParserError::InvalidCondition(condition_type.into_owned(), condition))
                    }

                    let statements = self.read_loop_body(context, return_type)?;

                    Statement::While(condition, statements)
                },
                Token::If => {
                    let condition = self.read_expression(context)?;
                    let condition_type = self.get_type_from_expression(None, &condition, context)?;
                    if *condition_type != Type::Bool {
                        return Err(ParserError::InvalidCondition(condition_type.into_owned(), condition))
                    }

                    self.expect_token(Token::BraceOpen)?;
                    let body = self.read_body(context, return_type)?;
                    let else_statement = if self.peek_is(Token::Else) {
                        self.advance()?;
                        Some(if self.peek_is(Token::If) {
                            let statement = self.read_statement(context, return_type)?;
                            vec![statement.ok_or(ParserError::UnexpectedToken(Token::If))?]
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
                Token::Let => Statement::Variable(self.read_variable(context, false)?),
                Token::Return => {
                    let opt: Option<Expression> = if let Some(return_type) = return_type {
                        let expr = self.read_expr(None, true, true, Some(return_type), context)?;
                        let expr_type = self.get_type_from_expression(None, &expr, context)?;
                        if !expr_type.is_compatible_with(return_type) {
                            return Err(ParserError::InvalidValueType(expr_type.into_owned(), return_type.clone()))
                        }
                        Some(expr)
                    } else {
                        None
                    };

                    // we can't have anything after a return
                    if self.peek_is_not(Token::BraceClose) {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Return(opt)
                }
                Token::Continue => {
                    if !context.is_in_a_loop() {
                        return Err(ParserError::UnexpectedToken(Token::Continue));
                    }

                    // we can't have anything after a continue
                    if self.peek_is_not(Token::BraceClose) {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Continue
                },
                Token::Break => {
                    if !context.is_in_a_loop() {
                        return Err(ParserError::UnexpectedToken(Token::Break));
                    }

                    // we can't have anything after a break
                    if self.peek_is_not(Token::BraceClose) {
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Break
                },
                token => {
                    self.tokens.push_front(token);
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
        let mut statements: Vec<Statement> = Vec::new();
        while let Some(statement) = self.read_statement(context, return_type)? {
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
        context.begin_scope();

        let token = self.advance()?;
        let (instance_name, for_type, name) = if !entry && token == Token::ParenthesisOpen {
            let instance_name = self.next_identifier()?;
            let for_type = self.read_type()?;
            let id = context.register_variable(instance_name, for_type.clone())?;
            self.expect_token(Token::ParenthesisClose)?;

            (Some(id), Some(for_type), self.next_identifier()?)
        } else {
            let Token::Identifier(name) = token else {
                return Err(ParserError::ExpectedIdentifierToken(token))
            };
            (None, None, name)
        };

        self.expect_token(Token::ParenthesisOpen)?;
        let parameters = self.read_parameters()?;
        self.expect_token(Token::ParenthesisClose)?;

        // all entries must return a u64 value without being specified
        let return_type: Option<Type> = if entry {
            // an entrypoint cannot be a method
            if for_type.is_some() {
                return Err(ParserError::EntryFunctionCannotHaveForType)
            }

            Some(Type::U64)
        } else if self.peek_is(Token::ReturnType) { // read returned type
            self.advance()?;
            Some(self.read_type()?)
        } else {
            None
        };

        let types: Vec<Type> = parameters.iter().map(|p| p.1.clone()).collect();
        let id = self.functions_mapper.register(Signature::new(name.to_owned(), for_type.clone(), types))?;
        if self.has_function(id) {
            return Err(ParserError::FunctionSignatureAlreadyExist) 
        }

        let has_return_type = return_type.is_some();

        let mut new_params = Vec::with_capacity(parameters.len());
        for (name, param_type) in parameters {
            let id = context.register_variable(name, param_type.clone())?;
            new_params.push(Parameter::new(id, param_type));
        }

        self.expect_token(Token::BraceOpen)?;
        let statements = self.read_body(context, &return_type)?;

        context.end_scope();

        // verify that the function ends with a return
        if has_return_type && !Self::ends_with_return(&statements)? {
            return Err(ParserError::NoReturnFound)
        }

        let function = match entry {
            true => FunctionType::Entry(EntryFunction::new(new_params, statements, context.max_variables_count() as u16)),
            false => FunctionType::Declared(DeclaredFunction::new(
                for_type,
                instance_name,
                new_params,
                statements,
                return_type,
                context.max_variables_count() as u16
            ))
        };

        // push function before reading statements to allow recursive calls
        self.functions.push(function);

        Ok(())
    }

    // Read a type with the following syntax:
    // import "filename.xel";
    // or with an alias:
    // import "filename.xel" as alias;
    fn read_import(&mut self) -> Result<(), ParserError<'a>> {
        let path = self.advance()?;

        let Token::Value(Literal::String(path)) = path else {
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
                None => Err(ParserError::FunctionNotFound)
            }
        }
    }

    // Verify that a type name is not already used
    fn is_name_available(&self, name: &str) -> bool {
        self.struct_manager.get_by_name(name).is_err()
            && self.enum_manager.get_by_name(name).is_err()
    }

    /**
     * Example: Message { message_id: u64, message: string }
     * Rules:
     * - Structure name should start with a uppercase (ascii alphabet) character
     */
    fn read_struct(&mut self) -> Result<(), ParserError<'a>> {
        let name = self.next_identifier()?;

        // Verify that we don't have a type with the same name
        if !self.is_name_available(name) {
            return Err(ParserError::TypeNameAlreadyUsed(name))
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

        self.expect_token(Token::BraceOpen)?;
        let params = self.read_parameters()?;
        let mut fields = Vec::with_capacity(params.len());
        for (name, param_type) in params {
            fields.push((name, param_type));
        }

        self.expect_token(Token::BraceClose)?;

        self.struct_manager.add(Cow::Borrowed(name), fields)?;

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

        // Verify that we don't have a type with the same name
        if !self.is_name_available(name) {
            return Err(ParserError::TypeNameAlreadyUsed(name))
        }

        // check if the first letter is in uppercase
        match name.chars().nth(0) {
            Some(v) => {
                if !v.is_ascii_alphabetic() || !v.is_uppercase() {
                    return Err(ParserError::InvalidEnumName(name))
                }
            },
            None => return Err(ParserError::EmptyEnumName)
        };

        self.expect_token(Token::BraceOpen)?;
        let mut variants = Vec::new();
        while self.peek_is_identifier() {
            let variant_name = self.next_identifier()?;

            // Variant name should be unique
            if variants.iter().any(|(name, _)| *name == variant_name) {
                return Err(ParserError::EnumVariantAlreadyUsed(variant_name))
            }

            let fields = if self.peek_is(Token::BraceOpen) {
                self.expect_token(Token::BraceOpen)?;
                let fields = self.read_parameters()?;

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

        self.expect_token(Token::BraceClose)?;

        self.enum_manager.add(Cow::Borrowed(name), variants)?;

        Ok(())
    }

    // Parse the tokens and return a Program
    // The function mapper is also returned for external calls
    pub fn parse(mut self) -> Result<(Program, FunctionMapper<'a>), ParserError<'a>> {
        let mut context: Context = Context::new();
        while let Some(token) = self.next() {
            match token {
                Token::Import => {
                    self.read_import()?;
                    continue;
                }
                Token::Const => {
                    let var = self.read_variable(&mut context, true)?;
                    let id = var.id;
                    if !self.constants.insert(var) {
                        return Err(ParserError::VariableIdAlreadyUsed(id))
                    }
                },
                Token::Function => self.read_function(false, &mut context)?,
                Token::Entry => self.read_function(true, &mut context)?,
                Token::Struct => self.read_struct()?,
                Token::Enum => self.read_enum()?,
                token => return Err(ParserError::UnexpectedToken(token))
            };
        }

        let program = Program::with(self.constants, self.struct_manager.finalize(), self.enum_manager.finalize(), self.functions);
        Ok((program, self.functions_mapper))
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
        let parser = Parser::new(VecDeque::from(tokens), env);
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

    fn test_parser_statement_with_return_type(tokens: Vec<Token>, variables: Vec<(&str, Type)>, return_type: Type) -> Vec<Statement> {
        let env = EnvironmentBuilder::new();
        test_parser_statement_with(tokens, variables, &Some(return_type), env)
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
                    value_type: Type::U64,
                    value: Expression::RangeConstructor(
                        Box::new(Expression::Value(Value::U64(0))),
                        Box::new(Expression::Value(Value::U64(10))),
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
                    value: Expression::Value(Value::Null)
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
                    value: Expression::MapConstructor(Vec::new(), Type::U64, Type::String)
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
        assert_eq!(
            statements[0],
            Statement::Variable(
                DeclarationStatement {
                    id: 0,
                    value_type: Type::Map(Box::new(Type::U64), Box::new(Type::String)),
                    value: Expression::MapConstructor(
                        vec![
                            (Expression::Value(Value::U64(0)), Expression::Value(Value::String("hello".to_owned())))
                        ],
                        Type::U64,
                        Type::String
                    )
                }
            )
        );
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
                    value: Expression::MapConstructor(Vec::new(), Type::U64, Type::Map(Box::new(Type::U64), Box::new(Type::String)))
                }
            )
        );
    }

    #[test]
    fn test_for_each_range() {
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
                Expression::RangeConstructor(
                    Box::new(Expression::Value(Value::U64(0))),
                    Box::new(Expression::Value(Value::U64(10))),
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
                    value: Expression::ArrayConstructor(vec![
                        Expression::Value(Value::U64(1)),
                        Expression::Value(Value::U64(2)),
                        Expression::Value(Value::U64(3))
                    ])
                }
            ),
            Statement::Return(Some(Expression::Operator(
                Operator::GreaterThan,
                Box::new(Expression::Value(Value::U32(0))),
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
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
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
            Token::If,
            Token::Identifier("i"),
            Token::OperatorLessThan,
            Token::Value(Literal::U64(10)),
            Token::BraceOpen,
            Token::BraceClose,
            Token::Return
        ];

        let statements = test_parser_statement(tokens.clone(), vec![("i", Type::U64)]);
        assert_eq!(statements, vec![
            Statement::If(
                Expression::Operator(
                    Operator::LessThan,
                    Box::new(Expression::Variable(0)),
                    Box::new(Expression::Value(Value::U64(10)))
                ),
                Vec::new(),
                None
            ),
            Statement::Return(None)
        ]);

        tokens.push(Token::Value(Literal::U64(0)));


        let statements = test_parser_statement_with_return_type(tokens, vec![("i", Type::U64)], Type::U64);
        assert_eq!(statements, vec![
            Statement::If(
                Expression::Operator(
                    Operator::LessThan,
                    Box::new(Expression::Variable(0)),
                    Box::new(Expression::Value(Value::U64(10)))
                ),
                Vec::new(),
                None
            ),
            Statement::Return(Some(Expression::Value(Value::U64(0))))
        ]);
    }

    #[test]
    fn test_if_else_if_else_return() {
        // if i < 10 {} else if i < 20 {} else {} return 0
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
            Token::BraceClose,
            Token::Else,
            Token::BraceOpen,
            Token::BraceClose,
            Token::Return,
            Token::Value(Literal::U64(0))
        ];

        let statements = test_parser_statement_with_return_type(tokens, vec![("i", Type::U64)], Type::U64);
        assert_eq!(statements, vec![
            Statement::If(
                Expression::Operator(
                    Operator::LessThan,
                    Box::new(Expression::Variable(0)),
                    Box::new(Expression::Value(Value::U64(10)))
                ),
                Vec::new(),
                Some(vec![
                    Statement::If(
                        Expression::Operator(
                            Operator::LessThan,
                            Box::new(Expression::Variable(0)),
                            Box::new(Expression::Value(Value::U64(20)))
                        ),
                        Vec::new(),
                        Some(Vec::new())
                    )
                ])
            ),
            Statement::Return(Some(Expression::Value(Value::U64(0))))
        ]);
    }

    #[test]
    fn test_ends_with_return() {
        const RETURN: Statement = Statement::Return(Some(Expression::Value(Value::U64(0))));
        let statements = vec![RETURN];
        assert!(Parser::ends_with_return(&statements).unwrap());

        let statements = vec![RETURN, Statement::Expression(Expression::Value(Value::U64(0)))];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if ... return
        let statements = vec![Statement::If(Expression::Value(Value::Boolean(true)), Vec::new(), None), RETURN];
        assert!(Parser::ends_with_return(&statements).unwrap());

        let statements = vec![Statement::If(Expression::Value(Value::Boolean(true)), Vec::new(), None)];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if else
        let statements = vec![Statement::If(Expression::Value(Value::Boolean(true)), Vec::new(), Some(Vec::new()))];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if return else return
        let statements = vec![
            Statement::If(Expression::Value(Value::Boolean(true)), vec![RETURN], Some(vec![RETURN]))
        ];
        assert!(Parser::ends_with_return(&statements).unwrap());

        // if return else if return else no return
        let statements = vec![
            Statement::If(
                Expression::Value(Value::Boolean(true)),
                vec![RETURN],
                Some(vec![
                    Statement::If(Expression::Value(Value::Boolean(true)), vec![RETURN], None)
                ])
            )
        ];
        assert!(!Parser::ends_with_return(&statements).unwrap());

        // if return else if return else return
        let statements = vec![
            Statement::If(
                Expression::Value(Value::Boolean(true)),
                vec![RETURN],
                Some(vec![
                    Statement::If(Expression::Value(Value::Boolean(true)), vec![RETURN], Some(vec![RETURN]))
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
        env.register_constant(Type::U64, "MAX", Value::U64(u64::MAX));
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