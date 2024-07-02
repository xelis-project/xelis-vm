mod scope;
mod context;
mod error;

pub use self::error::ParserError;
use self::context::Context;

use crate::{
    expressions::{Operator, Statement, Expression, DeclarationStatement, Parameter},
    functions::{CustomFunction, FunctionType},
    types::{Value, Type, Struct, RefMap},
    Environment,
    Token
};
use std::{
    collections::{HashMap, VecDeque},
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
    pub constants: Vec<DeclarationStatement>,
    pub structures: HashMap<String, Struct>,
    pub functions: Vec<FunctionType>
}

type VariableId = String;

pub struct Parser<'a> {
    constants: Vec<DeclarationStatement>,
    tokens: VecDeque<Token>,
    functions: Vec<FunctionType>,
    structures: HashMap<String, Struct>,
    env: &'a Environment
}

impl<'a> Parser<'a> {
    pub fn new(tokens: VecDeque<Token>, env: &'a Environment) -> Self {
        Parser {
            constants: Vec::new(),
            tokens,
            functions: Vec::new(),
            structures: HashMap::new(),
            env,
        }
    }

    // Consume the next token
    fn next(&mut self) -> Result<Token, ParserError> {
        self.tokens.pop_front().ok_or(ParserError::ExpectedToken)
    }

    // Peek the next token without consuming it
    fn peek(&self) -> Result<&Token, ParserError> {
        self.tokens.front().ok_or(ParserError::ExpectedToken)
    }

    // Limited to 32 characters
    fn next_identifier(&mut self) -> Result<String, ParserError> {
        match self.next()? {
            Token::Identifier(id) => if id.len() <= 32 {
                Ok(id)
            } else {
                Err(ParserError::VariableTooLong(id))
            },
            token => Err(ParserError::ExpectedIdentifierToken(token))
        }
    }

    // Check if the next token is an identifier
    fn next_is_identifier(&self) -> bool {
        self.peek().ok().filter(|t| match t {
            Token::Identifier(_) => true,
            _ => false
        }).is_some()
    }

    // Require a specific token
    fn expect_token(&mut self, expected: Token) -> Result<Token, ParserError> {
        let token = self.next()?;
        if token != expected {
            return Err(ParserError::InvalidToken(token, expected)) 
        }
        Ok(token)
    }

    /**
     * Example: let message: string[] = ["hello", "world", "!"];
     * Types: (unsigned)
     * - byte
     * - short
     * - int
     * - long
     * - string
     * - bool
     * - Struct (Structure with name that starts with a uppercase letter)
     * - T[] (where T is any above Type)
     */
    fn read_type(&mut self) -> Result<Type, ParserError> {
        let type_name = self.next_identifier()?;
        let mut _type = match Type::from_string(&type_name, &self.structures) {
            Some(v) => v,
            None => return Err(ParserError::TypeNotFound(type_name))
        };

        match self.peek()? {
            Token::BracketOpen => {
                while *self.peek()? == Token::BracketOpen { // support multi dimensional arrays
                    self.expect_token(Token::BracketOpen)?;
                    self.expect_token(Token::BracketClose)?;
                    _type = Type::Array(Box::new(_type));
                }
            },
            _ => {}
        };

        Ok(_type)
    }

    // this function don't verify, but only returns the type of an expression
    // all tests should be done when constructing an expression, not here
    fn get_type_from_expression(&mut self, expression: &Expression, context: &mut Context) -> Result<Type, ParserError> {
        let _type: Type = match expression {
            Expression::ArrayConstructor(ref values) => match values.get(0) {
                Some(v) => Type::Array(Box::new(self.get_type_from_expression(v, context)?)),
                None => return Err(ParserError::EmptyArrayConstructor) // cannot determine type from empty array
            },
            Expression::Variable(ref var_name) => {
                let _type = context.get_type_of_variable(var_name)?.clone();
                _type
            },
            Expression::FunctionCall(name, parameters) => {
                let mut types: Vec<Type> = vec![];
                for param in parameters {
                    types.push(self.get_type_from_expression(param, context)?);
                }

                let mut _types: Vec<&Type> = vec![];
                for t in &types {
                    _types.push(t);
                }

                let func = self.get_function(context.get_current_type(), name, &_types)?;
                match &func.return_type() {
                    Some(ref v) => v.clone(),
                    None => return Err(ParserError::FunctionNoReturnType(name.clone()))
                }
            },
            Expression::Value(ref val) => match Type::from_value(val, &RefMap::from_vec(vec![&self.structures, self.env.get_structures()])) { // we have to clone everything due to this
                Some(v) => v,
                None => return Err(ParserError::EmptyValue)
            },
            Expression::ArrayCall(path, _) => {
                match self.get_type_from_expression(path, context)? {
                    Type::Array(_type) => *_type,
                    _ => return Err(ParserError::InvalidArrayCall)
                }
            },
            Expression::SubExpression(expr) => self.get_type_from_expression(expr, context)?,
            Expression::StructConstructor(name, _) => {
                if !self.get_structure(name).is_ok() {
                    return Err(ParserError::StructureNotFound(name.clone()))
                }

                Type::Struct(name.clone())
            }
            Expression::Path(left, right) => {
                let var_type = self.get_type_from_expression(left, context)?;
                context.set_current_type(var_type, self)?;
                let _type = self.get_type_from_expression(right, context)?;
                context.remove_current_type();
                _type
            },
            Expression::Operator(op, left, right) => { // TODO verify all operations
                match op {
                    Operator::Or
                    | Operator::Equals
                    | Operator::NotEquals
                    | Operator::GreaterOrEqual
                    | Operator::GreaterThan
                    | Operator::LessOrEqual
                    | Operator::LessThan
                    | Operator::And => Type::Boolean,
                    Operator::Assign
                    | Operator::AssignPlus
                    | Operator::AssignMinus
                    | Operator::AssignDivide
                    | Operator::AssignMultiply => Type::Null, // Assignation doesn't returns anything
                    _ => {
                        let left_type = self.get_type_from_expression(left, context)?;
                        let right_type = self.get_type_from_expression(right, context)?;
        
                        if left_type == Type::String || right_type == Type::String {
                            Type::String
                        } else {
                            left_type
                        }
                    }
                }
            },
            Expression::IsNot(_) => Type::Boolean,
            Expression::Ternary(_, expr, _) => self.get_type_from_expression(expr, context)?
        };

        Ok(_type)
    }

    fn read_expression(&mut self, context: &mut Context) -> Result<Expression, ParserError> {
        self.read_expr(true, None, context)
    }

    fn read_expr(&mut self, accept_operator: bool, number_type: Option<&Type>, context: &mut Context) -> Result<Expression, ParserError> {
        let mut required_operator = false;
        let mut last_expression: Option<Expression> = None;
        while !self.peek()?.should_stop() && ((required_operator == self.peek()?.is_operator()) || (*self.peek()? == Token::BracketOpen && last_expression.is_none())) {
            if !accept_operator && required_operator {
                break
            }

            let expr: Expression = match self.next()? {
                Token::BracketOpen => {
                    match last_expression {
                        Some(v) => {
                            if !self.get_type_from_expression(&v, context)?.is_array() {
                                return Err(ParserError::InvalidArrayCall)
                            }

                            let index = self.read_expression(context)?;
                            let index_type = self.get_type_from_expression(&index, context)?;
                            if index_type != Type::Int {
                                return Err(ParserError::InvalidArrayCallIndexType(index_type))
                            }

                            self.expect_token(Token::BracketClose)?;
                            required_operator = !required_operator;
                            Expression::ArrayCall(Box::new(v), Box::new(index))
                        },
                        None => { // require at least one value in a array constructor
                            let mut expressions: Vec<Expression> = vec![];
                            let mut array_type: Option<Type> = None;
                            while *self.peek()? != Token::BracketClose {
                                let expr = self.read_expression(context)?;
                                match &array_type { // array values must have the same type
                                    Some(t) => {
                                        let _type = self.get_type_from_expression(&expr, context)?;
                                        if _type != *t {
                                            return Err(ParserError::InvalidTypeInArray(_type, t.clone()))
                                        }
                                    },
                                    None => { // first value rules the type of array
                                        array_type = Some(self.get_type_from_expression(&expr, context)?);
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
                    let expr = self.read_expression(context)?;
                    self.expect_token(Token::ParenthesisClose)?;
                    Expression::SubExpression(Box::new(expr))
                },
                Token::Identifier(id) => {
                    match self.peek()? {
                        Token::ParenthesisOpen => {
                            self.next()?; // we remove the token from the list
                            let mut parameters: Vec<Expression> = vec![];
                            let mut types: Vec<Type> = vec![];
                            while *self.peek()? != Token::ParenthesisClose { // read parameters for function call
                                let expr = self.read_expression(context)?;
                                types.push(self.get_type_from_expression(&expr, context)?);
                                parameters.push(expr);

                                if *self.peek()? == Token::Comma {
                                    self.expect_token(Token::Comma)?;
                                }
                            }

                            let mut _types: Vec<&Type> = vec![];
                            for t in &types {
                                _types.push(t);
                            }

                            let func = self.get_function(context.get_current_type(), &id, &_types)?;
                            // Function call cannot call entry function
                            if func.is_entry() {
                                return Err(ParserError::FunctionNotFound(id, parameters.len()))
                            }

                            self.expect_token(Token::ParenthesisClose)?;
                            Expression::FunctionCall(id, parameters)
                        },
                        _ => {
                            if context.has_variable(&id) {
                                Expression::Variable(id)
                            } else if self.structures.contains_key(&id) {
                                self.expect_token(Token::BraceOpen)?;
                                let (name, len) = {
                                    let structure = self.get_structure(&id)?;
                                    let name = structure.name.clone();
                                    let len = structure.fields.len();
                                    (name, len)
                                };
                                let mut fields = HashMap::new();
                                for _ in 0..len {
                                    let field_name = self.next_identifier()?;
                                    let field_value = match self.next()? {
                                        Token::Comma => {
                                            if context.has_variable(&field_name) {
                                                Expression::Variable(field_name.clone())
                                            } else {
                                                return Err(ParserError::UnexpectedVariable(field_name)) 
                                            }
                                        }
                                        Token::Colon => {
                                            let value = self.read_expression(context)?;
                                            if *self.peek()? == Token::Comma {
                                                self.next()?;
                                            }
                                            value
                                        }
                                        token => {
                                            return Err(ParserError::UnexpectedToken(token))
                                        }
                                    };

                                    let field_type = self.get_type_from_expression(&field_value, context)?;
                                    let expected_field_type = match self.get_structure(&id)?.fields.get(&field_name) {
                                        Some(v) => v,
                                        None => return Err(ParserError::InvalidStructField(field_name.clone()))
                                    };
                                    if *expected_field_type != field_type {
                                        return Err(ParserError::InvalidValueType(field_type, expected_field_type.clone()))
                                    }
                                    fields.insert(field_name, field_value);
                                }
                                self.expect_token(Token::BraceClose)?;
                                Expression::StructConstructor(name, fields)
                            } else {
                                return Err(ParserError::UnexpectedVariable(id))
                            }
                        }
                    }
                },
                Token::Number(value) => Expression::Value(match number_type {
                    Some(t) => match t {
                        Type::Byte => Value::Byte(convert!(value)),
                        Type::Short => Value::Short(convert!(value)),
                        Type::Int => Value::Int(value),
                        Type::Long => Value::Long(convert!(value)),
                        _ => return Err(ParserError::ExpectedNumberType)
                    },
                    None => Value::Int(value)
                }),
                Token::Long(value) => {
                    Expression::Value(Value::Long(value))
                },
                Token::ValString(value) => {
                    Expression::Value(Value::String(value))
                },
                Token::True => {
                    Expression::Value(Value::Boolean(true))
                },
                Token::False => {
                    Expression::Value(Value::Boolean(false))
                },
                Token::Null => {
                    Expression::Value(Value::Null)
                },
                Token::Dot => {
                    match last_expression {
                        Some(value) => {
                            let _type = self.get_type_from_expression(&value, context)?;
                            context.set_current_type(_type, self)?;
                            let right_expr = self.read_expr(false, None, context)?;
                            context.remove_current_type();
                            required_operator = !required_operator; // because we read operator DOT + right expression
                            Expression::Path(Box::new(value), Box::new(right_expr))
                        },
                        None => return Err(ParserError::UnexpectedToken(Token::Dot))
                    }
                },
                Token::IsNot => { // it's an operator, but not declared as
                    let expr = self.read_expression(context)?;
                    let expr_type = self.get_type_from_expression(&expr, context)?;
                    if expr_type != Type::Boolean {
                        return Err(ParserError::InvalidValueType(expr_type, Type::Boolean))
                    }

                    Expression::IsNot(Box::new(expr))
                },
                Token::OperatorTernary => match last_expression { // condition ? expr : expr
                    Some(expr) => {
                        if self.get_type_from_expression(&expr, context)? != Type::Boolean {
                            return Err(ParserError::InvalidCondition(expr, Type::Boolean))
                        }

                        let valid_expr = self.read_expr(true, number_type, context)?;
                        let first_type = self.get_type_from_expression(&valid_expr, context)?;
                        self.expect_token(Token::Colon)?;
                        let else_expr = self.read_expr(true, number_type, context)?;
                        let else_type = self.get_type_from_expression(&else_expr, context)?;
                        
                        if first_type != else_type { // both expr should have the SAME type.
                            return Err(ParserError::InvalidValueType(else_type, first_type))
                        }
                        required_operator = !required_operator;
                        Expression::Ternary(Box::new(expr), Box::new(valid_expr), Box::new(else_expr))
                    },
                    None => return Err(ParserError::InvalidTernaryNoPreviousExpression)
                },
                token => {
                    if !token.is_operator() {
                        return Err(ParserError::UnexpectedToken(token))
                    }
                    match last_expression {
                        Some(e) => {
                            required_operator = !required_operator;
                            let left_type = self.get_type_from_expression(&e, context)?;
                            context.remove_current_type();
                            let expr = self.read_expr(true, Some(&left_type), context)?;
                            let right_type = self.get_type_from_expression(&expr, context)?;

                            let op: Operator = match Operator::value_of(&token) {
                                Some(op) => op,
                                None => return Err(ParserError::OperatorNotFound(token))
                            };
                            match &op {
                                Operator::Minus | Operator::Modulo | Operator::Divide | Operator::Multiply
                                | Operator::AssignMinus | Operator::AssignDivide | Operator::AssignMultiply
                                | Operator::BitwiseLeft | Operator::BitwiseRight
                                | Operator::GreaterThan | Operator::LessThan | Operator::LessOrEqual
                                | Operator::GreaterOrEqual => {
                                    if left_type != right_type || !left_type.is_number() || !right_type.is_number() {
                                        return Err(ParserError::InvalidOperationNotSameType(left_type, right_type))
                                    }
                                },
                                Operator::Plus => {
                                    if left_type != Type::String && right_type != Type::String {
                                        if left_type != right_type {
                                            return Err(ParserError::InvalidOperationNotSameType(left_type, right_type))
                                        }
                                    }
                                },
                                Operator::And | Operator::Or => {
                                    if left_type != Type::Boolean {
                                        return Err(ParserError::InvalidOperationNotSameType(left_type, Type::Boolean))
                                    }

                                    if right_type != Type::Boolean {
                                        return Err(ParserError::InvalidOperationNotSameType(right_type, Type::Boolean))
                                    }
                                },
                                _ => if left_type != right_type {
                                    return Err(ParserError::InvalidOperationNotSameType(left_type, right_type))
                                }
                            };

                            Expression::Operator(op, Box::new(e), Box::new(expr))
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
    fn read_body(&mut self, context: &mut Context) -> Result<Vec<Statement>, ParserError> {
        context.create_new_scope();
        self.expect_token(Token::BraceOpen)?;
        let statements = self.read_statements(context)?;
        self.expect_token(Token::BraceClose)?;
        context.remove_last_scope();
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
    fn read_variable(&mut self, context: &mut Context) -> Result<DeclarationStatement, ParserError> {
        let name = self.next_identifier()?;
        if context.has_variable(&name) {
            return Err(ParserError::VariableNameAlreadyUsed(name))
        }

        self.expect_token(Token::Colon)?;
        let value_type = self.read_type()?;
        let value: Expression = if *self.peek()? == Token::OperatorAssign {
            self.expect_token(Token::OperatorAssign)?;
            let expr = self.read_expr(true, Some(&value_type), context)?;
            let expr_type = match self.get_type_from_expression(&expr, context) {
                Ok(_type) => _type,
                Err(e) => match e { // support empty array declaration
                    ParserError::EmptyArrayConstructor if value_type.is_array() => value_type.clone(),
                    _ => return Err(e)
                }
            };
            if !expr_type.is_compatible_with(&value_type) {
                return Err(ParserError::InvalidValueType(expr_type, value_type))
            }

            expr
        } else {
            Expression::Value(Value::Null)
        };

        context.register_variable(name.clone(), value_type.clone())?;

        Ok(DeclarationStatement {
            name,
            value_type,
            value
        })
    }

    fn read_loop_body(&mut self, context: &mut Context) -> Result<Vec<Statement>, ParserError> {
        let old_value = context.is_in_loop; // support loop in loop
        context.is_in_loop = true;
        let statements = self.read_body(context)?;
        context.is_in_loop = old_value;

        Ok(statements)
    }

    fn read_statements(&mut self, context: &mut Context) -> Result<Vec<Statement>, ParserError> {
        let mut statements: Vec<Statement> = vec![];
        let mut has_if = false;
        while *self.peek()? != Token::BraceClose {
            let statement: Statement = match self.peek()? {
                Token::For => { // Example: for i: int = 0; i < 10; i += 1 {}
                    self.expect_token(Token::For)?;
                    context.create_new_scope();
                    let var = self.read_variable(context)?;
                    let condition = self.read_expression(context)?;
                    let condition_type = self.get_type_from_expression(&condition, context)?;
                    if  condition_type != Type::Boolean {
                        return Err(ParserError::InvalidCondition(condition, condition_type))
                    }

                    let increment = self.read_expression(context)?;
                    match &increment { // allow only assignations on this expr
                        Expression::Operator(op, _, _) if op.is_assignation() => {},
                        _ => {
                            return Err(ParserError::InvalidForExpression(increment))
                        }
                    }
                    let statements = self.read_loop_body(context)?;
                    context.remove_last_scope();

                    Statement::For(var, condition, increment, statements)
                }
                Token::ForEach => { // Example: foreach a in array {}
                    self.expect_token(Token::ForEach)?;
                    
                    context.create_new_scope();
                    let variable: String = match self.next()? {
                        Token::Identifier(v) => v,
                        token => return Err(ParserError::UnexpectedToken(token))
                    };
                    self.expect_token(Token::In)?;
                    let expr = self.read_expression(context)?;
                    let expr_type = self.get_type_from_expression(&expr, context)?;
                    if !expr_type.is_array() { // verify that we can iter on it
                        return Err(ParserError::InvalidValueType(expr_type, Type::Array(Box::new(Type::Any))))
                    }
                    context.register_variable(variable.clone(), expr_type.get_array_type().clone())?;
                    let statements = self.read_loop_body(context)?;
                    context.remove_last_scope();

                    Statement::ForEach(variable, expr, statements)
                },
                Token::While => { // Example: while i < 10 {}
                    self.expect_token(Token::While)?;

                    let condition = self.read_expression(context)?;
                    let condition_type = self.get_type_from_expression(&condition, context)?;
                    if  condition_type != Type::Boolean {
                        return Err(ParserError::InvalidCondition(condition, condition_type))
                    }

                    let statements = self.read_loop_body(context)?;

                    Statement::While(condition, statements)
                },
                Token::If => {
                    self.expect_token(Token::If)?;
                    let condition = self.read_expression(context)?;
                    let condition_type = self.get_type_from_expression(&condition, context)?;
                    if  condition_type != Type::Boolean {
                        return Err(ParserError::InvalidCondition(condition, condition_type))
                    }

                    Statement::If(condition, self.read_body(context)?)
                },
                Token::Else => {
                    let token = self.expect_token(Token::Else)?;
                    if !has_if {
                        return Err(ParserError::NoIfBeforeElse(token))
                    }

                    if *self.peek()? == Token::If {
                        self.expect_token(Token::If)?;
                        let condition = self.read_expression(context)?;
                        let condition_type = self.get_type_from_expression(&condition, context)?;
                        if  condition_type != Type::Boolean {
                            return Err(ParserError::InvalidCondition(condition, condition_type))
                        }
                        Statement::ElseIf(condition, self.read_body(context)?)
                    } else {
                        Statement::Else(self.read_body(context)?)
                    }
                },
                Token::BraceOpen => {
                    Statement::Scope(self.read_body(context)?)
                },
                Token::Let => {
                    self.expect_token(Token::Let)?;
                    Statement::Variable(self.read_variable(context)?)
                }
                Token::Return => {
                    self.expect_token(Token::Return)?;
                    let opt: Option<Expression> = if context.return_type.is_some() {
                        let expr = self.read_expression(context)?;
                        let expr_type = self.get_type_from_expression(&expr, context)?;
                        if let Some(return_type) = &context.return_type {
                            if expr_type != *return_type {
                                return Err(ParserError::InvalidValueType(expr_type, return_type.clone()))
                            }
                        }
                        Some(expr)

                    } else {
                        None
                    };

                    if *self.peek()? != Token::BraceClose { // we can't have anything after a return
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Return(opt)
                }
                Token::Continue => {
                    let token = self.expect_token(Token::Continue)?;
                    if !context.is_in_loop {
                        return Err(ParserError::UnexpectedToken(token));
                    }

                    if *self.peek()? != Token::BraceClose { // we can't have anything after a continue
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Continue
                },
                Token::Break => {
                    let token = self.expect_token(Token::Break)?;
                    if !context.is_in_loop {
                        return Err(ParserError::UnexpectedToken(token));
                    }

                    if *self.peek()? != Token::BraceClose { // we can't have anything after a break
                        return Err(ParserError::DeadCodeNotAllowed);
                    }

                    Statement::Break
                },
                _ => Statement::Expression(self.read_expression(context)?)
            };

            match &statement {
                Statement::If(_, _) | Statement::ElseIf(_, _) => {
                    has_if = true;
                },
                _ => {
                    has_if = false;
                }
            };

            statements.push(statement);
        }
        Ok(statements)
    }

    // Read the parameters for a function
    fn read_parameters(&mut self) -> Result<Vec<Parameter>, ParserError> {
        let mut parameters: Vec<Parameter> = Vec::new();
        while self.next_is_identifier() {
            let name = self.next_identifier()?;
            self.expect_token(Token::Colon)?;
            let value_type = self.read_type()?;

            // verify that we have unique names here
            if parameters.iter().any(|p| *p.get_name() == name) {
                return Err(ParserError::VariableNameAlreadyUsed(name))
            }

            parameters.push(Parameter::new(name, value_type));

            if *self.peek()? != Token::Comma {
                break;
            }

            self.expect_token(Token::Comma)?;
        }

        Ok(parameters)
    }

    fn ends_with_return(&self, statements: &Vec<Statement>) -> Result<bool, ParserError> {
        let mut ok = false;
        let mut last_is_else = false;
        let size = statements.len();
        for (i, statement) in statements.into_iter().enumerate() {
            match statement {
                Statement::If(_, statements) => {
                    if i + 1 < size { // verify that there is not a if alone
                        ok = self.ends_with_return(statements)?;
                    }
                    last_is_else = false;
                }
                Statement::ElseIf(_, statements) => {
                    if ok {
                        ok = self.ends_with_return(statements)?;
                    }
                    last_is_else = false;
                }
                Statement::Else(statements) => {
                    if ok {
                        ok = self.ends_with_return(statements)?;
                        if ok {
                            last_is_else = true;
                            
                            if i + 1 < size { // if we have all case that returns something, then we don't allow dead code.
                                return Err(ParserError::DeadCodeNotAllowed) 
                            }
                        }
                    }
                }
                Statement::Return(_) => {
                    if last_is_else { // this return would be useless because if & else have a return, so we don't accept this one.
                        return Err(ParserError::ReturnAlreadyInElse)
                    }

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
     * - func foo(): int { ... }
     * - func foo(a: int, b: int) { ... }
     * - func (f Foo) bar() { ... }
     * Rules:
     * - Signature is based on function name, and parameters
     * - Entry function is a "public callable" function and must return a int value
     */
    fn read_function(&mut self, entry: bool, context: &Context) -> Result<(), ParserError> {
        let mut context: Context = context.clone();
        context.create_new_scope();
        let (instance_name, for_type) = if *self.peek()? == Token::ParenthesisOpen {
            self.next()?;
            let instance_name = self.next_identifier()?;
            let for_type = self.read_type()?;
            if !for_type.is_struct() { // TODO only types that are declared by the same program
                return Err(ParserError::InvalidFunctionType(for_type))
            }

            context.register_variable(instance_name.clone(), for_type.clone())?;
            self.expect_token(Token::ParenthesisClose)?;

            (Some(instance_name), Some(for_type))
        } else {
            (None, None)
        };

        let name = self.next_identifier()?;
        self.expect_token(Token::ParenthesisOpen)?;
        let parameters = self.read_parameters()?;
        self.expect_token(Token::ParenthesisClose)?;

        let return_type: Option<Type> = if entry { // all entries must return a int value
            Some(Type::Int)
        } else if *self.peek()? == Token::Colon { // read returned type
            self.next()?;
            Some(self.read_type()?)
        } else {
            None
        };

        if let Some(v) = &return_type {
            context.return_type = Some(v.clone());
        }

        for param in &parameters {
            context.register_variable(param.get_name().clone(), param.get_type().clone())?;
        }

        let statements = vec![];
        let function = FunctionType::Custom(CustomFunction::new(
            name,
            for_type,
            instance_name,
            parameters,
            statements,
            entry,
            return_type
        ));

        let params: Vec<&Type> = function.get_parameters_types();
        if self.has_function(&function.for_type(), function.get_name(), &params) {
           return Err(ParserError::FunctionSignatureAlreadyExist(function.get_name().clone())) 
        }
        self.functions.push(function); // push function before reading statements to allow recursive calls

        let statements = self.read_body(&mut context)?;
        if context.return_type.is_some() && !self.ends_with_return(&statements)? {
            return Err(ParserError::NoReturnFound)
        }

        context.remove_last_scope();

        match self.functions.last_mut() {
            Some(function) => {
                if let FunctionType::Custom(f) = function {
                    f.set_statements(statements);
                }
            },
            None => return Err(ParserError::FunctionNotFound(String::from("last one"), 0)) // shouldn't happen
        };

        Ok(())
    }

    fn has_function(&self, for_type: &Option<Type>, name: &String, params: &Vec<&Type>) -> bool {
        self.get_function(for_type, name, params).is_ok()
    }

    // get a function exist based on signature (name + params)
    fn get_function(&self, for_type: &Option<Type>, name: &String, params: &Vec<&Type>) -> Result<&FunctionType, ParserError> {
        let mut functions: Vec<&FunctionType> = self.functions.iter().map(|v| v).collect(); // merge two in one
        for f in self.env.get_functions() {
            functions.push(f);
        }

        'funcs: for f in functions {
            if *f.get_name() == *name && f.get_parameters_count() == params.len() {
                let same_type: bool = 
                if let Some(type_a) = for_type {
                    if let Some(type_b) = f.for_type() {
                        type_a.is_compatible_with(type_b)
                    } else {
                        false
                    }
                } else {
                    *for_type == *f.for_type()
                };

                if same_type {
                    let types = f.get_parameters_types();
                    for i in 0..params.len() {
                        let _type = types[i];
                        if *params[i] != *_type && *_type != Type::Any {
                            /*if let Some(for_type) = f.for_type() {
                                if _type == Type::T && for_type == params[i] {
                                    continue;
                                }
                            }*/
                            continue 'funcs;
                        }
                    }
    
                    return Ok(&f);
                }
            }
        }

        Err(ParserError::FunctionNotFound(name.clone(), params.len()))
    }

    fn get_structure(&self, name: &String) -> Result<&Struct, ParserError> {
        match self.structures.get(name) {
            Some(v) => Ok(v),
            None => match self.env.get_structure(name) {
                Some(v) => Ok(v),
                None => return Err(ParserError::StructureNotFound(name.clone()))
            }
        }
    }

    /**
     * Example: Message { message_id: int, message: string }
     * Rules:
     * - Structure name should start with a uppercase letter
     * - only letters in name
     */
    fn read_struct(&mut self) -> Result<Struct, ParserError> {
        let name = self.next_identifier()?;
        if !name.chars().all(char::is_alphabetic) {
            return Err(ParserError::InvalidStructureName(name))
        }

        match name.chars().nth(0) { // check if the first letter is in uppercase
            Some(v) => {
                let chars: Vec<char> =  v.to_uppercase().collect();
                if chars.len() == 0 || chars[0] != v {
                    return Err(ParserError::InvalidStructureName(name))
                }
            },
            None => return Err(ParserError::EmptyValue)
        };

        self.expect_token(Token::BraceOpen)?;
        let mut fields: HashMap<String, Type> = HashMap::new();
        for param in self.read_parameters()? {
            let (name, _type) = param.consume();
            fields.insert(name, _type);
        }
        self.expect_token(Token::BraceClose)?;

        Ok(Struct {
            name,
            fields
        })
    }

    pub fn parse(mut self) -> Result<Program, ParserError> {
        // parse all tokens
        let mut context: Context = Context::new();
        while self.tokens.len() > 0 {
            match self.next()? {
                Token::Import => {}, // TODO
                Token::Const => {
                    let var = self.read_variable(&mut context)?;
                    self.constants.push(var);
                },
                Token::Function => {
                    self.read_function(false, &context)?;
                },
                Token::Entry => {
                    self.read_function(true, &context)?;
                },
                Token::Struct => {
                    let new_struct = self.read_struct()?;
                    if self.structures.contains_key(&new_struct.name) {
                        return Err(ParserError::StructNameAlreadyUsed(new_struct.name))
                    }

                    self.structures.insert(new_struct.name.clone(), new_struct);
                },
                token => return Err(ParserError::UnexpectedToken(token))
            };
        }

        Ok(Program {
            constants: self.constants,
            structures: self.structures,
            functions: self.functions,
        })
    }
}