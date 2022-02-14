use crate::expressions::{Statement, Expression, DeclarationStatement, Parameter};
use crate::functions::{CustomFunction, FunctionType};
use crate::types::{Value, Type, Struct};
use crate::environment::Environment;
use crate::token::Token;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Program {
    pub constants: Vec<DeclarationStatement>,
    pub structures: HashMap<String, Struct>,
    pub functions: Vec<FunctionType>
}

#[derive(Clone)]
struct Context {
    variables: Vec<HashMap<String, Type>>,
    return_type: Option<Type>,
    has_if: bool,
    is_in_loop: bool,
    current_type: Option<Type>, // used by path walkthrough
}

impl Context {
    pub fn new() -> Self {
        Self {
            variables: vec![HashMap::new()], // first is for constants
            return_type: None,
            has_if: false,
            is_in_loop: false,
            current_type: None
        }
    }

    pub fn set_current_type(&mut self, current_type: Type) -> Result<(), ParserError> {
        self.remove_current_type(); // prevent any bug

        match &current_type {
            Type::Struct(ref s) => {
                self.create_new_scope();
                for (name, _type) in &s.fields {
                    self.register_variable(name.clone(), _type.clone())?;
                }
            }
            _ => {}
        }
        self.current_type = Some(current_type);
        Ok(())
    }

    pub fn remove_current_type(&mut self) {
        if let Some(t) = self.current_type.take() {
            match t {
                Type::Struct(_) => {
                    self.remove_last_scope();
                }
                _ => {}
            }
        }
    }

    pub fn get_current_type(&self) -> &Option<Type> {
        &self.current_type
    }

    pub fn get_type_of_variable(&self, key: &String) -> Result<&Type, ParserError> {
        for vars in &self.variables {
            if let Some(_type) = vars.get(key) {
                return Ok(_type)
            }
        }

        Err(ParserError::UnexpectedVariable(key.clone()))
    }

    pub fn has_variable(&self, key: &String) -> bool {
        for vars in &self.variables {
            if vars.contains_key(key) {
                return true
            }
        }

        match &self.current_type {
            Some(v) => match v {
                Type::Struct(ref structure) => structure.fields.contains_key(key),
                _ => false
            },
            None => false
        }
    }

    pub fn register_variable(&mut self, key: String, var_type: Type) -> Result<(), ParserError> {
        if self.has_variable(&key) {
            return Err(ParserError::VariableNameAlreadyUsed(key))
        }

        let last = self.variables.len() - 1;
        self.variables[last].insert(key, var_type);

        Ok(())
    }

    pub fn create_new_scope(&mut self) {
        self.variables.push(HashMap::new());
    }

    pub fn remove_last_scope(&mut self) {
        if self.has_scope() {
            self.variables.remove(self.variables.len() - 1);
        }
    }

    pub fn has_scope(&self) -> bool {
        self.variables.len() > 0
    }

    pub fn reset(&mut self) {
        self.variables.drain(1..self.variables.len());
        self.return_type = None;
        self.has_if = false;
    }
}

pub struct Parser {
    constants: Vec<DeclarationStatement>,
    tokens: Vec<Token>,
    functions: Vec<FunctionType>,
    structures: HashMap<String, Struct>,
    context: Context
}

#[derive(Debug)]
pub enum ParserError {
    ExpectedIdentifierToken(Token),
    UnexpectedToken(Token),
    InvalidToken(Token, Token),
    TypeNotFound(String),
    NoIfBeforeElse(Token),
    StructNameAlreadyUsed(String),
    VariableNameAlreadyUsed(String),
    FunctionSignatureAlreadyExist(String),
    UnexpectedVariable(String),
    InvalidStructField(String),
    InvalidStructureName(String),
    StructureNotFound(String),
    FunctionNotFound(String, usize),
    FunctionNoReturnType(String),
    EmptyValue,
    InvalidArrayCall,
    NotImplemented,
    InvalidOperation,
    InvalidOperationNotSameType(Type, Type),
    InvalidArrayCallIndexType(Type),
    InvalidTypeInArray(Type, Type),
    InvalidValueType(Type, Type),
    InvalidFunctionType(Type),
    EmptyArrayConstructor
}

impl Parser {
    pub fn new(tokens: Vec<Token>, env: Environment) -> Self {
        // first, we add the environment
        let mut functions = Vec::new();
        let mut structures = HashMap::new();
        let (env_funcs, env_structures) = env.consume();
        for func in env_funcs {
            functions.push(FunctionType::Native(func));
        }

        for structure in env_structures {
            structures.insert(structure.name.clone(), structure);
        }

        Parser {
            constants: Vec::new(),
            tokens,
            context: Context::new(),
            functions,
            structures
        }
    }

    fn next(&mut self) -> Token {
        self.tokens.remove(0)
    }

    fn see(&self) -> &Token {
        &self.tokens[0]
    }

    fn next_identifier(&mut self) -> Result<String, ParserError> {
        match self.next() {
            Token::Identifier(id) => Ok(id),
            token => Err(ParserError::ExpectedIdentifierToken(token))
        }
    }

    fn next_is_identifier(&self) -> bool {
        match &self.tokens[0] {
            Token::Identifier(_) => true,
            _ => false
        }
    }

    fn expect_token(&mut self, expected: Token) -> Result<Token, ParserError> {
        let token = self.next();
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

        match self.see() {
            Token::BracketOpen => {
                while *self.see() == Token::BracketOpen { // support multi dimensional arrays
                    self.expect_token(Token::BracketOpen)?;
                    self.expect_token(Token::BracketClose)?;
                    _type = Type::Array(Box::new(_type));
                }
            },
            _ => {}
        };

        Ok(_type)
    }

    fn get_type_from_expression(&mut self, expression: &Expression) -> Result<Type, ParserError> {
        let _type: Type = match expression {
            Expression::ArrayConstructor(ref values) => match values.get(0) {
                Some(v) => Type::Array(Box::new(self.get_type_from_expression(v)?)),
                None => return Err(ParserError::EmptyArrayConstructor) // TODO Real error
            },
            Expression::Variable(ref var_name) => {
                let _type = self.context.get_type_of_variable(var_name)?.clone();
                _type
            },
            Expression::FunctionCall(name, parameters) => {
                let mut types: Vec<Type> = vec![];
                for param in parameters {
                    types.push(self.get_type_from_expression(param)?);
                }

                let mut _types: Vec<&Type> = vec![];
                for t in &types {
                    _types.push(t);
                }

                let func = self.get_function(self.context.get_current_type(), name, &_types)?;
                match &func.return_type() {
                    Some(ref v) => v.clone(),
                    None => return Err(ParserError::FunctionNoReturnType(name.clone()))
                }
            },
            Expression::Value(ref val) => match Type::from_value(val, &self.structures) { // we have to clone everything due to this
                Some(v) => v,
                None => return Err(ParserError::EmptyValue)
            },
            Expression::ArrayCall(path, _) => {
                match self.get_type_from_expression(path)? {
                    Type::Array(_type) => *_type,
                    _ => return Err(ParserError::InvalidArrayCall)
                }
            },
            Expression::SubExpression(expr) => self.get_type_from_expression(expr)?,
            Expression::StructConstructor(name, _) => {
                match self.structures.get(name) {
                    Some(s) => Type::Struct(s.clone()),
                    None => return Err(ParserError::StructureNotFound(name.clone()))
                }
            }
            Expression::Path(left, right) => {
                let var_type = self.get_type_from_expression(left)?;
                self.context.set_current_type(var_type)?;
                let _type = self.get_type_from_expression(right)?;
                self.context.remove_current_type();
                _type
            },
            Expression::Operator(_, left, right) => { // TODO verify all operations
                let left_type = self.get_type_from_expression(left)?;
                let right_type = self.get_type_from_expression(right)?;

                if left_type == Type::String || right_type == Type::String {
                    Type::String
                } else {
                    left_type
                }
            },
        };

        Ok(_type)
    }

    fn read_expression(&mut self) -> Result<Expression, ParserError> {
        let mut required_operator = false;
        let mut last_expression: Option<Expression> = None;
        while !self.see().should_stop() && (required_operator == self.see().is_operator() || (*self.see() == Token::BracketOpen && last_expression.is_none())) {
            let expr: Expression = match self.next() {
                Token::BracketOpen => {
                    match last_expression {
                        Some(v) => {
                            if !self.get_type_from_expression(&v)?.is_array() {
                                return Err(ParserError::InvalidArrayCall)
                            }

                            let index = self.read_expression()?;
                            let index_type = self.get_type_from_expression(&index)?;
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
                            while *self.see() != Token::BracketClose {
                                let expr = self.read_expression()?;
                                match &array_type { // array values must have the same type
                                    Some(t) => {
                                        let _type = self.get_type_from_expression(&expr)?;
                                        if _type != *t {
                                            return Err(ParserError::InvalidTypeInArray(_type, t.clone()))
                                        }
                                    },
                                    None => { // first value rules the type of array
                                        array_type = Some(self.get_type_from_expression(&expr)?);
                                    }
                                };
                                expressions.push(expr);

                                if *self.see() == Token::Comma {
                                    self.expect_token(Token::Comma)?;
                                }
                            }

                            self.expect_token(Token::BracketClose)?;
                            Expression::ArrayConstructor(expressions)
                        }
                    }
                },
                Token::ParenthesisOpen => {
                    let expr = self.read_expression()?;
                    self.expect_token(Token::ParenthesisClose)?;
                    Expression::SubExpression(Box::new(expr))
                },
                Token::Identifier(id) => {
                    match self.see() {
                        Token::ParenthesisOpen => {
                            self.next(); // we remove the token from the list
                            let mut parameters: Vec<Expression> = vec![];
                            let mut types: Vec<Type> = vec![];
                            while *self.see() != Token::ParenthesisClose { // read parameters for function call
                                let expr = self.read_expression()?;
                                types.push(self.get_type_from_expression(&expr)?);
                                parameters.push(expr);

                                if *self.see() == Token::Comma {
                                    self.expect_token(Token::Comma)?;
                                }
                            }

                            let mut _types: Vec<&Type> = vec![];
                            for t in &types {
                                _types.push(t);
                            }

                            // Function call cannot call entry function
                            if self.get_function(self.context.get_current_type(), &id, &_types)?.is_entry() {
                                return Err(ParserError::FunctionNotFound(id, parameters.len()))
                            }

                            self.expect_token(Token::ParenthesisClose)?;
                            Expression::FunctionCall(id, parameters)
                        },
                        _ => {
                            if self.context.has_variable(&id) {
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
                                    if fields.contains_key(&field_name) || !self.get_structure(&id)?.fields.contains_key(&field_name) {
                                        return Err(ParserError::InvalidStructField(field_name))
                                    }

                                    match self.next() {
                                        Token::Comma => {
                                            if self.context.has_variable(&field_name) {
                                                fields.insert(field_name.clone(), Expression::Variable(field_name));
                                            } else {
                                                return Err(ParserError::UnexpectedVariable(field_name)) 
                                            }
                                        }
                                        Token::Colon => {
                                            fields.insert(field_name, self.read_expression()?);
                                            if *self.see() == Token::Comma {
                                                self.next();
                                            }
                                        }
                                        token => {
                                            return Err(ParserError::UnexpectedToken(token))
                                        }
                                    };
                                }
                                self.expect_token(Token::BraceClose)?;
                                Expression::StructConstructor(name, fields)
                            } else {
                                return Err(ParserError::UnexpectedVariable(id))
                            }
                        }
                    }
                },
                Token::Number(value) => { // TODO byte, short
                    Expression::Value(Value::Int(value))
                },
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
                            let _type = self.get_type_from_expression(&value)?;
                            self.context.set_current_type(_type)?;
                            let right_expr = self.read_expression()?;
                            self.context.remove_current_type();
                            required_operator = !required_operator; // because we read operator DOT + right expression
                            Expression::Path(Box::new(value), Box::new(right_expr))
                        },
                        None => return Err(ParserError::UnexpectedToken(Token::Dot))
                    }
                }
                token => {
                    if !token.is_operator() {
                        return Err(ParserError::UnexpectedToken(token))
                    }
                    match last_expression {
                        Some(e) => {
                            self.context.remove_current_type();
                            required_operator = !required_operator;
                            let left_type = self.get_type_from_expression(&e)?;
                            let expr = self.read_expression()?;
                            let right_type = self.get_type_from_expression(&expr)?;
                            if left_type != Type::String && right_type != Type::String {
                                if left_type != right_type {
                                    return Err(ParserError::InvalidOperationNotSameType(left_type, right_type))
                                }
                            }

                            Expression::Operator(token, Box::new(e), Box::new(expr))
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
    fn read_body(&mut self) -> Result<Vec<Statement>, ParserError> {
        self.context.create_new_scope();
        self.expect_token(Token::BraceOpen)?;
        let statements = self.read_statements()?;
        self.expect_token(Token::BraceClose)?;
        self.context.remove_last_scope();
        Ok(statements)
    }

    /**
     * Example: let hello: string = "hello";
     * Rules:
     * - Every variable must be declared with 'let' keyword
     * - Variable name must alphanumeric characters
     * - Must provide value type
     * - If no value is set, Null is set by default
     */
    fn read_variable(&mut self) -> Result<DeclarationStatement, ParserError> {
        let name = self.next_identifier()?;
        if self.context.has_variable(&name) {
            return Err(ParserError::VariableNameAlreadyUsed(name))
        }

        self.expect_token(Token::Colon)?;
        let value_type = self.read_type()?;
        let value: Expression = if *self.see() == Token::OperatorAssign {
            self.expect_token(Token::OperatorAssign)?;
            let expr = self.read_expression()?;
            let expr_type = match self.get_type_from_expression(&expr) {
                Ok(_type) => _type,
                Err(e) => match e { // support empty array declaration
                    ParserError::EmptyArrayConstructor if value_type.is_array() => value_type.clone(),
                    _ => return Err(e)
                }
            };
            if expr_type != value_type {
                return Err(ParserError::InvalidValueType(expr_type, value_type))
            }

            expr
        } else {
            Expression::Value(Value::Null)
        };

        self.context.register_variable(name.clone(), value_type.clone())?;

        Ok(DeclarationStatement {
            name,
            value_type,
            value
        })
    }

    fn read_loop_body(&mut self) -> Result<Vec<Statement>, ParserError> {
        let old_value = self.context.is_in_loop; // support loop in loop
        self.context.is_in_loop = true;
        let statements = self.read_body()?;
        self.context.is_in_loop = old_value;

        Ok(statements)
    }

    fn read_statements(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut statements: Vec<Statement> = vec![];
        while *self.see() != Token::BraceClose {
            let statement: Statement = match self.see() {
                Token::For => {
                    self.expect_token(Token::For)?;
                    self.expect_token(Token::Let)?;
                    let var = self.read_variable()?;
                    let condition = self.read_expression()?;
                    let increment = self.read_expression()?;
                    let statements = self.read_loop_body()?;

                    Statement::For(var, condition, increment, statements)
                }
                Token::ForEach => {
                    self.expect_token(Token::ForEach)?;
                    let variable: String = match self.next() {
                        Token::Identifier(v) => v,
                        token => return Err(ParserError::UnexpectedToken(token))
                    };

                    let condition = self.read_expression()?;
                    let statements = self.read_loop_body()?;

                    Statement::ForEach(variable, condition, statements)
                },
                Token::While => {
                    self.expect_token(Token::While)?;

                    let condition = self.read_expression()?;
                    let statements = self.read_loop_body()?;

                    Statement::While(condition, statements)
                },
                Token::If => {
                    self.expect_token(Token::If)?;
                    let condition = self.read_expression()?;
                    Statement::If(condition, self.read_body()?)
                },
                Token::Else => {
                    let token = self.expect_token(Token::Else)?;
                    if !self.context.has_if {
                        return Err(ParserError::NoIfBeforeElse(token))
                    }

                    if *self.see() == Token::If {
                        self.expect_token(Token::If)?;
                        Statement::ElseIf(self.read_expression()?, self.read_body()?)
                    } else {
                        Statement::Else(self.read_body()?)
                    }
                },
                Token::BraceOpen => {
                    Statement::Scope(self.read_body()?)
                },
                Token::Let => {
                    self.expect_token(Token::Let)?;
                    Statement::Variable(self.read_variable()?)
                }
                Token::Return => {
                    self.expect_token(Token::Return)?;
                    let opt: Option<Expression> = if self.context.return_type.is_some() {
                        let expr = self.read_expression()?;
                        let expr_type = self.get_type_from_expression(&expr)?;
                        if let Some(return_type) = &self.context.return_type {
                            if expr_type != *return_type {
                                return Err(ParserError::InvalidValueType(expr_type, return_type.clone()))
                            }
                        }
                        Some(expr)

                    } else {
                        None
                    };

                    if *self.see() != Token::BraceClose { // we can't have anything after a return
                        return Err(ParserError::UnexpectedToken(self.next()));
                    }

                    Statement::Return(opt)
                }
                Token::Continue => {
                    let token = self.expect_token(Token::Continue)?;
                    if !self.context.is_in_loop {
                        return Err(ParserError::UnexpectedToken(token));
                    }

                    if *self.see() != Token::BraceClose { // we can't have anything after a continue
                        return Err(ParserError::UnexpectedToken(self.next()));
                    }

                    Statement::Continue
                },
                Token::Break => {
                    let token = self.expect_token(Token::Break)?;
                    if !self.context.is_in_loop {
                        return Err(ParserError::UnexpectedToken(token));
                    }

                    if *self.see() != Token::BraceClose { // we can't have anything after a break
                        return Err(ParserError::UnexpectedToken(self.next()));
                    }

                    Statement::Break
                },
                _ => Statement::Expression(self.read_expression()?)
            };

            match &statement {
                Statement::If(_, _) | Statement::ElseIf(_, _) => {
                    self.context.has_if = true;
                },
                _ => {
                    self.context.has_if = false;
                }
            };

            statements.push(statement);
        }

        Ok(statements)
    }

    fn read_parameters(&mut self) -> Result<Vec<Parameter>, ParserError> {
        let mut parameters: Vec<Parameter> = vec![];
        loop {
            if self.next_is_identifier() {
                let name = self.next_identifier()?;
                self.expect_token(Token::Colon)?;
                let value_type = self.read_type()?;
                
                if parameters.len() > 0 && parameters.iter().any(|p| *p.get_name() == name) { // verify that we have unique names here
                    return Err(ParserError::VariableNameAlreadyUsed(name))
                }
                parameters.push(Parameter::new(name, value_type));

                if *self.see() != Token::Comma {
                    break;
                }
    
                self.expect_token(Token::Comma)?;
            } else {
                break;
            }
        }

        Ok(parameters)
    }

    /**
     * Examples:
     * - entry foo() { ... }
     * - function foo() { ... }
     * - function foo(): int { ... }
     * - function foo(a: int, b: int) { ... }
     * - function (f Foo) bar() { ... }
     * Rules:
     * - Signature is based on function name, and parameters
     * - Entry function are "public callable" function and must return a int value
     */
    fn read_function(&mut self, entry: bool) -> Result<(), ParserError> {
        self.context.reset();
        self.context.create_new_scope();
        let (instance_name, for_type) = if *self.see() == Token::ParenthesisOpen {
            self.next();
            let instance_name = self.next_identifier()?;
            let for_type = self.read_type()?;
            if !for_type.is_struct() { // TODO only types that are declared by the same program
                return Err(ParserError::InvalidFunctionType(for_type))
            }

            self.context.register_variable(instance_name.clone(), for_type.clone())?;
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
        } else if *self.see() == Token::Colon { // read returned type
            self.next();
            Some(self.read_type()?)
        } else {
            None
        };

        if let Some(v) = &return_type {
            self.context.return_type = Some(v.clone());
        }

        for param in &parameters {
            self.context.register_variable(param.get_name().clone(), param.get_type().clone())?;
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

        let statements = self.read_body()?;        
        self.context.remove_last_scope();

        let last = self.functions.len() - 1;
        match self.functions.get_mut(last) {
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
        'funcs: for f in &self.functions {
            if *f.get_name() == *name && f.get_parameters_count() == params.len() {
                let types = f.get_parameters_types();
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
            None => return Err(ParserError::StructureNotFound(name.clone()))
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
        while self.tokens.len() > 0 {
            match self.next() {
                Token::Import => {},
                Token::Const => {
                    let var = self.read_variable()?;
                    self.constants.push(var);
                },
                Token::Function => {
                    self.read_function(false)?;
                },
                Token::Entry => {
                    self.read_function(true)?;
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