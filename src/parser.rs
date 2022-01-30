use crate::token::Token;
use crate::types::{Value, Type, Struct};
use std::collections::HashMap;


#[derive(Debug)]
pub enum Expression {
    FunctionCall(String, Vec<Expression>), // function name, parameters
    ArrayCall(Box<Expression>, Box<Expression>), // expr, index
    ArrayConstructor(Vec<Expression>),
    StructConstructor(String, HashMap<String, Expression>),
    Variable(String), // variable name
    Value(Value), // hardcoded value
    Operator(Token, Box<Expression>, Box<Expression>),
    SubExpression(Box<Expression>), // ( ... )
    Path(Box<Expression>, Box<Expression>)
}

#[derive(Debug)]
pub struct DeclarationStatement {
    pub name: String,
    pub value_type: Type,
    pub value: Expression,
}

#[derive(Debug)]
pub enum Statement {
    If(Expression, Vec<Statement>),
    Else(Vec<Statement>),
    ElseIf(Expression, Vec<Statement>),
    While(Expression, Vec<Statement>),
    ForEach(String, Expression, Vec<Statement>), // for a in array
    For(DeclarationStatement, Expression, Expression, Vec<Statement>), // for i: int = 0; i < 10; i++ (; will not be saved)
    Expression(Expression),
    Return(Option<Expression>),
    Scope(Vec<Statement>),
    Break,
    Continue,
    Variable(DeclarationStatement),
}


#[derive(Debug)]
pub struct Parameter {
    name: String,
    value_type: Type
}

#[derive(Debug)]
pub struct Function {
    name: String,
    for_type: Option<Type>,
    instance_name: Option<String>,
    parameters: Vec<Parameter>,
    statements: Vec<Statement>,
    entry: bool,
    return_type: Option<Type>
}

#[derive(Debug)]
pub struct Program {
    constants: Vec<DeclarationStatement>,
    structures: HashMap<String, Struct>,
    functions: Vec<Function>
}

#[derive(Clone)]
struct Context {
    variables: Vec<HashMap<String, Type>>,
    return_value: bool,
    has_if: bool,
    is_in_loop: bool,
    current_type: Option<Type>, // used by path walkthrough
}

impl Context {
    pub fn new() -> Self {
        Self {
            variables: vec![HashMap::new()],
            return_value: false,
            has_if: false,
            is_in_loop: false,
            current_type: None
        }
    }

    pub fn set_current_type(&mut self, current_type: Type) {
        self.remove_current_type(); // prevent any bug

        match &current_type {
            Type::Struct(ref s) => {
                self.create_new_scope();
                for (name, _type) in &s.fields {
                    self.register_variable(name.clone(), _type.clone());
                }
            }
            _ => {}
        }
        self.current_type = Some(current_type);
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

    pub fn register_variable(&mut self, key: String, var_type: Type) {
        let last = self.variables.len() - 1;
        self.variables[last].insert(key, var_type);
    }

    pub fn create_new_scope(&mut self) {
        self.variables.push(HashMap::new());
    }

    pub fn remove_last_scope(&mut self) {
        self.variables.remove(self.variables.len() - 1);
    }

    pub fn has_scope(&self) -> bool {
        self.variables.len() > 0
    }

    pub fn reset(&mut self) {
        self.variables.clear();
        self.return_value = false;
        self.has_if = false;
    }
}

pub struct Parser {
    constants: Vec<DeclarationStatement>,
    tokens: Vec<Token>,
    functions: Vec<Function>,
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
    StructureNotFound(String),
    FunctionNotFound(String, usize),
    InvalidArrayCall,
    NotImplemented,
    InvalidOperation
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            constants: Vec::new(),
            tokens,
            context: Context::new(),
            functions: vec![],
            structures: HashMap::new()
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

    fn read_expression(&mut self) -> Result<Expression, ParserError> {
        let mut required_operator = false;
        let mut last_expression: Option<Expression> = None;
        while !self.see().should_stop() && (required_operator == self.see().is_operator() || (*self.see() == Token::BracketOpen && last_expression.is_none())) {
            let expr: Expression = match self.next() {
                Token::BracketOpen => {
                    match last_expression {
                        Some(v) => {
                            match &v { // TODO verify if the last expression is compatible for a array call
                                Expression::Variable(ref var) => {
                                    if !self.context.get_type_of_variable(var)?.is_array() {
                                        return Err(ParserError::InvalidArrayCall)
                                    }
                                }
                                Expression::ArrayCall(_, _)  | Expression::FunctionCall(_, _) | Expression::ArrayConstructor(_) => {

                                },
                                _ => return Err(ParserError::InvalidArrayCall)
                            }

                            let index = self.read_expression()?;
                            self.expect_token(Token::BracketClose)?;
                            required_operator = !required_operator;
                            Expression::ArrayCall(Box::new(v), Box::new(index))
                        },
                        None => { // require at least one value in a array constructor
                            let mut expressions: Vec<Expression> = vec![];
                            loop {
                                let expr = self.read_expression()?;
                                expressions.push(expr);
                                if *self.see() != Token::Comma {
                                    break;
                                } else {
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
                            while *self.see() != Token::ParenthesisClose { // read parameters for function call
                                parameters.push(self.read_expression()?);
                                if *self.see() == Token::Comma {
                                    self.expect_token(Token::Comma)?;
                                }
                            }
                            if !self.has_function_with_size(self.context.get_current_type(), &id, parameters.len()) {
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
                            match &value {
                                Expression::Variable(id) => {
                                    self.context.set_current_type(self.context.get_type_of_variable(&id)?.clone());
                                }
                                _ => {}
                            };
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
                            required_operator = !required_operator;
                            Expression::Operator(token, Box::new(e), Box::new(self.read_expression()?))
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
        self.expect_token(Token::Let)?;
        let name = self.next_identifier()?;
        if self.context.has_variable(&name) {
            return Err(ParserError::VariableNameAlreadyUsed(name))
        }

        self.expect_token(Token::Colon)?;
        let value_type = self.read_type()?;
        let value: Expression = if *self.see() == Token::OperatorAssign {
            self.expect_token(Token::OperatorAssign)?;
            self.read_expression()?
        } else {
            Expression::Value(Value::Null)
        };

        self.context.register_variable(name.clone(), value_type.clone());
        Ok(DeclarationStatement {
            name,
            value_type,
            value
        })
    }

    fn read_statements(&mut self) -> Result<Vec<Statement>, ParserError> {
        let mut statements: Vec<Statement> = vec![];
        while *self.see() != Token::BraceClose {
            let statement: Statement = match self.see() {
                Token::For => {
                    self.expect_token(Token::For)?;
                    let var = self.read_variable()?;
                    let condition = self.read_expression()?;
                    let increment = self.read_expression()?;
                    
                    self.context.is_in_loop = true;
                    let statements = self.read_body()?;
                    self.context.is_in_loop = false;

                    Statement::For(var, condition, increment, statements)
                }
                Token::ForEach => {
                    self.expect_token(Token::ForEach)?;
                    let variable: String = match self.next() {
                        Token::Identifier(v) => v,
                        token => return Err(ParserError::UnexpectedToken(token))
                    };

                    let condition = self.read_expression()?;
                    self.context.is_in_loop = true;
                    let statements = self.read_body()?;
                    self.context.is_in_loop = false;

                    Statement::ForEach(variable, condition, statements)
                },
                Token::While => {
                    self.expect_token(Token::While)?;

                    let condition = self.read_expression()?;
                    self.context.is_in_loop = true;
                    let statements = self.read_body()?;
                    self.context.is_in_loop = false;

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
                    Statement::Variable(self.read_variable()?)
                }
                Token::Return => {
                    self.expect_token(Token::Return)?;
                    let opt: Option<Expression> = if self.context.return_value {
                        Some(self.read_expression()?)
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
                
                if parameters.len() > 0 && parameters.iter().any(|p| *p.name == name) { // verify that we have unique names here
                    return Err(ParserError::VariableNameAlreadyUsed(name))
                }
                parameters.push(Parameter {
                    name,
                    value_type
                });

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
    fn read_function(&mut self, entry: bool) -> Result<Function, ParserError> {
        self.context.reset();
        self.context.create_new_scope();
        let (instance_name, for_type) = if *self.see() == Token::ParenthesisOpen {
            self.next();
            let instance_name = self.next_identifier()?;
            let for_type = self.read_type()?;
            self.context.register_variable(instance_name.clone(), for_type.clone());
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
            self.context.return_value = true;
            Some(Type::Byte) // TODO entry should really return something ?
        } else if *self.see() == Token::Colon { // read returned type
            self.context.return_value = true;
            self.next();
            Some(self.read_type()?)
        } else {
            None
        };

        for param in &parameters {
            self.context.register_variable(param.name.clone(), param.value_type.clone());
        }
        let statements = self.read_body()?;
        self.context.remove_last_scope();

        Ok(Function {
            name,
            instance_name,
            for_type,
            parameters,
            statements,
            entry,
            return_type
        })
    }

    // verify if a function exist based on signature (name + params)
    fn has_function(&self, for_type: &Option<Type>, name: &String, params: &Vec<&Type>) -> bool {
        self.functions.iter().any(|f| -> bool {
            if f.for_type == *for_type && *f.name == *name && f.parameters.len() == params.len() {
                let mut val = true;
                for i in 0..params.len() {
                    if *params[i] != f.parameters[i].value_type {
                        val = false;
                        break;
                    }
                }
                val
            } else {
                false
            }
        })
    }

    fn has_function_with_size(&self, for_type: &Option<Type>, name: &String, params: usize) -> bool {
        self.functions.iter().any(|f| f.for_type == *for_type && *f.name == *name && f.parameters.len() == params)
    }

    fn get_structure(&self, name: &String) -> Result<&Struct, ParserError> {
        match self.structures.get(name) {
            Some(v) => Ok(v),
            None => return Err(ParserError::StructureNotFound(name.clone()))
        }
    }

    fn read_struct(&mut self) -> Result<Struct, ParserError> {
        let name = self.next_identifier()?;
        self.expect_token(Token::BraceOpen)?;
        let mut fields: HashMap<String, Type> = HashMap::new();
        for param in self.read_parameters()? {
            fields.insert(param.name, param.value_type);
        }
        self.expect_token(Token::BraceClose)?;

        Ok(Struct {
            name,
            fields
        })
    }

    fn create_function(&mut self, entry: bool) -> Result<(), ParserError> {
        let function = self.read_function(entry)?;
        let params: Vec<&Type> = function.parameters.iter().map(|s| &s.value_type).collect();
        if self.has_function(&function.for_type, &function.name, &params) {
           return Err(ParserError::FunctionSignatureAlreadyExist(function.name)) 
        }
        self.functions.push(function);

        Ok(())
    }

    pub fn parse(mut self) -> Result<Program, ParserError> { // TODO return a program
        while self.tokens.len() > 0 {
            match self.next() {
                Token::Import => {},
                Token::Const => {
                    let var_name = self.next_identifier()?;
                    self.expect_token(Token::Colon)?;
                    let var_type = self.read_type()?;
                    self.expect_token(Token::OperatorAssign)?;
                    let expr = self.read_expression()?;
                    self.context.register_variable(var_name.clone(), var_type.clone());
                    self.constants.push(DeclarationStatement {
                        name: var_name,
                        value_type: var_type,
                        value: expr
                    });
                },
                Token::Function => {
                    self.create_function(false)?;
                },
                Token::Entry => {
                    self.create_function(true)?;
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