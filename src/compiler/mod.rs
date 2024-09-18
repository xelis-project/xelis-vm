use crate::{IdentifierType, Program, Type, Value};

pub enum CompilerError {
    NotImplemented
}

pub struct Binary {

}

pub enum Operator {
    Equals, // ==
    NotEquals, // !=
    And, // &&
    Or, // ||
    GreaterThan, // >
    LessThan, // <
    GreaterOrEqual, // >=
    LessOrEqual, // <=
    Plus, // +
    Minus, // -
    Multiply, // *
    Divide, // /
    Rem, // %

    BitwiseXor, // ^
    BitwiseAnd, // &
    BitwiseOr, // |
    BitwiseLeft, // <<
    BitwiseRight, // >>
}

pub enum OpCode {
    // push value
    Value(Value),
    // load from stack, push
    Get(IdentifierType),
    // pop, store in stack
    Set(IdentifierType),
    // pop last => get_sub_value => push
    On(Box<OpCode>),
    // pop value => push value
    Cast(Type),
    // pop value => push !value
    Not,
    // left = right
    Assign,
    // pop both, execute op, push
    Op(Operator),
    // pop condition => true => add opcode
    OpAnd(Box<OpCode>),
    // pop condition => false => add opcode
    OpOr(Box<OpCode>),
    // pop condition => execute one branch => push
    Ternary {
        left: Box<OpCode>,
        right: Box<OpCode>
    },
    // pop args, pop on_value => call function
    FunctionCall {
        args: usize,
        on_value: bool,
        id: IdentifierType
    },
    // pop 2 values => left.get_sub_value(right)
    ArrayCall,
    // pop N values => create array 
    Array {
        args: usize
    },
    // pop N values => create struct
    Struct {
        args: usize,
        id: IdentifierType
    },
    // pop => condition => add opcode
    IfElse {
        if_body: Vec<OpCode>,
        else_body: Vec<OpCode>
    }
}

pub struct Chunk {
    op_codes: Vec<OpCode>
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            op_codes: Vec::new()
        }
    }

    pub fn push(&mut self, op_code: OpCode) {
        self.op_codes.push(op_code);
    }
}

// Its main purpose is to compile the program
pub struct Compiler {
    program: Program,
}

impl Compiler {
    pub fn new(program: Program) -> Self {
        Compiler {
            program
        }
    }

    pub fn compile(&self) -> Result<Binary, CompilerError> {
        let mut chunks: Vec<Chunk> = Vec::new();

        for (id, function) in &self.program.functions {
            let mut chunk = Chunk::new();
            // function.get_statemnet
            chunks.push(chunk);
        }
        Ok(Binary {

        })
    }
}