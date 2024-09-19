#[derive(Debug)]
pub enum OpCode {
    // load constant
    Constant,
    // load from registers, push in stack
    MemoryLoad,
    // pop, store in registers
    MemorySet,
    // pop, store in registers[index]
    MemoryAssign,
    // load from stack, load u16, load sub value, push
    // used as array call and struct field access
    SubLoad,
    // pop value only
    Pop,
    // push copied value
    Copy,
    // Copy N value
    Copy2,

    // Swap top and N value
    Swap,
    // Swap N and Y values
    Swap2,

    // pop value, jump
    Jump,
    // pop value, jump if false
    JumpIfFalse,
    // pop value, get iterable length, push
    IterableLength,
    // Return at the end of chunk
    Return,

    // pop index, pop array => push array[index]
    // Allow up to u32 index
    ArrayCall,
    // pop value => push value
    Cast,
    // pop args u8 count, on_value bool, fn id
    FunctionCall,
    // pop length, pop N values => create array
    NewArray,
    // pop type id, pop N values => create struct
    NewStruct,

    // Operators
    // +
    Add,
    // -
    Sub,
    // *
    Mul,
    // /
    Div,
    // %
    Mod,
    // **
    Pow,
    // &
    And,
    // |
    Or,
    // ^
    Xor,
    // <<
    Shl,
    // >>
    Shr,
    // ==
    Eq,
    // !
    Neg,
    // >
    Gt,
    // <
    Lt,
    // >=
    Gte,
    // <=
    Lte,
    // ++
    Inc,
    // --
    Dec,
}

impl OpCode {
    // Convert the OpCode to a byte
    #[inline]
    pub fn as_byte(&self) -> u8 {
        match self {
            OpCode::Constant => 0,
            OpCode::MemoryLoad => 1,
            OpCode::MemorySet => 2,
            OpCode::MemoryAssign => 3,
            OpCode::SubLoad => 4,
            OpCode::Pop => 5,
            OpCode::Copy => 6,
            OpCode::Copy2 => 7,
            OpCode::Swap => 8,
            OpCode::Swap2 => 9,
            OpCode::Jump => 10,
            OpCode::JumpIfFalse => 11,
            OpCode::IterableLength => 12,
            OpCode::Return => 13,
            OpCode::ArrayCall => 14,
            OpCode::Cast => 15,
            OpCode::FunctionCall => 16,
            OpCode::NewArray => 17,
            OpCode::NewStruct => 18,
            OpCode::Add => 19,
            OpCode::Sub => 20,
            OpCode::Mul => 21,
            OpCode::Div => 22,
            OpCode::Mod => 23,
            OpCode::Pow => 24,
            OpCode::And => 25,
            OpCode::Or => 26,
            OpCode::Xor => 27,
            OpCode::Shl => 28,
            OpCode::Shr => 29,
            OpCode::Eq => 30,
            OpCode::Neg => 31,
            OpCode::Gt => 32,
            OpCode::Lt => 33,
            OpCode::Gte => 34,
            OpCode::Lte => 35,
            OpCode::Inc => 36,
            OpCode::Dec => 37,
        }
    }

    // Convert a byte to an OpCode
    #[inline]
    pub fn from_byte(byte: u8) -> Option<OpCode> {
        Some(match byte {
            0 => OpCode::Constant,
            1 => OpCode::MemoryLoad,
            2 => OpCode::MemorySet,
            3 => OpCode::MemoryAssign,
            4 => OpCode::SubLoad,
            5 => OpCode::Pop,
            6 => OpCode::Copy,
            7 => OpCode::Copy2,
            8 => OpCode::Swap,
            9 => OpCode::Swap2,
            10 => OpCode::Jump,
            11 => OpCode::JumpIfFalse,
            12 => OpCode::IterableLength,
            13 => OpCode::Return,
            14 => OpCode::ArrayCall,
            15 => OpCode::Cast,
            16 => OpCode::FunctionCall,
            17 => OpCode::NewArray,
            18 => OpCode::NewStruct,
            19 => OpCode::Add,
            20 => OpCode::Sub,
            21 => OpCode::Mul,
            22 => OpCode::Div,
            23 => OpCode::Mod,
            24 => OpCode::Pow,
            25 => OpCode::And,
            26 => OpCode::Or,
            27 => OpCode::Xor,
            28 => OpCode::Shl,
            29 => OpCode::Shr,
            30 => OpCode::Eq,
            31 => OpCode::Neg,
            32 => OpCode::Gt,
            33 => OpCode::Lt,
            34 => OpCode::Gte,
            35 => OpCode::Lte,
            36 => OpCode::Inc,
            37 => OpCode::Dec,
            _ => return None,
        })
    }
}