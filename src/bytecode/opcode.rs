#[derive(Debug)]
pub enum OpCode {
    // load constant
    Constant,
    // load from registers, push in stack
    MemoryLoad,
    // pop, set in registers[index]
    MemorySet,
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
    // Return will stop processing the opcodes of a chunk
    Return,

    // pop index, pop array => push array[index]
    // Allow up to u32 index
    ArrayCall,
    // pop value => push value
    Cast,
    // pop args u16 count, on_value bool, fn id u16
    InvokeChunk,
    // Same as InvokeChunk, but for system calls
    SysCall,
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

    // Assign Operators
    // =
    // Useful for subload, array call, etc.. compatibility
    Assign,
    // +=
    AssignAdd,
    // -=
    AssignSub,
    // *=
    AssignMul,
    // /=
    AssignDiv,
    // %=
    AssignMod,
    // **=
    AssignPow,
    // &=
    AssignAnd,
    // |=
    AssignOr,
    // ^=
    AssignXor,
    // <<=
    AssignShl,
    // >>=
    AssignShr,

    // ++
    Inc,
    // --
    Dec,
}

impl OpCode {
    // Convert the OpCode to a byte
    #[inline]
    pub const fn as_byte(&self) -> u8 {
        match self {
            OpCode::Constant => 0,
            OpCode::MemoryLoad => 1,
            OpCode::MemorySet => 2,
            OpCode::SubLoad => 3,
            OpCode::Pop => 4,
            OpCode::Copy => 5,
            OpCode::Copy2 => 6,
            OpCode::Swap => 7,
            OpCode::Swap2 => 8,
            OpCode::Jump => 9,
            OpCode::JumpIfFalse => 10,
            OpCode::IterableLength => 11,
            OpCode::Return => 12,
            OpCode::ArrayCall => 13,
            OpCode::Cast => 14,
            OpCode::InvokeChunk => 15,
            OpCode::SysCall => 16,
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

            OpCode::Assign => 36,
            OpCode::AssignAdd => 37,
            OpCode::AssignSub => 38,
            OpCode::AssignMul => 39,
            OpCode::AssignDiv => 40,
            OpCode::AssignMod => 41,
            OpCode::AssignPow => 42,
            OpCode::AssignAnd => 43,
            OpCode::AssignOr => 44,
            OpCode::AssignXor => 45,
            OpCode::AssignShl => 46,
            OpCode::AssignShr => 47,

            OpCode::Inc => 48,
            OpCode::Dec => 49,
        }
    }

    // Convert a byte to an OpCode
    #[inline]
    pub const fn from_byte(byte: u8) -> Option<OpCode> {
        Some(match byte {
            0 => OpCode::Constant,
            1 => OpCode::MemoryLoad,
            2 => OpCode::MemorySet,
            3 => OpCode::SubLoad,
            4 => OpCode::Pop,
            5 => OpCode::Copy,
            6 => OpCode::Copy2,
            7 => OpCode::Swap,
            8 => OpCode::Swap2,
            9 => OpCode::Jump,
            10 => OpCode::JumpIfFalse,
            11 => OpCode::IterableLength,
            12 => OpCode::Return,
            13 => OpCode::ArrayCall,
            14 => OpCode::Cast,
            15 => OpCode::InvokeChunk,
            16 => OpCode::SysCall,
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

            36 => OpCode::Assign,
            37 => OpCode::AssignAdd,
            38 => OpCode::AssignSub,
            39 => OpCode::AssignMul,
            40 => OpCode::AssignDiv,
            41 => OpCode::AssignMod,
            42 => OpCode::AssignPow,
            43 => OpCode::AssignAnd,
            44 => OpCode::AssignOr,
            45 => OpCode::AssignXor,
            46 => OpCode::AssignShl,
            47 => OpCode::AssignShr,

            48 => OpCode::Inc,
            49 => OpCode::Dec,
            _ => return None,
        })
    }

    #[inline]
    pub const fn as_assign_operator(self) -> Option<Self> {
        Some(match self {
            OpCode::Add => OpCode::AssignAdd,
            OpCode::Sub => OpCode::AssignSub,
            OpCode::Mul => OpCode::AssignMul,
            OpCode::Div => OpCode::AssignDiv,
            OpCode::Mod => OpCode::AssignMod,
            OpCode::Pow => OpCode::AssignPow,
            OpCode::And => OpCode::AssignAnd,
            OpCode::Or => OpCode::AssignOr,
            OpCode::Xor => OpCode::AssignXor,
            OpCode::Shl => OpCode::AssignShl,
            OpCode::Shr => OpCode::AssignShr,
            _ => return None,
        })
    } 
}