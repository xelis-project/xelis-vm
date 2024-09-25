#[derive(Debug)]
pub enum OpCode {
    // load constant
    Constant,
    // load from registers, push in stack
    MemoryLoad,
    // pop, store in registers[index]
    MemoryStore,
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
            OpCode::MemoryStore => 2,
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
            OpCode::InvokeChunk => 16,
            OpCode::SysCall => 17,
            OpCode::NewArray => 18,
            OpCode::NewStruct => 19,

            // Operators
            OpCode::Add => 20,
            OpCode::Sub => 21,
            OpCode::Mul => 22,
            OpCode::Div => 23,
            OpCode::Mod => 24,
            OpCode::Pow => 25,
            OpCode::And => 26,
            OpCode::Or => 27,
            OpCode::Xor => 28,
            OpCode::Shl => 29,
            OpCode::Shr => 30,
            OpCode::Eq => 31,
            OpCode::Neg => 32,
            OpCode::Gt => 33,
            OpCode::Lt => 34,
            OpCode::Gte => 35,
            OpCode::Lte => 36,

            // Assign Operators
            OpCode::Assign => 37,
            OpCode::AssignAdd => 38,
            OpCode::AssignSub => 39,
            OpCode::AssignMul => 40,
            OpCode::AssignDiv => 41,
            OpCode::AssignMod => 42,
            OpCode::AssignPow => 43,
            OpCode::AssignAnd => 44,
            OpCode::AssignOr => 45,
            OpCode::AssignXor => 46,
            OpCode::AssignShl => 47,
            OpCode::AssignShr => 48,

            // Extra
            OpCode::Inc => 49,
            OpCode::Dec => 50,
        }
    }

    // Convert a byte to an OpCode
    #[inline]
    pub const fn from_byte(byte: u8) -> Option<OpCode> {
        Some(match byte {
            0 => OpCode::Constant,
            1 => OpCode::MemoryLoad,
            2 => OpCode::MemoryStore,
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
            16 => OpCode::InvokeChunk,
            17 => OpCode::SysCall,
            18 => OpCode::NewArray,
            19 => OpCode::NewStruct,

            20 => OpCode::Add,
            21 => OpCode::Sub,
            22 => OpCode::Mul,
            23 => OpCode::Div,
            24 => OpCode::Mod,
            25 => OpCode::Pow,
            26 => OpCode::And,
            27 => OpCode::Or,
            28 => OpCode::Xor,
            29 => OpCode::Shl,
            30 => OpCode::Shr,
            31 => OpCode::Eq,
            32 => OpCode::Neg,
            33 => OpCode::Gt,
            34 => OpCode::Lt,
            35 => OpCode::Gte,
            36 => OpCode::Lte,

            37 => OpCode::Assign,
            38 => OpCode::AssignAdd,
            39 => OpCode::AssignSub,
            40 => OpCode::AssignMul,
            41 => OpCode::AssignDiv,
            42 => OpCode::AssignMod,
            43 => OpCode::AssignPow,
            44 => OpCode::AssignAnd,
            45 => OpCode::AssignOr,
            46 => OpCode::AssignXor,
            47 => OpCode::AssignShl,
            48 => OpCode::AssignShr,

            49 => OpCode::Inc,
            50 => OpCode::Dec,

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