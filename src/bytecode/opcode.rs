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
    // No need to create an iterator
    IterableLength,
    // Prepare an iterator
    IterableBegin,
    // read u32, iterator next, jump if empty
    IterableNext,

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
            OpCode::IterableBegin => 12,
            OpCode::IterableNext => 13,
            OpCode::Return => 14,
            OpCode::ArrayCall => 15,
            OpCode::Cast => 16,
            OpCode::InvokeChunk => 17,
            OpCode::SysCall => 18,
            OpCode::NewArray => 19,
            OpCode::NewStruct => 20,

            OpCode::Add => 21,
            OpCode::Sub => 22,
            OpCode::Mul => 23,
            OpCode::Div => 24,
            OpCode::Mod => 25,
            OpCode::Pow => 26,
            OpCode::And => 27,
            OpCode::Or => 28,
            OpCode::Xor => 29,
            OpCode::Shl => 30,
            OpCode::Shr => 31,
            OpCode::Eq => 32,
            OpCode::Neg => 33,
            OpCode::Gt => 34,
            OpCode::Lt => 35,
            OpCode::Gte => 36,
            OpCode::Lte => 37,

            OpCode::Assign => 38,
            OpCode::AssignAdd => 39,
            OpCode::AssignSub => 40,
            OpCode::AssignMul => 41,
            OpCode::AssignDiv => 42,
            OpCode::AssignMod => 43,
            OpCode::AssignPow => 44,
            OpCode::AssignAnd => 45,
            OpCode::AssignOr => 46,
            OpCode::AssignXor => 47,
            OpCode::AssignShl => 48,
            OpCode::AssignShr => 49,

            OpCode::Inc => 50,
            OpCode::Dec => 51,
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
            12 => OpCode::IterableBegin,
            13 => OpCode::IterableNext,
            14 => OpCode::Return,
            15 => OpCode::ArrayCall,
            16 => OpCode::Cast,
            17 => OpCode::InvokeChunk,
            18 => OpCode::SysCall,
            19 => OpCode::NewArray,
            20 => OpCode::NewStruct,

            21 => OpCode::Add,
            22 => OpCode::Sub,
            23 => OpCode::Mul,
            24 => OpCode::Div,
            25 => OpCode::Mod,
            26 => OpCode::Pow,
            27 => OpCode::And,
            28 => OpCode::Or,
            29 => OpCode::Xor,
            30 => OpCode::Shl,
            31 => OpCode::Shr,
            32 => OpCode::Eq,
            33 => OpCode::Neg,
            34 => OpCode::Gt,
            35 => OpCode::Lt,
            36 => OpCode::Gte,
            37 => OpCode::Lte,

            38 => OpCode::Assign,
            39 => OpCode::AssignAdd,
            40 => OpCode::AssignSub,
            41 => OpCode::AssignMul,
            42 => OpCode::AssignDiv,
            43 => OpCode::AssignMod,
            44 => OpCode::AssignPow,
            45 => OpCode::AssignAnd,
            46 => OpCode::AssignOr,
            47 => OpCode::AssignXor,
            48 => OpCode::AssignShl,
            49 => OpCode::AssignShr,

            50 => OpCode::Inc,
            51 => OpCode::Dec,
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