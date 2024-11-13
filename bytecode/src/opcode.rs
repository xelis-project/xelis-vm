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
    IteratorBegin,
    // read u32, iterator next, jump if empty
    IteratorNext,
    // End of an iterator
    IteratorEnd,

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
    // N..Y
    NewRange,
    // pop length, pop N values => create map
    NewMap,

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
    // Convert the OpCode to a usize
    #[inline]
    pub const fn as_usize(&self) -> usize {
        self.as_byte() as usize
    }

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
            OpCode::IteratorBegin => 12,
            OpCode::IteratorNext => 13,
            OpCode::IteratorEnd => 14,
            OpCode::Return => 15,
            OpCode::ArrayCall => 16,
            OpCode::Cast => 17,
            OpCode::InvokeChunk => 18,
            OpCode::SysCall => 19,
            OpCode::NewArray => 20,
            OpCode::NewStruct => 21,
            OpCode::NewRange => 22,
            OpCode::NewMap => 23,

            OpCode::Add => 24,
            OpCode::Sub => 25,
            OpCode::Mul => 26,
            OpCode::Div => 27,
            OpCode::Mod => 28,
            OpCode::Pow => 29,
            OpCode::And => 30,
            OpCode::Or => 31,
            OpCode::Xor => 32,
            OpCode::Shl => 33,
            OpCode::Shr => 34,
            OpCode::Eq => 35,
            OpCode::Neg => 36,
            OpCode::Gt => 37,
            OpCode::Lt => 38,
            OpCode::Gte => 39,
            OpCode::Lte => 40,

            OpCode::Assign => 41,
            OpCode::AssignAdd => 42,
            OpCode::AssignSub => 43,
            OpCode::AssignMul => 44,
            OpCode::AssignDiv => 45,
            OpCode::AssignMod => 46,
            OpCode::AssignPow => 47,
            OpCode::AssignAnd => 48,
            OpCode::AssignOr => 49,
            OpCode::AssignXor => 50,
            OpCode::AssignShl => 51,
            OpCode::AssignShr => 52,

            OpCode::Inc => 53,
            OpCode::Dec => 54,
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
            12 => OpCode::IteratorBegin,
            13 => OpCode::IteratorNext,
            14 => OpCode::IteratorEnd,
            15 => OpCode::Return,
            16 => OpCode::ArrayCall,
            17 => OpCode::Cast,
            18 => OpCode::InvokeChunk,
            19 => OpCode::SysCall,
            20 => OpCode::NewArray,
            21 => OpCode::NewStruct,
            22 => OpCode::NewRange,
            23 => OpCode::NewMap,

            24 => OpCode::Add,
            25 => OpCode::Sub,
            26 => OpCode::Mul,
            27 => OpCode::Div,
            28 => OpCode::Mod,
            29 => OpCode::Pow,
            30 => OpCode::And,
            31 => OpCode::Or,
            32 => OpCode::Xor,
            33 => OpCode::Shl,
            34 => OpCode::Shr,
            35 => OpCode::Eq,
            36 => OpCode::Neg,
            37 => OpCode::Gt,
            38 => OpCode::Lt,
            39 => OpCode::Gte,
            40 => OpCode::Lte,

            41 => OpCode::Assign,
            42 => OpCode::AssignAdd,
            43 => OpCode::AssignSub,
            44 => OpCode::AssignMul,
            45 => OpCode::AssignDiv,
            46 => OpCode::AssignMod,
            47 => OpCode::AssignPow,
            48 => OpCode::AssignAnd,
            49 => OpCode::AssignOr,
            50 => OpCode::AssignXor,
            51 => OpCode::AssignShl,
            52 => OpCode::AssignShr,

            53 => OpCode::Inc,
            54 => OpCode::Dec,
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