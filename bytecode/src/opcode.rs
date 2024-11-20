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
    // Pop N values
    PopN,
    // push copied value
    Copy,
    // Copy value at N
    CopyN,

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
    // pop length, pop N values => create array with N values
    NewArray,
    // pop type id, pop N values => create struct
    NewStruct,
    // N..Y
    NewRange,
    // pop length, pop N entries (N * (key + value)) => create map with N entries
    NewMap,
    // read enum id u16, variant id u8, pop N values => create enum
    NewEnum,

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
            OpCode::PopN => 5,
            OpCode::Copy => 6,
            OpCode::CopyN => 7,
            OpCode::Swap => 8,
            OpCode::Swap2 => 9,
            OpCode::Jump => 10,
            OpCode::JumpIfFalse => 11,
            OpCode::IterableLength => 12,
            OpCode::IteratorBegin => 13,
            OpCode::IteratorNext => 14,
            OpCode::IteratorEnd => 15,
            OpCode::Return => 16,
            OpCode::ArrayCall => 17,
            OpCode::Cast => 18,
            OpCode::InvokeChunk => 19,
            OpCode::SysCall => 20,

            OpCode::NewArray => 21,
            OpCode::NewStruct => 22,
            OpCode::NewRange => 23,
            OpCode::NewMap => 24,
            OpCode::NewEnum => 25,

            OpCode::Add => 26,
            OpCode::Sub => 27,
            OpCode::Mul => 28,
            OpCode::Div => 29,
            OpCode::Mod => 30,
            OpCode::Pow => 31,
            OpCode::And => 32,
            OpCode::Or => 33,
            OpCode::Xor => 34,
            OpCode::Shl => 35,
            OpCode::Shr => 36,
            OpCode::Eq => 37,
            OpCode::Neg => 38,
            OpCode::Gt => 39,
            OpCode::Lt => 40,
            OpCode::Gte => 41,
            OpCode::Lte => 42,

            OpCode::Assign => 43,
            OpCode::AssignAdd => 44,
            OpCode::AssignSub => 45,
            OpCode::AssignMul => 46,
            OpCode::AssignDiv => 47,
            OpCode::AssignMod => 48,
            OpCode::AssignPow => 49,
            OpCode::AssignAnd => 50,
            OpCode::AssignOr => 51,
            OpCode::AssignXor => 52,
            OpCode::AssignShl => 53,
            OpCode::AssignShr => 54,

            OpCode::Inc => 55,
            OpCode::Dec => 56,
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
            5 => OpCode::PopN,
            6 => OpCode::Copy,
            7 => OpCode::CopyN,
            8 => OpCode::Swap,
            9 => OpCode::Swap2,
            10 => OpCode::Jump,
            11 => OpCode::JumpIfFalse,
            12 => OpCode::IterableLength,
            13 => OpCode::IteratorBegin,
            14 => OpCode::IteratorNext,
            15 => OpCode::IteratorEnd,
            16 => OpCode::Return,
            17 => OpCode::ArrayCall,
            18 => OpCode::Cast,
            19 => OpCode::InvokeChunk,
            20 => OpCode::SysCall,

            21 => OpCode::NewArray,
            22 => OpCode::NewStruct,
            23 => OpCode::NewRange,
            24 => OpCode::NewMap,
            25 => OpCode::NewEnum,

            26 => OpCode::Add,
            27 => OpCode::Sub,
            28 => OpCode::Mul,
            29 => OpCode::Div,
            30 => OpCode::Mod,
            31 => OpCode::Pow,
            32 => OpCode::And,
            33 => OpCode::Or,
            34 => OpCode::Xor,
            35 => OpCode::Shl,
            36 => OpCode::Shr,
            37 => OpCode::Eq,
            38 => OpCode::Neg,
            39 => OpCode::Gt,
            40 => OpCode::Lt,
            41 => OpCode::Gte,
            42 => OpCode::Lte,
            43 => OpCode::Assign,

            44 => OpCode::AssignAdd,
            45 => OpCode::AssignSub,
            46 => OpCode::AssignMul,
            47 => OpCode::AssignDiv,
            48 => OpCode::AssignMod,
            49 => OpCode::AssignPow,
            50 => OpCode::AssignAnd,
            51 => OpCode::AssignOr,
            52 => OpCode::AssignXor,
            53 => OpCode::AssignShl,
            54 => OpCode::AssignShr,

            55 => OpCode::Inc,
            56 => OpCode::Dec,
            _ => return None,
        })
    }

    // Convert an OpCode to an assign operator
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

    // Get how many arguments bytes (overhead) the OpCode takes
    #[inline]
    pub fn arguments_bytes(&self) -> usize {
        match self {
            OpCode::Constant => 2, // u16 id
            OpCode::MemoryLoad => 2, // u16 id
            OpCode::MemorySet => 2, // u16 id
            OpCode::SubLoad => 2, // u16 id

            OpCode::PopN => 1, // u8 count
            OpCode::CopyN => 1, // u8 index
            OpCode::Swap => 1, // u8 index
            OpCode::Swap2 => 2, // u8 index, u8 index

            OpCode::Jump => 4, // u32 addr
            OpCode::JumpIfFalse => 4, // u32 addr
            OpCode::IteratorNext => 4, // u32 addr
            OpCode::ArrayCall => 4, // index u32
            OpCode::Cast => 1, // primitive type id u8
            OpCode::InvokeChunk => 4, // id u16, on_value bool, args u8
            OpCode::SysCall => 4, // id u16, on_value bool, args u8

            OpCode::NewArray => 1, // u8 initial values
            OpCode::NewStruct => 2, // struct type id u16
            OpCode::NewRange => 0,
            OpCode::NewMap => 1, // u8 initial values

            _ => 0,
        }
    }
}