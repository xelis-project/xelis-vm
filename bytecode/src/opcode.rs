#[derive(Debug)]
pub enum OpCode {
    // load constant
    Constant,
    // load from registers, push in stack
    MemoryLoad,
    // pop, set in registers[index]
    MemorySet,
    // load from stack, read u8, load sub value, push
    // types are limited to max u8 sub values
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
    // &&
    And,
    // ||
    Or,
    // &
    BitwiseAnd,
    // |
    BitwiseOr,
    // ^
    BitwiseXor,
    // <<
    BitwiseShl,
    // >>
    BitwiseShr,
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
    AssignBitwiseAnd,
    // |=
    AssignBitwiseOr,
    // ^=
    AssignBitwiseXor,
    // <<=
    AssignBitwiseShl,
    // >>=
    AssignBitwiseShr,

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
            
            OpCode::BitwiseAnd => 32,
            OpCode::BitwiseOr => 33,
            OpCode::BitwiseXor => 34,
            OpCode::BitwiseShl => 35,
            OpCode::BitwiseShr => 36,
            
            OpCode::And => 37,
            OpCode::Or => 38,
            OpCode::Eq => 39,
            OpCode::Neg => 40,
            OpCode::Gt => 41,
            OpCode::Lt => 42,
            OpCode::Gte => 43,
            OpCode::Lte => 44,

            OpCode::Assign => 45,
            OpCode::AssignAdd => 46,
            OpCode::AssignSub => 47,
            OpCode::AssignMul => 48,
            OpCode::AssignDiv => 49,
            OpCode::AssignMod => 50,
            OpCode::AssignPow => 51,

            OpCode::AssignBitwiseAnd => 52,
            OpCode::AssignBitwiseOr => 53,
            OpCode::AssignBitwiseXor => 54,
            OpCode::AssignBitwiseShl => 55,
            OpCode::AssignBitwiseShr => 56,

            OpCode::Inc => 57,
            OpCode::Dec => 58,
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

            32 => OpCode::BitwiseAnd,
            33 => OpCode::BitwiseOr,
            34 => OpCode::BitwiseXor,
            35 => OpCode::BitwiseShl,
            36 => OpCode::BitwiseShr,

            37 => OpCode::And,
            38 => OpCode::Or,
            39 => OpCode::Eq,
            40 => OpCode::Neg,
            41 => OpCode::Gt,
            42 => OpCode::Lt,
            43 => OpCode::Gte,
            44 => OpCode::Lte,

            45 => OpCode::Assign,
            46 => OpCode::AssignAdd,
            47 => OpCode::AssignSub,
            48 => OpCode::AssignMul,
            49 => OpCode::AssignDiv,
            50 => OpCode::AssignMod,
            51 => OpCode::AssignPow,

            52 => OpCode::AssignBitwiseAnd,
            53 => OpCode::AssignBitwiseOr,
            54 => OpCode::AssignBitwiseXor,
            55 => OpCode::AssignBitwiseShl,
            56 => OpCode::AssignBitwiseShr,

            57 => OpCode::Inc,
            58 => OpCode::Dec,
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

            OpCode::BitwiseAnd => OpCode::AssignBitwiseAnd,
            OpCode::BitwiseOr => OpCode::AssignBitwiseOr,
            OpCode::BitwiseXor => OpCode::AssignBitwiseXor,
            OpCode::BitwiseShl => OpCode::AssignBitwiseShl,
            OpCode::BitwiseShr => OpCode::AssignBitwiseShr,
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
            OpCode::SubLoad => 1, // u8 id

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