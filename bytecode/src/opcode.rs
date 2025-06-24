#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    // load constant
    Constant,
    // load from registers, push in stack
    MemoryLoad,
    // pop, set in registers[index]
    MemorySet,
    // pop from registers and push in stack
    // If there is no 
    MemoryPop,
    // Return u16 memory len
    // u32 is returned because memory/pointers can't be bigger than that
    MemoryLen,
    // Transform the stored value at registers[index]
    // into an owned value (similar to Copy)
    MemoryToOwned,
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
    // Make the top stack value an owned variant
    // Parameters passed through chunks may be pointers
    // and people may want to NOT modify "upper" value
    ToOwned,

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
    // pop length, pop N values => create object with N values
    NewObject,
    // N..Y
    NewRange,
    // pop length, pop N entries (N * (key + value)) => create map with N entries
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
    // Take the top object/array
    // and push its values
    // into the stack
    Flatten,
    // used for an optimized matching
    // where we don't have to copy the value
    // and provide a general usage for matching
    // Params:
    // magic_byte: variant id > 0
    // address: u32
    // If first_byte match is true, it will pop it and flatten
    // the object itself
    Match,
    // Similar to invoke chunk / syscall
    // chunk id is taken from stack
    DynamicCall,
}

impl OpCode {
    // Convert the OpCode to a usize
    #[inline(always)]
    pub const fn as_usize(&self) -> usize {
        *self as usize
    }

    // Convert the OpCode to a byte
    #[inline(always)]
    pub const fn as_byte(&self) -> u8 {
        *self as u8
    }

    // Convert a byte to an OpCode
    #[inline]
    pub const fn from_byte(byte: u8) -> Option<OpCode> {
        Some(match byte {
            0 => OpCode::Constant,

            1 => OpCode::MemoryLoad,
            2 => OpCode::MemorySet,
            3 => OpCode::MemoryPop,
            4 => OpCode::MemoryPop,
            5 => OpCode::MemoryToOwned,

            6 => OpCode::SubLoad,
            7 => OpCode::Pop,
            8 => OpCode::PopN,
            9 => OpCode::Copy,
            10 => OpCode::CopyN,
            11 => OpCode::ToOwned,

            12 => OpCode::Swap,
            13 => OpCode::Swap2,
            14 => OpCode::Jump,
            15 => OpCode::JumpIfFalse,

            16 => OpCode::IterableLength,
            17 => OpCode::IteratorBegin,
            18 => OpCode::IteratorNext,
            19 => OpCode::IteratorEnd,

            20 => OpCode::Return,
            21 => OpCode::ArrayCall,
            22 => OpCode::Cast,
            23 => OpCode::InvokeChunk,
            24 => OpCode::SysCall,

            25 => OpCode::NewObject,
            26 => OpCode::NewRange,
            27 => OpCode::NewMap,

            28 => OpCode::Add,
            29 => OpCode::Sub,
            30 => OpCode::Mul,
            31 => OpCode::Div,
            32 => OpCode::Mod,
            33 => OpCode::Pow,

            34 => OpCode::BitwiseAnd,
            35 => OpCode::BitwiseOr,
            36 => OpCode::BitwiseXor,
            37 => OpCode::BitwiseShl,
            38 => OpCode::BitwiseShr,

            39 => OpCode::And,
            40 => OpCode::Or,
            41 => OpCode::Eq,
            42 => OpCode::Neg,
            43 => OpCode::Gt,
            44 => OpCode::Lt,
            45 => OpCode::Gte,
            46 => OpCode::Lte,

            47 => OpCode::Assign,
            48 => OpCode::AssignAdd,
            49 => OpCode::AssignSub,
            50 => OpCode::AssignMul,
            51 => OpCode::AssignDiv,
            52 => OpCode::AssignMod,
            53 => OpCode::AssignPow,

            54 => OpCode::AssignBitwiseAnd,
            55 => OpCode::AssignBitwiseOr,
            56 => OpCode::AssignBitwiseXor,
            57 => OpCode::AssignBitwiseShl,
            58 => OpCode::AssignBitwiseShr,

            59 => OpCode::Inc,
            60 => OpCode::Dec,
            61 => OpCode::Flatten,
            62 => OpCode::Match,
            63 => OpCode::DynamicCall,
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
            OpCode::MemoryToOwned => 2, // u16 id
            OpCode::SubLoad => 1, // u8 id

            OpCode::PopN => 1, // u8 count
            OpCode::CopyN => 1, // u8 index
            OpCode::Swap => 1, // u8 index
            OpCode::Swap2 => 2, // u8 index, u8 index

            OpCode::Jump => 4, // u32 addr
            OpCode::JumpIfFalse => 4, // u32 addr
            OpCode::IteratorNext => 4, // u32 addr
            OpCode::Cast => 1, // primitive type id u8
            OpCode::InvokeChunk => 3, // id u16, args u8
            OpCode::SysCall => 2, // id u16

            OpCode::NewObject => 1, // u8 initial values
            OpCode::NewMap => 1, // u8 initial values
            OpCode::Match => 5, // magic byte + u32 addr
            OpCode::DynamicCall => 1, // params len

            _ => 0,
        }
    }
}