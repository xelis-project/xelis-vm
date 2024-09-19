#[derive(Debug)]
pub enum OpCode {
    // load constant
    Constant,
    // load from registers, push in stack
    MemoryLoad,
    // load from stack, load u16, load sub value, push
    // used as array call and struct field access
    SubLoad,
    // pop index, pop array => push array[index]
    // Allow up to u32 index
    ArrayCall,
    // pop, store in registers
    MemorySet,
    // pop value => push value
    Cast,
    // pop value => push !value
    Not,
    // left = right
    Assign,
    // pop args, pop on_value => call function
    // {
    //     args: u8,
    //     on_value: bool,
    //     id: IdentifierType
    // }
    FunctionCall,
    // pop N values => create array
    // {
    //     args: usize
    // }
    NewArray,
    // pop N values => create struct
    // {
    //     args: u8,
    //     id: IdentifierType
    // }
    NewStruct,
    Jump,
    JumpIfFalse,
    // End of the chunk
    End,

    // Operators
    Add,
}

impl OpCode {
    // Convert the OpCode to a byte
    #[inline]
    pub fn as_byte(&self) -> u8 {
        match self {
            Self::Constant => 0,
            Self::MemoryLoad => 1,
            Self::SubLoad => 2,
            Self::ArrayCall => 3,
            Self::MemorySet => 4,
            Self::Cast => 5,
            Self::Not => 6,
            Self::Assign => 7,
            Self::FunctionCall => 8,
            Self::NewArray => 9,
            Self::NewStruct => 10,
            Self::Jump => 11,
            Self::JumpIfFalse => 12,
            Self::End => 13,
            Self::Add => 14,
        }
    }

    // Convert a byte to an OpCode
    #[inline]
    pub fn from_byte(byte: u8) -> Option<OpCode> {
        Some(match byte {
            0 => Self::Constant,
            1 => Self::MemoryLoad,
            2 => Self::SubLoad,
            3 => Self::ArrayCall,
            4 => Self::MemorySet,
            5 => Self::Cast,
            6 => Self::Not,
            7 => Self::Assign,
            8 => Self::FunctionCall,
            9 => Self::NewArray,
            10 => Self::NewStruct,
            11 => Self::Jump,
            12 => Self::JumpIfFalse,
            13 => Self::End,
            14 => Self::Add,
            _ => return None
        })
    }
}