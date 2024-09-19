pub enum OpCode {
    // load constant
    Constant,
    // load from stack, push
    Load,
    // load from stack, load u16, load sub value, push
    // used as array call and struct field access
    LoadSub,
    // pop index, pop array => push array[index]
    // Allow up to u32 index
    ArrayCall,
    // pop, store in stack
    Register,
    // pop value => push value
    Cast,
    // pop value => push !value
    Not,
    // left = right
    Assign,
    // pop both, execute op, push
    Op,
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
    //     args: usize,
    //     id: IdentifierType
    // }
    NewStruct,
    Jump,
    JumpIfFalse,
}

impl OpCode {
    pub fn as_byte(&self) -> u8 {
        match self {
            Self::Constant => 0,
            Self::Load => 1,
            Self::LoadSub => 2,
            Self::ArrayCall => 3,
            Self::Register => 4,
            Self::Cast => 5,
            Self::Not => 6,
            Self::Assign => 7,
            Self::Op => 8,
            Self::FunctionCall => 9,
            Self::NewArray => 10,
            Self::NewStruct => 11,
            Self::Jump => 12,
            Self::JumpIfFalse => 13,
        }
    }

    pub fn from_byte(byte: u8) -> Option<OpCode> {
        Some(match byte {
            0 => Self::Constant,
            1 => Self::Load,
            2 => Self::LoadSub,
            3 => Self::ArrayCall,
            4 => Self::Register,
            5 => Self::Cast,
            6 => Self::Not,
            7 => Self::Assign,
            8 => Self::Op,
            9 => Self::FunctionCall,
            10 => Self::NewArray,
            11 => Self::NewStruct,
            12 => Self::Jump,
            13 => Self::JumpIfFalse,
            _ => return None
        })
    }
}