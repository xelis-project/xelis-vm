use std::str::FromStr;

use xelis_bytecode::{Chunk, OpCode};

// OpCode with Args
#[derive(Debug)]
pub enum OpCodeWithArgs {
    // load constant at index u16, push in stack
    Constant {
        // Constant id
        index: u16
    },
    // load at index u16 from registers, push in stack
    MemoryLoad {
        // Register index
        register_index: u16
    },
    // pop last value from stack, set in registers[index]
    MemorySet {
        register_index: u16
    },
    // load from stack, load u16, load sub value, push
    // used as array call and struct field access
    SubLoad {
        // Sub index
        index: u8
    },
    // pop value from stack only
    Pop,
    // Pop N values
    PopN {
        // Pop count
        count: u8
    },
    // copy the last value on stack, and push copied value
    Copy,
    // Copy N value in stack
    CopyN {
        stack_index: u8
    },
    // Swap top and N value
    Swap {
        stack_index: u8
    },
    // Swap A and B values
    Swap2 {
        a_stack_index: u8,
        b_stack_index: u8
    },
    // pop value, jump
    Jump {
        // Jump to address
        addr: u32
    },
    // pop value, jump if false
    JumpIfFalse {
        addr: u32
    },
    // pop value, get iterable length, push
    IterableLength,
    IteratorBegin,
    // Push next element of an iterable
    // If no element left, jump to defined address and pop the iterator
    IteratorNext {
        // Jump to address if no next element
        addr: u32
    },
    // Pop the iterator
    IteratorEnd,
    // Return will stop processing the opcodes of a chunk
    Return,

    // pop index, pop array => push array[index]
    // Allow up to u32 index
    ArrayCall {
        // Array index
        index: u32
    },
    // pop value => push value
    Cast {
        primitive_type_id: u8
    },
    // pop args u8 count, on_value bool, chunk id
    InvokeChunk {
        // Function id
        chunk_id: u16,
        // On value
        on_value: bool,
        // Args count
        args_count: u8
    },
    // Same as InvokeChunk, but for system calls
    SysCall {
        // System call id
        sys_call_id: u16,
        // On value
        on_value: bool,
        // Args count
        args_count: u8
    },
    // pop length, pop N values => create array
    NewArray {
        // Array length
        length: u8
    },
    // pop type id, pop N values => create struct
    NewStruct {
        // Struct id
        struct_id: u16
    },
    // N..Y
    // pop start, pop end, push range
    NewRange,
    // pop length, pop N values => create map
    NewMap {
        // Pop N * 2 values from stack and create a map
        length: u8
    },

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
    BitwiseAnd,
    // |
    BitwiseOr,
    // ^
    BitwiseXor,
    // <<
    BitwiseShl,
    // >>
    BitwiseShr,

    // &&
    And,
    // ||
    Or,
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

impl OpCodeWithArgs {
    // Convert the OpCodeWithArgs to an OpCode
    pub fn as_opcode(&self) -> OpCode {
        match self {
            OpCodeWithArgs::Constant { .. } => OpCode::Constant,
            OpCodeWithArgs::MemoryLoad { .. } => OpCode::MemoryLoad,
            OpCodeWithArgs::MemorySet { .. } => OpCode::MemorySet,
            OpCodeWithArgs::SubLoad { .. } => OpCode::SubLoad,
            OpCodeWithArgs::Pop => OpCode::Pop,
            OpCodeWithArgs::PopN { .. } => OpCode::PopN,
            OpCodeWithArgs::Copy => OpCode::Copy,
            OpCodeWithArgs::CopyN { .. } => OpCode::CopyN,
            OpCodeWithArgs::Swap { .. } => OpCode::Swap,
            OpCodeWithArgs::Swap2 { .. } => OpCode::Swap2,
            OpCodeWithArgs::Jump { .. } => OpCode::Jump,
            OpCodeWithArgs::JumpIfFalse { .. } => OpCode::JumpIfFalse,

            OpCodeWithArgs::IterableLength => OpCode::IterableLength,
            OpCodeWithArgs::IteratorBegin => OpCode::IteratorBegin,
            OpCodeWithArgs::IteratorNext { .. } => OpCode::IteratorNext,
            OpCodeWithArgs::IteratorEnd => OpCode::IteratorEnd,

            OpCodeWithArgs::Return => OpCode::Return,
            OpCodeWithArgs::ArrayCall { .. } => OpCode::ArrayCall,
            OpCodeWithArgs::Cast { .. } => OpCode::Cast,
            OpCodeWithArgs::InvokeChunk { .. } => OpCode::InvokeChunk,
            OpCodeWithArgs::SysCall { .. } => OpCode::SysCall,
            OpCodeWithArgs::NewArray { .. } => OpCode::NewArray,
            OpCodeWithArgs::NewStruct { .. } => OpCode::NewStruct,
            OpCodeWithArgs::NewRange => OpCode::NewRange,
            OpCodeWithArgs::NewMap { .. } => OpCode::NewMap,

            OpCodeWithArgs::Add => OpCode::Add,
            OpCodeWithArgs::Sub => OpCode::Sub,
            OpCodeWithArgs::Mul => OpCode::Mul,
            OpCodeWithArgs::Div => OpCode::Div,
            OpCodeWithArgs::Mod => OpCode::Mod,
            OpCodeWithArgs::Pow => OpCode::Pow,

            OpCodeWithArgs::BitwiseAnd => OpCode::BitwiseAnd,
            OpCodeWithArgs::BitwiseOr => OpCode::BitwiseOr,
            OpCodeWithArgs::BitwiseXor => OpCode::BitwiseXor,
            OpCodeWithArgs::BitwiseShl => OpCode::BitwiseShl,
            OpCodeWithArgs::BitwiseShr => OpCode::BitwiseShr,

            OpCodeWithArgs::And => OpCode::And,
            OpCodeWithArgs::Or => OpCode::Or,
            OpCodeWithArgs::Eq => OpCode::Eq,
            OpCodeWithArgs::Neg => OpCode::Neg,
            OpCodeWithArgs::Gt => OpCode::Gt,
            OpCodeWithArgs::Lt => OpCode::Lt,
            OpCodeWithArgs::Gte => OpCode::Gte,
            OpCodeWithArgs::Lte => OpCode::Lte,

            OpCodeWithArgs::Assign => OpCode::Assign,
            OpCodeWithArgs::AssignAdd => OpCode::AssignAdd,
            OpCodeWithArgs::AssignSub => OpCode::AssignSub,
            OpCodeWithArgs::AssignMul => OpCode::AssignMul,
            OpCodeWithArgs::AssignDiv => OpCode::AssignDiv,
            OpCodeWithArgs::AssignMod => OpCode::AssignMod,
            OpCodeWithArgs::AssignPow => OpCode::AssignPow,
            OpCodeWithArgs::AssignBitwiseAnd => OpCode::AssignBitwiseAnd,
            OpCodeWithArgs::AssignBitwiseOr => OpCode::AssignBitwiseOr,
            OpCodeWithArgs::AssignXor => OpCode::AssignBitwiseXor,
            OpCodeWithArgs::AssignShl => OpCode::AssignBitwiseShl,
            OpCodeWithArgs::AssignShr => OpCode::AssignBitwiseShr,
            

            OpCodeWithArgs::Inc => OpCode::Inc,
            OpCodeWithArgs::Dec => OpCode::Dec,
        }
    }

    // Write the OpCodeWithArgs to a chunk
    pub fn write_to_chunk(&self, chunk: &mut Chunk) {
        chunk.emit_opcode(self.as_opcode());
        match self {
            OpCodeWithArgs::Constant { index } => chunk.write_u16(*index),
            OpCodeWithArgs::MemoryLoad { register_index } => chunk.write_u16(*register_index),
            OpCodeWithArgs::MemorySet { register_index } => chunk.write_u16(*register_index),
            OpCodeWithArgs::SubLoad { index } => chunk.write_u8(*index),
            OpCodeWithArgs::PopN { count } => chunk.write_u8(*count),
            OpCodeWithArgs::CopyN { stack_index } => chunk.write_u8(*stack_index),
            OpCodeWithArgs::Swap { stack_index } => chunk.write_u8(*stack_index),
            OpCodeWithArgs::Swap2 { a_stack_index, b_stack_index } => {
                chunk.write_u8(*a_stack_index);
                chunk.write_u8(*b_stack_index);
            },
            OpCodeWithArgs::Jump { addr } => chunk.write_u32(*addr),
            OpCodeWithArgs::JumpIfFalse { addr } => chunk.write_u32(*addr),
            OpCodeWithArgs::ArrayCall { index } => chunk.write_u32(*index),
            OpCodeWithArgs::Cast { primitive_type_id } => chunk.write_u8(*primitive_type_id),
            OpCodeWithArgs::InvokeChunk { chunk_id, on_value, args_count } => {
                chunk.write_u16(*chunk_id);
                chunk.write_bool(*on_value);
                chunk.write_u8(*args_count);
            },
            OpCodeWithArgs::SysCall { sys_call_id, on_value, args_count } => {
                chunk.write_u16(*sys_call_id);
                chunk.write_bool(*on_value);
                chunk.write_u8(*args_count);
            },
            OpCodeWithArgs::IteratorNext { addr } => chunk.write_u32(*addr),
            OpCodeWithArgs::NewArray { length } => chunk.write_u8(*length),
            OpCodeWithArgs::NewStruct { struct_id } => chunk.write_u16(*struct_id),
            OpCodeWithArgs::NewMap { length } => chunk.write_u8(*length),
            _ => {}
        }
    }

    pub fn from_str_with_labels(s: &str, chunks: &[&str], goto: &[(&str, u32)]) -> Result<Self, &'static str> {
        let mut parts = s.split_whitespace();
        let op_code = parts.next().ok_or("Missing OpCode")?;
        let args = parts.collect::<Vec<&str>>();

        Ok(match op_code.to_uppercase().as_str() {
            "CONSTANT" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Constant {
                    index: args[0].parse().map_err(|_| "Invalid index")?
                }
            }
            "MEMORYLOAD" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::MemoryLoad {
                    register_index: args[0].parse().map_err(|_| "Invalid register index")?
                }
            }
            "MEMORYSET" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::MemorySet {
                    register_index: args[0].parse().map_err(|_| "Invalid register index")?
                }
            }
            "SUBLOAD" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::SubLoad {
                    index: args[0].parse().map_err(|_| "Invalid index")?
                }
            }
            "POP" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Pop
            },
            "POPN" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::PopN {
                    count: args[0].parse().map_err(|_| "Invalid count")?
                }
            },
            "COPY" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Copy
            }
            "COPYN" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::CopyN {
                    stack_index: args[0].parse().map_err(|_| "Invalid stack index")?
                }
            }
            "SWAP" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Swap {
                    stack_index: args[0].parse().map_err(|_| "Invalid stack index")?
                }
            }
            "SWAP2" => {
                if args.len() != 2 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Swap2 {
                    a_stack_index: args[0].parse().map_err(|_| "Invalid stack index")?,
                    b_stack_index: args[1].parse().map_err(|_| "Invalid stack index")?
                }
            },
            "JUMP" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                let addr_arg = args[0];
                let addr = if addr_arg.starts_with(":") {
                    let label = &addr_arg[1..];
                    goto.iter().find(|(l, _)| *l == label).map(|(_, a)| *a).ok_or("Invalid label")?
                } else {
                    addr_arg.parse().map_err(|_| "Invalid address")?
                };

                OpCodeWithArgs::Jump {
                    addr
                }
            },
            "JUMPIFFALSE" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                let addr_arg = args[0];
                let addr = if addr_arg.starts_with(":") {
                    let label = &addr_arg[1..];
                    goto.iter().find(|(l, _)| *l == label).map(|(_, a)| *a).ok_or("Invalid label")?
                } else {
                    addr_arg.parse().map_err(|_| "Invalid address")?
                };

                OpCodeWithArgs::JumpIfFalse {
                    addr
                }
            },
            "ITERABLELENGTH" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::IterableLength
            },
            "ITERATORBEGIN" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::IteratorBegin
            },
            "ITERATORNEXT" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                let addr_arg = args[0];
                let addr = if addr_arg.starts_with(":") {
                    let label = &addr_arg[1..];
                    goto.iter().find(|(l, _)| *l == label).map(|(_, a)| *a).ok_or("Invalid label")?
                } else {
                    addr_arg.parse().map_err(|_| "Invalid address")?
                };

                OpCodeWithArgs::IteratorNext {
                    addr
                }
            },
            "ITERATOREND" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::IteratorEnd
            },
            "RETURN" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Return
            },
            "ARRAYCALL" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::ArrayCall {
                    index: args[0].parse().map_err(|_| "Invalid index")?
                }
            },
            "CAST" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Cast {
                    primitive_type_id: args[0].parse().map_err(|_| "Invalid type id")?
                }
            },
            "INVOKECHUNK" => {
                if args.len() != 3 {
                    return Err("Invalid args count");
                }

                let arg_chunk = args[0];
                let chunk_id = if arg_chunk.starts_with("#") {
                    let label = &arg_chunk[1..];
                    chunks.iter().position(|&c| c == label).ok_or("Invalid chunk label")? as u16
                } else {
                    arg_chunk.parse().map_err(|_| "Invalid chunk id")?
                };
                
                OpCodeWithArgs::InvokeChunk {
                    chunk_id,
                    on_value: args[1].parse().map_err(|_| "Invalid on value bool")?,
                    args_count: args[2].parse().map_err(|_| "Invalid args count")?
                }
            },
            "SYSCALL" => {
                if args.len() != 3 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::SysCall {
                    sys_call_id: args[0].parse().map_err(|_| "Invalid sys call id")?,
                    on_value: args[1].parse().map_err(|_| "Invalid on value bool")?,
                    args_count: args[2].parse().map_err(|_| "Invalid args count")?
                }
            }
            "NEWARRAY" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::NewArray {
                    length: args[0].parse().map_err(|_| "Invalid length")?
                }
            },
            "NEWSTRUCT" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::NewStruct {
                    struct_id: args[0].parse().map_err(|_| "Invalid struct id")?
                }
            },
            "NEWRANGE" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::NewRange
            },
            "NEWMAP" => {
                if args.len() != 1 {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::NewMap {
                    length: args[0].parse().map_err(|_| "Invalid length")?
                }
            },
            "ADD" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Add
            },
            "SUB" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Sub
            },
            "MUL" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Mul
            },
            "DIV" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Div
            },
            "MOD" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Mod
            },
            "POW" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Pow
            },
            "BITWISE_AND" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::BitwiseAnd
            },
            "BITWISE_OR" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::BitwiseOr
            },
            "BITWISE_XOR" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::BitwiseXor
            },
            "BITWISE_SHL" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::BitwiseShl
            },
            "BITWISE_SHR" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::BitwiseShr
            },
            "AND" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::And
            },
            "OR" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Or
            },
            "EQ" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Eq
            },
            "NEG" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Neg
            },
            "GT" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Gt
            },
            "LT" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Lt
            },
            "GTE" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Gte
            },
            "LTE" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Lte
            },
            "ASSIGN" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Assign
            },
            "ASSIGNADD" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::AssignAdd
            },
            "ASSIGNSUB" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::AssignSub
            },
            "ASSIGNMUL" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::AssignMul
            },
            "ASSIGNDIV" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::AssignDiv
            },
            "ASSIGNMOD" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::AssignMod
            },
            "ASSIGNPOW" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::AssignPow
            },
            "ASSIGNAND" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::AssignBitwiseAnd
            },
            "ASSIGNOR" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::AssignBitwiseOr
            },
            "ASSIGNXOR" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::AssignXor
            },
            "ASSIGNSHL" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::AssignShl
            },
            "ASSIGNSHR" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::AssignShr
            },
            "INC" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Inc
            },
            "DEC" => {
                if !args.is_empty() {
                    return Err("Invalid args count");
                }

                OpCodeWithArgs::Dec
            },
            _ => return Err("Invalid OpCode")
        })
    }
}

impl FromStr for OpCodeWithArgs {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        OpCodeWithArgs::from_str_with_labels(s, &[], &[])
    }
}