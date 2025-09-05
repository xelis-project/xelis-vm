use std::fmt;

use xelis_bytecode::{Access, ChunkReader, ChunkReaderError, Module};

use crate::opcode::OpCodeWithArgs;

pub struct Dump {
    pub chunks: Vec<(Vec<OpCodeWithArgs>, Access)>
}

pub struct Disassembler<'a> {
    module: &'a Module
}

impl<'a> Disassembler<'a> {
    pub fn new(module: &'a Module) -> Self {
        Self {
            module
        }
    }

    pub fn disasemble(&mut self) -> Result<Dump, ChunkReaderError> {
        let mut chunks = Vec::with_capacity(self.module.chunks().len());
        for (chunk, access) in self.module.chunks() {
            let mut opcodes = Vec::new();
            let mut reader = ChunkReader::new(chunk, 0);
            while reader.has_next() {
                let opcode = reader.read_op_code()?;

                let v = OpCodeWithArgs::from_opcode(opcode, &mut reader)?;
                opcodes.push(v);
            }

            chunks.push((opcodes, *access));
        }

        Ok(Dump { chunks })
    }
}

impl fmt::Display for Dump {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let v = self.chunks.iter()
            .enumerate()
            .map(|(i, (opcodes, access))| {
                let formatted = opcodes.iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join("\n");

                format!("#chunk{i} {access}\n{formatted}")
            })
            .collect::<Vec<String>>()
            .join("\n");
        
        write!(f, "{}", v)
    }
}

#[cfg(test)]
mod tests {
    use crate::{Assembler, Disassembler};

    #[test]
    fn test_disassemble() {
        let source = "
            #chunk0 internal
            CONSTANT 0
            COPY
            ADD
        ";

        let assembler = Assembler::new(source);
        let module = assembler.assemble().unwrap();

        let mut disassembler = Disassembler::new(&module);
        let result = disassembler.disasemble().unwrap();

        assert_eq!(result.to_string(), source.trim().replace("  ", ""));
    }
}