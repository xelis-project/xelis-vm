//! Follow a potential call result until an instruction reads it.
//!
//! The compiler does not count standalone voidable calls on its expression
//! stack. Bytecode has no standalone flag, so the next opcode alone cannot
//! distinguish them from calls used in nested arguments or conditional values.

use crate::{
    instruction::{Instruction, StackEffect},
    DecompilerError, Engine, State,
};
use silex_bytecode::OpCode;
use silex_types::Type;
use std::collections::{BTreeMap, BTreeSet};

fn result_is_used(
    code: &[Instruction],
    chunk: usize,
    start: usize,
    returns_value: bool,
    mut call_effect: impl FnMut(usize) -> Result<StackEffect, DecompilerError>,
) -> Result<bool, DecompilerError> {
    // Only the number of values above the candidate matters. Reading/copying
    // it already proves use; there is no need to track copies of the candidate.
    let mut work = vec![(start + 1, 0usize)];
    let mut seen = BTreeSet::new();
    let target = |offset| {
        code.binary_search_by_key(&offset, |i| i.offset)
            .unwrap_or(code.len())
    };
    while let Some((pc, mut above)) = work.pop() {
        let Some(instruction) = code.get(pc) else {
            continue;
        };
        if !seen.insert((pc, above)) {
            continue;
        }
        if seen.len() > 4096 {
            return Err(DecompilerError::new(
                chunk,
                instruction.offset,
                "call result analysis exceeds 4096 states",
            ));
        }
        let effect = match instruction.op {
            OpCode::Return => {
                if returns_value && above == 0 {
                    return Ok(true);
                }
                // A return terminates this path, not the other queued branches.
                continue;
            }
            OpCode::Jump => {
                work.push((target(instruction.arg), above));
                continue;
            }
            OpCode::JumpIfFalse => {
                if above == 0 {
                    return Ok(true);
                }
                work.push((target(instruction.arg), above - 1));
                StackEffect::new(1, 0)
            }
            OpCode::IteratorNext => {
                // Exhaustion jumps without pushing an item.
                work.push((target(instruction.arg), above));
                StackEffect::new(0, 1)
            }
            OpCode::Copy => {
                if above == 0 {
                    return Ok(true);
                }
                StackEffect::new(0, 1)
            }
            OpCode::Swap | OpCode::Swap2 => {
                let a = if matches!(instruction.op, OpCode::Swap) {
                    0
                } else {
                    instruction.extra
                };
                let b = instruction.arg;
                if above == a {
                    above = b
                } else if above == b {
                    above = a
                }
                StackEffect::new(0, 0)
            }
            OpCode::Flatten if above == 0 => return Ok(true),
            OpCode::SysCall | OpCode::InvokeChunk | OpCode::DynamicCall => call_effect(pc)?,
            _ => instruction.stack_effect().ok_or_else(|| {
                DecompilerError::new(
                    chunk,
                    instruction.offset,
                    format!(
                        "cannot determine call result use across {:?}",
                        instruction.op
                    ),
                )
            })?,
        };
        if above < effect.inputs {
            return Ok(true);
        }
        work.push((pc + 1, above - effect.inputs + effect.outputs));
    }
    Ok(false)
}

impl<M> Engine<'_, '_, '_, M> {
    pub(super) fn result_is_used(
        &self,
        instruction: usize,
        state: &State,
    ) -> Result<bool, DecompilerError> {
        self.result_is_used_inner(instruction, state, &mut BTreeMap::new(), 0)
    }

    fn result_is_used_inner(
        &self,
        instruction: usize,
        state: &State,
        cache: &mut BTreeMap<usize, Option<bool>>,
        depth: usize,
    ) -> Result<bool, DecompilerError> {
        if let Some(used) = cache.get(&instruction) {
            // Re-entering a call being analyzed reaches another loop iteration
            // without reading its previous result. Compiler expression values
            // cannot survive that iteration boundary; stored locals can.
            return Ok(used.unwrap_or(false));
        }
        if depth > 128 {
            return Err(self.error(
                self.code[instruction].offset,
                "nested call result analysis exceeds 128",
            ));
        }
        cache.insert(instruction, None);
        let used = result_is_used(
            self.code,
            self.id,
            instruction,
            self.signatures[self.id].return_type.is_some(),
            |pc| {
                let (inputs, return_type) = self.call_signature(pc, state)?;
                let returns = match return_type {
                    Some(Type::Voidable(_)) => {
                        self.result_is_used_inner(pc, state, cache, depth + 1)?
                    }
                    Some(_) => true,
                    None => false,
                };
                Ok(StackEffect::new(inputs, usize::from(returns)))
            },
        )?;
        cache.insert(instruction, Some(used));
        Ok(used)
    }

    fn call_signature<'s>(
        &'s self,
        pc: usize,
        state: &'s State,
    ) -> Result<(usize, Option<&'s Type>), DecompilerError> {
        let ins = &self.code[pc];
        Ok(match ins.op {
            OpCode::SysCall => {
                let f = self
                    .environment
                    .get_functions_mapper()
                    .get_function(&(ins.arg as u16))
                    .ok_or_else(|| self.error(ins.offset, "unknown syscall ID"))?;
                (
                    f.parameters.len() + usize::from(f.require_instance),
                    f.return_type.as_ref(),
                )
            }
            OpCode::InvokeChunk => {
                let signature = self
                    .signatures
                    .get(ins.arg)
                    .ok_or_else(|| self.error(ins.offset, "unknown chunk ID"))?;
                (ins.extra, signature.return_type.as_ref())
            }
            OpCode::DynamicCall => {
                let pointer = pc
                    .checked_sub(1)
                    .and_then(|i| self.code.get(i))
                    .filter(|i| matches!(i.op, OpCode::MemoryLoad))
                    .and_then(|i| state.registers.get(&i.arg))
                    .ok_or_else(|| {
                        self.error(
                            ins.offset,
                            "dynamic call signature is erased; supply a function signature",
                        )
                    })?;
                let ty = pointer
                    .parameter
                    .map(|p| &self.signatures[self.id].parameters[p])
                    .unwrap_or(&pointer.ty);
                let return_type = match ty {
                    Type::Function(f) => f.return_type(),
                    Type::Closure(f) => f.return_type(),
                    _ => {
                        return Err(self.error(
                            ins.offset,
                            "dynamic call signature is erased; supply a function signature",
                        ))
                    }
                };
                (ins.arg + 1, return_type)
            }
            _ => unreachable!("analysis only resolves call instructions"),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use OpCode::*;

    fn used(instructions: &[(OpCode, usize)]) -> Result<bool, DecompilerError> {
        let code = instructions
            .iter()
            .enumerate()
            .map(|(offset, (op, arg))| Instruction {
                offset,
                op: *op,
                arg: *arg,
                extra: 0,
            })
            .collect::<Vec<_>>();
        result_is_used(&code, 0, 0, true, |_| panic!("unexpected call"))
    }

    #[test]
    fn a_return_does_not_hide_a_use_on_another_branch() {
        assert!(used(&[
            (SysCall, 0),
            (Constant, 0),
            (JumpIfFalse, 5),
            (Constant, 0),
            (Return, 0),
            (Pop, 0),
            (Return, 0),
        ])
        .unwrap());
    }

    #[test]
    fn iterator_exhaustion_does_not_push_an_item() {
        assert!(used(&[
            (SysCall, 0),
            (IteratorNext, 4),
            (Constant, 0),
            (Return, 0),
            (Pop, 0),
            (Return, 0),
        ])
        .unwrap());
    }

    #[test]
    fn register_ownership_does_not_touch_the_expression_stack() {
        assert!(used(&[(SysCall, 0), (MemoryToOwned, 0), (MemorySet, 1)]).unwrap());
        assert!(!used(&[(SysCall, 0), (MemoryToOwned, 0), (Constant, 0), (Return, 0)]).unwrap());
    }

    #[test]
    fn reading_or_dropping_the_top_value_proves_use() {
        for op in [Copy, ToOwned, Inc, Dec, Pop, Flatten] {
            assert!(used(&[(SysCall, 0), (op, 0)]).unwrap(), "{op:?}");
        }
        assert!(used(&[(SysCall, 0), (Constant, 0), (PopN, 2)]).unwrap());
    }

    #[test]
    fn unknown_arity_and_analysis_limits_are_errors() {
        assert!(used(&[(SysCall, 0), (Constant, 0), (Flatten, 0)]).is_err());
        assert!(used(&[(SysCall, 0), (Constant, 0), (Jump, 1)])
            .unwrap_err()
            .message
            .contains("4096 states"));
        assert!(!used(&[(SysCall, 0), (Jump, 1)]).unwrap());
    }
}
