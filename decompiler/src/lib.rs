//! Best-effort decompilation of a [`Module`] to Silex source.
//!
//! Supply the same [`EnvironmentBuilder`] used to compile the contract: syscall
//! IDs are positional and the runtime environment alone does not retain names.
//! Local names are synthesized. Erased types can be supplied with
//! [`Decompiler::with_function_signature`]. Unsupported instructions and
//! unrecoverable patterns return errors. See the crate README for limits.

mod analysis;
mod error;
mod instruction;
mod value;

pub use error::DecompilerError;
use instruction::{decode, Instruction};
use silex_ast::Token;
use silex_builder::EnvironmentBuilder;
use silex_bytecode::{Access, Module, OpCode};
use silex_lexer::Lexer;
use silex_parser::Parser;
use silex_types::{FnType, Primitive, Type, ValueCell};
use std::collections::{BTreeMap, BTreeSet};
use value::{joined, literal, map, refine, unpack, Expr, Generics};

/// Optional source metadata for a chunk. Instance arguments are explicit parameters.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionSignature {
    pub name: String,
    pub parameters: Vec<Type>,
    pub return_type: Option<Type>,
}

pub struct Decompiler<'a, 'env, M> {
    module: &'a Module,
    environment: &'a EnvironmentBuilder<'env, M>,
    signatures: BTreeMap<usize, FunctionSignature>,
}

impl<'a, 'env, M> Decompiler<'a, 'env, M> {
    pub fn new(module: &'a Module, environment: &'a EnvironmentBuilder<'env, M>) -> Self {
        Self {
            module,
            environment,
            signatures: BTreeMap::new(),
        }
    }

    pub fn with_function_signature(mut self, chunk: usize, signature: FunctionSignature) -> Self {
        self.signatures.insert(chunk, signature);
        self
    }

    /// Recover source without changing the module or environment.
    pub fn decompile(self) -> Result<String, DecompilerError> {
        let instructions = self
            .module
            .chunks()
            .iter()
            .enumerate()
            .map(|(id, c)| decode(&c.chunk, id))
            .collect::<Result<Vec<_>, _>>()?;
        let mut signatures = Vec::new();
        let mut starts = Vec::new();
        for (id, code) in instructions.iter().enumerate() {
            let error = |m| DecompilerError::new(id, 0, m);
            let access = &self.module.chunks()[id].access;
            let mut parameters = Vec::new();
            let mut start = 0;
            // Compiler prologue: [CAST type] MEMORY_SET register, in argument order.
            while start < code.len() {
                let mut i = start;
                let ty = if matches!(code[i].op, OpCode::Cast) {
                    let ty = Type::primitive_type_from_byte(code[i].arg as u8)
                        .ok_or_else(|| error("invalid parameter cast"))?;
                    i += 1;
                    ty
                } else {
                    Type::Any
                };
                if i >= code.len() || !matches!(code[i].op, OpCode::MemorySet) {
                    break;
                }
                if code[i].arg != parameters.len() {
                    return Err(error("non-sequential parameter registers"));
                }
                parameters.push(ty);
                start = i + 1;
            }
            let mut name = format!("function{id}");
            while self
                .environment
                .get_functions_mapper()
                .get_by_signature(&name, None)
                .is_ok()
                || self.signatures.values().any(|s| s.name == name)
            {
                name.push('x');
            }
            let mut signature = FunctionSignature {
                name,
                parameters,
                return_type: Some(Type::Any),
            };
            let prologue_types = signature.parameters.clone();
            if let Some(types) = access.parameters() {
                if !self.signatures.contains_key(&id) {
                    signature.parameters = types
                        .iter()
                        .map(|t| {
                            unpack(t, self.environment, 0)
                                .map_err(|m| DecompilerError::new(id, 0, m))
                        })
                        .collect::<Result<_, _>>()?;
                }
            }
            if let Access::Hook { id: hook_id } = access {
                let (name, hook) = self
                    .environment
                    .get_hooks()
                    .iter()
                    .find(|(_, h)| h.hook_id == *hook_id)
                    .ok_or_else(|| error("unknown hook ID in environment"))?;
                signature.name = (*name).into();
                signature.parameters = hook.parameters.iter().map(|(_, t)| t.clone()).collect();
                signature.return_type = hook.return_type.clone();
            }
            if let Some(hint) = self.signatures.get(&id) {
                signature = hint.clone();
            }
            let prologue_count = code[..start]
                .iter()
                .filter(|i| matches!(i.op, OpCode::MemorySet))
                .count();
            if signature.parameters.len() != prologue_count {
                return Err(error("signature disagrees with parameter prologue"));
            }
            if prologue_types
                .iter()
                .zip(&signature.parameters)
                .any(|(cast, ty)| *cast != Type::Any && cast != ty)
            {
                return Err(error("signature disagrees with parameter cast"));
            }
            starts.push(start);
            signatures.push(signature);
        }
        if self.signatures.keys().any(|id| *id >= instructions.len()) {
            return Err(DecompilerError::new(
                0,
                0,
                "signature references an unknown chunk",
            ));
        }

        // Function types are packed as u16 in bytecode. A dynamic call gives
        // us its arity, so reconstruct an erased parameter type from the call
        // site itself; later inference refines its argument and return types.
        let mut inferred_dynamic_parameters = BTreeSet::new();
        for (chunk, code) in instructions.iter().enumerate() {
            for (index, instruction) in code.iter().enumerate() {
                if !matches!(instruction.op, OpCode::DynamicCall) || index == 0 {
                    continue;
                }
                let pointer = &code[index - 1];
                if !matches!(pointer.op, OpCode::MemoryLoad)
                    || pointer.arg >= signatures[chunk].parameters.len()
                    || matches!(
                        signatures[chunk].parameters[pointer.arg],
                        Type::Function(_) | Type::Closure(_)
                    )
                {
                    continue;
                }
                signatures[chunk].parameters[pointer.arg] = Type::Function(FnType::new(
                    None,
                    false,
                    vec![Type::Any; instruction.arg],
                    Some(Type::Any),
                ));
                inferred_dynamic_parameters.insert((chunk, pointer.arg));
            }
        }

        // Propagate parameter/return types across calls, including recursive calls.
        // Bound iterations so adversarial modules cannot cause unbounded inference.
        let mut binding_types = BTreeMap::new();
        for _ in 0..(instructions.len() * 2 + 4).min(128) {
            let previous = signatures.clone();
            let previous_bindings = binding_types.clone();
            let mut sources = Vec::new();
            let mut first_error = None;
            for (id, code) in instructions.iter().enumerate() {
                let mut engine = Engine {
                    module: self.module,
                    environment: self.environment,
                    code,
                    id,
                    signatures: &mut signatures,
                    binding_types: &mut binding_types,
                    returned: None,
                    inferred_dynamic_parameters: &inferred_dynamic_parameters,
                };
                let mut state = State::default();
                for (i, ty) in engine.signatures[id].parameters.iter().enumerate() {
                    let mut expr = Expr::new(format!("arg{i}"), ty.clone());
                    expr.parameter = Some(i);
                    state.registers.insert(i, expr);
                }
                let result = engine.block(starts[id], code.len(), &mut state, None, 0);
                match result {
                    Ok(lines) => {
                        if let Some(ty) = engine.returned {
                            if !self.signatures.contains_key(&id)
                                && !matches!(self.module.chunks()[id].access, Access::Hook { .. })
                            {
                                engine.signatures[id].return_type = ty;
                            }
                        }
                        let signature = &engine.signatures[id];
                        let prefix = match self.module.chunks()[id].access {
                            Access::Internal => "fn",
                            Access::All { .. } => "pub fn",
                            Access::Entry { .. } => "entry",
                            Access::Hook { .. } => "hook",
                        };
                        let params = signature
                            .parameters
                            .iter()
                            .enumerate()
                            .map(|(i, t)| format!("arg{i}: {t}"))
                            .collect::<Vec<_>>()
                            .join(", ");
                        let ret = signature
                            .return_type
                            .as_ref()
                            .map(|t| format!(" -> {t}"))
                            .unwrap_or_default();
                        sources.push(format!(
                            "{prefix} {}({params}){ret} {{\n{}}}\n",
                            signature.name,
                            indent(&lines)
                        ));
                    }
                    Err(e) => {
                        if first_error.is_none() {
                            first_error = Some(e);
                        }
                    }
                }
            }
            // Restore authoritative hints after inference.
            for (id, signature) in &self.signatures {
                signatures[*id] = signature.clone();
            }
            if signatures == previous && binding_types == previous_bindings {
                if let Some(e) = first_error {
                    return Err(e);
                }
                let source = sources.join("\n");
                // Apply the actual language rules, including environment name
                // resolution. Never return placeholder syntax as valid source.
                let tokens = Lexer::new(&source)
                    .collect::<Result<Vec<_>, _>>()
                    .map_err(|e| {
                        DecompilerError::new(0, 0, format!("recovered source failed lexing: {e}"))
                    })?;
                let mut nesting = 0usize;
                for token in &tokens {
                    match &token.token {
                        Token::ParenthesisOpen | Token::BracketOpen | Token::BraceOpen => {
                            nesting += 1
                        }
                        Token::ParenthesisClose | Token::BracketClose | Token::BraceClose => {
                            nesting = nesting.saturating_sub(1)
                        }
                        _ => {}
                    }
                    if nesting > 128 {
                        return Err(DecompilerError::new(
                            0,
                            0,
                            "recovered source nesting exceeds 128",
                        ));
                    }
                }
                Parser::with(tokens.into_iter(), self.environment)
                    .parse()
                    .map_err(|e| {
                        DecompilerError::new(
                            0,
                            0,
                            format!("recovered source needs more type information: {e}"),
                        )
                    })?;
                return Ok(source);
            }
        }
        Err(DecompilerError::new(
            0,
            0,
            "type inference did not converge; supply function signatures",
        ))
    }
}

#[derive(Clone, Default)]
struct State {
    stack: Vec<Expr>,
    registers: BTreeMap<usize, Expr>,
}

#[derive(Clone)]
struct Line {
    offset: usize,
    text: String,
}
fn line(lines: &mut Vec<Line>, offset: usize, text: impl Into<String>) {
    lines.push(Line {
        offset,
        text: text.into(),
    });
}

fn expression_statement(lines: &mut Vec<Line>, offset: usize, text: String) {
    // A leading map literal is otherwise parsed as a scope. The semicolon
    // prevents the next expression from becoming a call/index on this one.
    let text = if text.starts_with('{') {
        format!("({text});")
    } else {
        format!("{text};")
    };
    line(lines, offset, text);
}
fn indent(lines: &[Line]) -> String {
    let mut output = String::new();
    let mut quoted = false;
    let mut escaped = false;
    let mut start = true;
    for line in lines {
        for ch in line.text.chars().chain(std::iter::once('\n')) {
            if start {
                // A literal can contain actual newlines. Indentation inside it
                // would change the contract's string/byte constant.
                if !quoted {
                    output.push_str("    ");
                }
                start = false;
            }
            output.push(ch);
            if escaped {
                escaped = false;
            } else if quoted && ch == '\\' {
                escaped = true;
            } else if ch == '"' {
                quoted = !quoted;
            }
            if ch == '\n' {
                start = true;
            }
        }
    }
    output
}

struct Engine<'a, 'env, 's, M> {
    module: &'a Module,
    environment: &'a EnvironmentBuilder<'env, M>,
    code: &'a [Instruction],
    id: usize,
    signatures: &'s mut [FunctionSignature],
    binding_types: &'s mut BTreeMap<(usize, String), Type>,
    returned: Option<Option<Type>>,
    inferred_dynamic_parameters: &'s BTreeSet<(usize, usize)>,
}

impl<M> Engine<'_, '_, '_, M> {
    fn error(&self, offset: usize, message: impl Into<String>) -> DecompilerError {
        DecompilerError::new(self.id, offset, message)
    }

    fn pop(&self, state: &mut State, offset: usize) -> Result<Expr, DecompilerError> {
        state
            .stack
            .pop()
            .ok_or_else(|| self.error(offset, "stack underflow"))
    }

    fn args(
        &self,
        state: &mut State,
        n: usize,
        offset: usize,
    ) -> Result<Vec<Expr>, DecompilerError> {
        if n > state.stack.len() {
            return Err(self.error(offset, "not enough call/constructor arguments"));
        }
        Ok(state.stack.split_off(state.stack.len() - n))
    }

    fn target(&self, offset: usize) -> usize {
        self.code
            .binary_search_by_key(&offset, |i| i.offset)
            .unwrap_or(self.code.len())
    }

    fn constrain(&mut self, value: &mut Expr, ty: &Type) {
        let ty = ty.map_generic_type(Some(&Type::Any));
        if let Some(p) = value.parameter {
            let current = &self.signatures[self.id].parameters[p];
            self.signatures[self.id].parameters[p] = refine(current, &ty);
        }
        let target = if value.object.is_some() && matches!(ty, Type::Struct(_) | Type::Tuples(_)) {
            ty
        } else {
            refine(&value.ty, &ty)
        };
        value.as_type(&target);
        value.ty = target;
        if let Some(name) = &value.binding {
            let key = (self.id, name.clone());
            let current = self.binding_types.entry(key).or_insert(Type::Any);
            *current = refine(current, &value.ty);
            value.ty = current.clone();
        }
    }

    fn binding(&mut self, name: String, ty: Type) -> Expr {
        let current = self
            .binding_types
            .entry((self.id, name.clone()))
            .or_insert(Type::Any);
        *current = refine(current, &ty);
        Expr::named(name, current.clone())
    }

    fn dynamic_register(&self, register: usize) -> bool {
        self.code.windows(2).any(|window| {
            matches!(window[0].op, OpCode::MemoryLoad)
                && window[0].arg == register
                && matches!(window[1].op, OpCode::DynamicCall)
        })
    }

    fn function_pointer(&self, id: u16) -> Option<(String, Type)> {
        // A non-closure function pointer is encoded as the module chunk ID.
        // The compiler has already removed the environment-function offset.
        let chunk = id as usize;
        if !self.module.is_callable_chunk(chunk) {
            return None;
        }
        let signature = self.signatures.get(chunk)?;
        Some((
            signature.name.clone(),
            Type::Function(FnType::new(
                None,
                false,
                signature.parameters.clone(),
                signature.return_type.clone(),
            )),
        ))
    }

    fn call_result(
        &mut self,
        state: &mut State,
        lines: &mut Vec<Line>,
        instruction: usize,
        text: String,
        ty: Option<Type>,
        origin_chunk: Option<usize>,
    ) -> Result<(), DecompilerError> {
        let offset = self.code[instruction].offset;
        let voidable_call = matches!(ty, Some(Type::Voidable(_)));
        let ty = match ty {
            Some(Type::Voidable(inner)) => {
                self.result_is_used(instruction, state)?.then_some(*inner)
            }
            other => other,
        };
        if let Some(ty) = ty {
            // Calls are expressions in Silex. Keep them on the reconstructed
            // expression stack instead of inventing a source local for them.
            let mut value = Expr::new(text, ty);
            value.voidable_call = voidable_call;
            value.origin_chunk = origin_chunk;
            state.stack.push(value);
        } else {
            expression_statement(lines, offset, text);
        }
        Ok(())
    }

    fn tuple_pattern(
        &mut self,
        value: Expr,
        ty: &Type,
        pc: &mut usize,
        reserved: usize,
        state: &mut State,
    ) -> Result<(String, Type), DecompilerError> {
        let mut types = match ty {
            Type::Tuples(types) => types.clone(),
            Type::Array(inner) => {
                // Packed type metadata cannot distinguish a homogeneous tuple
                // from an array. A nested Flatten marks tuple destructuring;
                // count its stores and leave the parent's pending fields for
                // the caller.
                let actions = self
                    .code
                    .get(*pc..)
                    .unwrap_or(&[])
                    .iter()
                    .take_while(|instruction| {
                        matches!(instruction.op, OpCode::MemorySet | OpCode::Pop)
                    })
                    .count();
                let arity = actions.saturating_sub(reserved);
                if arity < 2 {
                    return Err(self.error(0, "cannot infer nested tuple arity from opcodes"));
                }
                vec![*inner.clone(); arity]
            }
            _ => return Err(self.error(0, format!("cannot recover tuple pattern type from {ty}"))),
        };
        let mut pattern = vec![String::new(); types.len()];
        for index in (0..types.len()).rev() {
            let mut field = value
                .object
                .as_ref()
                .and_then(|values| values.get(index))
                .cloned()
                .unwrap_or_else(|| {
                    Expr::new(format!("({}).{index}", value.text), types[index].clone())
                });
            self.constrain(&mut field, &types[index]);
            let instruction = self
                .code
                .get(*pc)
                .ok_or_else(|| self.error(0, "incomplete tuple destructuring opcodes"))?;
            let (field_pattern, nested_type) = match instruction.op {
                OpCode::MemorySet => {
                    let name = format!("local{}", instruction.offset);
                    let local = self.binding(name.clone(), field.ty.clone());
                    self.constrain(&mut field, &local.ty);
                    state.registers.insert(instruction.arg, local);
                    *pc += 1;
                    (name, None)
                }
                OpCode::Pop => {
                    *pc += 1;
                    ("_".into(), None)
                }
                OpCode::Flatten => {
                    *pc += 1;
                    let (pattern, inferred_type) =
                        self.tuple_pattern(field, &types[index], pc, reserved + index, state)?;
                    (pattern, Some(inferred_type))
                }
                _ => {
                    return Err(
                        self.error(instruction.offset, "unexpected tuple destructuring opcode")
                    )
                }
            };
            pattern[index] = field_pattern;
            if let Some(nested_type) = nested_type {
                types[index] = nested_type;
            }
        }
        Ok((format!("({})", pattern.join(", ")), Type::Tuples(types)))
    }

    fn block(
        &mut self,
        start: usize,
        end: usize,
        state: &mut State,
        loop_targets: Option<(usize, usize)>,
        depth: usize,
    ) -> Result<Vec<Line>, DecompilerError> {
        if depth > 128 {
            return Err(self.error(0, "control-flow nesting exceeds 128"));
        }
        let mut lines = Vec::new();
        let mut i = start;
        while i < end {
            let instruction = &self.code[i];
            let Instruction {
                offset,
                op,
                arg,
                extra,
            } = *instruction;
            match op {
                OpCode::Constant => {
                    let value = self
                        .module
                        .constants()
                        .get_index(arg)
                        .ok_or_else(|| self.error(offset, "unknown constant ID"))?;
                    let mut expr = literal(value, 0).map_err(|m| self.error(offset, m))?;
                    if let ValueCell::Primitive(Primitive::U16(id)) = value {
                        expr.function_pointer = self.function_pointer(*id);
                    }
                    state.stack.push(expr);
                }
                OpCode::MemoryLoad => {
                    let mut value = state
                        .registers
                        .get(&arg)
                        .cloned()
                        .ok_or_else(|| self.error(offset, "load from an unknown register"))?;
                    if let Some(p) = value.parameter {
                        value.ty = self.signatures[self.id].parameters[p].clone();
                    }
                    state.stack.push(value);
                }
                OpCode::MemorySet => {
                    let mut value = self.pop(state, offset)?;
                    let name = format!("local{offset}");
                    if self.dynamic_register(arg) {
                        if let Some((function_name, function_type)) = &value.function_pointer {
                            value.text = function_name.clone();
                            value.ty = function_type.clone();
                        }
                    }
                    let mut local = self.binding(name.clone(), value.ty.clone());
                    local.function_pointer = value.function_pointer.clone();
                    self.constrain(&mut value, &local.ty);
                    line(
                        &mut lines,
                        offset,
                        format!("let {name}: {} = {}", value.ty, value.text),
                    );
                    state.registers.insert(arg, local);
                }
                OpCode::MemoryToOwned => {
                    if !state.registers.contains_key(&arg) {
                        return Err(self.error(offset, "unknown register"));
                    }
                    // The compiler inserts this automatically on parameter assignment.
                }
                OpCode::Pop | OpCode::PopN => {
                    let count = if matches!(op, OpCode::Pop) { 1 } else { arg };
                    // PopN drops the whole suffix. Emit deferred expressions
                    // in their original evaluation order, not reverse pop order.
                    for value in self.args(state, count, offset)? {
                        let text = if value.voidable_call {
                            format!("({})", value.text)
                        } else {
                            value.text
                        };
                        expression_statement(&mut lines, offset, text);
                    }
                }
                OpCode::Cast => {
                    let value = self.pop(state, offset)?;
                    let ty = Type::primitive_type_from_byte(arg as u8)
                        .ok_or_else(|| self.error(offset, "invalid cast type"))?;
                    state
                        .stack
                        .push(Expr::new(format!("({} as {ty})", value.text), ty));
                }
                OpCode::Neg => {
                    let mut value = self.pop(state, offset)?;
                    self.constrain(&mut value, &Type::Bool);
                    state
                        .stack
                        .push(Expr::new(format!("(!{})", value.text), Type::Bool));
                }
                OpCode::NewObject => {
                    let values = self.args(state, arg, offset)?;
                    state.stack.push(Expr::object(values));
                }
                OpCode::Flatten => {
                    let mut value = self.pop(state, offset)?;
                    let types = match (&value.ty, &value.object) {
                        (Type::Tuples(types), _) => types.clone(),
                        (_, Some(values)) => values.iter().map(|v| v.ty.clone()).collect(),
                        (Type::Array(inner), None) => {
                            let arity = self
                                .code
                                .get(i + 1..)
                                .unwrap_or(&[])
                                .iter()
                                .take_while(|next| {
                                    matches!(next.op, OpCode::MemorySet | OpCode::Pop)
                                })
                                .count();
                            vec![*inner.clone(); arity]
                        }
                        _ => {
                            return Err(
                                self.error(offset, "cannot recover destructured tuple types")
                            )
                        }
                    };
                    if types.len() < 2 {
                        return Err(
                            self.error(offset, "singleton/empty destructuring is unsupported")
                        );
                    }
                    value.as_type(&Type::Tuples(types.clone()));
                    let mut pc = i + 1;
                    let pattern_type = Type::Tuples(types);
                    let (pattern, inferred_type) =
                        self.tuple_pattern(value.clone(), &pattern_type, &mut pc, 0, state)?;
                    if let Some(chunk) = value.origin_chunk {
                        self.signatures[chunk].return_type = Some(inferred_type);
                    }
                    line(
                        &mut lines,
                        offset,
                        format!("let {pattern} = {}", value.text),
                    );
                    i = pc;
                    continue;
                }
                OpCode::NewRange => {
                    let right = self.pop(state, offset)?;
                    let left = self.pop(state, offset)?;
                    state.stack.push(Expr::new(
                        format!("({}..{})", left.text, right.text),
                        Type::Range(Box::new(left.ty)),
                    ));
                }
                OpCode::NewMap => {
                    let values = self.args(state, arg * 2, offset)?;
                    state.stack.push(map(values
                        .chunks_exact(2)
                        .map(|p| (p[0].clone(), p[1].clone()))
                        .collect()));
                }
                OpCode::ArrayCall | OpCode::SubLoad => {
                    let index = if matches!(op, OpCode::ArrayCall) {
                        Some(self.pop(state, offset)?)
                    } else {
                        None
                    };
                    let value = self.pop(state, offset)?;
                    let (text, ty) = match (&value.ty, index) {
                        (Type::Struct(s), None) => {
                            let (name, ty) = s
                                .fields()
                                .get(arg)
                                .ok_or_else(|| self.error(offset, "unknown struct field"))?;
                            (format!("({}).{name}", value.text), ty.clone())
                        }
                        (Type::Tuples(types), None) => (
                            format!("({}).{arg}", value.text),
                            types
                                .get(arg)
                                .cloned()
                                .ok_or_else(|| self.error(offset, "unknown tuple field"))?,
                        ),
                        (Type::Array(ty), index) => (
                            format!(
                                "({})[{}]",
                                value.text,
                                index.map(|v| v.text).unwrap_or_else(|| format!("{arg}u32"))
                            ),
                            *ty.clone(),
                        ),
                        (Type::Bytes, Some(index)) => {
                            (format!("({})[{}]", value.text, index.text), Type::U8)
                        }
                        _ => {
                            return Err(self.error(
                                offset,
                                "cannot recover sub-value type; supply a function signature",
                            ))
                        }
                    };
                    state.stack.push(Expr::new(text, ty));
                }
                OpCode::SysCall => {
                    let f = self
                        .environment
                        .get_functions_mapper()
                        .get_function(&(arg as u16))
                        .ok_or_else(|| self.error(offset, format!("unknown syscall ID {arg}")))?;
                    let mut args = self.args(state, f.parameters.len(), offset)?;
                    let mut instance = if f.require_instance {
                        Some(self.pop(state, offset)?)
                    } else {
                        None
                    };
                    let mut generics = Generics::default();
                    if let (Some(value), Some(ty)) = (&instance, &f.on_type) {
                        generics.infer(ty, &value.ty);
                    }
                    for (value, (_, ty)) in args.iter().zip(&f.parameters) {
                        generics.infer(ty, &value.ty);
                    }
                    if let (Some(value), Some(ty)) = (&mut instance, &f.on_type) {
                        self.constrain(value, &generics.resolve(ty));
                    }
                    let on_type = instance.as_ref().map(|v| &v.ty).or(f.on_type.as_ref());
                    for (value, (_, ty)) in args.iter_mut().zip(&f.parameters) {
                        self.constrain(
                            value,
                            &refine(&ty.map_generic_type(on_type), &generics.resolve(ty)),
                        );
                    }
                    let name = if let Some(instance) = &instance {
                        format!("({}).{}", instance.text, f.name)
                    } else if let Some(ty) = &f.on_type {
                        format!("{ty}::{}", f.name)
                    } else {
                        f.name.into()
                    };
                    let ty = f
                        .return_type
                        .as_ref()
                        .map(|t| refine(&t.map_generic_type(on_type), &generics.resolve(t)));
                    self.call_result(
                        state,
                        &mut lines,
                        i,
                        format!("{name}({})", joined(&args)),
                        ty,
                        None,
                    )?;
                }
                OpCode::DynamicCall => {
                    let mut function = self.pop(state, offset)?;
                    let args = self.args(state, arg, offset)?;
                    if let Some(parameter) = function
                        .parameter
                        .filter(|p| self.inferred_dynamic_parameters.contains(&(self.id, *p)))
                    {
                        let returns_value = self.result_is_used(i, state)?;
                        let return_type = returns_value.then_some(Type::Any);
                        let function_type = Type::Function(FnType::new(
                            None,
                            false,
                            args.iter().map(|value| value.ty.clone()).collect(),
                            return_type.clone(),
                        ));
                        self.signatures[self.id].parameters[parameter] = function_type.clone();
                        function.ty = function_type;
                    }
                    let return_type = match &function.ty {
                        Type::Function(ty) => ty.return_type().cloned(),
                        Type::Closure(ty) => ty.return_type().cloned(),
                        _ => {
                            return Err(self.error(
                                offset,
                                "dynamic call signature is erased; supply a function signature",
                            ));
                        }
                    };
                    self.call_result(
                        state,
                        &mut lines,
                        i,
                        format!("{}({})", function.text, joined(&args)),
                        return_type,
                        None,
                    )?;
                }
                OpCode::InvokeChunk => {
                    let signature = self
                        .signatures
                        .get(arg)
                        .cloned()
                        .ok_or_else(|| self.error(offset, "unknown chunk ID"))?;
                    if !self.module.is_callable_chunk(arg) {
                        return Err(self.error(offset, "call to entry or hook chunk"));
                    }
                    if extra != signature.parameters.len() {
                        return Err(self.error(offset, "call arity disagrees with chunk prologue"));
                    }
                    let mut args = self.args(state, extra, offset)?;
                    for (p, (value, ty)) in args.iter_mut().zip(&signature.parameters).enumerate() {
                        self.constrain(value, ty);
                        self.signatures[arg].parameters[p] = refine(ty, &value.ty);
                    }
                    let return_type = signature.return_type;
                    self.call_result(
                        state,
                        &mut lines,
                        i,
                        format!("{}({})", signature.name, joined(&args)),
                        return_type,
                        Some(arg),
                    )?;
                }
                OpCode::Return => {
                    let mut value = if state.stack.is_empty() {
                        None
                    } else {
                        Some(self.pop(state, offset)?)
                    };
                    if let (Some(value), Some(expected)) =
                        (&mut value, self.signatures[self.id].return_type.clone())
                    {
                        self.constrain(value, &expected);
                    }
                    if !state.stack.is_empty() {
                        return Err(self.error(offset, "multiple values at return"));
                    }
                    let ty = value.as_ref().map(|v| v.ty.clone());
                    match &self.returned {
                        Some(Some(existing))
                            if ty.as_ref().is_some_and(|t| {
                                *t != Type::Any && *existing != Type::Any && *t != *existing
                            }) =>
                        {
                            return Err(self.error(offset, "incompatible return types"))
                        }
                        Some(Some(existing)) if *existing != Type::Any => {}
                        _ => self.returned = Some(ty),
                    }
                    line(
                        &mut lines,
                        offset,
                        value
                            .map(|v| format!("return {}", v.text))
                            .unwrap_or_else(|| "return".into()),
                    );
                    break;
                }
                OpCode::Copy => {
                    // COPY [NEG] JUMP_IF_FALSE ... AND/OR. Boolean operands
                    // can also be combined with the equivalent bitwise opcode.
                    let neg = self
                        .code
                        .get(i + 1)
                        .is_some_and(|v| matches!(v.op, OpCode::Neg));
                    let jump_i = i + 1 + usize::from(neg);
                    let jump = self
                        .code
                        .get(jump_i)
                        .filter(|v| matches!(v.op, OpCode::JumpIfFalse))
                        .ok_or_else(|| {
                            self.error(
                                offset,
                                "COPY outside a short-circuit expression is unsupported",
                            )
                        })?;
                    let target = self.target(jump.arg);
                    if target <= jump_i + 1
                        || target > end
                        || !matches!(
                            (neg, self.code[target - 1].op),
                            (false, OpCode::And | OpCode::BitwiseAnd)
                                | (true, OpCode::Or | OpCode::BitwiseOr)
                        )
                    {
                        return Err(self.error(offset, "unrecognized short-circuit expression"));
                    }
                    let mut left = self.pop(state, offset)?;
                    self.constrain(&mut left, &Type::Bool);
                    let mut branch = state.clone();
                    let mut body =
                        self.block(jump_i + 1, target - 1, &mut branch, loop_targets, depth + 1)?;
                    let mut right = self.pop(&mut branch, offset)?;
                    self.constrain(&mut right, &Type::Bool);
                    if left.ty != Type::Bool || right.ty != Type::Bool {
                        return Err(self.error(offset, "short-circuit operands must be boolean"));
                    }
                    if branch.stack != state.stack {
                        return Err(self.error(offset, "unbalanced short-circuit stack"));
                    }
                    if body.is_empty() {
                        state.stack.push(Expr::new(
                            format!(
                                "({} {} {})",
                                left.text,
                                if neg { "||" } else { "&&" },
                                right.text
                            ),
                            Type::Bool,
                        ));
                        i = target;
                        continue;
                    }
                    let name = format!("logic{offset}");
                    line(
                        &mut lines,
                        offset,
                        format!("let {name}: bool = {}", left.text),
                    );
                    line(&mut body, offset, format!("{name} = {}", right.text));
                    line(
                        &mut lines,
                        offset,
                        format!(
                            "if {}{name} {{\n{}}}",
                            if neg { "!" } else { "" },
                            indent(&body)
                        ),
                    );
                    state.stack.push(Expr::new(name, Type::Bool));
                    i = target;
                    continue;
                }
                OpCode::JumpIfFalse => {
                    let mut condition = self.pop(state, offset)?;
                    self.constrain(&mut condition, &Type::Bool);
                    let target = self.target(arg);
                    if target <= i || target > end {
                        return Err(self.error(offset, "unstructured conditional jump"));
                    }
                    let tail = &self.code[target - 1];
                    if matches!(tail.op, OpCode::Jump) && tail.arg <= offset {
                        let loop_start = self.target(tail.arg);
                        if loop_start < start {
                            return Err(self.error(offset, "loop crosses a source block"));
                        }
                        let mut body_state = state.clone();
                        let body = self.block(
                            i + 1,
                            target - 1,
                            &mut body_state,
                            Some((tail.arg, arg)),
                            depth + 1,
                        )?;
                        if body_state.stack != state.stack {
                            return Err(self.error(offset, "unbalanced loop stack"));
                        }
                        // Calls in the condition must execute on every iteration.
                        let split = lines
                            .iter()
                            .position(|l| l.offset >= tail.arg)
                            .unwrap_or(lines.len());
                        let mut loop_lines = lines.split_off(split);
                        line(
                            &mut loop_lines,
                            offset,
                            format!("if !({}) {{\n    break\n}}", condition.text),
                        );
                        loop_lines.extend(body);
                        line(
                            &mut lines,
                            tail.arg,
                            format!("while true {{\n{}}}", indent(&loop_lines)),
                        );
                        i = target;
                        continue;
                    }
                    let has_else = matches!(tail.op, OpCode::Jump) && tail.arg >= arg;
                    let mut finish = if has_else {
                        self.target(tail.arg)
                    } else {
                        target
                    };
                    if finish > end {
                        return Err(self.error(offset, "conditional crosses a source block"));
                    }
                    let mut yes = state.clone();
                    let yes_lines = self.block(
                        i + 1,
                        if has_else { target - 1 } else { target },
                        &mut yes,
                        loop_targets,
                        depth + 1,
                    )?;
                    let mut no = state.clone();
                    let mut no_lines = if has_else {
                        self.block(target, finish, &mut no, loop_targets, depth + 1)?
                    } else {
                        Vec::new()
                    };
                    // The compiler can patch an unreachable branch exit to the
                    // final RETURN itself, sharing it with the other branch.
                    if has_else
                        && yes.stack == state.stack
                        && no.stack.len() == state.stack.len() + 1
                        && self
                            .code
                            .get(finish)
                            .is_some_and(|v| matches!(v.op, OpCode::Return))
                        && yes_lines
                            .last()
                            .is_some_and(|l| l.text.starts_with("return"))
                    {
                        no_lines.extend(self.block(
                            finish,
                            finish + 1,
                            &mut no,
                            loop_targets,
                            depth + 1,
                        )?);
                        finish += 1;
                    }
                    if yes.stack.len() == state.stack.len() + 1
                        && no.stack.len() == state.stack.len() + 1
                        && yes_lines.is_empty()
                        && no_lines.is_empty()
                    {
                        let yes = self.pop(&mut yes, offset)?;
                        let no = self.pop(&mut no, offset)?;
                        state.stack.push(Expr::new(
                            format!("({} ? {} : {})", condition.text, yes.text, no.text),
                            yes.ty,
                        ));
                    } else {
                        if yes.stack != state.stack || no.stack != state.stack {
                            return Err(
                                self.error(offset, "cannot merge expression stacks at conditional")
                            );
                        }
                        let mut text =
                            format!("if {} {{\n{}}}", condition.text, indent(&yes_lines));
                        if !no_lines.is_empty() {
                            text.push_str(&format!(" else {{\n{}}}", indent(&no_lines)));
                        }
                        line(&mut lines, offset, text);
                    }
                    i = finish;
                    continue;
                }
                OpCode::Jump => {
                    if let Some((next, exit)) = loop_targets {
                        if arg == next || arg == exit {
                            line(
                                &mut lines,
                                offset,
                                if arg == next { "continue" } else { "break" },
                            );
                            break;
                        }
                    }
                    if self.target(arg) == end {
                        break;
                    }
                    return Err(
                        self.error(offset, "unstructured jump (including for-loop continue)")
                    );
                }
                OpCode::IteratorBegin => {
                    let iterable = self.pop(state, offset)?;
                    let next = self
                        .code
                        .get(i + 1)
                        .filter(|v| matches!(v.op, OpCode::IteratorNext))
                        .ok_or_else(|| self.error(offset, "missing iterator next"))?;
                    let target = self.target(next.arg);
                    if target >= end
                        || target <= i + 3
                        || !matches!(self.code[target].op, OpCode::IteratorEnd)
                        || !matches!(self.code[target - 1].op, OpCode::Jump)
                        || self.code[target - 1].arg != next.offset
                    {
                        return Err(self.error(offset, "unstructured iterator"));
                    }
                    let store = &self.code[i + 2];
                    if !matches!(store.op, OpCode::MemorySet) {
                        return Err(self.error(offset, "iterator destructuring is unsupported"));
                    }
                    let ty = match iterable.ty {
                        Type::Array(t) | Type::Range(t) => *t,
                        Type::Bytes => Type::U8,
                        _ => return Err(self.error(offset, "unknown iterable element type")),
                    };
                    let name = format!("item{offset}");
                    let mut body_state = state.clone();
                    body_state.registers.insert(store.arg, Expr::new(&name, ty));
                    let body = self.block(
                        i + 3,
                        target - 1,
                        &mut body_state,
                        Some((next.offset, next.arg)),
                        depth + 1,
                    )?;
                    if body_state.stack != state.stack {
                        return Err(self.error(offset, "unbalanced iterator stack"));
                    }
                    line(
                        &mut lines,
                        offset,
                        format!(
                            "foreach {name} in {} {{\n{}}}",
                            iterable.text,
                            indent(&body)
                        ),
                    );
                    i = target + 1;
                    continue;
                }
                OpCode::And | OpCode::Or => {
                    // Bare AND/OR are eager VM operations. Only the COPY/jump
                    // pattern above represents source-level short circuiting.
                    let mut right = self.pop(state, offset)?;
                    let mut left = self.pop(state, offset)?;
                    self.constrain(&mut left, &Type::Bool);
                    self.constrain(&mut right, &Type::Bool);
                    let left_name = format!("left{offset}");
                    let right_name = format!("right{offset}");
                    line(
                        &mut lines,
                        offset,
                        format!("let {left_name}: bool = {}", left.text),
                    );
                    line(
                        &mut lines,
                        offset,
                        format!("let {right_name}: bool = {}", right.text),
                    );
                    let symbol = if matches!(op, OpCode::And) {
                        "&&"
                    } else {
                        "||"
                    };
                    state.stack.push(Expr::new(
                        format!("({left_name} {symbol} {right_name})"),
                        Type::Bool,
                    ));
                }
                _ => {
                    if let Some((symbol, assign, boolean)) = operator(op) {
                        let mut right = self.pop(state, offset)?;
                        let mut left = self.pop(state, offset)?;
                        self.constrain(&mut left, &right.ty);
                        self.constrain(&mut right, &left.ty);
                        if assign {
                            line(
                                &mut lines,
                                offset,
                                format!("{} {symbol} {}", left.text, right.text),
                            );
                        } else {
                            let ty = if boolean { Type::Bool } else { left.ty };
                            state.stack.push(Expr::new(
                                format!("({} {symbol} {})", left.text, right.text),
                                ty,
                            ));
                        }
                    } else {
                        return Err(self.error(offset, format!("unsupported opcode {op:?}")));
                    }
                }
            }
            i += 1;
        }
        Ok(lines)
    }
}

fn operator(op: OpCode) -> Option<(&'static str, bool, bool)> {
    use OpCode::*;
    Some(match op {
        Add => ("+", false, false),
        Sub => ("-", false, false),
        Mul => ("*", false, false),
        Div => ("/", false, false),
        Mod => ("%", false, false),
        Pow => ("**", false, false),
        BitwiseAnd => ("&", false, false),
        BitwiseOr => ("|", false, false),
        BitwiseXor => ("^", false, false),
        BitwiseShl => ("<<", false, false),
        BitwiseShr => (">>", false, false),
        Eq => ("==", false, true),
        Gt => (">", false, true),
        Gte => (">=", false, true),
        Lt => ("<", false, true),
        Lte => ("<=", false, true),
        Assign => ("=", true, false),
        AssignAdd => ("+=", true, false),
        AssignSub => ("-=", true, false),
        AssignMul => ("*=", true, false),
        AssignDiv => ("/=", true, false),
        AssignMod => ("%=", true, false),
        AssignPow => ("**=", true, false),
        AssignBitwiseAnd => ("&=", true, false),
        AssignBitwiseOr => ("|=", true, false),
        AssignBitwiseXor => ("^=", true, false),
        AssignBitwiseShl => ("<<=", true, false),
        AssignBitwiseShr => (">>=", true, false),
        _ => return None,
    })
}
