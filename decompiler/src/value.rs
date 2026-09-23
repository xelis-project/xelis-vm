use silex_builder::EnvironmentBuilder;
use silex_types::{NumberType, Primitive, Type, TypePacked, ValueCell};
use std::collections::BTreeMap;

#[derive(Default)]
pub(crate) struct Generics(BTreeMap<Option<u8>, Type>);

impl Generics {
    pub fn infer(&mut self, template: &Type, actual: &Type) {
        match (template, actual) {
            (Type::T(id), actual) => {
                let current = self.0.entry(*id).or_insert(Type::Any);
                *current = refine(current, actual);
            }
            (Type::Array(a), Type::Array(b))
            | (Type::Optional(a), Type::Optional(b))
            | (Type::Range(a), Type::Range(b)) => self.infer(a, b),
            (Type::Map(ak, av), Type::Map(bk, bv)) => {
                self.infer(ak, bk);
                self.infer(av, bv);
            }
            (Type::Tuples(a), Type::Tuples(b)) if a.len() == b.len() => {
                for (a, b) in a.iter().zip(b) {
                    self.infer(a, b);
                }
            }
            _ => {}
        }
    }

    pub fn resolve(&self, template: &Type) -> Type {
        match template {
            Type::T(id) => self.0.get(id).cloned().unwrap_or(Type::Any),
            Type::Array(t) => Type::Array(Box::new(self.resolve(t))),
            Type::Optional(t) => Type::Optional(Box::new(self.resolve(t))),
            Type::Range(t) => Type::Range(Box::new(self.resolve(t))),
            Type::Map(k, v) => Type::Map(Box::new(self.resolve(k)), Box::new(self.resolve(v))),
            Type::Tuples(types) => Type::Tuples(types.iter().map(|t| self.resolve(t)).collect()),
            _ => template.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Expr {
    pub text: String,
    pub ty: Type,
    pub parameter: Option<usize>,
    pub binding: Option<String>,
    pub object: Option<Vec<Expr>>,
    pub pending: bool,
}

impl Expr {
    pub fn new(text: impl Into<String>, ty: Type) -> Self {
        Self {
            text: text.into(),
            ty,
            parameter: None,
            binding: None,
            object: None,
            pending: false,
        }
    }

    pub fn pending(mut self) -> Self {
        self.pending = true;
        self
    }

    pub fn named(name: String, ty: Type) -> Self {
        let mut value = Self::new(name.clone(), ty);
        value.binding = Some(name);
        value
    }

    pub fn object(values: Vec<Self>) -> Self {
        let ty = if let Some(first) = values
            .first()
            .filter(|first| values.iter().all(|v| v.ty == first.ty))
        {
            Type::Array(Box::new(first.ty.clone()))
        } else if values.is_empty() {
            Type::Array(Box::new(Type::Any))
        } else {
            Type::Tuples(values.iter().map(|v| v.ty.clone()).collect())
        };
        let mut result = Self::new("", ty.clone());
        result.object = Some(values);
        result.as_type(&ty);
        result.pending()
    }

    pub fn as_type(&mut self, ty: &Type) {
        if let Some(values) = &mut self.object {
            match ty {
                Type::Struct(s) if s.fields().len() == values.len() => {
                    let fields = values
                        .iter_mut()
                        .zip(s.fields())
                        .map(|(v, (name, ty))| {
                            v.as_type(ty);
                            format!("{name}: {}", v.text)
                        })
                        .collect::<Vec<_>>()
                        .join(", ");
                    self.text = format!("{} {{ {fields} }}", s.name());
                }
                Type::Tuples(types) if types.len() == values.len() => {
                    for (v, ty) in values.iter_mut().zip(types) {
                        v.as_type(ty);
                    }
                    self.text = format!("({})", joined(values));
                }
                Type::Array(inner) => {
                    for v in values.iter_mut() {
                        v.as_type(inner);
                    }
                    self.text = format!("[{}]", joined(values));
                }
                _ => return,
            }
            self.ty = ty.clone();
        } else if self.ty == Type::Any {
            self.ty = ty.clone();
        }
    }
}

pub(crate) fn joined(values: &[Expr]) -> String {
    values
        .iter()
        .map(|v| v.text.as_str())
        .collect::<Vec<_>>()
        .join(", ")
}

pub(crate) fn refine(current: &Type, expected: &Type) -> Type {
    match (current, expected) {
        (Type::Any | Type::T(_), _) => expected.clone(),
        (Type::Array(a), Type::Array(b)) => Type::Array(Box::new(refine(a, b))),
        (Type::Optional(a), Type::Optional(b)) => Type::Optional(Box::new(refine(a, b))),
        (Type::Range(a), Type::Range(b)) => Type::Range(Box::new(refine(a, b))),
        (Type::Map(ak, av), Type::Map(bk, bv)) => {
            Type::Map(Box::new(refine(ak, bk)), Box::new(refine(av, bv)))
        }
        (Type::Tuples(a), Type::Tuples(b)) if a.len() == b.len() => {
            Type::Tuples(a.iter().zip(b).map(|(a, b)| refine(a, b)).collect())
        }
        _ => current.clone(),
    }
}

// Silex escapes the next character literally. Rust/JSON's \n and \u escapes
// would change the value; retain actual control characters in the literal.
fn quoted(value: &str) -> String {
    format!("\"{}\"", value.replace('\\', "\\\\").replace('"', "\\\""))
}

pub(crate) fn primitive(value: &Primitive) -> Result<Expr, String> {
    let (text, ty) = match value {
        Primitive::Null => ("null".into(), Type::Optional(Box::new(Type::Any))),
        Primitive::Boolean(v) => (v.to_string(), Type::Bool),
        Primitive::U8(v) => (format!("{v}u8"), Type::U8),
        Primitive::U16(v) => (format!("{v}u16"), Type::U16),
        Primitive::U32(v) => (format!("{v}u32"), Type::U32),
        Primitive::U64(v) => (format!("{v}u64"), Type::U64),
        Primitive::U128(v) => (format!("{v}u128"), Type::U128),
        Primitive::U256(v) => (format!("{v}u256"), Type::U256),
        Primitive::String(v) => (quoted(v), Type::String),
        Primitive::Range(v) => {
            let start = primitive(&v.0)?;
            let end = primitive(&v.1)?;
            (
                format!("({}..{})", start.text, end.text),
                Type::Range(Box::new(start.ty)),
            )
        }
        Primitive::Opaque(_) => return Err("opaque constant has no source literal".into()),
    };
    Ok(Expr::new(text, ty))
}

pub(crate) fn literal(value: &ValueCell, depth: usize) -> Result<Expr, String> {
    if depth > 128 {
        return Err("constant nesting exceeds 128".into());
    }
    Ok(match value {
        ValueCell::Primitive(v) => primitive(v)?,
        ValueCell::Bytes(v) => {
            let text = std::str::from_utf8(v)
                .map_err(|_| "non-UTF-8 bytes have no byte-string literal")?;
            Expr::new(format!("b{}", quoted(text)), Type::Bytes)
        }
        ValueCell::Object(values) => Expr::object(
            values
                .iter()
                .map(|v| literal(v.as_ref(), depth + 1))
                .collect::<Result<_, _>>()?,
        ),
        ValueCell::Map(values) => {
            let entries = values
                .iter()
                .map(|(k, v)| Ok((literal(k, depth + 1)?, literal(v.as_ref(), depth + 1)?)))
                .collect::<Result<Vec<_>, String>>()?;
            map(entries)
        }
    })
}

pub(crate) fn map(entries: Vec<(Expr, Expr)>) -> Expr {
    let (key, value) = entries
        .first()
        .map(|(k, v)| (k.ty.clone(), v.ty.clone()))
        .unwrap_or((Type::Any, Type::Any));
    let text = entries
        .iter()
        .map(|(k, v)| format!("{}: {}", k.text, v.text))
        .collect::<Vec<_>>()
        .join(", ");
    Expr::new(
        format!("{{{text}}}"),
        Type::Map(Box::new(key), Box::new(value)),
    )
    .pending()
}

pub(crate) fn unpack<M>(
    ty: &TypePacked,
    env: &EnvironmentBuilder<M>,
    depth: usize,
) -> Result<Type, String> {
    if depth > 128 {
        return Err("type nesting exceeds 128".into());
    }
    let inner = |ty| unpack(ty, env, depth + 1);
    let number = |n: &NumberType| match n {
        NumberType::U8 => Type::U8,
        NumberType::U16 => Type::U16,
        NumberType::U32 => Type::U32,
        NumberType::U64 => Type::U64,
        NumberType::U128 => Type::U128,
        NumberType::U256 => Type::U256,
    };
    Ok(match ty {
        TypePacked::Any => Type::Any,
        TypePacked::Number(n) => number(n),
        TypePacked::String => Type::String,
        TypePacked::Bool => Type::Bool,
        TypePacked::Bytes => Type::Bytes,
        TypePacked::Optional(t) => Type::Optional(Box::new(inner(t)?)),
        TypePacked::Array(t) => Type::Array(Box::new(inner(t)?)),
        TypePacked::Range(n) => Type::Range(Box::new(number(n))),
        TypePacked::Map(k, v) => Type::Map(Box::new(inner(k)?), Box::new(inner(v)?)),
        TypePacked::Tuples(types) => {
            Type::Tuples(types.iter().map(inner).collect::<Result<_, _>>()?)
        }
        TypePacked::Opaque(id) => Type::Opaque(
            env.get_opaque_manager()
                .get_by_id(id)
                .ok_or("unknown opaque type ID")?
                .clone(),
        ),
        TypePacked::OneOf(_) => {
            return Err(
                "packed enum has no retained source type; supply a function signature".into(),
            )
        }
    })
}
