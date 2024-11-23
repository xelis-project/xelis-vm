use xelis_types::{Path, Type, Value, ValueCell, ValueError};

#[derive(Debug)]
pub struct PathIterator<'a> {
    inner: Path<'a>,
    index: Value,
}

impl<'a> PathIterator<'a> {
    pub fn new(inner: Path<'a>) -> Result<Self, ValueError> {
        let index = match inner.as_ref().as_value() {
            ValueCell::Default(Value::Range(_, _, index_type)) => match index_type {
                Type::U8 => Value::U8(0),
                Type::U16 => Value::U16(0),
                Type::U32 => Value::U32(0),
                Type::U64 => Value::U64(0),
                Type::U128 => Value::U128(0),
                Type::U256 => Value::U256(0u64.into()),
                _ => return Err(ValueError::InvalidPrimitiveType),
            },
            _ => Value::U32(0),
        };

        Ok(PathIterator { inner, index })
    }

    pub fn next(&mut self) -> Result<Option<Path<'a>>, ValueError> {
        let index = self.index.clone();
        self.index.increment()?;

        let value = self.inner.as_ref();
        Ok(match value.as_value() {
            ValueCell::Array(v) => {
                let index = index.to_u32()? as usize;
                v.get(index)
                .map(|v| Path::Wrapper(v.reference()))
            },
            ValueCell::Default(Value::Range(start, end, _type)) => {
                if index >= **start && index < **end {
                    Some(Path::Owned(ValueCell::Default(index)))
                } else {
                    None
                }
            },
            _ => None,
        })
    }
}