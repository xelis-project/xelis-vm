use xelis_types::{StackValue, Type, Value, ValueCell, ValueError};

#[derive(Debug)]
pub struct PathIterator {
    inner: StackValue,
    index: Value,
}

impl PathIterator {
    pub fn new(inner: StackValue) -> Result<Self, ValueError> {
        let index = match inner.as_ref() {
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

    pub fn next(&mut self) -> Result<Option<StackValue>, ValueError> {
        let index = self.index.clone();
        self.index.increment()?;

        let value = self.inner.as_ref();
        Ok(match value {
            ValueCell::Array(v) => {
                let index = index.to_u32()? as usize;
                v.get(index)
                .map(|v| v.clone().into())
            },
            ValueCell::Default(Value::Range(start, end, _type)) => {
                if index >= **start && index < **end {
                    Some(index.into())
                } else {
                    None
                }
            },
            _ => None,
        })
    }
}