use xelis_types::{StackValue, Value, ValueCell, ValueError};

#[derive(Debug)]
pub struct ValueIterator {
    inner: StackValue,
    index: Value,
}

impl ValueIterator {
    pub fn new(inner: StackValue) -> Result<Self, ValueError> {
        let index = match inner.as_ref() {
            ValueCell::Default(Value::Range(range)) => range.0.clone(),
            _ => Value::U32(0),
        };

        Ok(ValueIterator { inner, index })
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
            ValueCell::Default(Value::Range(range)) => {
                // Only need to check top bound, because index is init by low bound
                if index < range.1 {
                    Some(index.into())
                } else {
                    None
                }
            },
            _ => None,
        })
    }
}