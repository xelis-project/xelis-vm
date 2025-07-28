use xelis_types::{StackValue, Primitive, ValueCell, ValueError};

#[derive(Debug)]
pub struct ValueIterator {
    inner: StackValue,
    index: Primitive,
}

impl ValueIterator {
    pub fn new(inner: StackValue) -> Result<Self, ValueError> {
        let index = match inner.as_ref() {
            ValueCell::Default(Primitive::Range(range)) => range.0.clone(),
            _ => Primitive::U32(0),
        };

        Ok(ValueIterator { inner, index })
    }

    pub fn next(&mut self) -> Result<Option<StackValue>, ValueError> {
        let index = self.index.clone();
        self.index.increment()?;

        let value = self.inner.as_ref();
        Ok(match value {
            ValueCell::Object(v) => {
                let index = index.to_u32()? as usize;
                v.get(index)
                .map(|v| v.clone().into())
            },
            ValueCell::Bytes(v) => {
                let index = index.to_u32()? as usize;
                v.get(index)
                    .map(|v| Primitive::U8(*v).into())
            },
            ValueCell::Default(Primitive::Range(range)) => {
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