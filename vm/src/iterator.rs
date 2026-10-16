use silex_types::{PrimitiveInteger, StackValue, Primitive, ValueCell, ValueError};

#[derive(Debug)]
pub struct ValueIterator {
    inner: StackValue,
    index: PrimitiveInteger,
}

impl ValueIterator {
    pub fn new(inner: StackValue) -> Result<Self, ValueError> {
        let index = match inner.as_ref() {
            ValueCell::Primitive(Primitive::Range(range)) => range.start(),
            _ => PrimitiveInteger::U32(0),
        };

        Ok(ValueIterator { inner, index })
    }

    pub fn next(&mut self) -> Result<Option<StackValue>, ValueError> {
        let index = self.index;
        self.index.increment();

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
                    .map(|v| Primitive::Integer(PrimitiveInteger::U8(*v)).into())
            },
            ValueCell::Primitive(Primitive::Range(range)) => {
                // Only need to check top bound, because index is init by low bound
                if index < range.end() {
                    Some(index.into())
                } else {
                    None
                }
            },
            _ => None,
        })
    }
}