use silex_types::{PrimitiveInteger, StackValue, Primitive, RuntimeValue, ValueError, Heap};

#[derive(Debug)]
pub struct ValueIterator {
    inner: StackValue,
    index: PrimitiveInteger,
}

impl ValueIterator {
    pub fn new(inner: StackValue, heap: &Heap) -> Result<Self, ValueError> {
        let index = match inner.as_ref(heap) {
            RuntimeValue::Primitive(Primitive::Range(range)) => range.start(),
            _ => PrimitiveInteger::U32(0),
        };

        Ok(ValueIterator { inner, index })
    }

    #[inline(always)]
    pub fn next(&mut self, heap: &mut Heap) -> Result<Option<StackValue>, ValueError> {
        let index = self.index;
        self.index.increment();

        let value = self.inner.as_ref(heap);
        Ok(match value {
            RuntimeValue::Object(v) => {
                let index = index.to_u32()? as usize;
                let id = v.get(index);
                id.cloned().map(Into::into)
            },
            RuntimeValue::Bytes(v) => {
                let index = index.to_u32()? as usize;
                v.get(index)
                    .map(|v| Primitive::Integer(PrimitiveInteger::U8(*v)).into())
            },
            RuntimeValue::Primitive(Primitive::Range(range)) => {
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