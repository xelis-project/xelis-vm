use crate::{values::ValueError, Path};

pub struct PathIterator<'a> {
    inner: Path<'a>,
    index: usize,
}

impl<'a> PathIterator<'a> {
    pub fn new(inner: Path<'a>) -> Self {
        PathIterator { inner, index: 0 }
    }

    pub fn next(&mut self) -> Result<Option<Path<'a>>, ValueError> {
        let index = self.index;
        self.index += 1;
        self.inner.as_ref()
            .as_vec()
            .map(|v| v.get(index).map(|v| Path::Wrapper(v.clone())))
    }
}