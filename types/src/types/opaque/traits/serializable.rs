/// Object-safe serialization capability for opaque values.
/// Implemented automatically for types implementing Octet's `Serializable`.
pub trait DynSerializable {
    fn get_size(&self) -> usize;
    /// Append the encoded value and return the number of bytes written.
    /// On failure the buffer may contain a partially encoded value.
    fn serialize(&self, buffer: &mut Vec<u8>) -> Result<usize, matriochka::Error>;
}

impl<T: octet::Serializable> DynSerializable for T {
    fn get_size(&self) -> usize {
        self.size()
    }

    fn serialize(&self, buffer: &mut Vec<u8>) -> Result<usize, matriochka::Error> {
        let start = buffer.len();
        let mut writer = octet::Writer::new(&mut *buffer);
        self.write(&mut writer)?;

        Ok(buffer.len() - start)
    }
}