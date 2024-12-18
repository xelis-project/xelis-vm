pub trait Serializable {
    // Expected size of the type when serialized
    // Even if is_serializable returns false, this function should return the size of the type
    fn get_size(&self) -> usize {
        8
    }

    // Serialize the type to a buffer
    // The buffer may be pre-allocated and the function should return the number of bytes written
    fn serialize(&self, _: &mut Vec<u8>) -> usize {
        0
    }

    // Check if the type is supported by the serialization
    fn is_serializable(&self) -> bool {
        true
    }
}