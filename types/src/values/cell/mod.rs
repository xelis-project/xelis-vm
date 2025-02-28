mod stack_value;

use std::{
    borrow::Cow,
    collections::HashMap,
    fmt,
    hash::{Hash, Hasher}
};
use crate::{opaque::OpaqueWrapper, Opaque, Type, U256};
use super::{Constant, Value, ValueError};

pub use stack_value::*;

// Give inner mutability for values with inner types.
// This is NOT thread-safe due to the RefCell usage.
#[derive(Debug, Clone, Eq)]
pub enum ValueCell {
    Default(Value),
    Array(Vec<ValueCell>),

    // Map cannot be used as a key in another map
    // Key must be immutable also!
    Map(HashMap<ValueCell, ValueCell>),
}

impl PartialEq for ValueCell {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Default(a), Self::Default(b)) => a == b,
            (Self::Array(a), Self::Array(b)) => a == b,

            (Self::Map(a), Self::Map(b)) => a == b,
            _ => false
        }
    }
}

impl Hash for ValueCell {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Fast path
        if let Self::Default(v) = self {
            v.hash(state);
            return;
        }

        let mut stack = vec![self];
        while let Some(value) = stack.pop() {
            match value {
                Self::Default(v) => v.hash(state),
                Self::Array(values) => {
                    12u8.hash(state);
                    stack.extend(values);
                },
                Self::Map(map) => {
                    13u8.hash(state);
                    stack.extend(map.iter().flat_map(|(k, v)| [k, v]))
                }
            }
        }
    }
}

impl Default for ValueCell {
    fn default() -> Self {
        Self::Default(Default::default())
    }
}

impl From<Value> for ValueCell {
    fn from(value: Value) -> Self {
        Self::Default(value)
    }
}

impl From<Constant> for ValueCell {
    fn from(value: Constant) -> Self {
        match value {
            Constant::Default(v) => Self::Default(v),
            Constant::Array(values) => Self::Array(values.into_iter().map(|v| v.into()).collect()),
            Constant::Map(map) => Self::Map(map.into_iter().map(|(k, v)| (k.into(), v.into())).collect()),
            Constant::Typed(values, _) => Self::Array(values.into_iter().map(|v| v.into()).collect())
        }
    }
}

impl ValueCell {
    // Calculate the depth of the value
    pub fn calculate_depth<'a>(&'a self, max_depth: usize) -> Result<usize, ValueError> {
        // Prevent allocation if the value is a default value
        if matches!(self, Self::Default(_)) {
            return Ok(0);
        }

        let mut stack = vec![(self, 0)];
        let mut biggest_depth = 0;

        while let Some((next, depth)) = stack.pop() {
            if depth > max_depth {
                return Err(ValueError::MaxDepthReached);
            }

            if depth > biggest_depth {
                biggest_depth = depth;
            }

            match next {
                ValueCell::Default(_) => {},
                ValueCell::Array(values) => {
                    for value in values {
                        stack.push((value, depth + 1));
                    }
                },
                ValueCell::Map(map) => {
                    for (_, v) in map {
                        stack.push((v, depth + 1));
                    }
                }
            };
        }

        Ok(biggest_depth)
    }

    #[inline]
    pub fn is_null(&self) -> bool {
        match &self {
            Self::Default(Value::Null) => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_string(&self) -> bool {
        match &self {
            Self::Default(Value::String(_)) => true,
            _ => false
        }
    }

    #[inline]
    pub fn is_map(&self) -> bool {
        match &self {
            Self::Map(_) => true,
            _ => false
        }
    }

    #[inline]
    pub fn as_u8(&self) -> Result<u8, ValueError> {
        match self {
            Self::Default(Value::U8(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U8))
        }
    }

    #[inline]
    pub fn as_u16(&self) -> Result<u16, ValueError> {
        match self {
            Self::Default(Value::U16(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U16))
        }
    }

    #[inline]
    pub fn as_u32(&self) -> Result<u32, ValueError> {
        match self {
            Self::Default(Value::U32(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U32))
        }
    }

    #[inline]
    pub fn as_u64(&self) -> Result<u64, ValueError> {
        match self {
            Self::Default(Value::U64(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U64))
        }
    }

    #[inline]
    pub fn as_u128(&self) -> Result<u128, ValueError> {
        match self {
            Self::Default(Value::U128(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U128))
        }
    }

    #[inline]
    pub fn as_u256(&self) -> Result<U256, ValueError> {
        match self {
            Self::Default(Value::U256(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U256))
        }
    }

    #[inline]
    pub fn as_string(&self) -> Result<&str, ValueError> {
        match self {
            Self::Default(Value::String(n)) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::String))
        }
    }

    #[inline]
    pub fn as_bool(&self) -> Result<bool, ValueError> {
        match self {
            Self::Default(Value::Boolean(n)) => Ok(*n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Bool))
        }
    }

    #[inline]
    pub fn as_map(&self) -> Result<&HashMap<ValueCell, ValueCell>, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedStruct)
        }
    }

    #[inline]
    pub fn as_mut_map(&mut self) -> Result<&mut HashMap<ValueCell, ValueCell>, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedStruct),
        }
    }

    #[inline]
    pub fn as_vec<'a>(&'a self) -> Result<&'a Vec<ValueCell>, ValueError> {
        match self {
            Self::Array(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn as_mut_vec<'a>(&'a mut self) -> Result<&'a mut Vec<ValueCell>, ValueError> {
        match self {
            Self::Array(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn take_as_optional(&mut self) -> Result<Option<Self>, ValueError> {
        match self {
            Self::Default(Value::Null) => Ok(None),
            v => {
                let value = std::mem::take(v);
                Ok(Some(value))
            }
        }
    }

    #[inline]
    pub fn to_u8(self) -> Result<u8, ValueError> {
        match self {
            Self::Default(Value::U8(n)) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U8))
        }
    }

    #[inline]
    pub fn to_u16(self) -> Result<u16, ValueError> {
        match self {
            Self::Default(Value::U16(n)) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U16))
        }
    }

    #[inline]
    pub fn to_u32(self) -> Result<u32, ValueError> {
        match self {
            Self::Default(Value::U32(n)) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U32))
        }
    }

    #[inline]
    pub fn to_u64(self) -> Result<u64, ValueError> {
        match self {
            Self::Default(Value::U64(n)) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U64))
        }
    }

    #[inline]
    pub fn to_u128(self) -> Result<u128, ValueError> {
        match self {
            Self::Default(Value::U128(n)) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U128))
        }
    }

    #[inline]
    pub fn to_u256(self) -> Result<U256, ValueError> {
        match self {
            Self::Default(Value::U256(n)) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::U256))
        }
    }

    #[inline]
    pub fn to_string(self) -> Result<String, ValueError> {
        match self {
            Self::Default(Value::String(n)) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::String))
        }
    }

    #[inline]
    pub fn to_bool(self) -> Result<bool, ValueError> {
        match self {
            Self::Default(Value::Boolean(n)) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Bool))
        }
    }

    #[inline]
    pub fn to_vec(self) -> Result<Vec<ValueCell>, ValueError> {
        match self {
            Self::Array(values) => Ok(values),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn to_opaque(self) -> Result<OpaqueWrapper, ValueError> {
        match self {
            Self::Default(Value::Opaque(opaque)) => Ok(opaque),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_opaque(&self) -> Result<&OpaqueWrapper, ValueError> {
        match self {
            Self::Default(Value::Opaque(opaque)) => Ok(opaque),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_opaque_mut(&mut self) -> Result<&mut OpaqueWrapper, ValueError> {
        match self {
            Self::Default(Value::Opaque(opaque)) => Ok(opaque),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_opaque_type<T: Opaque>(&self) -> Result<&T, ValueError> {
        match self {
            Self::Default(Value::Opaque(opaque)) => opaque.as_ref::<T>(),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn as_opaque_type_mut<T: Opaque>(&mut self) -> Result<&mut T, ValueError> {
        match self {
            Self::Default(Value::Opaque(opaque)) => opaque.as_mut::<T>(),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    pub fn into_opaque_type<T: Opaque>(self) -> Result<T, ValueError> {
        match self {
            Self::Default(Value::Opaque(opaque)) => opaque.into_inner::<T>(),
            _ => Err(ValueError::ExpectedOpaque)
        }
    }

    #[inline]
    pub fn is_serializable(&self) -> bool {
        match self {
            Self::Default(Value::Opaque(op)) => op.is_serializable(),
            _ => true
        }
    }

    #[inline]
    pub fn as_range(&self) -> Result<(&Value, &Value, &Type), ValueError> {
        self.as_value().and_then(Value::as_range)
    }

    #[inline]
    pub fn to_range(self) -> Result<(Value, Value, Type), ValueError> {
        self.into_value().and_then(Value::to_range)
    }

    // Check if the value is a number
    #[inline]
    pub fn is_number(&self) -> bool {
        match self {
            Self::Default(v) => v.is_number(),
            _ => false
        }
    }

    // Increment the value
    pub fn increment(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Self::Default(v) => v.increment()?,
            _ => return Err(ValueError::OperationNotNumberType)
        })
    }

    // Decrement the value
    pub fn decrement(&mut self) -> Result<(), ValueError> {
        Ok(match self {
            Self::Default(v) => v.decrement()?,
            _ => return Err(ValueError::OperationNotNumberType)
        })
    }

    // Cast value to string
    #[inline]
    pub fn cast_to_string(self) -> Result<String, ValueError> {
        match self {
            Self::Default(v) => v.cast_to_string(),
            _ => Err(ValueError::InvalidCastType(Type::String))
        }
    }

    // Transform a value to a string
    #[inline]
    pub fn as_string_formatted<'a>(&'a self) -> Result<Cow<'a, str>, ValueError> {
        match self {
            Self::Default(v) => v.as_string_formatted(),
            _ => Err(ValueError::ExpectedValueOfType(Type::String))
        }
    }

    // Cast the value to the expected type
    pub fn mut_checked_cast_to_primitive_type(&mut self, expected: &Type) -> Result<(), ValueError> {
        let take = std::mem::take(self);
        let value = take.checked_cast_to_primitive_type(expected)?;
        *self = value;
        Ok(())
    }

    // Cast without loss in the expected type
    #[inline]
    pub fn checked_cast_to_primitive_type(self, expected: &Type) -> Result<Self, ValueError> {
        match expected {
            Type::U8 => self.checked_cast_to_u8().map(Value::U8),
            Type::U16 => self.checked_cast_to_u16().map(Value::U16),
            Type::U32 => self.checked_cast_to_u32().map(Value::U32),
            Type::U64 => self.checked_cast_to_u64().map(Value::U64),
            Type::U128 => self.checked_cast_to_u128().map(Value::U128),
            Type::U256 => self.checked_cast_to_u256().map(Value::U256),
            Type::String => self.cast_to_string().map(Value::String),
            Type::Bool => self.cast_to_bool().map(Value::Boolean),
            Type::Optional(inner) => {
                if self.is_null() {
                    return Ok(self)
                } else {
                    return self.checked_cast_to_primitive_type(inner)
                }
            },
            Type::Range(inner) => {
                let (start, end, _) = self.to_range()?;
                let start = start.checked_cast_to_primitive_type(inner)?;
                let end = end.checked_cast_to_primitive_type(inner)?;
                Ok(Value::Range(Box::new(start), Box::new(end), *inner.clone()))
            },
            _ => Err(ValueError::InvalidCastType(expected.clone()))
        }.map(Self::Default)
    }

    // Cast to u8, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u8(self) -> Result<u8, ValueError> {
        self.into_value().and_then(Value::checked_cast_to_u8)
    }

    // Cast to u16, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u16(self) -> Result<u16, ValueError> {
        self.into_value().and_then(Value::checked_cast_to_u16)
    }

    // Cast to u32, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u32(self) -> Result<u32, ValueError> {
        self.into_value().and_then(Value::checked_cast_to_u32)
    }

    // Cast to u64, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u64(self) -> Result<u64, ValueError> {
        self.into_value().and_then(Value::checked_cast_to_u64)
    }

    // Cast to u128, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u128(self) -> Result<u128, ValueError> {
        self.into_value().and_then(Value::checked_cast_to_u128)
    }

    // Cast to u256, return an error if value is too big
    #[inline]
    pub fn checked_cast_to_u256(self) -> Result<U256, ValueError> {
        self.into_value().and_then(Value::checked_cast_to_u256)
    }

    // Cast value to bool
    #[inline]
    pub fn cast_to_bool(self) -> Result<bool, ValueError> {
        self.into_value().and_then(Value::cast_to_bool)
    }

    // Cast value to u8
    #[inline]
    pub fn cast_to_u8(self) -> Result<u8, ValueError> {
        self.into_value().and_then(Value::cast_to_u8)
    }

    // Cast value to u16
    #[inline]
    pub fn cast_to_u16(self) -> Result<u16, ValueError> {
        self.into_value().and_then(Value::cast_to_u16)
    }

    // Cast value to u32
    #[inline]
    pub fn cast_to_u32(self) -> Result<u32, ValueError> {
        self.into_value().and_then(Value::cast_to_u32)
    }

    // Cast value to u64
    #[inline]
    pub fn cast_to_u64(self) -> Result<u64, ValueError> {
        self.into_value().and_then(Value::cast_to_u64)
    }

    // Cast value to u128
    #[inline]
    pub fn cast_to_u128(self) -> Result<u128, ValueError> {
        self.into_value().and_then(Value::cast_to_u128)
    }

    // Cast value to u256
    #[inline]
    pub fn cast_to_u256(self) -> Result<U256, ValueError> {
        self.into_value().and_then(Value::cast_to_u256)
    }

    #[inline(always)]
    pub fn as_value(&self) -> Result<&Value, ValueError> {
        match self {
            Self::Default(v) => Ok(v),
            _ => Err(ValueError::ExpectedValueOfType(Type::Any))
        }
    }

    #[inline(always)]
    pub fn into_value(self) -> Result<Value, ValueError> {
        match self {
            Self::Default(v) => Ok(v),
            _ => Err(ValueError::ExpectedValueOfType(Type::Any))
        }
    }

    // Clone all the SubValue into a new ValueCell
    // We need to do it in iterative way to prevent any stackoverflow
    pub fn into_owned(self) -> Result<Self, ValueError> {
        if matches!(self, Self::Default(_)) {
            return Ok(self)
        }

        #[derive(Debug)]
        enum QueueItem {
            Value(Value),
            Array {
                len: usize,
            },
            Map {
                len: usize,
            }
        }

        let mut stack = vec![self];
        let mut queue = Vec::new();
        // let mut pointers = HashSet::new();

        // Disassemble
        while let Some(value) = stack.pop() {
            match value {
                Self::Default(v) => queue.push(QueueItem::Value(v)),
                Self::Array(values) => {
                    queue.push(QueueItem::Array { len: values.len() });
                    for value in values.into_iter().rev() {
                        stack.push(value);
                    }
                },
                Self::Map(map) => {
                    queue.push(QueueItem::Map { len: map.len() });
                    for (k, v) in map.into_iter() {
                        stack.push(k);
                        stack.push(v);
                    }
                }
            }
        };

        // Assemble back
        while let Some(item) = queue.pop() {
            match item {
                QueueItem::Value(v) => {
                    stack.push(ValueCell::Default(v));
                },
                QueueItem::Array { len } => {
                    let mut values = Vec::with_capacity(len);
                    for _ in 0..len {
                        values.push(
                            stack.pop()
                                .ok_or(ValueError::ExpectedValue)?
                                .into()
                        );
                    }
                    stack.push(ValueCell::Array(values));
                },
                QueueItem::Map { len } => {
                    let mut map = HashMap::with_capacity(len);
                    for _ in 0..len {
                        let value = stack.pop().ok_or(ValueError::ExpectedValue)?;
                        let key = stack.pop().ok_or(ValueError::ExpectedValue)?;
                        map.insert(key, value.into());
                    }
                    stack.push(ValueCell::Map(map));
                }
            }
        }

        stack.pop().ok_or(ValueError::ExpectedValue)
    }
}

impl fmt::Display for ValueCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Default(v) => write!(f, "{}", v),
            Self::Array(values) => {
                let s: Vec<String> = values.iter().map(|v| format!("{}", v)).collect();
                write!(f, "[{}]", s.join(", "))
            },
            Self::Map(map) => {
                let s: Vec<String> = map.iter().map(|(k, v)| format!("{}: {}", k, v)).collect();
                write!(f, "map{}{}{}", "{", s.join(", "), "}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_max_depth() {
        let mut map = ValueCell::Map(HashMap::new());
        for _ in 0..100 {
            let mut inner_map = HashMap::new();
            inner_map.insert(Value::U8(10).into(), map.into());
            map = ValueCell::Map(inner_map);
        }

        assert!(matches!(map.calculate_depth(100), Ok(100)));
        assert!(matches!(map.calculate_depth(99), Err(ValueError::MaxDepthReached)));
    }

    #[test]
    fn test_into_owned() {
        let array = ValueCell::Array(vec![
            ValueCell::Default(Value::U8(10)).into(),
            ValueCell::Default(Value::U8(20)).into(),
            ValueCell::Default(Value::U8(30)).into(),
        ]);

        assert_eq!(array, array.clone().into_owned().unwrap());
    }

    #[test]
    fn test_std_hash() {
        // Create a map that contains a map that contains a map...
        let mut map = ValueCell::Map(HashMap::new());
        for _ in 0..100000 {
            let mut inner_map = HashMap::new();
            inner_map.insert(Value::U8(10).into(), map.into());
            map = ValueCell::Map(inner_map);
        }

        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        map.hash(&mut hasher);

        // TODO: drop correctly the map without stack overflow
        std::mem::forget(map);
    }
}