mod path;

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt,
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut},
    ptr
};
use crate::{opaque::OpaqueWrapper, EnumValueType, Opaque, StructType, Type, U256};
use super::{Constant, SubValue, Value, ValueError};

pub use path::*;

// Give inner mutability for values with inner types.
// This is NOT thread-safe due to the RefCell usage.
#[derive(Debug, Clone, Eq)]
pub enum ValueCell {
    Default(Value),
    Struct(Vec<SubValue>, StructType),
    Array(Vec<SubValue>),
    Optional(Option<SubValue>),

    // Map cannot be used as a key in another map
    Map(HashMap<ValueCell, SubValue>),
    Enum(Vec<SubValue>, EnumValueType),
}

impl PartialEq for ValueCell {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Default(a), Self::Default(b)) => a == b,
            (Self::Struct(a, _), Self::Struct(b, _)) => a == b,
            (Self::Array(a), Self::Array(b)) => a == b,
            (Self::Optional(a), Self::Optional(b)) => a == b,

            // Support null comparison
            (Self::Optional(a), Self::Default(Value::Null)) => a.is_none(),
            (Self::Default(Value::Null), Self::Optional(b)) => b.is_none(),

            (Self::Map(a), Self::Map(b)) => a == b,
            (Self::Enum(a, _), Self::Enum(b, _)) => a == b,
            _ => false
        }
    }
}

// Wrapper to drop the value without stackoverflow
#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct ValueCellWrapper(pub ValueCell);

impl Drop for ValueCellWrapper {
    fn drop(&mut self) {
        if matches!(self.0, ValueCell::Default(_)) {
            return
        }

        let mut stack = vec![std::mem::take(&mut self.0)];
        while let Some(value) = stack.pop() {
            match value {
                ValueCell::Default(_) => {},
                ValueCell::Struct(fields, _) => stack.extend(fields.into_iter().map(SubValue::into_owned)),
                ValueCell::Array(values) => stack.extend(values.into_iter().map(SubValue::into_owned)),
                ValueCell::Optional(opt) => {
                    if let Some(value) = opt {
                        stack.push(value.into_owned());
                    }
                },
                ValueCell::Map(map) => stack.extend(map.into_iter().flat_map(|(k, v)| [k, v.into_owned()])),
                ValueCell::Enum(fields, _) => stack.extend(fields.into_iter().map(SubValue::into_owned)),
            }
        }
    }
}

impl Deref for ValueCellWrapper {
    type Target = ValueCell;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ValueCellWrapper {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Hash for ValueCell {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash_with_pointers(state, &mut HashSet::new());
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
            Constant::Struct(fields, _type) => Self::Struct(fields.into_iter().map(|v| v.into()).collect(), _type),
            Constant::Array(values) => Self::Array(values.into_iter().map(|v| v.into()).collect()),
            Constant::Optional(value) => Self::Optional(value.map(|v| (*v).into())),
            Constant::Map(map) => Self::Map(map.into_iter().map(|(k, v)| (k.into(), v.into())).collect()),
            Constant::Enum(fields, _type) => Self::Enum(fields.into_iter().map(|v| v.into()).collect(), _type),
        }
    }
}

impl ValueCell {
    pub(crate) fn hash_with_pointers<H: Hasher>(&self, state: &mut H, tracked_pointers: &mut HashSet<*const Self>) {
        if !tracked_pointers.insert(ptr::from_ref(self)) {
            // Cyclic reference detected
            return;
        }

        match self {
            ValueCell::Default(v) => {
                v.hash(state);
            },
            ValueCell::Struct(fields, _) => {
                12u8.hash(state);
                fields.iter()
                    .for_each(|field| field.borrow()
                        .hash_with_pointers(state, tracked_pointers)
                    );
            },
            ValueCell::Array(array) => {
                13u8.hash(state);
                array.iter()
                    .for_each(|field| field.borrow()
                        .hash_with_pointers(state, tracked_pointers)
                    );
            },
            ValueCell::Optional(v) => {
                14u8.hash(state);
                if let Some(v) = v {
                    v.borrow()
                        .hash_with_pointers(state, tracked_pointers);
                } else {
                    Self::Default(Value::Null).hash(state);
                }
            },
            ValueCell::Map(map) => {
                15u8.hash(state);
                map.iter()
                    .for_each(|(k, v)| {
                        k.hash(state);
                        v.borrow()
                            .hash_with_pointers(state, tracked_pointers);
                    });
            },
            ValueCell::Enum(fields, _) => {
                16u8.hash(state);
                fields.iter()
                    .for_each(|field| field.borrow()
                        .hash_with_pointers(state, tracked_pointers)
                    );
            },
        }
    }

    // Calculate the depth of the value
    pub fn calculate_depth(&self, max_depth: usize) -> Result<usize, ValueError> {
        // Prevent allocation if the value is a default value
        if matches!(self, Self::Default(_)) {
            return Ok(0);
        }

        let mut stack = vec![(Path::Borrowed(self), 0)];
        let mut biggest_depth = 0;

        while let Some((next, depth)) = stack.pop() {
            if depth > max_depth {
                return Err(ValueError::MaxDepthReached);
            }

            if depth > biggest_depth {
                biggest_depth = depth;
            }

            let handle = next.as_ref();
            let value = handle.as_value();
            match value {
                ValueCell::Default(_) => {},
                ValueCell::Array(values) => {
                    for value in values {
                        stack.push((Path::Wrapper(value.clone()), depth + 1));
                    }
                },
                ValueCell::Struct(fields, _) => {
                    for field in fields {
                        stack.push((Path::Wrapper(field.clone()), depth + 1));
                    }
                },
                ValueCell::Optional(opt) => {
                    if let Some(value) = opt {
                        stack.push((Path::Wrapper(value.clone()), depth + 1));
                    }
                },
                ValueCell::Map(map) => {
                    for (k, v) in map {
                        stack.push((Path::Owned(k.clone()), depth + 1));
                        stack.push((Path::Wrapper(v.clone()), depth + 1));
                    }
                },
                ValueCell::Enum(fields, _) => {
                    for field in fields {
                        stack.push((Path::Wrapper(field.clone()), depth + 1));
                    }
                },
            };
        }

        Ok(biggest_depth)
    }

    #[inline]
    pub fn is_null(&self) -> bool {
        match &self {
            Self::Default(Value::Null) => true,
            Self::Optional(opt) => opt.is_none(),
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
    pub fn as_string(&self) -> Result<&String, ValueError> {
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
    pub fn as_map(&self) -> Result<&HashMap<Self, SubValue>, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedStruct)
        }
    }

    #[inline]
    pub fn as_mut_map(&mut self) -> Result<&mut HashMap<Self, SubValue>, ValueError> {
        match self {
            Self::Map(map) => Ok(map),
            _ => Err(ValueError::ExpectedStruct),
        }
    }

    #[inline]
    pub fn as_vec<'a>(&'a self) -> Result<&'a Vec<SubValue>, ValueError> {
        match self {
            Self::Array(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn as_mut_vec<'a>(&'a mut self) -> Result<&'a mut Vec<SubValue>, ValueError> {
        match self {
            Self::Array(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn take_as_optional(&mut self) -> Option<Self> {
        match self {
            Self::Default(Value::Null) => None,
            Self::Optional(opt) => opt.take().map(SubValue::into_owned),
            v => {
                let value = std::mem::take(v);
                Some(value)
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
    pub fn to_map(self) -> Result<Vec<SubValue>, ValueError> {
        match self {
            Self::Struct(fields, _) => Ok(fields),
            _ => Err(ValueError::ExpectedStruct)
        }
    }

    #[inline]
    pub fn to_vec(self) -> Result<Vec<SubValue>, ValueError> {
        match self {
            Self::Array(n) => Ok(n),
            _ => Err(ValueError::ExpectedValueOfType(Type::Array(Box::new(Type::Any))))
        }
    }

    #[inline]
    pub fn to_sub_vec(self) -> Result<Vec<SubValue>, ValueError> {
        match self {
            Self::Array(values) => Ok(values),
            Self::Struct(fields, _) => Ok(fields),
            _ => Err(ValueError::SubValue)
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
    pub fn as_sub_vec(&self) -> Result<&Vec<SubValue>, ValueError> {
        match self {
            Self::Array(values) => Ok(values),
            Self::Struct(fields, _) => Ok(fields),
            _ => Err(ValueError::SubValue)
        }
    }

    #[inline]
    pub fn as_mut_sub_vec(&mut self) -> Result<&mut Vec<SubValue>, ValueError> {
        match self {
            Self::Array(values) => Ok(values),
            Self::Struct(fields, _) => Ok(fields),
            _ => Err(ValueError::SubValue)
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
                    return Ok(Self::Optional(None))
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
    pub fn into_owned(self) -> Self {
        match self {
            Self::Default(v) => Self::Default(v),
            Self::Struct(fields, _type) => {
                let mut new_fields = Vec::with_capacity(fields.len());
                for field in fields {
                    new_fields.push(field.into_owned().into());
                }
                Self::Struct(new_fields, _type)
            },
            Self::Array(values) => {
                let mut new_values = Vec::with_capacity(values.len());
                for value in values {
                    new_values.push(value.into_owned().into());
                }
                Self::Array(new_values)
            },
            Self::Optional(value) => Self::Optional(value.map(|v| v.into_owned().into())),
            Self::Map(map) => {
                let mut new_map = HashMap::with_capacity(map.len());
                for (k, v) in map {
                    new_map.insert(k.into_owned(), v.into_owned().into());
                }
                Self::Map(new_map)
            },
            Self::Enum(fields, _type) => {
                let mut new_fields = Vec::with_capacity(fields.len());
                for field in fields {
                    new_fields.push(field.into_owned().into());
                }
                Self::Enum(new_fields, _type)
            }
        }
    }
}

impl fmt::Display for ValueCell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Default(v) => write!(f, "{}", v),
            Self::Struct(fields, _type) => {
                let s: Vec<String> = fields.iter().enumerate().map(|(k, v)| format!("{}: {}", k, v.borrow())).collect();
                write!(f, "{:?} {} {} {}", _type, "{", s.join(", "), "}")
            },
            Self::Array(values) => {
                let s: Vec<String> = values.iter().map(|v| format!("{}", v.borrow())).collect();
                write!(f, "[{}]", s.join(", "))
            },
            Self::Optional(value) => match value.as_ref() {
                Some(value) => write!(f, "optional<{}>", value.borrow().to_string()),
                None => write!(f, "optional<null>")
            },
            Self::Map(map) => {
                let s: Vec<String> = map.iter().map(|(k, v)| format!("{}: {}", k, v.borrow())).collect();
                write!(f, "map{}{}{}", "{", s.join(", "), "}")
            },
            Self::Enum(fields, enum_type) => {
                let s: Vec<String> = fields.iter().enumerate().map(|(k, v)| format!("{}: {}", k, v.borrow())).collect();
                write!(f, "enum{:?} {} {} {}", enum_type, "{", s.join(", "), "}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::SubValue;

    use super::*;

    #[test]
    fn test_max_depth() {
        let mut map = ValueCell::Map(HashMap::new());
        for _ in 0..100 {
            let mut inner_map = HashMap::new();
            inner_map.insert(Value::U8(10).into(), SubValue::new(map));
            map = ValueCell::Map(inner_map);
        }

        assert!(matches!(map.calculate_depth(100), Ok(100)));
        assert!(matches!(map.calculate_depth(99), Err(ValueError::MaxDepthReached)));
    }

    #[test]
    fn test_recursive_cycle() {
        // Create a map that contains itself
        let map = SubValue::new(ValueCell::Map(HashMap::new()));
        {
            let mut m = map.borrow_mut();
            m.as_mut_map()
                .unwrap()
                .insert(Value::U8(10).into(), map.reference());
        }

        let owned = map.into_owned();
        let mut inner_map = HashMap::new();
        inner_map.insert(ValueCellWrapper(owned), Value::U8(10));
    }

    #[test]
    fn test_std_hash() {
        // Create a map that contains a map that contains a map...
        let mut map = ValueCell::Map(HashMap::new());
        for _ in 0..28000 {
            let mut inner_map = HashMap::new();
            inner_map.insert(Value::U8(10).into(), map.into());
            map = ValueCell::Map(inner_map);
        }

        println!("Map");
        let mut hasher = std::collections::hash_map::DefaultHasher::new();
        println!("hash");
        ValueCellWrapper(map).hash(&mut hasher);
        println!("Hash: {}", hasher.finish());
    }
}