use super::{Type, Value};

#[derive(Debug, Clone)]
pub struct Variable {
    value: Value,
    value_type: Type
}

impl Variable {
    pub fn new(value: Value, value_type: Type) -> Self {
        Self {
            value,
            value_type
        }
    }

    pub fn get_value(&self) -> &Value {
        &self.value
    }

    pub fn get_mut_value(&mut self) -> &mut Value {
        &mut self.value
    }

    pub fn set_value(&mut self, value: Value) {
        self.value = value;
    }

    pub fn get_type(&self) -> &Type {
        &self.value_type
    }
}