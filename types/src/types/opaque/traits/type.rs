use std::any::TypeId;

pub trait DynType {
    fn get_type_name(&self) -> &'static str;

    fn get_type(&self) -> TypeId;
}