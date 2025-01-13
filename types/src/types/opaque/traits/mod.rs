mod any;
mod eq;
mod hash;
mod json;
mod serializable;
mod r#type;

pub use any::*;
pub use eq::*;
pub use hash::*;
pub use json::*;
pub use serializable::*;
pub use r#type::*;

#[macro_export]
macro_rules! impl_opaque {
    ($name:expr, $type:ty) => {
        impl $crate::opaque::traits::DynType for $type {
            fn get_type_name(&self) -> &'static str {
                $name
            }

            fn get_type(&self) -> std::any::TypeId {
                std::any::TypeId::of::<$type>()
            }
        }

        impl $crate::opaque::Opaque for $type {
            fn clone_box(&self) -> Box<dyn $crate::opaque::Opaque> {
                Box::new(self.clone())
            }

            fn display(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", $name)
            }
        }
    };
    // If true is added, impl json helper
    ($name:expr, $type:ty, json) => {
        impl_opaque!($name, $type);
        impl $crate::opaque::traits::JSONHelper for $type {
            fn serialize_json(&self) -> Result<serde_json::Value, anyhow::Error> {
                Ok(serde_json::to_value(self)?)
            }

            fn is_json_supported(&self) -> bool {
                true
            }
        }
    };
}
