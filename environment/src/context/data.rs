use std::any::Any;
use better_any::{Tid, TidExt};

// Data is a wrapper around Any that allows for borrowed and mutable references.
pub enum Data<'ty, 'r> {
    Owned(Box<dyn Tid<'ty>>),
    Borrowed(&'r dyn Tid<'ty>),
    Mut(&'r mut dyn Tid<'ty>),
}

impl<'ty, 'r> Data<'ty, 'r> {
    // downcast_any allows for immutable access to the underlying value.
    pub fn downcast_ref_any<'b, T: Any>(&'b self) -> Option<&'b T> {
        match self {
            Data::Owned(value) => (**value).downcast_any_ref(),
            Data::Borrowed(value) => (*value).downcast_any_ref(),
            Data::Mut(value) => (*value).downcast_any_ref(),
        }
    }

    // downcast_any_mut allows for mutable access to the underlying value.
    pub fn downcast_mut_any<'b, T: Any>(&'b mut self) -> Option<&'b mut T> {
        match self {
            Data::Owned(value) => (**value).downcast_any_mut(),
            Data::Mut(value) => (*value).downcast_any_mut(),
            _ => None,
        }
    }

    // downcast allows for immutable access to the underlying value.
    pub fn downcast_ref<'b, T: Tid<'ty>>(&'b self) -> Option<&'b T> {
        match self {
            Data::Owned(value) => (**value).downcast_ref(),
            Data::Borrowed(value) => (*value).downcast_ref(),
            Data::Mut(value) => (*value).downcast_ref(),
        }
    }

    // downcast_mut allows for mutable access to the underlying value.
    pub fn downcast_mut<'b, T: Tid<'ty>>(&'b mut self) -> Option<&'b mut T> {
        match self {
            Data::Owned(value) => (**value).downcast_mut(),
            Data::Mut(value) => (*value).downcast_mut(),
            _ => None,
        }
    }

    // into_owned consumes the Data and returns the underlying value or clone it if it's borrowed.
    pub fn into_owned<T: Clone + Tid<'ty>>(self) -> Result<T, Self> {
        match self {
            Data::Owned(value) => match value.downcast_box::<T>() {
                Ok(value) => Ok(*value),
                Err(v) => Err(Data::Owned(v)),
            },
            Data::Borrowed(value) => match value.downcast_ref::<T>() {
                Some(value) => Ok(value.clone()),
                None => Err(Data::Borrowed(value)),
            },
            Data::Mut(value) => match value.downcast_ref::<T>() {
                Some(value) => Ok(value.clone()),
                None => Err(Data::Mut(value)),
            },
        }
    }

    // take consumes the Data and returns the underlying value if it's owned.
    pub fn try_take_owned<T: Tid<'ty>>(self) -> Result<T, Self> {
        match self {
            Data::Owned(value) => match value.downcast_box::<T>() {
                Ok(value) => Ok(*value),
                Err(v) => Err(Data::Owned(v)),
            },
            _ => Err(self),
        }
    }
}


#[cfg(test)]
mod tests {
    use better_any::tid;

    use super::*;

    #[derive(Debug, Clone)]
    struct Test;
    tid!(Test);

    #[test]
    fn test_data_owned() {
        let mut owned = Data::Owned(Box::new(Test));

        let _ = owned.downcast_ref::<Test>().unwrap();
        let _ = owned.downcast_mut::<Test>().unwrap();
        assert!(matches!(owned.into_owned::<Test>(), Ok(Test)));

        let owned = Data::Owned(Box::new(Test));
        assert!(matches!(owned.try_take_owned::<Test>(), Ok(Test)));
    }

    #[test]
    fn test_data_ref() {
        let test = Test;
        let mut borrowed = Data::Borrowed(&test);

        let _ = borrowed.downcast_ref::<Test>().unwrap();
        assert!(borrowed.downcast_mut::<Test>().is_none());
        assert!(matches!(borrowed.into_owned::<Test>(), Ok(Test)));

        let borrowed = Data::Borrowed(&test);
        assert!(matches!(borrowed.try_take_owned::<Test>(), Err(_)));
    }

    #[test]
    fn test_data_mut() {
        let mut test = Test;
        let mut mut_ref = Data::Mut(&mut test);

        let _ = mut_ref.downcast_ref::<Test>().unwrap();
        let _ = mut_ref.downcast_mut::<Test>().unwrap();
        assert!(matches!(mut_ref.into_owned::<Test>(), Ok(Test)));

        let mut_ref = Data::Mut(&mut test);
        assert!(matches!(mut_ref.try_take_owned::<Test>(), Err(_)));
    }
}