use xelis_types::Type;

#[derive(Debug)]
pub struct Hook<'a> {
    // Expected parameters of the hook function
    pub parameters: Vec<(&'a str, Type)>,
    // Expected return of the hook function
    pub return_type: Option<Type>,
    // Actual id in the final Environment
    pub hook_id: u8
}