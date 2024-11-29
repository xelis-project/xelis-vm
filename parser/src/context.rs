use xelis_types::{IdentifierType, Type};

#[derive(Clone, Debug)]
pub struct Context<'a> {
    // scopes are used to store variables
    scopes: Vec<(&'a str, Type)>,
    // checkpoints are used to manage scopes
    checkpoints: Vec<usize>,
    // max variables count for each scope depth
    max_variables_count: usize,
    // is_in_loop is used to allow the use of the break and continue keywords
    is_in_loop: bool,
}

impl<'a> Context<'a> {
    // create a new Context with a default scope
    pub fn new() -> Self {
        Self {
            // first is for constants
            scopes: Vec::new(),
            checkpoints: Vec::new(),
            max_variables_count: 0,
            is_in_loop: false,
        }
    }

    // get the value type of a variable registered in scopes using its name
    pub fn get_type_of_variable<'b>(&'b self, key: &IdentifierType) -> Option<&'b Type> {
        self.scopes.get(*key as usize)
            .map(|v| &v.1)
    }

    // returns true if this variable name is registered in scopes
    #[inline]
    pub fn has_variable(&self, key: &str) -> bool {
        self.get_variable_id(key).is_some()
    }

    // returns the maximal number of variables registered in scopes
    pub fn max_variables_count(&self) -> IdentifierType {
        self.max_variables_count as IdentifierType
    }

    pub fn get_variable_id(&self, key: &str) -> Option<IdentifierType> {
        self.scopes.iter().position(|(k, _)| *k == key).map(|v| v as IdentifierType)
    }

    // register a variable in the current scope
    pub fn register_variable(&mut self, key: &'a str, var_type: Type) -> Option<IdentifierType> {
        if self.has_variable(&key) {
            return None
        }

        Some(self.register_variable_unchecked(key, var_type))
    }

    // register a variable in the current scope unchecked
    pub fn register_variable_unchecked(&mut self, key: &'a str, var_type: Type) -> IdentifierType {
        self.scopes.push((key, var_type));

        (self.scopes.len() - 1) as IdentifierType
    }

    // Add a new scope empty in the Context
    pub fn begin_scope(&mut self) {
        self.checkpoints.push(self.scopes.len());
    }

    // Delete the latest scope added to the Context
    pub fn end_scope(&mut self) {
        let checkpoint = self.checkpoints.pop().unwrap();
        let len = self.scopes.len();
        if self.max_variables_count < len {
            self.max_variables_count = len;
        }
        self.scopes.truncate(checkpoint);
    }

    // returns if the Context is in a loop
    pub fn is_in_a_loop(&self) -> bool {
        self.is_in_loop
    }

    // set if the Context is in a loop
    pub fn set_in_a_loop(&mut self, is_in_loop: bool) {
        self.is_in_loop = is_in_loop;
    }
}
