use crate::InterpreterError;

// State is used to store the number of expressions executed and the number of recursive calls
// 
pub struct State {
    // Count the number of expressions executed
    count_expr: u64,
    // Count the number of recursive calls
    recursive: u16,
    // Maximum number of expressions that can be executed
    max_expr: Option<u64>,
    // Maximum number of recursive calls
    max_recursive: Option<u16>,
}

impl State {
    // Create a new state with the given limits
    pub fn new(max_expr: Option<u64>, max_recursive: Option<u16>) -> Self {
        Self {
            count_expr: 0,
            recursive: 0,
            max_expr,
            max_recursive,
        }
    }

    // increase the number of expressions executed
    pub fn increase_expressions_executed_by(&mut self, value: u64) -> Result<(), InterpreterError> {
        self.count_expr += value;

        if let Some(max_expr) = self.max_expr {
            if self.count_expr >= max_expr {
                return Err(InterpreterError::LimitReached)
            }
        }

        Ok(())
    }

    // increment the number of expressions executed
    pub fn increase_expressions_executed(&mut self) -> Result<(), InterpreterError> {
        self.increase_expressions_executed_by(1)
    }

    // decrement the number of expressions executed
    pub fn decrease_expressions_executed(&mut self) {
        self.count_expr -= 1;
    }

    // get the number of expressions executed
    pub fn get_expressions_executed(&self) -> u64 {
        self.count_expr
    }

    // increment the number of recursive calls
    pub fn increase_recursive_depth(&mut self) -> Result<(), InterpreterError> {
        self.recursive += 1;

        if let Some(max_recursive) = self.max_recursive {
            if self.recursive >= max_recursive {
                return Err(InterpreterError::RecursiveLimitReached)
            }
        }

        Ok(())
    }

    // decrement the number of recursive calls
    pub fn decrease_recursive_depth(&mut self) {
        self.recursive -= 1;
    }

    // Reset the state
    pub fn reset(&mut self) {
        self.count_expr = 0;
        self.recursive = 0;
    }
}
