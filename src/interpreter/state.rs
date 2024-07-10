use std::collections::HashMap;

use crate::{InterpreterError, VariableIdentifier};
use super::Value;

pub struct State {
    // Count the number of expressions executed
    count_expr: u64,
    // Count the number of recursive calls
    recursive: u16,
    // Maximum number of expressions that can be executed
    max_expr: u64,
    // Maximum number of recursive calls
    max_recursive: u16,
    // constants variables that can be used
    constants: HashMap<VariableIdentifier, Value>
}

impl State {
    // Create a new state with the given limits
    pub fn new(max_expr: u64, max_recursive: u16) -> Self {
        Self {
            count_expr: 0,
            recursive: 0,
            max_expr,
            max_recursive,
            constants: HashMap::new()
        }
    }

    // Constants variables registered in the state
    pub fn get_constant_value(&self, name: &VariableIdentifier) -> Option<&Value> {
        self.constants.get(name)
    }

    // increase the number of expressions executed
    pub fn increase_expressions_executed_by(&mut self, value: u64) -> Result<(), InterpreterError> {
        self.count_expr += value;

        if self.max_expr != 0 && self.count_expr >= self.max_expr {
            return Err(InterpreterError::LimitReached)
        }

        Ok(())
    }

    // increment the number of expressions executed
    pub fn increase_expressions_executed(&mut self) -> Result<(), InterpreterError> {
        self.count_expr += 1;

        if self.max_expr != 0 && self.count_expr >= self.max_expr {
            return Err(InterpreterError::LimitReached)
        }

        Ok(())
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

        if self.max_recursive != 0 && self.recursive >= self.max_recursive {
            return Err(InterpreterError::RecursiveLimitReached)
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
