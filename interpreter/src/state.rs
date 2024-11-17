use xelis_environment::Context;
use crate::InterpreterError;

// State is used to store the number of expressions executed and the number of recursive calls
pub struct State<'a> {
    // Count the number of expressions executed
    count_expr: u64,
    // Maximum number of expressions that can be executed
    max_expr: Option<u64>,
    // Count the number of recursive calls
    recursive: u16,
    // Maximum number of recursive calls
    max_recursive: Option<u16>,
    // Current cost of the program
    gas_usage: u64,
    // Program execution shouldn't exceed this limit
    max_gas_usage: Option<u64>,
    context: Context<'a>,
}

impl<'a> State<'a> {
    // Create a new state with the given limits
    pub fn new(max_expr: Option<u64>, max_recursive: Option<u16>, max_cost: Option<u64>) -> Self {
        Self::with_context(max_expr, max_recursive, max_cost, Default::default())
    }

    pub fn with_context(max_expr: Option<u64>, max_recursive: Option<u16>, max_cost: Option<u64>, context: Context<'a>) -> Self {
        Self {
            count_expr: 0,
            max_expr,
            recursive: 0,
            max_recursive,
            gas_usage: 0,
            max_gas_usage: max_cost,
            context,
        }
    }

    // Get the context
    pub fn context(&self) -> &Context<'a> {
        &self.context
    }

    // Get the mutable context
    pub fn context_mut(&mut self) -> &mut Context<'a> {
        &mut self.context
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
    #[inline(always)]
    pub fn increase_expressions_executed(&mut self) -> Result<(), InterpreterError> {
        self.increase_expressions_executed_by(1)
    }

    // decrement the number of expressions executed
    #[inline(always)]
    pub fn decrease_expressions_executed(&mut self) {
        self.count_expr -= 1;
    }

    // get the number of expressions executed
    #[inline(always)]
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

    // Increase the gas usage
    pub fn increase_gas_usage(&mut self, value: u64) -> Result<(), InterpreterError> {
        self.gas_usage += value;

        if let Some(max) = self.max_gas_usage {
            if self.gas_usage >= max {
                return Err(InterpreterError::GasLimitReached)
            }
        }

        Ok(())
    }

    // Get the total gas used by the program
    #[inline(always)]
    pub fn get_gas_usage(&self) -> u64 {
        self.gas_usage
    }

    // decrement the number of recursive calls
    #[inline(always)]
    pub fn decrease_recursive_depth(&mut self) {
        self.recursive -= 1;
    }

    // Reset the state
    #[inline(always)]
    pub fn reset(&mut self) {
        self.count_expr = 0;
        self.recursive = 0;
        self.gas_usage = 0;
    }
}
