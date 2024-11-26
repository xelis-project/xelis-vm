## Documentation for OpCodes and Their Arguments

This document details the various operation codes (opcodes) and their respective arguments.
Each opcode performs a specific operation, and the provided arguments determine its behavior.
Errors occur if the argument count or format is invalid.

---

### **OpCode Reference**

#### **CONSTANT**
- **Description**: Loads a constant value at a specified index.
- **Arguments**: 
  - `index` (integer): The index of the constant.

#### **MEMORYLOAD**
- **Description**: Loads a value from memory to a register.
- **Arguments**:
  - `register_index` (integer): The index of the register.

#### **MEMORYSET**
- **Description**: Stores a value into a memory register.
- **Arguments**:
  - `register_index` (integer): The index of the register.

#### **SUBLOAD**
- **Description**: Loads a sub-value using an index.
- **Arguments**:
  - `index` (integer): The field index of the sub-value.

#### **POP**
- **Description**: Removes the top value from the stack.
- **Arguments**: None.

#### **POPN**
- **Description**: Removes multiple values from the stack.
- **Arguments**:
  - `count` (integer): Number of values to remove.

#### **COPY**
- **Description**: Duplicates the top value of the stack.
- **Arguments**: None.

#### **COPYN**
- **Description**: Duplicates a value at a specific stack index.
- **Arguments**:
  - `stack_index` (integer): The index of the stack value to duplicate.

#### **SWAP**
- **Description**: Swaps the top value with a value at a specific stack index.
- **Arguments**:
  - `stack_index` (integer): The index of the stack value to swap.

#### **SWAP2**
- **Description**: Swaps two values on the stack.
- **Arguments**:
  - `a_stack_index` (integer): Index of the first value.
  - `b_stack_index` (integer): Index of the second value.

#### **JUMP**
- **Description**: Jumps to a specified address or label.
- **Arguments**:
  - `addr` (integer or label): Target address or label prefixed by `:`.

#### **JUMPIFFALSE**
- **Description**: Jumps to a specified address or label if the condition is false.
- **Arguments**:
  - `addr` (integer or label): Target address or label prefixed by `:`.

#### **ITERABLELENGTH**
- **Description**: Retrieves the length of an iterable.
- **Arguments**: None.

#### **ITERATORBEGIN**
- **Description**: Begins iteration over an iterable.
- **Arguments**: None.

#### **ITERATORNEXT**
- **Description**: Moves the iterator to the next item or jumps to a label if iteration ends.
- **Arguments**:
  - `addr` (integer or label): Target address or label prefixed by `:`.

#### **ITERATOREND**
- **Description**: Ends iteration.
- **Arguments**: None.

#### **RETURN**
- **Description**: Ends the execution of the current chunk or function.
- **Arguments**: None.

#### **ARRAYCALL**
- **Description**: Accesses an array element by index.
- **Arguments**:
  - `index` (integer): Index of the array element.

#### **CAST**
- **Description**: Casts a value to a specific primitive type.
- **Arguments**:
  - `primitive_type_id` (integer): Identifier of the target type.

#### **INVOKECHUNK**
- **Description**: Invokes a chunk of code with parameters.
- **Arguments**:
  - `chunk_id` (integer or label): Identifier or label of the chunk.
  - `on_value` (boolean): Whether the chunk operates on a value.
  - `args_count` (integer): Number of arguments passed.

#### **SYSCALL**
- **Description**: Invokes a system call.
- **Arguments**:
  - `sys_call_id` (integer): Identifier of the system call.
  - `on_value` (boolean): Whether the call operates on a value.
  - `args_count` (integer): Number of arguments passed.

#### **NEWARRAY**
- **Description**: Creates a new array with a specified length.
- **Arguments**:
  - `length` (integer): Number of elements.

#### **NEWSTRUCT**
- **Description**: Creates a new struct with a specified ID.
- **Arguments**:
  - `struct_id` (integer): Identifier of the struct.

#### **NEWRANGE**
- **Description**: Creates a new range object.
- **Arguments**: None.

#### **NEWMAP**
- **Description**: Creates a new map with a specified length.
- **Arguments**:
  - `length` (integer): Number of key-value pairs.

#### **NEG**
- **Description**: Negates the top value on the stack.
- **Arguments**: None.

---

### **Arithmetic Operations**
Perform arithmetic operations on values on the stack.

| OpCode | Description       | Arguments |
|--------|-------------------|-----------|
| ADD    | Addition          | None      |
| SUB    | Subtraction       | None      |
| MUL    | Multiplication    | None      |
| DIV    | Division          | None      |
| MOD    | Modulus           | None      |
| POW    | Power             | None      |

---

### **Logical and Bitwise Operations**
Perform logical and bitwise operations.

| OpCode | Description       | Arguments |
|--------|-------------------|-----------|
| AND    | Logical AND       | None      |
| OR     | Logical OR        | None      |
| XOR    | Logical XOR       | None      |
| SHL    | Bitwise shift left | None     |
| SHR    | Bitwise shift right | None    |

---

### **Comparison Operations**
Compare values on the stack.

| OpCode | Description       | Arguments |
|--------|-------------------|-----------|
| EQ     | Equality check    | None      |
| GT     | Greater than      | None      |
| LT     | Less than         | None      |
| GTE    | Greater or equal  | None      |
| LTE    | Less or equal     | None      |

---

### **Assignment Operations**
Modify values on the stack or in memory.

| OpCode       | Description                 | Arguments |
|--------------|-----------------------------|-----------|
| ASSIGN       | Basic assignment            | None      |
| ASSIGNADD    | Assignment with addition    | None      |
| ASSIGNSUB    | Assignment with subtraction | None      |

(*Additional `ASSIGN*` opcodes follow similar rules.*)

---

### **Increment/Decrement**
Modify values by 1.

| OpCode | Description       | Arguments |
|--------|-------------------|-----------|
| INC    | Increment         | None      |
| DEC    | Decrement         | None      |