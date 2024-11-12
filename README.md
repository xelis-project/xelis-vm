# XELIS VM
XVM is a virtual machine with its own interpreted language for the XELIS network developed in Rust.
It supports constants, functions, while/foreach loops, arrays and structures.
The syntax is strongly inspired by Rust.

## Crates

- `vm` is the main crate that contains Virtual Machine to execute a (op-code) compiled program.
- `assembler` is the crate that contains the assembler to convert an source code of raw instructions into a program.
- `compiler` is the crate that contains the compiler to convert an AST (Abstract Syntax Tree) program into an op-code program.
- `parser` is the crate that contains the parser to convert a list of tokens into an AST (Abstract Syntax Tree) program.
- `lexer` is the crate that contains the lexer to convert a source code into a list of tokens.

All the verifications are mainly made at the level of the Parser to check the conformity of the code to be interpreted.

The different primitive types are:
- `u8` (unsigned 8 bits)
- `u16` (unsigned 16 bits)
- `u32` (unsigned 32 bits)
- `u64` (unsigned 64 bits)
- `u128` (unsigned 128 bits)
- `u256` (unsigned 256 bits)
- `bool`
- `string`
- `struct`
- `optional<T>` where T is another type (it allow the value to be nullable)

File extension is `.xel`

## Documentation
the semicolon is **optional**, thus can be added if desired without any difference in the code.

Recursive functions are allowed, but limited to a configurable depth.

A environment system is completely customizable to set your own native functions.
This helps to manage exactly what a program can interact with.
Custom structs are also available.

### Numbers
An error will be returned by the interpreter if an overflow is detected without causing a panic.

**Rules**
- The value must be greater than or equal to `0`.
- You can put `_` (underscore) for a better readability.
- If no type is specified on the value, then `u64` will be the default.
- Array indexes are `u32` types.
- You can precise the type by adding `u8`, `u16`, `u32`, `u64`, `u128` or `u256` after the value.

**Examples**
```rust
let my_byte: u8 = 10
let my_u16: u16 = 70
let my_u32: u32 = 999
let my_int: u64 = 25655
let my_u128: u128 = 100_000_000u128
```

### Variable
for constant variable, it must be declared outside a function, with `const` keyword.

**Rules**
- Every variable must be declared with `let` or `const` keyword.
- Variable name must alphanumeric characters.
- Must provide value type.
- If no value is set, `null` is set by default.

**Examples**
```rust
const hello: string = "hello"
...
let world: string = "world"
```

### Casting
Values of built-in types can be casted into other built-in types easily using the keyword `as`.

**Rules**
- Both value types must be a built-in type.

**Examples**
```rust
let id: u128 = 1337
let b: u8 = id as u8
let id_str: string = id as string 
```

### Import
Instead of having one file with all your code, you can have multiple files that will be compiled into one final program.

**Rules**
- Have a unique alias if set
- No circular import
- ends with `.xel` if its a local import

**Examples**

`math` namespace
```rust
import "math.xel" as math;
...
math.sum(a, b)
```

no namespace:
```rust
sum(a, b)
```

### Function
`entry` function is a "public callable" function and must return a `u64` value.

**Rules**
- Must starts with `func` or `entry` keyword.
- Signature is based on function name and parameters.
- For type functions, the type must not be primitive.
- Recursive functions are allowed.

**Examples**
```go
entry foo() { ... }
fn foo() { ... }
fn foo() -> u64 { ... }
fn foo(a: u64, b: u64) { ... }
fn (f Foo) bar() { ... }
```

### Structure
A structure can contain other structures.

**Rules**
- The name must be unique.
- Name should start with a uppercase letter.
- Only letters are allowed in name.
- The last field does not need a comma.

**Examples**
```rust
struct MyStruct {
    message: string,
    value: u64
}
```

### Ternary
 
**Rules**
- A `bool` condition is required.
- The two values that can be returned must be of the same type.

**Examples**
```rust
let score: u64 = is_winner() ? 20 : 0
```

### Negate operator
 
**Rules**
- A `bool` condition is required after it.

**Examples**
```rust
let negative: bool = !condition
```

### Array

**Rules**
- All values must be of the same specified type.

**Examples**
```rust
let array: u64[] = [10, 20, 30, 40]
...
let dim: u64[][] = [[34, 17], [8, 14], [0, 69]]
```

### If

**Rules**
- Have a `bool` condition.

**Examples**
```rust
if condition {
	...
}

if (i > 20 && i != 25) || i == 0 {
	...
}
```
### Else

**Rules**
- It must be preceded by an `if` condition.

**Examples**
```rust
else {
	...
}
```

### Else if

**Rules**
- It must be preceded by an `if` or an `else if` condition.
- Have a boolean condition.

**Examples**
```rust
else if condition {
	...
}

else if my_struct != null {
	...
}
```

### While

**Rules**
- Have a boolean condition.

**Examples**
```rust
while condition {
	...
}
```

### Foreach

**Rules**
- Have the name of a variable.
- Have an array to go through

**Examples**
```rust
foreach val in values {
	...
}
```

You can also do on specific ranges:
```rust
foreach i in 0..10 {
	...
}
```

### For

**Rules**
- Have the name of a variable.
- Have a boolean condition.
- Have an assign operator.

**Examples**
```rust
for i: u64 = 0; i < 10; i += 1 {
	...
}
```

### Break

**Rules**
- Must be in a loop (`foreach`, `for`, `while`).

**Examples**
```rust
while condition {
	if i % 10 == 0 {
		break;
	}
	...
}
```

### Continue

**Rules**
- Must be in a loop (`foreach`, `for`, `while`).

**Examples**
```rust
while condition {
	if i % 10 == 0 {
		continue;
	}
	...
}
```

### Return

**Rules**
- Must not have any code after.
- If the function returns a value, the return must return a value.

**Examples**
```go
fn foo() -> string {
	return "Hello World!"
}

fn bar() {
	if condition {
		return
	}
	foo()
}
```

### Scope
Allows you to isolate a part of the code / variables created.

**Rules**
- No specific rules.

**Examples**
```rust
{
	...
}
```