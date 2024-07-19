# XELIS VM
XVM is a virtual machine with its own interpreted language for the XELIS network developed in Rust.
It supports constants, functions, while/foreach loops, arrays and structures.
The syntax is strongly inspired by Rust and Golang.

All the verifications are mainly made at the level of the Parser to check the conformity of the code to be interpreted.

The different primitive types are:
- `byte` (unsigned 8 bits)
- `short` (unsigned 16 bits)
- `int` (unsigned 64 bits)
- `long` (unsigned 128 bits)
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
- If no type is specified on the value, then `int` will be the default.

**Examples**
```rust
let my_byte: byte = 10
let my_short: short = 70
let my_int: int = 25655
let my_long: long = 100_000_000L
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
let id: long = 1337
let b: byte = id as byte
let id_str: string = id as string 
```

### Function
`entry` function is a "public callable" function and must return a `int` value.

**Rules**
- Must starts with `func` or `entry` keyword.
- Signature is based on function name and parameters.
- For type functions, the type must not be primitive.
- Recursive functions are allowed.

**Examples**
```go
entry foo() { ... }
func foo() { ... }
func foo(): int { ... }
func foo(a: int, b: int) { ... }
func (f Foo) bar() { ... }
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
    value: int
}
```

### Ternary
 
**Rules**
- A `bool` condition is required.
- The two values that can be returned must be of the same type.

**Examples**
```rust
let score: int = is_winner() ? 20 : 0
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
let array: int[] = [10, 20, 30, 40]
...
let dim: int[][] = [[34, 17], [8, 14], [0, 69]]
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
```csharp
foreach val in values {
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
for i: int = 0; i < 10; i += 1 {
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
func foo(): string {
	return "Hello World!"
}

func bar() {
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