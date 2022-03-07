# XELIS VM
Xelis is an interpreted language developed in Rust. It supports constants, functions, while/foreach loops, arrays and structures. The syntax is strongly inspired by Rust and Golang.

All the verifications are mainly made at the level of the Parser to check the conformity of the code to be interpreted.

The different primitive types are:
- `byte` (unsigned 8-bytes integer)
- `short` (unsigned 16-bytes integer)
- `int` (unsigned 64-bytes integer)
- `long` (unsigned 128-bytes integer)
- `bool`
- `string`
- `struct`

File extension is `.xel`

## Documentation
the semicolon is not mandatory, but can be added if desired without any difference in the code.

### Variable
for constant variable, it must be declared outside a function, with `const` keyword.

#### Rules
- Every variable must be declared with `let` or `const` keyword.
- Variable name must alphanumeric characters.
- Must provide value type.
- If no value is set, `null` is set by default.

#### Examples
```rust
const hello: string = "hello"
...
let world: string = "world"
```

### Function
`entry` function is a "public callable" function and must return a `int` value.

#### Rules
- Must starts with `func` or `entry` keyword.
- Signature is based on function name and parameters.
- For type functions, the type must not be primitive.
- Recursive functions are allowed.

#### Examples
```go
entry foo() { ... }
func foo() { ... }
func foo(): int { ... }
func foo(a: int, b: int) { ... }
func (f Foo) bar() { ... }
```

### Structure
A structure can contain other structures.

#### Rules
- The name must be unique.
- Name should start with a uppercase letter.
- Only letters are allowed in name.
- The last field does not need a comma.

#### Examples
```rust
struct MyStruct {
    message: string,
    value: int
}
```

### Ternary
 
#### Rules
- A `bool` condition is required.
- The two values that can be returned must be of the same type.

#### Examples
```rust
let score: int = is_winner() ? 20 : 0
```

### Negate operator
 
#### Rules
- A `bool` condition is required after it.

#### Examples
```rust
let negative: bool = !condition
```

### Array

#### Rules
- All values must be of the same specified type.

#### Examples
```rust
let  array: int[] = [10, 20, 30, 40]
...
let dim: int[][] = [[34, 17], [8, 14], [0, 69]]
```

### If

#### Rules
- Have a `bool` condition.

#### Examples
```rust
if condition {
	...
}

if (i > 20 && i != 25) || i == 0 {
	...
}
```
### Else

#### Rules
- It must be preceded by an `if` condition.

#### Examples
```rust
else {
	...
}
```

### Else if

#### Rules
- It must be preceded by an `if` or an `else if` condition.
- Have a boolean condition.

#### Examples
```rust
else if condition {
	...
}

else if my_struct != null {
	...
}
```