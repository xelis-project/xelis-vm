# XELIS VM
Xelis (anagram of Slixe) is an interpreted language developed in Rust. It supports constants, functions, while/foreach loops, arrays and structures. The syntax is strongly inspired by Rust and Golang.

All the verifications are mainly made at the level of the Parser to check the conformity of the code to be interpreted.

The different primitive types are:
- `byte` (u8)
- `short` (u16)
- `int` (u64)
- `long` (u128)
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

#### Examples
```rust
struct MyStruct {
    message: string,
    value: int
}
```

### Ternary
 
#### Rules
- A Boolean condition is required.
- The two values that can be returned must be of the same type.

#### Examples
```rust
let score: int = is_winner() ? 20 : 0
```