# XVM
XVM (XELIS Virtual Machine) is a customizable VM and language toolchain. The workspace includes the core VM plus crates for the Silex language: lexing, parsing, AST representation, bytecode, assembly/disassembly, compilation, environment construction, runtime types, and ABI generation.

The language used by XVM is **Silex**, whose syntax is inspired by Rust.

Unlike other VMs, XVM is designed to be customizable, including the default std. You can define your own native functions, hooks, structures, enums, and opaque Rust-backed types.
It also includes built-in primitive and container types.

XVM is designed to be deterministic, stable, and sandboxed to ensure performance and security without limiting its usage.
It is also compatible with `async` operations for better integration within your systems.

Most verification is performed by the parser and compiler so source code can be compiled into a well-formed opcode program.
There is also a validator next to the VM to ensure bytecode modules and their types are well-formed against a required environment before execution.

Thanks to the customizable environment, you can define your own types and native functions, register hooks, and override the default std functions if required.

For more flexibility, opaque types are available to keep logic in Rust directly, which can improve performance and expose a clean API to Silex.

## File Extensions

Silex source files use the `.slx` extension.

Compiled Silex bytecode modules use the `.slxc` extension. This is the default binary format written by `silex compile` and `silex asm`, and it is the preferred format for storing compiled programs.

JSON bytecode output is still available with `--format json` when a human-readable or tooling-friendly representation is needed, but `.slxc` is the normal compiled-module extension.

## CLI

The `silex` CLI is the main entry point for working with Silex programs.

It can:

- compile `.slx` source files into bytecode modules;
- run either source files or compiled modules;
- disassemble bytecode modules into readable assembly;
- assemble textual bytecode back into modules;
- generate JSON ABIs from Silex source.

Compiled modules are written as binary `.slxc` files by default, next to the input path. For inspection or tooling, pass `--format json` to write a human-readable bytecode representation instead.

| Command | Description |
| --- | --- |
| `compile` | Compile a `.slx` source file into a bytecode module. |
| `run` | Execute a `.slx` source file or compiled module. Uses the first entry chunk by default; pass `--entry ID` to choose another one. |
| `disasm` | Print a binary `.slxc` module, or a JSON bytecode module, as assembly. |
| `asm` | Assemble textual bytecode into a module. |
| `abi` | Generate a JSON ABI from `.slx` source. |

```sh
# Install the CLI
cargo install --path silex-cli

# Available commands
silex compile examples/factorial.slx -o factorial.slxc
silex run examples/factorial.slx 5
silex run factorial.slxc 5
silex disasm factorial.slxc
silex asm program.asm -o program.slxc
silex abi examples/factorial.slx
```

For compiled bytecode, prefer `.slxc`. If JSON bytecode is needed for inspection or external tooling, pass `--format json` and choose an explicit JSON output path. ABI files are JSON documents and default to `.abi.json`.

`run` accepts `null`, booleans, unsigned integers, and strings as positional arguments. Pass the JSON representation of a `ValueCell` for other values or an explicit numeric type.

## Crates

- `silex-abi` (`abi`): Generates a JSON ABI from Silex source or from a parsed program. It reports entry functions, parameters, outputs, and referenced internal structs/enums.
- `silex-assembler` (`assembler`): Converts textual opcode instructions into a bytecode `Module` and provides a disassembler for converting bytecode back into readable instructions.
- `silex-ast` (`ast`): Defines the AST used by the parser and compiler: expressions, statements, function declarations, hooks, entry functions, tokens, operators, and programs.
- `silex-builder` (`builder`): Builds an `Environment` by registering native functions, const functions, hooks, structs, enums, opaque types, and the default `xstd` library.
- `silex-bytecode` (`bytecode`): Defines the bytecode format: opcodes, chunks, module metadata, constants, access levels, hook chunk mappings, and serialization schemas.
- `silex-compiler` (`compiler`): Compiles a parsed AST `Program` into a bytecode `Module`, including stack/register management, control-flow jumps, calls, hooks, and constant handling.
- `silex-environment` (`environment`): Stores the runtime environment exposed to the parser and VM: native functions, registered opaque types, hooks, VM context, callbacks, gas/memory accounting, and environment errors.
- `silex-lexer` (`lexer`): Converts Silex source code into positioned tokens, including identifiers, literals, comments, operators, keywords, type names, strings, and bytes.
- `silex-parser` (`parser`): Converts tokens into an AST `Program`, resolves types and function signatures against an `EnvironmentBuilder`, validates language rules, and builds global mappings for functions, structs, and enums.
- `silex-cli` (`silex`): Command-line interface for compiling and running `.slx` Silex programs, assembling `.slxc` bytecode modules, generating ABIs, and disassembling compiled modules.
- `silex-types` (`types`): Provides the shared runtime and compile-time type system: primitive values, constants, cells, references, arrays, maps, structs, enums, opaque traits, numeric helpers, `U256`, and packed type checks.
- `xelis-vm` (`vm`): Executes bytecode modules with an instruction table, stack, call stack, module stack, VM context, native/syscall integration, hooks, entry invocation, and bytecode validation.

## Types

The supported types are:
- `u8` (unsigned 8 bits)
- `u16` (unsigned 16 bits)
- `u32` (unsigned 32 bits)
- `u64` (unsigned 64 bits)
- `u128` (unsigned 128 bits)
- `u256` (unsigned 256 bits)
- `bool`
- `string`
- `bytes`, a raw byte sequence backed by `Vec<u8>`
- struct types
- enum variants
- opaque Rust-backed types registered in the environment
- tuple types such as `(string, u64)`
- function types such as `fn(u64) -> bool`
- closure types such as `closure(u64) -> bool`
- `optional<T>` where `T` is another type, allowing the value to be `null`
- `range<T>` where `T` is a number type, allowing iteration in a `foreach` or functions such as `contains`
- `map<K, V>` where `K` is a hashable key type and `V` is a value type

Arrays of any type are also supported, but each array must contain one value type, for example `u64[]` or nested arrays such as `u64[][]`.

## OpCodes

The opcodes are the instructions that the VM will execute.
They are generated by the compiler and are executed by the VM.
See the [opcodes.md](opcodes.md) file for more information.

## Documentation
Semicolons are **optional** in statements and can be added without changing the code behavior.

Recursive functions are allowed, but limited to a configurable depth.

The environment system is customizable, so you can define your own native functions.
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
let my_u8: u8 = 10
let my_u16: u16 = 70
let my_u32: u32 = 999
let my_int: u64 = 25655
let my_u128: u128 = 100_000_000u128
let my_u256: u256 = 100_000_000u256
```

Each numeric type can be cast into another numeric type. If an overflow is detected while casting, the value is truncated.
```rust
let my_u8: u8 = 255
let my_u16: u16 = my_u8 as u16
```

Also, each numeric type has a `MIN` and `MAX` value that can be used.
```rust
let min: u8 = u8::MIN
let max: u8 = u8::MAX
```

### Variable
Constant variables must be declared outside a function with the `const` keyword.

**Rules**
- Every variable must be declared with `let` or `const` keyword.
- Variable names must be alphanumeric.
- A value type must be provided.
- If no value is set, `null` is set by default.

**Examples**
```rust
const hello: string = "hello"
...
let world: string = "world"
```

### Casting
Values of built-in types can be cast into other built-in types easily using the keyword `as`.
In case of an overflow, no error will be returned, but the value will be truncated.

**Rules**
- Both value types must be a built-in type.

**Examples**
```rust
let id: u128 = 1337
let b: u8 = id as u8
// id_str equals "255" due to the truncation
let id_str: string = id as string
```

### Import
Imports are reserved for splitting code across multiple files, but import resolution is not implemented yet.
The parser currently recognizes local import declarations and rejects absolute paths or paths containing `..`.

**Rules**
- Have a unique alias if set
- No circular import
- ends with `.slx` if its a local import

**Examples**
```rust
use math;
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
- Must start with `fn`, `pub fn`, `entry`, or `hook`.
- Signature is based on function name and parameters.
- Methods can only be declared on user-defined structs and enums.
- Recursive functions are allowed.

**Examples**
```rust
entry foo() { ... }
fn foo() { ... }
pub fn foo() { ... }
fn foo() -> u64 { ... }
fn foo(a: u64, b: u64) { ... }
fn (f Foo) bar() { ... }
hook on_transfer(amount: u64) { ... }
```

### Structure
A structure can contain other structures.

**Rules**
- The name must be unique.
- Name should start with an uppercase letter.
- The name must be a valid identifier.
- The last field does not need a comma.

**Examples**
```rust
struct MyStruct {
    message: string,
    value: u64
}
```

### Enum
An enum is a type that can have multiple variants.

**Rules**
- The name must be unique.
- Name should start with an uppercase letter.
- Variants must be unique.
- Each variant can contain fields or be fieldless.

**Examples**
```rust
enum MyEnum {
	A,
	B,
	C {
		value: u64
	},
	D {
		name: string,
		value: u64
	}
}
```

### Optional
An optional type is a type that can be `null`.

**Rules**
- The type must be specified.
- The value can be set to `null`.

**Examples**
```rust
let my_optional: optional<u64> = null
...
let opt: optional<string> = "Hello World!"
let s = opt.unwrap()
```

### Range
A range is a type that can be used to iterate over a range of values.

**Rules**
- The type must be specified and be a number type.
- The start and end values must be of the same type.
- The end value must be greater than the start value.

**Examples**
```rust
let my_range: range<u64> = 0..10
let _: bool = my_range.contains(5)
```

### Map
A map is a key-value store where the key and value can be of any type based on the declaration.
It is backed by an insertion-ordered `IndexMap`.

**Rules**
- The key and value types must be specified.
- The key type must be hashable; maps cannot be used as keys, even when nested inside another key type.

**Examples**
```rust
let my_map: map<string, u64> = {"hello": 10, "world": 20}
my_map.insert("foo", 30)
my_map.shift_remove("hello")
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
