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

## Crates

- `silex-abi` (`abi`): Generates a JSON ABI from Silex source or from a parsed program. It reports entry functions, parameters, outputs, and referenced internal structs/enums.
- `silex-assembler` (`assembler`): Converts textual opcode instructions into a bytecode `Module` and provides a disassembler for converting bytecode back into readable instructions.
- `silex-ast` (`ast`): Defines the AST used by the parser and compiler: expressions, statements, function declarations, hooks, entry functions, tokens, operators, and programs.
- `silex-builder` (`builder`): Builds an `Environment` by registering native functions, const functions, hooks, structs, enums, opaque types, and the default `xstd` library.
- `silex-bytecode` (`bytecode`): Defines the bytecode format: opcodes, chunks, module metadata, constants, access levels, hook chunk mappings, and serialization schemas.
- `silex-compiler` (`compiler`): Compiles a parsed AST `Program` into a bytecode `Module`, including stack/register management, control-flow jumps, calls, hooks, and constant handling.
- `silex-decompiler` (`decompiler`): Attempts to reconstruct Silex source from a compiled `Module`, resolving native functions and hooks against the environment and checking the result with the lexer/parser.
- `silex-environment` (`environment`): Stores the runtime environment exposed to the parser and VM: native functions, registered opaque types, hooks, VM context, callbacks, gas/memory accounting, and environment errors.
- `silex-lexer` (`lexer`): Converts Silex source code into positioned tokens, including identifiers, literals, comments, operators, keywords, type names, strings, and bytes.
- `silex-parser` (`parser`): Converts tokens into an AST `Program`, resolves types and function signatures against an `EnvironmentBuilder`, validates language rules, and builds global mappings for functions, structs, and enums.
- `silex-types` (`types`): Provides the shared runtime and compile-time type system: primitive values, constants, cells, references, arrays, maps, structs, enums, opaque traits, numeric helpers, `U256`, and packed type checks.
- `xelis-vm` (`vm`): Executes bytecode modules with an instruction table, stack, call stack, module stack, VM context, native/syscall integration, hooks, entry invocation, and bytecode validation.

## Runtime heap

Each `VM::run()` owns a local `Heap`. Runtime values use automatic reference
counting: `ValuePointer` owns an `Rc` handle to a stable slot
(eight bytes on x86-64). Each slot holds a weak storage reference. Heap payload
borrows also borrow an owning handle, so its last owner cannot be dropped while
a payload reference is live. Only the unique heap can mutate live payloads.

`Clone` retains a handle; ordinary Rust `Drop` releases it. Zero-owner payloads
are reclaimed through an iterative release queue, without tracing the heap.
Slots are tracked in a `HashSet` and removed when their last handle is dropped.
The slot allocation and its payload are then freed; there is no free-slot pool.
Ownership uses `Rc`'s count directly, excluding storage's own slot owner.
Reclamation uses a local pending vector and a reentrancy guard, without atomic
counters, a concurrent queue, or a mutex. `Heap::len()` counts registered slots;
the hash table and pending vector retain capacity until clear or teardown.

`Heap::new` is an unsafe constructor. `VM::run` and `VM::run_blocking` are
ordinary methods that create their heap internally. The VM trusts host-provided
native and opaque code to honor the confinement contract; it does not enforce
that contract for escaped aliases. The host must serialize every heap/handle
operation, including cloning,
dropping, native calls, opaque payload access, cancellation, and teardown.
Related handles must not be used independently on other threads. The complete
execution can migrate between async executor threads when all aliases remain
confined and inactive elsewhere, with a synchronized ownership handoff.
`Send + Sync` implementations rely on this
caller guarantee; they do not provide synchronization. The same obligation
covers any runtime handles retained in opaque results after completion.
Independent executions must not share runtime handles.

Native handlers and opaque destructors must not panic. The heap does not
attempt panic recovery.

Payload borrows still require both an owning handle and the appropriate heap
borrow. Weak storage references keep later sequential handle teardown valid
after heap destruction; those handles retain only empty slot allocations.
`Heap::get()` and `get_mut()` return references directly. Callers must use only
live handles from the originating heap. `Heap` has no safe `Default` constructor
that could bypass the confinement contract.

Ordinary `clone()`, assignment, collection cleanup, and `Drop` account for
owners automatically, including ordinary error paths.
Cycles and forgotten handles can retain payloads until the heap is cleared or
dropped. VM insertion and assignment still
detach values to preserve an acyclic graph. Logical gas and memory accounting
keeps the original eight-byte reference size, independently of physical layout.

```rust
use silex_types::{Heap, PrimitiveInteger, ValueError};

pub fn main() -> Result<(), ValueError> {
    // SAFETY: this heap and all its handles stay local and are used sequentially.
    let mut heap = unsafe { Heap::new() };
    let id = heap.insert(PrimitiveInteger::U64(1).into());
    let alias = id.clone();
    let previous = heap.get(&alias).as_u64()?;
    *heap.get_mut(&id) = PrimitiveInteger::U64(previous + 1).into();
    assert_eq!(heap.get(&alias).as_u64()?, 2);
    drop(id);
    drop(alias);
    assert!(heap.is_empty());
    Ok(())
}
```

Native functions receive a separate `&mut Heap` argument to resolve
`StackValue`s. `Heap::with_value` temporarily extracts a stack value and restores
it when the operation returns, including ordinary errors. Instances are borrowed
(`FnInstance` contains `&mut StackValue`) and `FnParams` borrows a mutable argument
vector. The caller releases arguments
left in that vector and the remaining instance after the call. Use
`instance.take()` or `parameters.remove(index)` to transfer an owner out.
Read-only arguments can stay in the vector. Callback states own their retained values and release them when
dropped. Async functions and callbacks borrow their arguments, context, and heap for the duration of their future.

`VMContext` directly exposes gas and memory accounting methods. The heap is a
local variable of each `VM::run()`. Queued inputs remain owned until execution starts; every completion, failure, or cancellation
discards the execution's stack, frames, callbacks, and heap. This also
frees remaining values and cycles at run exit. Heap errors propagate as
`VMError::HeapError`. Logical gas and memory accounting is preserved.

Module constants, entry arguments, and returned container trees use `OwnedValue`.
Parser expressions keep struct, enum, and tuple type metadata separately from
that owned data. Compile-time functions receive and return `OwnedValue`; use
`register_const_function_with_return_type` when their result needs a declared
struct, enum, or tuple type.
Import them with `heap.import(&owned)` and export runtime containers with
`heap.snapshot(&cell)?` (or `heap.snapshot_stack(&value)?`). Maps store owned, immutable keys. Runtime cells themselves
no longer implement standalone Serde, `Hash`, or equality without a heap; use
owned snapshots or `cell.equals(&other, &heap)` as appropriate.

Opaque types retaining runtime handles must expose them through `children` and
implement `deep_clone_in` to detach them using the supplied heap. The derive
option `#[opaque(deep_clone_in = expression)]` makes that heap available as
`heap`; the expression can use `?` to propagate `HeapError`. Opaque payloads retaining runtime handles remain tied to that heap,
including when their wrapper is held in an owned container. Portable opaque
payloads should contain owned data instead.
Use each handle only with its originating heap. Clearing or replacing that heap
invalidates remaining handles, including those in stack values, registers,
iterators, callbacks, and opaque payloads. Dropping expired handles is safe.
Ordinary Rust drops release runtime owners automatically. Opaque child visitors
borrow handles (`&ValuePointer`) rather than copying them. `VMContext::reset_usage()`
resets accounting only. Logical memory sizes and gas formulas are independent of
reference counters and slot layout.

## Serialization

Portable `OwnedValue` data uses tagged JSON objects. The `type` field identifies
the value's representation, and `value` contains its payload when present.
Scalar values use the same JSON format in `Primitive` and `OwnedValue`.
Runtime values must be exported or snapshotted through their heap before
serialization. The examples below show the JSON representation of each value.

**Scalar**

A 64-bit unsigned integer uses the `u64` tag and a decimal string payload.

```json
{
  "type": "u64",
  "value": "42"
}
```

**Primitives**

Null, booleans, strings, and every integer width. `u8`, `u16`, and `u32` use JSON numbers; `u64`, `u128`, and `u256` use decimal strings to preserve precision. Null has no `value` field.

```json
{
  "type": "object",
  "value": [
    {
      "type": "null"
    },
    {
      "type": "boolean",
      "value": true
    },
    {
      "type": "string",
      "value": "hello"
    },
    {
      "type": "u8",
      "value": 255
    },
    {
      "type": "u16",
      "value": 65535
    },
    {
      "type": "u32",
      "value": 4294967295
    },
    {
      "type": "u64",
      "value": "18446744073709551615"
    },
    {
      "type": "u128",
      "value": "340282366920938463463374607431768211455"
    },
    {
      "type": "u256",
      "value": "115792089237316195423570985008687907853269984665640564039457584007913129639935"
    }
  ]
}
```

**Bytes**

Byte buffers use the `bytes` tag and an array of numbers from 0 to 255.

```json
{
  "type": "bytes",
  "value": [
    0,
    127,
    255
  ]
}
```

**Nested arrays**

Arrays use the `object` tag, with each element encoded as another tagged value.
This example represents `[[1, 2], [3, 4]]` with 64-bit unsigned integers. Tuples,
structs, and enum representations also use objects; their Silex type metadata
is kept separately.

```json
{
  "type": "object",
  "value": [
    {
      "type": "object",
      "value": [
        {
          "type": "u64",
          "value": "1"
        },
        {
          "type": "u64",
          "value": "2"
        }
      ]
    },
    {
      "type": "object",
      "value": [
        {
          "type": "u64",
          "value": "3"
        },
        {
          "type": "u64",
          "value": "4"
        }
      ]
    }
  ]
}
```

**Map**

Maps use ordered lists of `[key, value]` pairs, even when the keys are strings.
Each key and value has its own type tag. This map associates `alice` with `10`
and `bob` with `20`.

```json
{
  "type": "map",
  "value": [
    [
      {
        "type": "string",
        "value": "alice"
      },
      {
        "type": "u64",
        "value": "10"
      }
    ],
    [
      {
        "type": "string",
        "value": "bob"
      },
      {
        "type": "u64",
        "value": "20"
      }
    ]
  ]
}
```

**Structured map key**

A map key can itself be an array. Here, the key is `[1, 2]` with 8-bit unsigned
integers, and its value is the byte buffer `[0, 255]`. Encoding entries as pairs
avoids JSON object-key restrictions.

```json
{
  "type": "map",
  "value": [
    [
      {
        "type": "object",
        "value": [
          {
            "type": "u8",
            "value": 1
          },
          {
            "type": "u8",
            "value": 2
          }
        ]
      },
      {
        "type": "bytes",
        "value": [
          0,
          255
        ]
      }
    ]
  ]
}
```

**Range**

Ranges encode their two endpoints with matching integer-width tags. This range
starts at `0` and ends before `10`.

```json
{
  "type": "range",
  "value": [
    {
      "type": "u64",
      "value": "0"
    },
    {
      "type": "u64",
      "value": "10"
    }
  ]
}
```

**Opaque**

Portable opaque values have an outer `opaque` tag. The nested `type` identifies
the registered host type, and its `value` contains that type's JSON payload.
Here, `ReadmePoint` contains coordinates `x = 3` and `y = 4`. Deserialization
requires a JSON decoder registered under the name `ReadmePoint`.

```json
{
  "type": "opaque",
  "value": {
    "type": "ReadmePoint",
    "value": {
      "x": 3,
      "y": 4
    }
  }
}
```

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
