# Lingua

A WASM/WASI native programming language written in Go.

## Features

- Compiles directly to WebAssembly binary format
- WASI support for I/O operations
- Rust-like syntax with expression-based semantics

## Language Features

### Types
- `i32` - 32-bit signed integer

### Operators
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&`, `||`, `!`
- Bitwise: `&`, `|`, `^`, `<<`, `>>`
- Unary: `-` (negation)

### Control Flow
- `if`/`else` expressions
- `loop` with condition
- `break` and `continue`
- `return` for early returns

### Global Variables
Global variables can be declared at module level:
```
global counter: i32 = 0
```

### Arrays
Arrays are heap-allocated and support indexing:
```
let arr: i32 = [1, 2, 3, 4, 5];
let first: i32 = arr[0];
arr[1] = 100;
```

### Built-in Functions

#### I/O Functions
- `print(str, len)` - print string with newline
- `println(n)` - print integer with newline
- `print_str(str, len)` - print string without newline
- `print_int(n)` - print integer without newline

#### Math Functions
- `abs(n)` - absolute value
- `min(a, b)` - minimum of two values
- `max(a, b)` - maximum of two values

#### String Functions
- `str_eq(addr1, len1, addr2, len2)` - compare two strings, returns 1 if equal, 0 otherwise
- `str_copy(src, len, dest)` - copy string from src to dest, returns dest

#### Memory Functions
- `load(addr)` - load i32 from memory
- `store(addr, val)` - store i32 to memory
- `drop(val)` - discard a value

#### System Functions
- `exit(code)` - exit program

## Example

```
fn factorial(n: i32): i32 {
    if n <= 1 { return 1 };
    n * factorial(n - 1)
}

fn _start(): i32 {
    let i: i32 = 1;
    loop i <= 10 {
        println(factorial(i));
        i = i + 1;
    };
    0
}
```

## Building

```bash
go build -o lingua ./cmd/lingua
```

## Usage

```bash
# Compile to binary WASM
./lingua input.lingua output.wasm

# Compile to WAT (text format) for debugging
./lingua input.lingua output.wat
```

Run with a WASM runtime:

```bash
wasmtime output.wasm
```

## Testing

```bash
go test ./...
```

## License

MIT
