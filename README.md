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

### Built-in Functions
- `print(str, len)` - print string with newline
- `println(n)` - print integer with newline
- `print_str(str, len)` - print string without newline
- `print_int(n)` - print integer without newline
- `abs(n)` - absolute value
- `min(a, b)` - minimum of two values
- `max(a, b)` - maximum of two values
- `exit(code)` - exit program
- `load(addr)` - load i32 from memory
- `store(addr, val)` - store i32 to memory
- `drop(val)` - discard a value

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
./lingua input.lingua output.wasm
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
