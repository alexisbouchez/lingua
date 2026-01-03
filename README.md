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
- `async` functions and `await` expressions

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
- `read_char()` - read a single character from stdin, returns character code or 0 on EOF
- `read_line()` - read a line from stdin, returns address of null-terminated string (max 255 chars)

#### Math Functions
- `abs(n)` - absolute value
- `min(a, b)` - minimum of two values
- `max(a, b)` - maximum of two values

#### String Functions
- `str_eq(addr1, len1, addr2, len2)` - compare two strings, returns 1 if equal, 0 otherwise
- `str_copy(src, len, dest)` - copy string from src to dest, returns dest
- `str_len(addr)` - returns length of null-terminated string
- `str_concat(addr1, len1, addr2, len2)` - concatenate two strings, returns address
- `str_substr(addr, start, len)` - extract substring, returns address

#### Memory Functions
- `malloc(size)` - allocate size bytes on heap, returns address
- `malloc_str(addr, len)` - allocate memory for string, returns address
- `memset(addr, val, len)` - set len bytes starting at addr to val, returns addr
- `memcpy(dest, src, len)` - copy len bytes from src to dest, returns dest
- `load(addr)` - load i32 from memory
- `store(addr, val)` - store i32 to memory
- `drop(val)` - discard a value

#### System Functions
- `exit(code)` - exit program

#### Async Functions
- `async_sleep(ms)` - async sleep for milliseconds
- `async_read(fd, buf, len)` - async file read
- `async_write(fd, buf, len)` - async file write

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

### Async/Await Example

```
async fn sleep_and_print(ms: i32): i32 {
    println(1); // Before sleep
    await async_sleep(ms);
    println(2); // After sleep
    0
}

fn _start(): i32 {
    sleep_and_print(100);
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

# Compile and run directly
./lingua run input.lingua
```

Run with a WASM runtime:

```bash
wasmtime output.wasm
```

The `run` command compiles the Lingua file to a temporary WASM file and executes it using `wasmtime` or `wasmedge` (whichever is available).

## Testing

```bash
go test ./...
```

## License

MIT
