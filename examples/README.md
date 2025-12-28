# Lingua Examples

This directory contains example programs demonstrating various features of the Lingua programming language.

## Quick Start

To compile and run any example:

```bash
# Build the compiler (if not already built)
go build -o ../lingua ../cmd/lingua

# Compile an example
../lingua hello.lingua hello.wasm

# Run with Wasmtime
wasmtime hello.wasm

# Or with WasmEdge
wasmedge hello.wasm
```

## Examples

### Basic Examples

#### `hello.lingua`
The classic "Hello, World!" program.

```lingua
fn _start(): i32 { print_str("Hello, World!\n", 14) }
```

**Output:**
```
Hello, World!
```

#### `add.lingua`
Simple function demonstrating addition and return values.

```lingua
fn add(a: i32, b: i32): i32 {
    a + b
}
```

**Demonstrates:**
- Function parameters
- Arithmetic operations
- Expression-based return values

#### `call.lingua`
Shows function composition and calling.

```lingua
fn square(x: i32): i32 { x * x }
fn sum_of_squares(a: i32, b: i32): i32 { square(a) + square(b) }
```

**Demonstrates:**
- Function composition
- Single-line functions
- Nested function calls

#### `double_add.lingua`
Demonstrates calling functions multiple times.

```lingua
fn double(x: i32): i32 { x + x }
fn quad(x: i32): i32 { double(double(x)) }
```

**Demonstrates:**
- Function reuse
- Multiple function calls in an expression

### Mathematical Examples

#### `max.lingua`
Finding the maximum of two numbers using if-else.

```lingua
fn max(a: i32, b: i32): i32 {
    if a > b { a } else { b }
}
```

**Demonstrates:**
- If-else expressions
- Comparison operators
- Conditional return values

#### `sum.lingua`
Calculates the sum of numbers from 0 to n using a loop.

```lingua
fn sum(n: i32, x: i32): i32 {
    let i: i32 = 0;
    let s: i32 = 0;
    loop i < n {
        s = s + i;
        i = i + 1;
    };
    s
}
```

**Demonstrates:**
- Local variables
- Loops with conditions
- Variable assignment
- Accumulator pattern

#### `factorial.lingua`
Recursive factorial calculation.

```lingua
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

**Output:**
```
1
2
6
24
120
720
5040
40320
362880
3628800
```

**Demonstrates:**
- Recursive functions
- Early return
- Loop with println
- WASI entry point (`_start`)

#### `fibonacci.lingua`
Recursive Fibonacci sequence generator.

```lingua
fn fib(n: i32): i32 {
    if n <= 1 { return n };
    fib(n - 1) + fib(n - 2)
}

fn _start(): i32 {
    let i: i32 = 0;
    loop i <= 15 {
        println(fib(i));
        i = i + 1;
    };
    0
}
```

**Output:**
```
0
1
1
2
3
5
8
13
21
34
55
89
144
233
377
610
```

**Demonstrates:**
- Recursive functions with multiple calls
- Classic algorithm implementation

### Control Flow Examples

#### `countdown.lingua`
Simple countdown loop.

```lingua
fn _start(): i32 {
    let i: i32 = 10;
    loop i > 0 {
        println(i);
        i = i - 1;
    };
    0
}
```

**Output:**
```
10
9
8
7
6
5
4
3
2
1
```

**Demonstrates:**
- Decrementing loops
- Loop conditions
- println builtin

#### `fizzbuzz.lingua`
Classic FizzBuzz implementation.

```lingua
fn _start(): i32 {
    let i: i32 = 1;
    loop i <= 20 {
        if i % 15 == 0 {
            print_str("FizzBuzz\n", 9);
        } else {
            if i % 3 == 0 {
                print_str("Fizz\n", 5);
            } else {
                if i % 5 == 0 {
                    print_str("Buzz\n", 5);
                } else {
                    println(i);
                    print_str("\n", 1);
                }
            }
        };
        i = i + 1;
    };
    0
}
```

**Output:**
```
1
2
Fizz
4
Buzz
Fizz
7
8
Fizz
Buzz
11
Fizz
13
14
FizzBuzz
16
17
Fizz
19
Buzz
```

**Demonstrates:**
- Modulo operator
- Nested if-else statements
- String printing
- Classic programming challenge

#### `hello2.lingua`
Alternative hello world using the `print` function.

```lingua
fn _start(): i32 { print("Hello, World!", 13) }
```

**Demonstrates:**
- `print()` function (with automatic newline)
- Alternative to `print_str()`

## Running the Examples

### With Wasmtime

```bash
# Install Wasmtime (if not already installed)
curl https://wasmtime.dev/install.sh -sSf | bash

# Compile and run
../lingua factorial.lingua factorial.wasm
wasmtime factorial.wasm
```

### With WasmEdge

```bash
# Install WasmEdge (if not already installed)
curl -sSf https://raw.githubusercontent.com/WasmEdge/WasmEdge/master/utils/install.sh | bash

# Compile and run
../lingua fibonacci.lingua fibonacci.wasm
wasmedge fibonacci.wasm
```

### With Wazero (Go)

The test file `run_test.go` demonstrates running examples with the Go-native Wazero runtime:

```bash
go test -v
```

## Building More Examples

When creating your own examples:

1. Use `fn _start(): i32` as the entry point for WASI programs
2. Return 0 for success, non-zero for errors
3. Remember to use semicolons for statements, not for return expressions
4. The last expression in a block is its return value

Example template:

```lingua
fn _start(): i32 {
    // Your code here
    println(42);
    0  // Return success
}
```

## See Also

- [Main README](../README.md) - Language overview and quick start
- [TUTORIAL.md](../TUTORIAL.md) - Comprehensive language tutorial
- [Test files](../codegen/*_test.go) - Advanced usage and edge cases
