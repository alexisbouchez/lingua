# Lingua Language Tutorial

Welcome to Lingua! This tutorial will guide you through the language features with practical examples.

## Table of Contents
1. [Getting Started](#getting-started)
2. [Basic Types and Variables](#basic-types-and-variables)
3. [Functions](#functions)
4. [Control Flow](#control-flow)
5. [Arrays](#arrays)
6. [Strings](#strings)
7. [Built-in Functions](#built-in-functions)
8. [Global Variables](#global-variables)

## Getting Started

### Installation

Build the Lingua compiler:

```bash
go build -o lingua ./cmd/lingua
```

### Your First Program

Create a file `hello.lingua`:

```lingua
fn _start(): i32 {
    print_str("Hello, World!\n", 14);
    0
}
```

Compile and run:

```bash
./lingua hello.lingua hello.wasm
wasmtime hello.wasm
```

> **Note**: The `_start()` function is the entry point for WASI programs.

## Basic Types and Variables

Lingua currently supports the following types:
- `i32` - 32-bit signed integer
- `i64` - 64-bit signed integer (planned)
- `f32` - 32-bit float (planned)
- `f64` - 64-bit float (planned)

### Local Variables

Declare local variables with `let`:

```lingua
fn main(): i32 {
    let x: i32 = 10;
    let y: i32 = 20;
    let sum: i32 = x + y;
    sum
}
```

Variables can be reassigned:

```lingua
fn counter(): i32 {
    let count: i32 = 0;
    count = count + 1;
    count = count + 1;
    count  // returns 2
}
```

## Functions

### Function Declaration

Functions are declared with the `fn` keyword:

```lingua
fn add(a: i32, b: i32): i32 {
    a + b
}
```

### Return Values

Functions use expression-based syntax. The last expression is the return value:

```lingua
fn double(x: i32): i32 {
    x * 2  // no semicolon = return value
}
```

You can also use explicit `return`:

```lingua
fn abs(x: i32): i32 {
    if x < 0 {
        return 0 - x;
    };
    x
}
```

### Calling Functions

```lingua
fn main(): i32 {
    let result: i32 = add(5, 3);
    println(result);  // prints 8
    0
}
```

## Control Flow

### If-Else Expressions

`if` expressions can return values:

```lingua
fn max(a: i32, b: i32): i32 {
    if a > b {
        a
    } else {
        b
    }
}
```

Statements in blocks require semicolons:

```lingua
fn sign(x: i32): i32 {
    if x > 0 {
        println(1);
        1
    } else {
        if x < 0 {
            println(0 - 1);
            0 - 1
        } else {
            println(0);
            0
        }
    }
}
```

### Loops

Loops execute while a condition is true:

```lingua
fn countdown(n: i32): i32 {
    let i: i32 = n;
    loop i > 0 {
        println(i);
        i = i - 1;
    };
    0
}
```

### Break and Continue

Use `break` to exit a loop and `continue` to skip to the next iteration:

```lingua
fn find_first_even(start: i32, limit: i32): i32 {
    let i: i32 = start;
    loop i < limit {
        if i % 2 == 0 {
            break;
        };
        i = i + 1;
    };
    i
}
```

## Arrays

Arrays are heap-allocated and support indexing:

```lingua
fn array_example(): i32 {
    let arr: i32 = [1, 2, 3, 4, 5];

    // Access elements
    let first: i32 = arr[0];    // 1
    let third: i32 = arr[2];    // 3

    // Modify elements
    arr[1] = 100;

    // Nested indexing
    let matrix: i32 = [[1, 2], [3, 4]];
    let val: i32 = matrix[0][1];  // 2

    first
}
```

## Strings

Strings are stored in the string table and accessed via memory addresses:

```lingua
fn string_example(): i32 {
    // Print a string
    print_str("Hello\n", 6);

    // String literals return their address
    let hello: i32 = "Hello";
    let len: i32 = 5;

    print_str(hello, len);
    0
}
```

## Built-in Functions

### I/O Functions

```lingua
fn io_example(): i32 {
    // Print strings
    print_str("No newline", 10);
    print("With newline", 12);

    // Print integers
    print_int(42);           // prints: 42
    println(100);            // prints: 100\n

    0
}
```

### Math Functions

```lingua
fn math_example(): i32 {
    let neg: i32 = abs(0 - 5);     // 5
    let minimum: i32 = min(10, 20); // 10
    let maximum: i32 = max(10, 20); // 20

    println(neg);
    println(minimum);
    println(maximum);
    0
}
```

### String Functions

```lingua
fn string_functions(): i32 {
    let s1: i32 = "hello";
    let s2: i32 = "hello";
    let s3: i32 = "world";

    // Compare strings
    let eq1: i32 = str_eq(s1, 5, s2, 5);  // 1 (equal)
    let eq2: i32 = str_eq(s1, 5, s3, 5);  // 0 (not equal)

    // Copy string
    let dest: i32 = 1000;  // destination address
    str_copy(s1, 5, dest);  // copy "hello" to address 1000

    0
}
```

## Global Variables

Declare global variables at module level:

```lingua
global counter: i32 = 0

fn increment(): i32 {
    counter = counter + 1;
    counter
}

fn _start(): i32 {
    println(increment());  // 1
    println(increment());  // 2
    println(increment());  // 3
    0
}
```

## Complete Examples

### Factorial

```lingua
fn factorial(n: i32): i32 {
    if n <= 1 {
        return 1;
    };
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

### Fibonacci

```lingua
fn fib(n: i32): i32 {
    if n <= 1 {
        return n;
    };
    fib(n - 1) + fib(n - 2)
}

fn _start(): i32 {
    let i: i32 = 0;
    loop i < 10 {
        println(fib(i));
        i = i + 1;
    };
    0
}
```

### FizzBuzz

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
                }
            }
        };
        i = i + 1;
    };
    0
}
```

## Operators Reference

### Arithmetic Operators
- `+` - Addition
- `-` - Subtraction
- `*` - Multiplication
- `/` - Division (integer)
- `%` - Modulo

### Comparison Operators
- `==` - Equal
- `!=` - Not equal
- `<` - Less than
- `>` - Greater than
- `<=` - Less than or equal
- `>=` - Greater than or equal

### Logical Operators
- `&&` - Logical AND
- `||` - Logical OR
- `!` - Logical NOT

### Bitwise Operators
- `&` - Bitwise AND
- `|` - Bitwise OR
- `^` - Bitwise XOR
- `<<` - Left shift
- `>>` - Right shift (arithmetic)

## Tips and Best Practices

1. **Use meaningful variable names**: `count` is better than `c`
2. **Prefer expressions over statements**: Use `if` expressions when possible
3. **Keep functions small**: Break complex logic into smaller functions
4. **Use early returns**: Simplify complex conditionals with early returns
5. **Comment complex logic**: Help others (and future you) understand the code

## Next Steps

- Explore the `examples/` directory for more code samples
- Read the language specification in `readme.txt`
- Check out the test files for edge cases and advanced usage

Happy coding in Lingua! 🚀
