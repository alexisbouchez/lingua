# Lingua Language Grammar Specification

This document provides a formal specification of the Lingua programming language syntax using Extended Backus-Naur Form (EBNF) notation.

## Notation Conventions

- `|` denotes alternation (choice)
- `[ ... ]` denotes optional elements
- `{ ... }` denotes zero or more repetitions
- `( ... )` denotes grouping
- `"..."` denotes terminal symbols (literal text)
- `<...>` denotes non-terminal symbols (grammar rules)

## Lexical Structure

### Keywords

```
keyword = "fn" | "let" | "global" | "return" | "if" | "else" | "loop"
        | "break" | "continue" | "i32" | "i64" | "f32" | "f64"
```

### Identifiers

```
identifier = letter { letter | digit | "_" }
letter     = "a".."z" | "A".."Z" | "_"
digit      = "0".."9"
```

### Literals

```
integer_literal = digit { digit }
string_literal  = '"' { string_char } '"'
string_char     = <any character except '"' and '\'>
                | '\' escape_sequence

escape_sequence = "n" | "t" | "r" | "\" | '"'
```

### Operators and Punctuation

```
operator = "+" | "-" | "*" | "/" | "%"
         | "==" | "!=" | "<" | ">" | "<=" | ">="
         | "&&" | "||" | "!"
         | "&" | "|" | "^" | "<<" | ">>"
         | "="

punctuation = "(" | ")" | "{" | "}" | "[" | "]"
            | "," | ";" | ":"
```

## Grammar Rules

### Program Structure

```
file = { global_decl | function_decl }

global_decl = "global" identifier ":" type "=" expression

function_decl = "fn" identifier "(" [ parameter_list ] ")" [ ":" type ] block
```

### Parameters

```
parameter_list = parameter { "," parameter }
parameter      = identifier ":" type
```

### Types

```
type = "i32" | "i64" | "f32" | "f64"
```

### Statements

```
statement = let_statement
          | assignment_statement
          | index_assignment_statement
          | expression_statement

let_statement = "let" identifier [ ":" type ] "=" expression ";"

assignment_statement = identifier "=" expression ";"

index_assignment_statement = index_expression "=" expression ";"

expression_statement = expression ";"
```

### Blocks

```
block = "{" { statement } [ expression ] "}"
```

A block consists of zero or more statements, optionally followed by a final expression. If present, the final expression is the value returned by the block.

### Expressions

Expressions are ordered by precedence (lowest to highest):

#### Logical OR
```
expression = logical_and_expr { "||" logical_and_expr }
```

#### Logical AND
```
logical_and_expr = bitwise_or_expr { "&&" bitwise_or_expr }
```

#### Bitwise OR
```
bitwise_or_expr = bitwise_xor_expr { "|" bitwise_xor_expr }
```

#### Bitwise XOR
```
bitwise_xor_expr = bitwise_and_expr { "^" bitwise_and_expr }
```

#### Bitwise AND
```
bitwise_and_expr = equality_expr { "&" equality_expr }
```

#### Equality
```
equality_expr = relational_expr { ("==" | "!=") relational_expr }
```

#### Relational
```
relational_expr = shift_expr { ("<" | ">" | "<=" | ">=") shift_expr }
```

#### Shift
```
shift_expr = additive_expr { ("<<" | ">>") additive_expr }
```

#### Additive
```
additive_expr = multiplicative_expr { ("+" | "-") multiplicative_expr }
```

#### Multiplicative
```
multiplicative_expr = unary_expr { ("*" | "/" | "%") unary_expr }
```

#### Unary
```
unary_expr = [ "!" | "-" ] primary_expr
```

#### Primary Expressions
```
primary_expr = integer_literal
             | string_literal
             | identifier
             | call_expression
             | array_literal
             | index_expression
             | if_expression
             | loop_expression
             | break_expression
             | continue_expression
             | return_expression
             | "(" expression ")"
```

### Control Flow Expressions

```
if_expression = "if" expression block [ "else" block ]

loop_expression = "loop" expression block

break_expression = "break"

continue_expression = "continue"

return_expression = "return" expression
```

### Function Calls

```
call_expression = identifier "(" [ argument_list ] ")"

argument_list = expression { "," expression }
```

### Arrays

```
array_literal = "[" [ expression { "," expression } ] "]"

index_expression = primary_expr "[" expression "]" { "[" expression ] }
```

Index expressions can be chained for multi-dimensional array access:
- `arr[0]` - single index
- `matrix[0][1]` - nested index

## Operator Precedence

From lowest to highest precedence:

1. Logical OR: `||`
2. Logical AND: `&&`
3. Bitwise OR: `|`
4. Bitwise XOR: `^`
5. Bitwise AND: `&`
6. Equality: `==`, `!=`
7. Relational: `<`, `>`, `<=`, `>=`
8. Shift: `<<`, `>>`
9. Additive: `+`, `-`
10. Multiplicative: `*`, `/`, `%`
11. Unary: `!`, `-` (negation)
12. Primary: literals, identifiers, calls, array access

## Associativity

All binary operators are left-associative.

## Semantics

### Expression-Based Syntax

Lingua uses expression-based syntax where blocks and control flow constructs can return values:

```lingua
let x: i32 = if condition { 10 } else { 20 };

let y: i32 = {
    let temp: i32 = 5;
    temp * 2  // block returns this value
};
```

### Statements vs Expressions

- **Statements** end with `;` and do not produce values
- **Expressions** can be used as values and appear without `;` when used as the final expression in a block

```lingua
fn example(): i32 {
    let x: i32 = 10;  // statement (with semicolon)
    x + 5             // expression (no semicolon - return value)
}
```

### Type Annotations

Type annotations are required in the following contexts:
- Function parameters: `fn foo(x: i32)`
- Function return types: `fn foo(): i32`
- Local variable declarations with `let`: `let x: i32 = 10`
- Global variable declarations: `global counter: i32 = 0`

### Variable Scoping

- Global variables are accessible from all functions
- Local variables are scoped to their enclosing block
- Parameters are scoped to the function body
- Variables must be declared before use

### Memory Model

- **Integers** are stored as i32 (32-bit signed integers)
- **Strings** are stored in a string table and accessed via memory addresses
- **Arrays** are heap-allocated and accessed via pointers
- **Globals** are stored in the WebAssembly global section

## Built-in Functions

The following functions are available as built-ins and do not need to be declared:

### I/O Functions
- `print(str: i32, len: i32): i32` - Print string with newline
- `println(n: i32): i32` - Print integer with newline
- `print_str(str: i32, len: i32): i32` - Print string without newline
- `print_int(n: i32): i32` - Print integer without newline
- `read_char(): i32` - Read a single character from stdin, returns character code or 0 on EOF
- `read_line(): i32` - Read a line from stdin, returns address of null-terminated string (max 255 chars)

### Math Functions
- `abs(n: i32): i32` - Absolute value
- `min(a: i32, b: i32): i32` - Minimum of two values
- `max(a: i32, b: i32): i32` - Maximum of two values

### String Functions
- `str_eq(addr1: i32, len1: i32, addr2: i32, len2: i32): i32` - Compare strings
- `str_copy(src: i32, len: i32, dest: i32): i32` - Copy string
- `str_len(addr: i32): i32` - Get length of null-terminated string
- `str_concat(addr1: i32, len1: i32, addr2: i32, len2: i32): i32` - Concatenate strings
- `str_substr(addr: i32, start: i32, len: i32): i32` - Extract substring

### Memory Functions
- `malloc(size: i32): i32` - Allocate size bytes on heap, returns address
- `malloc_str(addr: i32, len: i32): i32` - Allocate memory for string, returns address
- `memset(addr: i32, val: i32, len: i32): i32` - Set len bytes starting at addr to val, returns addr
- `memcpy(dest: i32, src: i32, len: i32): i32` - Copy len bytes from src to dest, returns dest
- `load(addr: i32): i32` - Load i32 from memory
- `store(addr: i32, val: i32): i32` - Store i32 to memory
- `drop(val: i32): i32` - Discard a value

### System Functions
- `exit(code: i32)` - Exit program (does not return)

## Example Programs

### Minimal Program

```lingua
fn _start(): i32 {
    0
}
```

### Variable Declaration and Assignment

```lingua
fn main(): i32 {
    let x: i32 = 10;
    let y: i32 = 20;
    x = x + y;
    x
}
```

### Function with Parameters

```lingua
fn add(a: i32, b: i32): i32 {
    a + b
}
```

### Control Flow

```lingua
fn factorial(n: i32): i32 {
    if n <= 1 {
        return 1;
    };
    n * factorial(n - 1)
}
```

### Loops

```lingua
fn sum_to_n(n: i32): i32 {
    let i: i32 = 0;
    let sum: i32 = 0;
    loop i < n {
        sum = sum + i;
        i = i + 1;
    };
    sum
}
```

### Arrays

```lingua
fn array_sum(): i32 {
    let arr: i32 = [1, 2, 3, 4, 5];
    let i: i32 = 0;
    let sum: i32 = 0;
    loop i < 5 {
        sum = sum + arr[i];
        i = i + 1;
    };
    sum
}
```

### Global Variables

```lingua
global counter: i32 = 0

fn increment(): i32 {
    counter = counter + 1;
    counter
}
```

## Reserved for Future Use

The following syntax elements are reserved for future language versions:

- Type declarations (`type`, `struct`)
- Enumerations (`enum`)
- Match expressions (`match`)
- Traits and implementations (`trait`, `impl`)
- Generics (`<T>`)
- References and borrowing (`&`, `&mut`)
- Modules and imports (`mod`, `use`)
- Visibility modifiers (`pub`, `priv`)

## Notes

1. **Entry Point**: WASI programs must define a `_start(): i32` function as the entry point
2. **Return Values**: Functions must return a value matching their declared return type
3. **Expression Blocks**: The final expression in a block (without semicolon) is the block's return value
4. **Statement Termination**: Statements must end with semicolons; the final expression in a block should not
5. **Type Safety**: Type mismatches will result in compilation errors (when semantic analysis is implemented)

## Compliance

This grammar specification describes the current implementation of Lingua. Future versions may extend the grammar while maintaining backward compatibility where possible.
