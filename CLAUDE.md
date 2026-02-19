# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Lingua is a minimal compiler for the Lingua language, written in C11. It produces native binaries directly (no assembler or linker needed) for Linux x86-64 (ELF) and macOS ARM64 (Mach-O).

## Build Commands

```bash
make              # Build compiler + VS Code extension
make lingua       # Build compiler only (output: ./lingua)
make clean        # Remove build artifacts
make install      # Build everything + install VS Code extension
```

Compiler flags: `-Wall -Wextra -std=c11 -Isrc`

## Running

```bash
./lingua main.lingua              # Build and execute immediately (temp binary)
./lingua build main.lingua -o out # Compile to standalone binary
./lingua completions <bash|zsh|fish> # Generate shell completions
./lingua --help                   # Show usage information
```

## VS Code Extension

The `lingua-vscode/` directory contains an LSP-based VS Code extension (syntax highlighting, diagnostics, completions, hover). Uses npm (not bun) for the extension build:

```bash
make vscode          # Build extension
make vscode-install  # Install .vsix to VS Code
```

## Architecture

The compiler follows a classic pipeline: **source -> lexer -> parser (AST) -> codegen -> native binary**.

The codegen stage operates in **dual mode**: programs using only `const` and compile-time expressions follow the original path (pre-compute all values, embed strings in the binary). Programs with `var int` or `var bool` declarations activate the **IR path**: an intermediate representation is generated, then lowered to native code with real stack frames, runtime arithmetic, runtime control flow (if/else branching, for loops with break/continue), and runtime bool printing.

- **`src/diagnostic.c/h`** - Diagnostic reporting. Provides `SourceLoc` for position tracking, `diag_emit()` for colored error/warning messages with source-line caret display, and `diag_error_no_loc()` for locationless errors. Errors are fatal (`exit(1)`); warnings continue compilation.
- **`src/lexer.c/h`** - Tokenizer. Produces tokens for identifiers, strings, ints (decimal and hex `0x`), floats, bools, arithmetic ops (`+`, `-`, `*`, `/`, `%`), comparison ops (`==`, `!=`, `>`, `>=`, `<`, `<=`), bitwise ops (`&`, `|`, `^`, `~`, `<<`, `>>`), compound assignment (`+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`), increment/decrement (`++`, `--`), `.` for member access, `[]` for indexing/slicing, and keywords (`break`, `continue`, `import`, `from`, `pub`, `spawn`). Handles escape sequences (including `\{`, `\}`), `//` line comments, `/* */` block comments, and position tracking for error reporting.
- **`src/parser.c/h`** - Recursive descent parser. Builds an AST with a recursive `Expr` type (tagged union) supporting literals, variable references, binary operations (arithmetic, comparison, logical, bitwise), unary operations (`-`, `~`), member access (`obj.field`), indexing (`s[i]`), slicing (`s[i:j]`), array literals (`EXPR_ARRAY_LIT` for `[expr, ...]`), channel literals (`EXPR_CHANNEL_LIT` for `channel<T>()`), and function calls in expressions (`EXPR_FN_CALL`). Precedence chain: `parse_or` > `parse_and` > `parse_comparison` > `parse_bitor` > `parse_bitxor` > `parse_bitand` > `parse_shift` > `parse_additive` > `parse_multiplicative` > `parse_unary` > `parse_postfix` > `parse_primary`. Includes `parse_interpolated_string()` for `"{expr}"` interpolation, `parse_update_clause()` for for-loop update with `++`/`--`/compound assignment, `try_parse_new_only()` for `new ClassName(...)` on RHS (all other function/method calls use `EXPR_FN_CALL` in expressions), and `parse_full_type()` for `Array<T>` and `Channel<T>` generic type annotations in variable declarations, function parameters, and return types. Supports `NODE_BREAK`, `NODE_CONTINUE`, `NODE_IMPORT`, `NODE_ENUM_DECL`, `NODE_SPAWN`, `is_pub` flag, and `parse_scope_depth`/`parse_loop_depth` tracking.
- **`src/codegen.h`** - Public codegen interface. Declares `codegen()` used by `main.c`.
- **`src/codegen/ir.h`** - IR instruction set and `IRProgram` struct. Defines opcodes (`IR_CONST_INT`, `IR_CONST_STR`, `IR_LOAD_LOCAL`, `IR_STORE_LOCAL`, arithmetic/bitwise/comparison ops, `IR_NEG`, `IR_BIT_NOT`, `IR_LABEL`, `IR_JMP`, `IR_JZ`, `IR_JNZ`, `IR_PRINT_STR`, `IR_PRINT_INT`, `IR_PRINT_BOOL`, `IR_EXIT`), `IRInstr` with virtual registers (unlimited, SSA-lite), `IRString` table, and `IRProgram` with vreg/label/slot allocators.
- **`src/codegen/ir.c`** - IR program init/free/emit/alloc implementations. Convenience functions for emitting common IR patterns (`ir_emit_const_int`, `ir_emit_binop`, `ir_emit_load`, `ir_emit_store`, `ir_emit_print_int`, `ir_emit_print_str`, `ir_emit_print_bool`, etc.).
- **`src/codegen/codegen.c`** - Platform-agnostic codegen entry point. Operates in dual mode: compile-time evaluation via `eval_expr()` (original path) and IR compilation via `ir_compile_expr()` (for expressions involving runtime `var int`/`var bool` variables). Key IR helpers: `expr_is_runtime(expr, st)` checks if an expression references any `has_slot` symbol; `ir_compile_expr(expr, st, prog)` compiles int/bool expressions to IR instructions; `expr_runtime_type(expr, st)` determines whether a runtime expression produces `VAL_BOOL` or `VAL_INT` for print dispatch; `ir_compile_stmts(stmts, st, prog, break_label, continue_label)` compiles a full statement list to IR (handles `NODE_VAR_DECL`, `NODE_ASSIGN`, `NODE_PRINT`, `NODE_IF_STMT`, `NODE_FOR_LOOP`, `NODE_BLOCK`, `NODE_BREAK`, `NODE_CONTINUE`, `NODE_MATCH_STMT`, `NODE_SPAWN`, `NODE_FN_CALL`); `flush_prints_to_ir(prints)` transitions from compile-time to IR mode by flushing pending print list entries as `IR_PRINT_STR`. `Symbol` now has `has_slot`/`slot` fields for runtime variable tracking. `g_ir` global points to the active `IRProgram`; `g_ir_mode` flag tracks whether IR prints have been emitted (for output ordering). `NODE_VAR_DECL` allocates IR slots for `var int` and `var bool`; `NODE_ASSIGN` emits `IR_STORE_LOCAL` for slotted vars; `NODE_PRINT` detects runtime expressions and emits `IR_PRINT_INT`/`IR_PRINT_BOOL`/`IR_PRINT_STR`; `NODE_IF_STMT` detects runtime conditions and compiles if/else chains to IR with `IR_JZ`/`IR_JMP` and labels; `NODE_FOR_LOOP` detects runtime loop variables and compiles for-loops to IR with loop/continue/break labels; `NODE_MATCH_STMT` detects runtime scrutinee and compiles match arms to IR with comparison chains and branch labels. At the end, `codegen()` chooses between `emit_binary()` (compile-time only), `emit_binary_ir()` (runtime IR), `emit_http_binary()`, or `emit_net_binary()`. Also handles compile-time evaluation of `Expr` trees, `SymTable` with parent-chain scoping, `EvalResult` values (including `VAL_OBJECT`, `VAL_ARRAY`, `VAL_CHANNEL`), `ClassTable`, `EnumTable`, `eval_new_expr()`, `evaluate_method_call()`, `ReturnCtx`, stdlib dispatch, and recursion (depth limit 1000).
- **`src/codegen/elf_x86_64.c`** - Linux x86-64 backend. Emits ELF binary with direct syscalls (no libc). Base address 0x400000. Implements `emit_binary()` (compile-time string prints), `emit_binary_ir()` (IR-based binary with stack frames, runtime arithmetic, runtime control flow via label/jump patching, and inline itoa subroutine), `emit_http_binary()` (TCP server with HTTP parsing and route matching), and `emit_net_binary()` (4 modes: TCP listen/connect, UDP listen/send) — all via raw syscalls. The IR backend maps vregs and local slots to stack positions (`[rbp - 8*(slot+1)]` for locals, `[rbp - 8*(slot_count + vreg + 1)]` for vregs), uses `rax`/`rcx`/`rdx` as expression temporaries, includes a complete itoa subroutine for `IR_PRINT_INT`, and emits inline branching for `IR_PRINT_BOOL` (test + branch to write "true" or "false" from data section).
- **`src/codegen/macho_arm64.c`** - macOS ARM64 backend. Emits Mach-O binary with full headers, segments, load commands, and symbol table. Page alignment 16384. `emit_binary_ir()` is stubbed (not yet supported).
- **`src/codegen/codegen_internal.h`** - Shared `Buffer` struct and write utilities (`buf_init`, `buf_write`, `buf_write8/16/32/64`, `buf_pad_to`, `buf_free`). Also defines `HttpRouteEntry` struct and declares `emit_http_binary()`, `NetMode`/`NetConfig` types with `emit_net_binary()` for `std/net`, includes `codegen/ir.h`, and declares `emit_binary_ir()` for the IR-based backend.
- **`src/import.c/h`** - Module import system. Resolves import paths (relative with `./` or project-root-relative), caches parsed module ASTs to avoid re-parsing, and detects circular imports via an import stack. `import_init()` sets the project root, `import_resolve()` resolves a path and returns the cached/parsed AST, `import_push_file()`/`import_pop_file()` manage the circular detection stack, `import_cleanup()` frees all cached modules. Uses `DiagContext` save/restore for multi-file diagnostic reporting.
- **`src/main.c`** - CLI entry point. Handles `build`, `completions`, direct execution, and `--help`.

## Lingua Language Specification

### Comments

```
// Line comment (to end of line)
/* Block comment (can span multiple lines) */
```

Unterminated `/* */` block comments are a compile error.

### Types

| Type     | Literal examples             | Description                  |
|----------|------------------------------|------------------------------|
| `string` | `"hello"`, `"line\n"`        | String with escape sequences |
| `int`    | `0`, `42`, `0xFF`, `0x1A`    | Integer (decimal or hex, parsed as `long`) |
| `float`  | `3.14`, `0.5`                | Floating-point (`double`)    |
| `bool`   | `true`, `false`              | Boolean                      |
| `Array<T>` | `[1, 2, 3]`, `["a", "b"]` | Generic array (`T` is `int`, `float`, `string`, or `bool`) |
| `Channel<T>` | `channel<int>()` | Channel for concurrent communication (`T` is a scalar type) |

### Escape Sequences (in string literals)

`\n` (newline), `\t` (tab), `\r` (carriage return), `\\` (backslash), `\"` (double quote), `\0` (null), `\{` (literal open brace), `\}` (literal close brace). Unknown escape sequences are a compile error.

### Expressions

Expressions are evaluated at compile time and follow this precedence (lowest to highest):

| Precedence  | Operators                        | Associativity | Description              |
|-------------|----------------------------------|---------------|--------------------------|
| 1 (lowest)  | `or`                             | Non-chaining  | Logical OR               |
| 2           | `and`                            | Non-chaining  | Logical AND              |
| 3           | `==` `!=` `>` `>=` `<` `<=`     | Non-chaining  | Comparison               |
| 4           | `\|`                             | Left          | Bitwise OR               |
| 5           | `^`                              | Left          | Bitwise XOR              |
| 6           | `&`                              | Left          | Bitwise AND              |
| 7           | `<<`, `>>`                       | Left          | Bitwise shift            |
| 8           | `+`, `-`                         | Left          | Addition, subtraction    |
| 9           | `*`, `/`, `%`                    | Left          | Multiply, divide, modulo |
| 10          | unary `-`, `~`                   | Right         | Negation, bitwise NOT    |
| 11          | `.field`, `[i]`, `[i:j]`, `()`  | Left          | Postfix                  |
| 12 (highest)| literals, `(expr)`, identifiers  | —             | Primary                  |

**Arithmetic rules:**
- `int op int -> int`, `float op float -> float`, `int op float -> float` (automatic promotion)
- `%` is integer-only; using `%` with floats is a compile error
- Division/modulo by zero is a compile error
- `string + <any>` or `<any> + string` performs concatenation (auto-converts the non-string operand)
- Arithmetic on `bool` or `string` (except `+`) is a compile error

**Bitwise rules:**
- `&`, `|`, `^`, `~`, `<<`, `>>` are integer-only; using them with other types is a compile error

**Comparison rules:**
- Both operands must have the same type (or int/float with promotion)
- `int`, `float`, and `string` support all comparison operators (`==`, `!=`, `>`, `>=`, `<`, `<=`)
- For `bool`, only `==` and `!=` are allowed
- For `Array`, only `==` and `!=` are allowed (element-wise comparison)
- For `Channel`, only `==` and `!=` are allowed (identity/pointer comparison)
- Result is always `bool`

**Parenthesized expressions:** `(expr)` for grouping

### Variable Declaration

```
const <name> = <expr>;          # immutable, type inferred
var <name> = <expr>;            # mutable, type inferred
const <name>: <type> = <expr>;  # immutable, type annotated
var <name>: <type> = <expr>;    # mutable, type annotated
```

- `<type>` must be one of: `int`, `float`, `string`, `bool`, `Array<T>`, or `Channel<T>` (where `T` is one of the scalar types).
- If a type annotation is present, the value's inferred type must match exactly (no coercion).
- `const` variables cannot be reassigned. `var` variables that are never reassigned produce a warning suggesting `const`.

### Assignment

```
<name> = <expr>;
```

- The variable must have been previously declared with `var`.
- Reassigning a `const` variable is a compile error.
- The assigned value's type must match the variable's declared type (no coercion).

### Compound Assignment

```
<name> += <expr>;
<name> -= <expr>;
<name> *= <expr>;
<name> /= <expr>;
<name> %= <expr>;
<name> &= <expr>;
<name> |= <expr>;
<name> ^= <expr>;
<name> <<= <expr>;
<name> >>= <expr>;
```

- Desugars to `<name> = <name> <op> <expr>` at parse time.
- Same type rules as the corresponding operator.

### Increment / Decrement

```
<name>++;
<name>--;
```

- Desugars to `<name> = <name> + 1` / `<name> = <name> - 1` at parse time.
- Also valid in for-loop update clauses: `for (var i = 0; i < n; i++) ...`

### Print

```
print(<expr>);                    # prints value followed by \n
print(<expr>, newline: false);    # prints value without trailing \n
```

- `<expr>` can be any expression (literal, variable, arithmetic, comparison, function call, etc.).
- Values are resolved at compile time and the result string is embedded in the binary.
- By default, `print` appends a newline (`\n`) after the output.
- Pass `newline: false` to suppress the trailing newline.

### Functions

```
fn <name>(<params>) {
    <body>
}

fn <name>(<params>) -> <return_type> {
    <body>
}

// Shorthand: single return statement without braces
fn <name>(<params>) -> <return_type> return <expr>;
```

- Parameters are typed: `name: type`. Parameters with defaults: `name: type = <expr>`.
- Required parameters must come before parameters with defaults.
- Return type is optional; omitting it makes the function void.
- Functions are evaluated at compile time (like everything else). Recursion is supported with a depth limit of 1000 calls.
- `return <expr>;` returns a value. Assigning a void function's result to a variable is a compile error.
- Function calls work as expressions: `fib(n - 1) + fib(n - 2)` is valid.

**Function calls:**

```
<name>(<args>);
<name>(arg1, arg2, param_name: arg3);
```

- Supports positional and named arguments. Positional arguments must come before named arguments.
- Named arguments use `name: value` syntax. Duplicate named arguments are a compile error.

### For Loop

```
for (var <name> = <expr>; <cond>; <update>) {
    <body>
}

// Braceless: single statement body
for (var <name> = <expr>; <cond>; <update>) <statement>;
```

- C-style for loop with init, condition, and update clauses.
- The init clause declares a loop variable (must use `var`).
- The condition must evaluate to a `bool`.
- The update clause supports: `<name> = <expr>`, `<name>++`, `<name>--`, and compound assignment (`<name> += <expr>`, etc.).
- The loop is unrolled at compile time. A safety limit of 10,000 iterations prevents infinite loops.
- The loop variable is scoped to the for loop and removed after it completes.

### Break and Continue

```
break;
continue;
```

- `break` exits the nearest enclosing for loop immediately.
- `continue` skips the rest of the current iteration and proceeds to the update clause.
- Using `break` or `continue` outside of a for loop is a compile error.

### Conditional Statements

```
if (<condition>) {
    <body>
}

if (<condition>) {
    <body>
} else {
    <body>
}

if (<condition>) {
    <body>
} else if (<condition>) {
    <body>
} else {
    <body>
}

// Braceless: single statement bodies
if (<condition>) <statement>;
if (<condition>) <statement>; else <statement>;
if (<condition>) <statement>; else if (<condition>) <statement>; else <statement>;
```

- Condition must evaluate to `bool`; non-bool condition is a compile error.
- Parentheses are required around the condition (like C/JS).
- `else if` chains are supported (parsed as `else { if ... }` internally).
- No trailing `;` required (like `fn` and `for` declarations).
- Evaluated at compile time: only the taken branch is processed.
- Variables declared inside an if/else body are scoped to that block.

### Match Statement

```
match (<expr>) {
    <pattern> => {
        <body>
    }
    _ => {
        <body>
    }
}

// Braceless: single statement arms
match (<expr>) {
    <pattern> => <statement>;
    _ => <statement>;
}
```

- Parentheses are required around the scrutinee expression.
- Matches the scrutinee expression against each arm's pattern top-to-bottom.
- Patterns are expressions (literals, variables, arithmetic) evaluated and compared with `==`.
- `_` is the wildcard arm (default case); must be last if present.
- The first matching arm's body is executed; remaining arms are skipped.
- Each arm body has its own scope (like `if`/`else` blocks).
- Supports all types: `int`, `float`, `string`, `bool` (same type-comparison rules as `==`).
- No trailing `;` required after the closing `}`.
- Evaluated at compile time (like everything else).

### String Interpolation

```
"Hello, {name}!"
"{a} + {b} = {a + b}"
"literal \{braces\}"
```

- Expressions inside `{...}` in string literals are evaluated and their result is concatenated into the string.
- Any valid expression can appear inside `{}`, including arithmetic, function calls, and variable references.
- Use `\{` and `\}` to include literal braces without interpolation.
- Interpolation is desugared to a chain of `+` (concatenation) operations at parse time.

### String Operations

String functions live in the `std/string` standard library module and must be explicitly imported before use:

```
import { len, trim } from "std/string";
import { len, trim, contains, replace, to_upper, to_lower, starts_with, ends_with, index_of, char_at, substr } from "std/string";
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `len(s)` | `string -> int` | Length of string |
| `trim(s)` | `string -> string` | Remove leading/trailing whitespace |
| `contains(s, sub)` | `string, string -> bool` | Check if `s` contains `sub` |
| `replace(s, old, new)` | `string, string, string -> string` | Replace all occurrences |
| `to_upper(s)` | `string -> string` | Convert to uppercase |
| `to_lower(s)` | `string -> string` | Convert to lowercase |
| `starts_with(s, prefix)` | `string, string -> bool` | Check prefix |
| `ends_with(s, suffix)` | `string, string -> bool` | Check suffix |
| `index_of(s, sub)` | `string, string -> int` | Index of first occurrence (-1 if not found) |
| `char_at(s, i)` | `string, int -> string` | Single character at index |
| `substr(s, start, end)` | `string, int, int -> string` | Substring from start to end (exclusive) |

User-defined functions with the same name as a stdlib function shadow the stdlib version. `len`, `contains`, and `index_of` are shared with `std/array` — they can be imported from either module and dispatch based on the first argument's type.

**Indexing and slicing:**

```
"hello"[0]      // "h" — single character access
"hello"[1:4]    // "ell" — substring slice (start inclusive, end exclusive)
```

- Negative indices wrap around from the end of the string.
- Out-of-bounds access is a compile error for indexing; slicing clamps to valid range.

### Arrays

Arrays are a first-class type with generic type annotations. All array operations are functional (return new arrays, never mutate).

**Array literals:**

```
const nums = [1, 2, 3];
const names: Array<string> = ["alice", "bob"];
const empty: Array<int> = [];
```

- Element types are inferred from the first element, or from the type annotation for empty arrays.
- All elements must have the same type; mixed-type arrays are a compile error.
- Nested arrays (`Array<Array<int>>`) are not supported.

**Indexing and slicing:**

```
nums[0]       // 1 — single element access
nums[1:3]     // [2, 3] — slice (start inclusive, end exclusive), returns new array
```

- Out-of-bounds indexing is a compile error.
- Slicing clamps to valid range.

**Equality:**

```
[1, 2, 3] == [1, 2, 3]   // true — element-wise comparison
[1, 2, 3] != [1, 2]      // true
```

- Only `==` and `!=` are supported for arrays.

**Type annotations in functions:**

```
fn sum(arr: Array<int>) -> int { ... }
fn make_range(n: int) -> Array<int> { ... }
```

- `Array<T>` can be used as parameter types and return types.

### Array Operations

Array functions live in the `std/array` standard library module and must be explicitly imported before use:

```
import { len, contains, index_of } from "std/array";
import { push, pop, shift, concat, reverse, sort, join, remove } from "std/array";
```

| Function | Signature | Description |
|----------|-----------|-------------|
| `len(arr)` | `Array<T> -> int` | Number of elements |
| `contains(arr, elem)` | `Array<T>, T -> bool` | Check if array contains element |
| `index_of(arr, elem)` | `Array<T>, T -> int` | Index of first occurrence (-1 if not found) |
| `push(arr, elem)` | `Array<T>, T -> Array<T>` | Return new array with element appended |
| `pop(arr)` | `Array<T> -> Array<T>` | Return new array without last element |
| `shift(arr)` | `Array<T> -> Array<T>` | Return new array without first element |
| `concat(arr1, arr2)` | `Array<T>, Array<T> -> Array<T>` | Return new array with both arrays concatenated |
| `reverse(arr)` | `Array<T> -> Array<T>` | Return new array with elements reversed |
| `sort(arr)` | `Array<T> -> Array<T>` | Return new array with elements sorted |
| `join(arr, sep)` | `Array<string>, string -> string` | Join elements with separator |
| `remove(arr, index)` | `Array<T>, int -> Array<T>` | Return new array with element at index removed |

`len`, `contains`, and `index_of` are shared with `std/string` — they can be imported from either module and dispatch based on the first argument's type.

### Concurrency (Channels and Spawn)

Lingua provides Go-style concurrency primitives: channels for communication and `spawn` for launching concurrent tasks.

**Channels:**

```
import { send, receive } from "std/concurrency";

const ch = channel<int>();     // Create a channel of int
send(ch, 42);                  // Send a value to the channel
const val = receive(ch);       // Receive a value from the channel
```

- `channel<T>()` creates a new channel. `T` must be a scalar type (`int`, `float`, `string`, or `bool`).
- Channels have reference semantics — passing a channel to a function shares the same underlying queue.
- `send(ch, value)` appends to the channel's buffer. The value's type must match the channel's element type. Never blocks (unbounded capacity).
- `receive(ch)` reads the next value from the channel. Receiving from an empty channel is a compile error.
- Channels support `==` and `!=` for identity comparison (pointer equality).
- `Channel<T>` can be used as function parameter types.
- Printing a channel shows `Channel<T>(N items)`.

**Spawn:**

```
fn worker(ch: Channel<int>) {
    send(ch, 42);
}

const ch = channel<int>();
spawn worker(ch);              // Execute worker(ch) immediately
print(receive(ch));            // 42
```

- `spawn` is a statement (like Go's `go`) — fire-and-forget, no return value.
- `spawn` must be followed by a function call (free function or method call).
- At compile time, `spawn` executes the function immediately and discards the result.
- `spawn` is a reserved keyword.

**Channel Operations (std/concurrency):**

| Function | Signature | Description |
|----------|-----------|-------------|
| `send(ch, value)` | `Channel<T>, T -> void` | Send a value to the channel |
| `receive(ch)` | `Channel<T> -> T` | Receive the next value from the channel |

### HTTP Server (std/http)

Lingua can compile HTTP server binaries that listen on a port, accept connections, parse HTTP requests, match routes, and send responses — all via raw Linux syscalls (no libc). Routes and responses are resolved at compile time; the emitted binary runs a real TCP accept loop.

```
import { get, post, listen } from "std/http";

get("/", "Hello, World!");
get("/about", "About page");
post("/echo", "Echo response");

listen(8080);
```

- `get(path, body)` — register a GET route with a static string response body
- `post(path, body)` — register a POST route with a static string response body
- `listen(port)` — emit an HTTP server binary that serves the registered routes on the given port
- `listen()` can only be called once per program
- Port must be between 1 and 65535
- Unmatched routes return HTTP 404 "Not Found"
- Linux x86-64 only; macOS ARM64 produces an error

**HTTP Functions (std/http):**

| Function | Signature | Description |
|----------|-----------|-------------|
| `get(path, body)` | `string, string -> void` | Register a GET route |
| `post(path, body)` | `string, string -> void` | Register a POST route |
| `listen(port)` | `int -> void` | Compile and emit HTTP server binary |

### TCP/UDP Networking (std/net)

Lingua can compile native networking binaries (TCP/UDP servers and clients) that perform socket operations via raw Linux syscalls (no libc). Each program uses one networking mode, configured at compile time, and `start()` triggers binary emission.

```
import { tcp_listen, start } from "std/net";
tcp_listen(8080, "Welcome to Lingua!\n");
start();

import { tcp_connect, start } from "std/net";
tcp_connect("127.0.0.1", 8080, "Hello server!\n");
start();

import { udp_listen, start } from "std/net";
udp_listen(9090, "Pong!\n");
start();

import { udp_send, start } from "std/net";
udp_send("127.0.0.1", 9090, "Ping!\n");
start();
```

- `tcp_listen(port, response)` — TCP server: accepts connections, sends static response, closes
- `tcp_connect(host, port, message)` — TCP client: connects, sends message, reads+prints response
- `udp_listen(port, response)` — UDP server: receives datagrams, replies with static response
- `udp_send(host, port, message)` — UDP client: sends datagram, reads+prints response
- `start()` — emit the configured network binary (must be called exactly once)
- Only one of `tcp_listen`/`tcp_connect`/`udp_listen`/`udp_send` per program
- Port must be between 1 and 65535
- Host must be an IPv4 dotted-decimal string (e.g. `"127.0.0.1"`)
- Linux x86-64 only; macOS ARM64 produces an error

**Net Functions (std/net):**

| Function | Signature | Description |
|----------|-----------|-------------|
| `tcp_listen(port, response)` | `int, string -> void` | Configure TCP server |
| `tcp_connect(host, port, message)` | `string, int, string -> void` | Configure TCP client |
| `udp_listen(port, response)` | `int, string -> void` | Configure UDP server |
| `udp_send(host, port, message)` | `string, int, string -> void` | Configure UDP client |
| `start()` | `void -> void` | Emit the configured network binary |

### Classes

```
class <Name> {
    <field>: <type>;
    fn <method>(<params>) [-> <return_type>] { <body> }
}

class <Child> extends <Parent> {
    <additional_field>: <type>;
    fn <method>(<params>) [-> <return_type>] { <body> }
}
```

- Classes define a named type with fields and methods.
- Fields are typed: `name: type;`. Supported field types: `int`, `float`, `string`, `bool`.
- Methods use the same syntax as functions (`fn`). Inside methods, class fields are accessible as local variables.
- Single inheritance with `extends`: child class inherits all parent fields and methods.
- Child classes can add new fields and override parent methods.
- Method dispatch walks the inheritance chain (child methods take priority).
- No trailing `;` required after the closing `}`.

**Object construction:**

```
const obj = new ClassName(field: value, ...);
const obj = new ClassName(value1, value2, ...);
```

- `new` creates an instance. Arguments are matched to fields by name or position.
- Positional arguments must come before named arguments.
- All fields must be provided; there are no default field values.
- Field types are checked at construction time.

**Field access:**

```
obj.field          // in expressions (returns the field value)
```

- Dot notation accesses object fields. Chains are supported: expressions like `obj.field` can appear anywhere an expression is expected.

**Field mutation:**

```
obj.field = <expr>;
```

- Requires the variable to be declared with `var` (not `const`).
- The assigned value's type must match the field's declared type.

**Method calls:**

```
const result = obj.method(args);   // capture return value
obj.method(args);                  // standalone (void or ignore result)
print(obj.method(args));           // in print
```

- Method calls support the same positional/named argument syntax as function calls.
- Methods can read and mutate the object's fields; mutations are propagated back to the object.
- Calling an undefined method walks the inheritance chain and errors if not found.

### Enums

```
enum <Name> {
    <Variant>,
    <Variant> = <int_value>,
    ...
}
```

- Enums define a named set of integer-backed variants.
- Variants without explicit values auto-increment from the previous value (starting at 0).
- Explicit `= <int>` resets the counter; subsequent implicit variants continue from that value + 1.
- Negative integer values are supported.
- Trailing comma after the last variant is optional.
- No trailing `;` required after the closing `}`.

**Accessing variants:**

```
const c = Color.Red;     // EnumName.Variant syntax
print(c);                // prints the integer value
```

- Enum variants are accessed via `EnumName.Variant` dot notation.
- Enum values are represented as `int` internally, so they support all int operations (comparison, arithmetic, etc.).

**With match statements:**

```
match (status) {
    HttpStatus.Ok => print("ok");
    HttpStatus.NotFound => print("not found");
    _ => print("unknown");
}
```

- Enum variants work as match arm patterns (compared via `==` as int values).

**With pub/import:**

```
pub enum Direction { North, South, East, West }
```

- `pub` makes an enum importable from other modules.

### Import and Pub

```
import { name1, name2 } from "path";
pub fn <name>(<params>) { ... }
pub const <name> = <expr>;
pub var <name> = <expr>;
pub class <Name> { ... }
pub enum <Name> { ... }
```

- `import` brings named symbols (functions, classes, enums, constants, variables) from another module into scope.
- The `.lingua` extension is automatically appended to the import path.
- Paths starting with `./` are relative to the importing file's directory. Other paths are relative to the project root.
- `pub` marks a declaration as publicly visible to importers. Only `pub` symbols can be imported; importing a non-public symbol is a compile error.
- Both `import` and `pub` are only valid at the top level (not inside functions, loops, etc.).
- Each module is parsed and evaluated only once (cached by absolute path).
- Circular imports are detected and produce an error with the full import chain.
- Transitive imports are supported: if A imports B which imports C, A gets B's symbols and B gets C's symbols.
- Side effects (e.g., `print`) in imported files are discarded.

### Block Scoping

```
{
    <body>
}
```

- Bare `{ ... }` blocks introduce a new scope.
- Variables declared inside a block are not visible outside it.
- Inner scopes can read and mutate variables from outer scopes.
- Variable shadowing is supported: a block can declare a variable with the same name as an outer variable; the inner one takes precedence within the block.
- Blocks can be nested arbitrarily deep.
- No trailing `;` required after the closing `}`.

### Identifiers

Identifiers start with a letter or `_`, followed by letters, digits, or `_`. The keywords `const`, `var`, `print`, `true`, `false`, `fn`, `return`, `and`, `or`, `for`, `if`, `else`, `match`, `class`, `new`, `extends`, `enum`, `break`, `continue`, `import`, `from`, `pub`, and `spawn` are reserved.

### Statements

A program is a sequence of statements. Every statement ends with `;` (except function declarations, class declarations, enum declarations, for loops, if/else statements, match statements, and bare blocks). Statement forms: variable declaration, assignment, compound assignment, increment/decrement, field assignment, print, function declaration, class declaration, enum declaration, function call, method call, return, for loop, if/else, match, block, break, continue, import, and spawn.

## Memory Model

The compiler currently evaluates everything at compile time — variables don't exist at runtime, only pre-resolved string literals for print statements get embedded in the binary. The following documents the target memory model as the language grows to support runtime behavior.

### Stack

- Every local variable gets a uniform **8-byte slot** on the stack, indexed by `[rbp - 8*(slot+1)]` (x86-64) or `[x29 - 8*(slot+1)]` (ARM64)
- `int` = 64-bit signed integer, `float` = 64-bit double, `bool` = 64-bit (0/1), `string` = 64-bit pointer to a length-prefixed object
- Standard frame pointer based: `push rbp; mov rbp, rsp; sub rsp, N` (x86-64) / `stp x29, x30, [sp, #-N]!; mov x29, sp` (ARM64)

### Strings

- Length-prefixed objects: `{u64 len, u8 data[]}`
- String **literals** live in the read-only data section (not copied to the arena)
- Dynamic strings (future: concatenation) are allocated on the arena
- Stack slot holds a pointer to the start of the struct; load `[ptr]` for length, `[ptr+8]` for data

### Arena (Heap)

- 1 MB region allocated at program startup via `mmap` (anonymous, read/write, no libc)
- **Bump pointer** in a dedicated callee-saved register: `r14` (x86-64), `x27` (ARM64)
- **Arena limit** in another callee-saved register: `r15` (x86-64), `x28` (ARM64)
- Allocation: `ptr = bump; bump += size; if (bump > limit) abort;`
- No individual deallocation — freed all at once on program exit (process teardown)

### Calling Convention

- Follows platform ABI: System V AMD64 (x86-64), AAPCS64 (ARM64)
- Arguments in `rdi, rsi, rdx, rcx, r8, r9` (x86-64) / `x0-x7` (ARM64)
- Return value in `rax` (x86-64) / `x0` (ARM64)

### Register Allocation

- No general register allocator — all variables live on the stack
- Expressions use temporary registers (`rax, rcx, rdx` on x86-64 / `x0-x3` on ARM64)
- Reserved: `rbp/rsp/r14/r15` (x86-64), `x29/sp/x30/x27/x28` (ARM64)

### Implementation Phases

Completed:
1. ~~**IR layer** — introduce `IRInstr`/`IRProgram` between codegen and backends~~
2. ~~**Stack frames + local variables** — `var int` and `var bool` exist at runtime as stack slots~~
3. ~~**Runtime comparisons + logical ops** — emit real `cmp`/`setCC` instructions~~
4. ~~**Runtime int-to-string** — inline `itoa` for `print(int_var)` without libc~~
5. ~~**Runtime control flow** — `if`/`else` branching and `for` loops with `break`/`continue` when conditions involve runtime variables~~
6. ~~**Runtime bool printing** — `IR_PRINT_BOOL` emits "true"/"false" via branch~~

Future:
7. **Function calls** — real `call`/`ret` with argument passing per ABI
8. **Arena allocator** — `mmap` at startup, bump pointer in `r14`/`x27`

## Keeping CLAUDE.md Up-to-Date

This file must be updated whenever a language feature, CLI command, build target, or architecture component is added or changed. Treat CLAUDE.md as part of the deliverable for any feature work.

## No Test Suite

There is currently no automated test infrastructure. Validation is done by compiling and running `.lingua` files manually.
