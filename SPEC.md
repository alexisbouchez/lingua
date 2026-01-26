# Lingua Language Specification

**A programming language designed for WebAssembly, not adapted to it.**

## Philosophy

Lingua is built from the ground up for WASM/WASI. No legacy, no compromise. Every feature maps cleanly to WebAssembly constructs while providing a delightful developer experience.

### Design Principles

1. **Zero friction** - If it's tedious, we fix it
2. **Explicit is good, verbose is bad** - Clear intent, minimal syntax
3. **WASM-native** - Every construct has a direct WASM translation
4. **Component-first** - Built for the Component Model era
5. **Batteries included** - WASI integration is first-class

---

## Syntax Overview

### Hello World

```lingua
fn main() {
    print("Hello, World!")
}
```

### Variables

```lingua
// Immutable by default (like Rust, but simpler)
let x = 42
let name = "Alice"

// Mutable when needed
let mut counter = 0
counter = counter + 1

// Type annotations (usually inferred)
let age: i32 = 25
let pi: f64 = 3.14159
```

### Functions

```lingua
// Simple function
fn add(a: i32, b: i32) -> i32 {
    a + b  // implicit return
}

// Multiple returns (maps to WASM multi-value)
fn divmod(a: i32, b: i32) -> (i32, i32) {
    (a / b, a % b)
}

// Destructuring
let (quotient, remainder) = divmod(10, 3)

// Named returns for clarity
fn parse(s: str) -> (value: i32, ok: bool) {
    // ...
}
let result = parse("42")
print(result.value)  // access by name!
```

### Control Flow

```lingua
// If expressions (not statements!)
let max = if a > b { a } else { b }

// Pattern matching - the star of the show
match value {
    0 => print("zero"),
    1..=9 => print("single digit"),
    n if n < 0 => print("negative"),
    _ => print("other"),
}

// Match with destructuring
match point {
    Point { x: 0, y: 0 } => "origin",
    Point { x: 0, y } => "on y-axis at {y}",
    Point { x, y: 0 } => "on x-axis at {x}",
    Point { x, y } => "at ({x}, {y})",
}

// Loops
loop {
    // infinite loop, break to exit
    if done { break }
}

while condition {
    // ...
}

for item in collection {
    // ...
}

// Loop labels for nested control
outer: for x in 0..10 {
    for y in 0..10 {
        if x * y > 50 { break outer }
    }
}
```

### Types

#### Primitives (map directly to WASM)

```lingua
// Integers
i8, i16, i32, i64    // signed
u8, u16, u32, u64    // unsigned

// Floats
f32, f64

// Other
bool                  // i32 in WASM
char                  // Unicode scalar (i32)
str                   // UTF-8 string
```

#### Composite Types

```lingua
// Structs (map to WASM GC structs)
struct Point {
    x: f64,
    y: f64,
}

let p = Point { x: 1.0, y: 2.0 }
let p2 = Point { ..p, y: 3.0 }  // spread syntax

// Enums with data (map to WASM GC variants)
enum Result<T, E> {
    Ok(T),
    Err(E),
}

enum Option<T> {
    Some(T),
    None,
}

// Simple enums (map to i32)
enum Color {
    Red,
    Green,
    Blue,
}

// Tuples
let pair: (i32, str) = (42, "hello")
let (num, text) = pair
```

#### Arrays and Slices

```lingua
// Fixed-size array (stack or static)
let nums: [i32; 5] = [1, 2, 3, 4, 5]

// Dynamic array (GC heap)
let vec: [i32] = [1, 2, 3]
vec.push(4)

// Slice (view into array)
let slice = nums[1..4]
```

### Error Handling

```lingua
// Results are the primary error mechanism
fn read_file(path: str) -> Result<str, IoError> {
    // ...
}

// The ? operator propagates errors
fn process() -> Result<(), Error> {
    let content = read_file("data.txt")?
    let parsed = parse(content)?
    Ok(())
}

// Match for explicit handling
match read_file("data.txt") {
    Ok(content) => print(content),
    Err(e) => print("Error: {e}"),
}

// or_else, unwrap_or, map, etc.
let content = read_file("data.txt")
    .unwrap_or("default")
```

### Strings and Interpolation

```lingua
// String interpolation with {}
let name = "World"
let greeting = "Hello, {name}!"

// Expressions in interpolation
let result = "The answer is {40 + 2}"

// Multi-line strings
let json = """
    {
        "name": "{name}",
        "value": {value}
    }
"""

// Raw strings (no escapes)
let regex = r"\\d+\\.\\d+"
```

### Closures

```lingua
// Type-inferred closures
let double = |x| x * 2
let sum = |a, b| a + b

// With type annotations
let parse: |str| -> i32 = |s| s.parse()

// Multi-line
let process = |data| {
    let cleaned = clean(data)
    let parsed = parse(cleaned)
    transform(parsed)
}

// Capturing
let multiplier = 3
let triple = |x| x * multiplier
```

### Generics

```lingua
// Generic functions
fn identity<T>(x: T) -> T {
    x
}

// Generic structs
struct Pair<A, B> {
    first: A,
    second: B,
}

// Trait bounds
fn print_all<T: Display>(items: [T]) {
    for item in items {
        print("{item}")
    }
}

// Where clauses for complex bounds
fn merge<K, V>(a: Map<K, V>, b: Map<K, V>) -> Map<K, V>
where
    K: Eq + Hash,
    V: Clone,
{
    // ...
}
```

### Traits (Interfaces)

```lingua
trait Display {
    fn display(self) -> str
}

trait Add<Rhs = Self> {
    type Output
    fn add(self, rhs: Rhs) -> Self::Output
}

// Implementation
impl Display for Point {
    fn display(self) -> str {
        "({self.x}, {self.y})"
    }
}

// Default implementations
trait Printable {
    fn to_string(self) -> str

    fn print(self) {
        print(self.to_string())
    }
}
```

### Modules and Imports

```lingua
// File: math/vector.ln
pub struct Vec2 {
    pub x: f64,
    pub y: f64,
}

pub fn dot(a: Vec2, b: Vec2) -> f64 {
    a.x * b.x + a.y * b.y
}

// File: main.ln
use math::vector::{Vec2, dot}
use std::io::*

fn main() {
    let v = Vec2 { x: 1.0, y: 2.0 }
    print("{}", dot(v, v))
}
```

---

## WASI Integration

### First-Class WASI Support

```lingua
// Import WASI interfaces naturally
use wasi::filesystem::{read, write, open}
use wasi::http::{fetch, Request, Response}
use wasi::cli::{args, env}

fn main() {
    // Command-line args
    for arg in args() {
        print(arg)
    }

    // Environment variables
    let home = env("HOME").unwrap_or("/")

    // File I/O
    let content = read("config.toml")?
    write("output.txt", processed)?
}
```

### HTTP Made Easy

```lingua
use wasi::http::*

fn main() -> Result<()> {
    // Simple GET
    let body = fetch("https://api.example.com/data")?.text()?

    // Full control
    let response = Request::get("https://api.example.com/users")
        .header("Authorization", "Bearer {token}")
        .send()?

    match response.status {
        200..=299 => print("Success: {}", response.json::<User>()?),
        _ => print("Error: {}", response.status),
    }
}
```

### Async/Await (Component Model native)

```lingua
// Async functions
async fn fetch_all(urls: [str]) -> [Result<str>] {
    // Concurrent execution
    urls.map(|url| async {
        fetch(url)?.text()
    }).await_all()
}

// Sequential await
async fn process() -> Result<()> {
    let data = fetch(url).await?
    let parsed = parse(data).await?
    save(parsed).await?
}

// Select first completed
async fn race() -> str {
    select {
        a = fetch(url1) => a?,
        b = fetch(url2) => b?,
    }
}
```

---

## Component Model Integration

### Defining Interfaces

```lingua
// Export a component interface
#[component]
interface Calculator {
    fn add(a: i32, b: i32) -> i32
    fn multiply(a: i32, b: i32) -> i32
}

// Implement the interface
impl Calculator {
    fn add(a: i32, b: i32) -> i32 { a + b }
    fn multiply(a: i32, b: i32) -> i32 { a * b }
}
```

### Resource Types

```lingua
// Resources map to Component Model resources
#[resource]
struct Connection {
    handle: i32,
}

impl Connection {
    #[constructor]
    fn new(url: str) -> Self { ... }

    fn query(self, sql: str) -> Result<Rows> { ... }

    #[drop]
    fn close(self) { ... }
}
```

### Importing Components

```lingua
// Import from another component
use component("my-lib:utils@1.0.0")::StringUtils

fn main() {
    let s = StringUtils::capitalize("hello")
}
```

---

## Memory Management

### GC by Default (WASM GC)

```lingua
// Heap allocated, garbage collected
let data = [1, 2, 3, 4, 5]  // GC array
let point = Point { x: 1, y: 2 }  // GC struct

// No manual memory management needed
// References are traced automatically
```

### Linear Memory When Needed

```lingua
// For performance-critical code
#[linear]
fn process_buffer(data: &[u8]) {
    // Direct linear memory access
    // More control, more responsibility
}

// Allocator control
#[linear]
mod allocator {
    static mut HEAP_PTR: i32 = 0

    pub fn alloc(size: i32) -> *u8 { ... }
    pub fn free(ptr: *u8, size: i32) { ... }
}
```

---

## Low-Level Access

### Inline WebAssembly

```lingua
fn fast_sqrt(x: f64) -> f64 {
    wasm {
        local.get $x
        f64.sqrt
    }
}

fn atomic_add(ptr: *i32, val: i32) -> i32 {
    wasm {
        local.get $ptr
        local.get $val
        i32.atomic.rmw.add
    }
}
```

### Direct WASM Type Mapping

```lingua
// When you need precise control
#[wasm(func (param i32 i32) (result i32))]
fn add_raw(a: i32, b: i32) -> i32 {
    a + b
}

// SIMD
#[wasm(v128)]
type Vec4f = [f32; 4]

fn dot4(a: Vec4f, b: Vec4f) -> f32 {
    wasm {
        local.get $a
        local.get $b
        f32x4.mul
        f32x4.extract_lane 0
        f32x4.extract_lane 1
        f32.add
        f32x4.extract_lane 2
        f32.add
        f32x4.extract_lane 3
        f32.add
    }
}
```

---

## Standard Library

### Core Types

```lingua
// Option and Result
Option<T> { Some(T), None }
Result<T, E> { Ok(T), Err(E) }

// Collections
Array<T>      // Dynamic array (GC)
Map<K, V>     // Hash map
Set<T>        // Hash set
String        // UTF-8 string (GC)

// I/O
Reader, Writer, Seeker
Buffer
```

### Iterators

```lingua
let nums = [1, 2, 3, 4, 5]

// Chainable operations
let result = nums
    .filter(|n| n % 2 == 0)
    .map(|n| n * 2)
    .sum()

// Lazy evaluation
let squares = (1..).map(|n| n * n)
let first_100 = squares.take(100).collect()
```

### String Operations

```lingua
let s = "Hello, World!"

s.len()                    // byte length
s.chars().count()          // character count
s.contains("World")        // substring check
s.split(",")               // iterator of parts
s.trim()                   // whitespace removal
s.replace("World", "Wasm") // substitution

// Parsing
"42".parse::<i32>()        // Result<i32, ParseError>
```

---

## Tooling

### Project Structure

```
my-project/
├── lingua.toml          # Project manifest
├── src/
│   ├── main.ln          # Entry point
│   └── lib/
│       └── utils.ln
├── tests/
│   └── utils_test.ln
└── build/
    └── my-project.wasm
```

### lingua.toml

```toml
[package]
name = "my-project"
version = "0.1.0"
edition = "2026"

[dependencies]
json = "1.2"
http-client = "0.5"

[wasi]
features = ["filesystem", "http", "random"]

[component]
world = "wasi:cli/command"
```

### CLI Commands

```bash
lingua new my-project      # Create new project
lingua build               # Compile to WASM
lingua run                 # Build and run with wasmtime
lingua test                # Run tests
lingua fmt                 # Format code
lingua check               # Type check without building
lingua doc                 # Generate documentation
```

---

## Grammar (EBNF)

```ebnf
program       = { item } ;
item          = fn_def | struct_def | enum_def | trait_def
              | impl_block | use_decl | mod_decl ;

fn_def        = [ "pub" ] [ "async" ] "fn" IDENT generics? "(" params? ")"
                [ "->" type ] block ;
params        = param { "," param } ;
param         = IDENT ":" type ;

struct_def    = [ "pub" ] "struct" IDENT generics? "{" fields? "}" ;
fields        = field { "," field } ;
field         = [ "pub" ] IDENT ":" type ;

enum_def      = [ "pub" ] "enum" IDENT generics? "{" variants "}" ;
variants      = variant { "," variant } ;
variant       = IDENT [ "(" types ")" ] ;

trait_def     = [ "pub" ] "trait" IDENT generics? [ ":" bounds ]
                "{" trait_items "}" ;
trait_items   = { fn_sig | fn_def } ;

impl_block    = "impl" generics? [ trait "for" ] type "{" impl_items "}" ;

block         = "{" { statement } [ expr ] "}" ;
statement     = let_stmt | expr_stmt | item ;
let_stmt      = "let" [ "mut" ] pattern [ ":" type ] "=" expr ";" ;
expr_stmt     = expr ";" ;

expr          = literal | IDENT | binary_expr | unary_expr | call_expr
              | field_expr | index_expr | if_expr | match_expr
              | loop_expr | block | closure | "(" expr ")" ;

binary_expr   = expr op expr ;
op            = "+" | "-" | "*" | "/" | "%" | "==" | "!=" | "<" | ">"
              | "<=" | ">=" | "&&" | "||" | "|" | "&" | "^" ;

if_expr       = "if" expr block [ "else" ( if_expr | block ) ] ;
match_expr    = "match" expr "{" match_arms "}" ;
match_arms    = match_arm { "," match_arm } ;
match_arm     = pattern [ "if" expr ] "=>" expr ;

pattern       = "_" | IDENT | literal | tuple_pat | struct_pat
              | enum_pat | range_pat ;

type          = IDENT | generic_type | tuple_type | fn_type | ref_type ;
generic_type  = IDENT "<" types ">" ;
types         = type { "," type } ;

generics      = "<" generic_params ">" ;
generic_params = generic_param { "," generic_param } ;
generic_param = IDENT [ ":" bounds ] ;
bounds        = type { "+" type } ;

literal       = INT | FLOAT | STRING | CHAR | "true" | "false" ;
```

---

## Compilation Model

```
   Lingua Source (.ln)
          │
          ▼
   ┌─────────────┐
   │    Lexer    │
   └─────────────┘
          │ Tokens
          ▼
   ┌─────────────┐
   │   Parser    │
   └─────────────┘
          │ AST
          ▼
   ┌─────────────┐
   │ Type Check  │
   └─────────────┘
          │ Typed AST
          ▼
   ┌─────────────┐
   │  HIR Lower  │
   └─────────────┘
          │ HIR
          ▼
   ┌─────────────┐
   │  MIR Lower  │
   └─────────────┘
          │ MIR (CFG)
          ▼
   ┌─────────────┐
   │ WASM Codegen│
   └─────────────┘
          │
          ▼
    WASM Binary (.wasm)
    or Component (.wasm)
```

---

## Version

Lingua 0.1.0 - Initial Design
Target: WebAssembly 3.0, WASI Preview 2, Component Model
