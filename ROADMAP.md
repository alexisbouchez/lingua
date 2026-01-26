# Lingua Development Roadmap

## Phase 1: Foundation (MVP)
**Goal:** Compile a minimal subset to working WASM

### Milestone 1.1: Lexer & Parser
- [ ] Token definitions for all Lingua tokens
- [ ] Lexer with proper error reporting and source locations
- [ ] Recursive descent parser
- [ ] AST data structures
- [ ] Pretty printer for AST debugging

### Milestone 1.2: Type System Core
- [ ] Primitive types (i32, i64, f32, f64, bool)
- [ ] Type inference engine (Hindley-Milner based)
- [ ] Type checking pass
- [ ] Error messages with source locations

### Milestone 1.3: Basic Code Generation
- [ ] WASM binary emitter
- [ ] Function compilation
- [ ] Local variables
- [ ] Basic arithmetic expressions
- [ ] Control flow (if/else, loops)
- [ ] Function calls

### Milestone 1.4: CLI Tool
- [ ] `lingua build` command
- [ ] `lingua run` command (via wasmtime)
- [ ] Error formatting and display
- [ ] Basic project structure

**Deliverable:** Can compile and run:
```lingua
fn main() {
    let x = 40 + 2
    print(x)
}
```

---

## Phase 2: Core Language
**Goal:** Feature-complete core language

### Milestone 2.1: Composite Types
- [ ] Struct definitions and instantiation
- [ ] Enum definitions (simple and with data)
- [ ] Tuple types
- [ ] Pattern matching (exhaustiveness checking)
- [ ] WASM GC integration

### Milestone 2.2: Generics
- [ ] Generic functions
- [ ] Generic structs
- [ ] Generic enums
- [ ] Type parameter bounds
- [ ] Monomorphization

### Milestone 2.3: Traits
- [ ] Trait definitions
- [ ] Trait implementations
- [ ] Trait bounds
- [ ] Associated types
- [ ] Default implementations

### Milestone 2.4: Memory Model
- [ ] GC array types
- [ ] Slice types
- [ ] String type (with UTF-8)
- [ ] Reference semantics

### Milestone 2.5: Error Handling
- [ ] Result and Option types
- [ ] `?` operator
- [ ] Pattern matching on Results
- [ ] Error propagation

**Deliverable:** Can compile:
```lingua
struct Point { x: f64, y: f64 }

fn distance<T: Number>(a: Point, b: Point) -> f64 {
    let dx = a.x - b.x
    let dy = a.y - b.y
    (dx*dx + dy*dy).sqrt()
}

fn main() -> Result<()> {
    let p1 = Point { x: 0.0, y: 0.0 }
    let p2 = Point { x: 3.0, y: 4.0 }
    print(distance(p1, p2))
    Ok(())
}
```

---

## Phase 3: WASI Integration
**Goal:** Full WASI Preview 2 support

### Milestone 3.1: WASI Imports
- [ ] WIT file parsing
- [ ] WASI type mapping
- [ ] Import statement handling
- [ ] Canonical ABI implementation

### Milestone 3.2: Core WASI Interfaces
- [ ] `wasi:io` (streams, poll)
- [ ] `wasi:clocks` (wall-clock, monotonic)
- [ ] `wasi:random`
- [ ] `wasi:filesystem`
- [ ] `wasi:cli`

### Milestone 3.3: Network WASI
- [ ] `wasi:sockets` (TCP, UDP)
- [ ] `wasi:http`

### Milestone 3.4: Async Support
- [ ] Async function syntax
- [ ] Await expressions
- [ ] Future type
- [ ] Pollable integration

**Deliverable:** Can compile:
```lingua
use wasi::http::*
use wasi::filesystem::*

async fn main() -> Result<()> {
    let response = fetch("https://example.com").await?
    write("output.html", response.text()?)?
    Ok(())
}
```

---

## Phase 4: Component Model
**Goal:** Full Component Model support

### Milestone 4.1: Component Exports
- [ ] Interface definitions
- [ ] World targeting
- [ ] Component binary generation

### Milestone 4.2: Resources
- [ ] Resource types
- [ ] Constructors
- [ ] Drop handlers
- [ ] Borrow/own semantics

### Milestone 4.3: Component Imports
- [ ] Component dependency resolution
- [ ] Cross-component calls
- [ ] Type translation

**Deliverable:** Can build and compose components

---

## Phase 5: Developer Experience
**Goal:** Production-ready tooling

### Milestone 5.1: Standard Library
- [ ] Core types (Option, Result)
- [ ] Collections (Array, Map, Set)
- [ ] String utilities
- [ ] Iterators
- [ ] Math functions

### Milestone 5.2: Tooling
- [ ] Formatter (`lingua fmt`)
- [ ] Language server (LSP)
- [ ] Documentation generator
- [ ] Package manager
- [ ] REPL

### Milestone 5.3: Testing
- [ ] Test framework
- [ ] Assertions
- [ ] Test runner
- [ ] Coverage reports

### Milestone 5.4: Optimization
- [ ] Dead code elimination
- [ ] Inlining
- [ ] Constant folding
- [ ] WASM-specific optimizations

---

## Phase 6: Advanced Features
**Goal:** Full language vision

### Milestone 6.1: Advanced Patterns
- [ ] Or patterns
- [ ] Range patterns
- [ ] Binding patterns
- [ ] Guard clauses

### Milestone 6.2: Closures
- [ ] Closure types
- [ ] Capture analysis
- [ ] Move semantics

### Milestone 6.3: Macros
- [ ] Declarative macros
- [ ] Procedural macros (maybe)

### Milestone 6.4: Low-Level
- [ ] Inline WASM
- [ ] Linear memory access
- [ ] SIMD support
- [ ] Atomics/threads

---

## Timeline (Aspirational)

| Phase | Duration | Status |
|-------|----------|--------|
| Phase 1 | 2-3 months | 🚧 In Progress |
| Phase 2 | 3-4 months | ⏳ Planned |
| Phase 3 | 2-3 months | ⏳ Planned |
| Phase 4 | 2-3 months | ⏳ Planned |
| Phase 5 | 3-4 months | ⏳ Planned |
| Phase 6 | Ongoing | ⏳ Future |

---

## Success Metrics

### Phase 1 Complete When:
- Can compile hello world to WASM
- Can run via wasmtime
- Tests pass for all basic operations

### Phase 2 Complete When:
- Can compile generic code
- Pattern matching works correctly
- Error handling is ergonomic

### Phase 3 Complete When:
- Can read/write files
- Can make HTTP requests
- Async code works

### Phase 4 Complete When:
- Can build reusable components
- Components can be composed
- Interop with other component languages

### Phase 5 Complete When:
- IDE integration works
- Package ecosystem exists
- Documentation is comprehensive

### Language "1.0" When:
- All phases complete
- Stable syntax and semantics
- Production users exist
