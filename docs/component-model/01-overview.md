# WebAssembly Component Model Overview

## Purpose

The Component Model enables WebAssembly modules to be composed together with well-defined interfaces. It provides:

- High-level, language-neutral interface types
- Module composition and linking
- Capability-based security
- Virtualization support

## Core Concepts

### Components vs Modules

| Aspect | Core Module | Component |
|--------|-------------|-----------|
| Scope | Single compilation unit | Composition of modules |
| Types | Low-level (i32, f64, etc.) | High-level (strings, records, etc.) |
| Linking | Import/export by name | Typed interfaces |
| Communication | Shared memory | Canonical ABI |

### Gated Features (Post-MVP)

- 🪙 Value imports/exports and component-level start functions
- 🔀 Asynchronous operations
- 🧵 Threading built-ins
- 📝 Error-context type
- 🔗 Canonical interface names

## Component Structure

Components organize definitions into 12 index spaces:

**Component-level (5):**
1. Types
2. Functions
3. Values
4. Components
5. Instances

**Core-level (7):**
1. Core Types
2. Core Functions
3. Core Tables
4. Core Memories
5. Core Globals
6. Core Tags
7. Core Modules

## Instance Definitions

Create module or component instances by instantiation:

```wat
(component
  (import "wasi:cli/run" (instance $run
    (export "run" (func))
  ))

  (core module $main
    (import "wasi:cli/run" "run" (func))
    ;; ...
  )

  (core instance $main_inst (instantiate $main
    (with "wasi:cli/run" (instance $run))
  ))
)
```

## Alias Definitions

Project definitions from other components:

### Export Aliases
```wat
(alias export $instance "name" (func $f))
```

### Core Export Aliases
```wat
(alias core export $core_instance "memory" (core memory $mem))
```

### Outer Aliases (de Bruijn indices)
```wat
(alias outer 1 $type (type $t))
```

## Type System

### Primitive Types
- `bool`, `char`, `string`
- `s8`, `s16`, `s32`, `s64` (signed integers)
- `u8`, `u16`, `u32`, `u64` (unsigned integers)
- `f32`, `f64` (floating point)

### Container Types
- `list<T>` - dynamic array
- `record { field: T, ... }` - named fields
- `tuple<T, U, ...>` - anonymous fields
- `variant { case(T), ... }` - tagged union
- `enum { case, ... }` - variant without payloads
- `flags { flag, ... }` - bit flags
- `option<T>` - nullable
- `result<T, E>` - success or error

### Handle Types (Resources)
- `own<R>` - owned handle
- `borrow<R>` - borrowed handle

### Async Types (Post-MVP)
- `future<T>` - single async value
- `stream<T>` - async sequence

## Canonical ABI

Defines how component-level values map to/from linear memory:

### Lifting
Converts low-level WebAssembly values to high-level component values:
- Read from linear memory
- Decode according to type rules
- Validate string encoding (UTF-8)

### Lowering
Converts high-level component values to low-level WebAssembly values:
- Allocate in linear memory
- Encode according to type rules
- Return pointer/length pairs

### Memory Layout

| Type | Size | Alignment |
|------|------|-----------|
| bool | 1 | 1 |
| s8/u8 | 1 | 1 |
| s16/u16 | 2 | 2 |
| s32/u32 | 4 | 4 |
| s64/u64 | 8 | 8 |
| f32 | 4 | 4 |
| f64 | 8 | 8 |
| char | 4 | 4 |
| string | 8 | 4 |
| list<T> | 8 | 4 |

### Canonical Built-ins

~40 canonical instructions including:
- `resource.new`, `resource.drop`, `resource.rep`
- `task.cancel`, `task.return`, `task.wait`
- `waitable-set.new`, `waitable-set.wait`
- `stream.new`, `stream.read`, `stream.write`
- `future.new`, `future.read`, `future.write`

## JavaScript Integration

Components can embed in JavaScript through:

### JS API
```javascript
const component = await WebAssembly.compileComponent(bytes);
const instance = await WebAssembly.instantiate(component, imports);
```

### ESM Integration
```javascript
import { myFunc } from "./my-component.wasm";
```

## Binary Format

### Preamble
- Magic: `0x00 0x61 0x73 0x6D`
- Version: `0x0d 0x00` (pre-1.0)
- Layer: `0x01 0x00` (component layer)

### Sections (13 total)
1. Core Module
2. Core Instance
3. Core Type
4. Component
5. Instance
6. Alias
7. Type
8. Canon (canonical definitions)
9. Start
10. Import
11. Export
12. Value

## WASI Preview 2 Integration

The Component Model is the foundation for WASI Preview 2:

- All WASI interfaces defined in WIT
- Components import/export WASI interfaces
- Hosts provide interface implementations
- Components can virtualize WASI for nested components
