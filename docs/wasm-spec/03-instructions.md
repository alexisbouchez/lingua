# WebAssembly Instructions Reference

## Overview

WebAssembly instructions form the computational foundation of the language, operating on a **stack machine model** where instructions manipulate an implicit operand stack by consuming and producing values.

Instructions are categorized into **plain** and **structured** types, with support for S-expression folded form as syntactic abbreviation.

## Instruction Categories

### 1. Parametric Instructions
Basic stack manipulation operations:
- `unreachable` – trap unconditionally
- `nop` – no operation
- `drop` – remove top stack value
- `select` – conditional selection with optional type annotation

### 2. Control Instructions
Structured flow control with optional label binding:
- **`block`** – sequential block
- **`loop`** – loop with backward branch target
- **`if...else...end`** – conditional branching
- **`try_table`** – exception handling with catch clauses
- **`br`** – unconditional branch
- **`br_if`** – conditional branch
- **`br_table`** – indirect branch via lookup table
- **`br_on_null`**, **`br_on_non_null`** – reference null checks
- **`br_on_cast`**, **`br_on_cast_fail`** – type-based branching
- **`call`** – direct function call
- **`call_ref`** – indirect call via reference
- **`call_indirect`** – indirect call via table
- **`return_call`** – tail call
- **`return_call_indirect`** – indirect tail call
- **`throw`**, **`throw_ref`** – exception throwing

### 3. Variable Instructions
Local and global variable access:
- `local.get`, `local.set`, `local.tee` – local variable operations
- `global.get`, `global.set` – global variable operations

### 4. Table Instructions
Dynamic table manipulation:
- `table.get`, `table.set` – element access
- `table.size`, `table.grow` – size management
- `table.fill`, `table.copy`, `table.init` – bulk operations
- `elem.drop` – drop element segment

### 5. Memory Instructions
Load/store operations with memory arguments:

**Integer loads:**
- `i32.load`, `i64.load`
- `i32.load8_s`, `i32.load8_u`, `i32.load16_s`, `i32.load16_u`
- `i64.load8_s`, `i64.load8_u`, `i64.load16_s`, `i64.load16_u`
- `i64.load32_s`, `i64.load32_u`

**Float loads:**
- `f32.load`, `f64.load`

**Vector loads:**
- `v128.load` and lane-specific variants

**Store operations:**
- Corresponding `*.store` instructions for all types

**Memory management:**
- `memory.size`, `memory.grow`
- `memory.fill`, `memory.copy`, `memory.init`
- `data.drop`

Memory arguments include optional offset and alignment, defaulting to zero and natural alignment respectively.

### 6. Reference Instructions
Typed reference operations:
- `ref.null` – null reference creation
- `ref.func` – function reference creation
- `ref.is_null` – null check
- `ref.as_non_null` – assert non-null
- `ref.eq` – reference equality
- `ref.test`, `ref.cast` – type testing and casting

### 7. Aggregate Instructions (GC)
Object and collection manipulation:

**i31 boxing:**
- `ref.i31` – box integer to i31ref
- `i31.get_s`, `i31.get_u` – unbox with sign extension

**Struct operations:**
- `struct.new`, `struct.new_default` – allocation
- `struct.get`, `struct.get_s`, `struct.get_u` – field access
- `struct.set` – field mutation

**Array operations:**
- `array.new`, `array.new_default`, `array.new_fixed`, `array.new_data`, `array.new_elem` – allocation
- `array.get`, `array.get_s`, `array.get_u` – element access
- `array.set` – element mutation
- `array.len` – length query
- `array.fill`, `array.copy` – bulk operations

**Type conversion:**
- `any.convert_extern`, `extern.convert_any`

### 8. Numeric Instructions
Operate on `i32`, `i64`, `f32`, `f64` types:

**Constants:**
- `i32.const`, `i64.const`, `f32.const`, `f64.const`

**Comparisons:**
- `eqz` (integers only)
- `eq`, `ne`
- `lt`, `gt`, `le`, `ge` (with `_s`/`_u` variants for integers)

**Unary operations:**
- Integers: `clz`, `ctz`, `popcnt`
- Floats: `abs`, `neg`, `sqrt`, `ceil`, `floor`, `trunc`, `nearest`

**Binary arithmetic:**
- `add`, `sub`, `mul`
- Integers: `div_s`, `div_u`, `rem_s`, `rem_u`
- Floats: `div`, `min`, `max`, `copysign`

**Bitwise (integers):**
- `and`, `or`, `xor`
- `shl`, `shr_s`, `shr_u`
- `rotl`, `rotr`

**Conversions:**
- `wrap`, `extend`, `trunc`, `convert`, `reinterpret`
- Saturating truncation variants

### 9. Vector Instructions (SIMD)
Operations on 128-bit vectors:
- Splats, lane extraction/replacement
- Shape-specific arithmetic
- Swizzles, shuffles
- Cross-lane operations

## Binary Encoding Quick Reference

| Category | Opcode Range |
|----------|-------------|
| Parametric | 0x00-0x01, 0x1A-0x1C |
| Control | 0x02-0x13 |
| Variables | 0x20-0x24 |
| Memory | 0x28-0x40, 0xFC 8-11 |
| Tables | 0x25-0x26, 0xFC 12-17 |
| References | 0xD0-0xD4, 0xFB 20-23 |
| Aggregates | 0xFB 0-19, 26-30 |
| Numeric | 0x41-0xC4, 0xFC 0-7 |
| Vector | 0xFD prefix |

## Key Structural Concepts

- **Label indexing** is relative by nesting depth (label 0 = innermost enclosing control structure)
- **Branches** only exit outward from their target structure
- **Memory arguments** include static offset and alignment exponent
- **Expressions** are instruction sequences, sometimes restricted to constant forms for initialization
- **Block types** use type use syntax analogous to function types
- **Default indices** for table and memory default to 0 for backward compatibility
