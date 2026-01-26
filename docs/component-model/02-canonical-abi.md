# Canonical ABI Specification

## Overview

The Canonical ABI defines how component-level values are read from and written to linear memory. It bridges component-model abstractions with core WebAssembly semantics.

## Core Operations

### Lifting
Converts low-level WebAssembly values to high-level component abstractions:
- Reads bytes from linear memory
- Interprets according to type schema
- Validates invariants (e.g., UTF-8 encoding)

### Lowering
Converts high-level component values to low-level WebAssembly values:
- Allocates space in linear memory
- Writes bytes according to type schema
- Returns handles/pointers

## Runtime Architecture

### Store
The `Store` manages execution and maintains:
- `pending` list of threads
- Component instances
- Resource tables

### Embedder Interface
- `invoke()` - Execute functions
- `tick()` - Allow suspended threads to resume

### ComponentInstance
Each instance contains:
- `Table` for opaque handles
- Backpressure state
- Exclusive access tracking
- Parent-child relationships

**Reentrancy Prevention:** The system checks ancestor relationships between caller and callee components to prevent recursive reentrancy.

## Memory Layout Rules

### Scalars

| Type | Flat Type | Size | Alignment |
|------|-----------|------|-----------|
| bool | i32 | 1 | 1 |
| s8 | i32 | 1 | 1 |
| u8 | i32 | 1 | 1 |
| s16 | i32 | 2 | 2 |
| u16 | i32 | 2 | 2 |
| s32 | i32 | 4 | 4 |
| u32 | i32 | 4 | 4 |
| s64 | i64 | 8 | 8 |
| u64 | i64 | 8 | 8 |
| f32 | f32 | 4 | 4 |
| f64 | f64 | 8 | 8 |
| char | i32 | 4 | 4 |

### Strings
- Represented as (pointer, length) pair
- Pointer: i32 offset into linear memory
- Length: i32 byte count
- Encoding: UTF-8 (validated on lift)
- Alignment: 4

### Lists
- Represented as (pointer, length) pair
- Pointer: i32 offset to element array
- Length: i32 element count
- Element alignment determines list alignment

### Records
- Fields laid out in declaration order
- Each field aligned to its natural alignment
- Total size rounded up to max field alignment

```
record point { x: f64, y: f64 }

Offset 0: x (f64, 8 bytes)
Offset 8: y (f64, 8 bytes)
Total: 16 bytes, alignment 8
```

### Variants
- Discriminant: smallest int type that fits case count
- Payload: union of all case payloads
- Alignment: max(discriminant alignment, max payload alignment)

```
variant result { ok(string), err(u32) }

Offset 0: discriminant (u8)
Offset 4: payload (max of string=8, u32=4 → 8 bytes)
Total: 12 bytes, alignment 4
```

### Flags
- Represented as smallest int type fitting all flags
- ≤8 flags → u8
- ≤16 flags → u16
- ≤32 flags → u32
- ≤64 flags → u64
- >64 flags → array of u32

### Option<T>
- Equivalent to `variant { none, some(T) }`
- Optimization: if T is a pointer type, use null for none

### Result<T, E>
- Equivalent to `variant { ok(T), err(E) }`

### Handles (own/borrow)
- Represented as i32 table index
- Own handles transfer ownership
- Borrow handles are temporary references

## Flattening

For function calls, types are "flattened" into sequences of core wasm values:

### Flat Value Limits
- Max flat params: 16
- Max flat results: 1

If exceeded, values are passed via memory pointer.

### Flattening Rules

```
flatten(bool) = [i32]
flatten(u8)   = [i32]
flatten(s64)  = [i64]
flatten(f32)  = [f32]
flatten(f64)  = [f64]
flatten(string) = [i32, i32]  // ptr, len
flatten(list<T>) = [i32, i32] // ptr, len
flatten(record {...}) = concat(flatten(field) for field in fields)
flatten(own<R>) = [i32]
flatten(borrow<R>) = [i32]
```

## Canonical Definitions

### Resource Operations
```wat
(canon resource.new $R (core func $f))
(canon resource.drop $R (core func $f))
(canon resource.rep $R (core func $f))
```

### Lifting/Lowering
```wat
(canon lift (core func $f) (func (param string) (result u32))
  (memory $mem) (realloc $realloc))

(canon lower (func $f)
  (core func $core_f)
  (memory $mem) (realloc $realloc))
```

### Required Core Functions
- `realloc`: `(func (param i32 i32 i32 i32) (result i32))`
  - Parameters: old_ptr, old_size, align, new_size
  - Returns: new_ptr
- Memory export for string/list access

## Error Handling

All out-of-memory conditions trap via `unreachable`:
- Simplifies error handling
- No partial failure states
- Host can catch and report

## Concurrency (Post-MVP)

### Task Management
- Tasks form a tree structure
- Parent-child relationships tracked
- Subtasks can be cancelled

### Synchronization Primitives
- `waitable-set`: Set of things to wait on
- `stream`: Async byte/value sequence
- `future`: Single async value

### State Machine
```
Task States:
  Starting → Running → Returned
                    → Cancelled
                    → Blocked (waiting)
```

## String Encoding

### UTF-8 Validation
On lift, strings are validated:
- Valid UTF-8 byte sequences only
- No surrogate code points
- No overlong encodings

### Encoding Parameter
Components can specify string encoding:
- `utf8` (default)
- `utf16`
- `latin1+utf16`

## Example: Lifting a String

```python
def lift_string(cx, ptr, len):
    # Read bytes from memory
    bytes = cx.memory[ptr:ptr+len]

    # Validate and decode UTF-8
    try:
        s = bytes.decode('utf-8')
    except:
        trap()

    # Validate no surrogates
    for c in s:
        if 0xD800 <= ord(c) <= 0xDFFF:
            trap()

    return s
```

## Example: Lowering a String

```python
def lower_string(cx, s):
    # Encode to UTF-8
    bytes = s.encode('utf-8')

    # Allocate memory
    ptr = cx.realloc(0, 0, 1, len(bytes))

    # Write to memory
    cx.memory[ptr:ptr+len(bytes)] = bytes

    return (ptr, len(bytes))
```
