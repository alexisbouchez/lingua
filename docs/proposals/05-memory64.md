# WebAssembly Memory64 Proposal

## Status: Phase 3 (Implementation)

Extends WebAssembly to support linear memory larger than 4GB by allowing 64-bit memory indices.

## Purpose

- Support memories >4GB (2³² bytes)
- Enable languages requiring 64-bit pointer widths
- Maintain backward compatibility with 32-bit memories

## Key Changes

### Address Type
New `addrtype` for memories and tables:
- `i32` (default, 32-bit)
- `i64` (64-bit)

### Memory Declaration

```wat
;; 32-bit memory (default)
(memory $small 1 16)

;; 64-bit memory
(memory $large i64 1 1000000)
```

### Table Declaration (Table64)

```wat
;; 64-bit indexed table
(table $big i64 1000000 funcref)
```

## Instruction Changes

Memory instructions automatically use the memory's address type:

```wat
;; For i64 memory, addresses are i64
(i32.load (i64.const 0x100000000))  ;; address > 4GB
(i32.store (i64.const 0x100000000) (i32.const 42))
```

### Size and Growth

```wat
;; Returns i64 for 64-bit memory
(memory.size)  ;; i64 page count

;; Takes i64 delta, returns i64 previous size
;; Returns 2^64-1 on failure (not -1)
(memory.grow (i64.const 100))
```

### Limits

| Address Type | Max Pages |
|--------------|-----------|
| i32 | 2¹⁶ (65536) = 4GB |
| i64 | 2⁴⁸ = 16PB theoretical |

Practical limits depend on host system.

## Binary Format

### Limits Encoding

| Flags | Meaning |
|-------|---------|
| 0x00 | i32, min only |
| 0x01 | i32, min and max |
| 0x04 | i64, min only |
| 0x05 | i64, min and max |
| 0x02 | i32, shared, min and max |
| 0x06 | i64, shared, min and max |

### Offset Encoding
Memory instruction offsets support 64-bit values for i64 memories.

## Text Format

```wat
;; Explicit address type
(memory $m i64 1 100)

;; With shared
(memory $shared i64 1 100 shared)
```

## Implementation Status

| Component | Memory64 | Table64 |
|-----------|----------|---------|
| V8/Chrome | ✓ | In progress |
| Firefox | ✓ | In progress |
| Spec interpreter | ✓ | ✓ |
| wabt | ✓ | ✓ |
| binaryen | ✓ | ✓ |
| emscripten | ✓ | - |

## Example

```wat
(module
  ;; 64-bit memory: can grow beyond 4GB
  (memory $heap i64 1)

  ;; Allocate at high address
  (func (export "write_high") (param $offset i64) (param $value i32)
    (i32.store (local.get $offset) (local.get $value)))

  (func (export "read_high") (param $offset i64) (result i32)
    (i32.load (local.get $offset)))

  ;; Memory operations return/take i64
  (func (export "grow") (param $pages i64) (result i64)
    (memory.grow (local.get $pages)))

  (func (export "size") (result i64)
    (memory.size))
)
```

## Interoperability

- 32-bit and 64-bit memories can coexist (with multi-memory)
- Functions can work with either type via address parameter
- Importing/exporting preserves address type requirement
