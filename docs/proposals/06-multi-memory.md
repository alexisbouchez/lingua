# WebAssembly Multi-Memory Proposal

## Status: Shipped (Phase 5)

Enables single WebAssembly modules to use multiple memories simultaneously.

## Motivation

### Use Cases

1. **Security** - Separate public shared memory from private encapsulated memory
2. **Isolation** - Distinguish thread-shared memory from single-threaded memory
3. **Persistence** - Different memory lifetimes for selective state persistence
4. **Linking** - Static linking of multi-memory modules
5. **Scaling** - Workaround for 4GB address space limitations
6. **Polyfilling** - Emulate GC, interface types, etc.

## Syntax

### Multiple Memory Declarations

```wat
(module
  (memory $public 1 16)
  (memory $private 1 4)
  (memory $shared 1 100 shared)
)
```

### Memory Instructions with Index

```wat
;; Load from specific memory
(i32.load $private (local.get $ptr))
(i32.load 1 (local.get $ptr))  ;; by index

;; Store to specific memory
(i32.store $public (local.get $ptr) (local.get $val))

;; Memory operations
(memory.size $private)
(memory.grow $private (i32.const 1))
(memory.copy $dst $src
  (local.get $dst_ptr)
  (local.get $src_ptr)
  (local.get $len))
```

### Default Memory

Memory index 0 is the default when not specified:

```wat
;; These are equivalent
(i32.load (local.get $ptr))
(i32.load 0 (local.get $ptr))
```

## Import/Export

```wat
(module
  (import "env" "shared_mem" (memory $shared 1 100 shared))
  (import "env" "private_mem" (memory $private 1))

  (memory $local 1)

  (export "local_mem" (memory $local))
)
```

## Data Segments

```wat
(module
  (memory $mem0 1)
  (memory $mem1 1)

  ;; Initialize mem0
  (data (memory $mem0) (i32.const 0) "Hello")

  ;; Initialize mem1
  (data (memory $mem1) (i32.const 0) "World")

  ;; Passive segment (no memory association until memory.init)
  (data $greeting "Hi there")
)
```

## Binary Encoding

Memory index encoded in alignment field (bit 6):

| Bit 6 | Meaning |
|-------|---------|
| 0 | Memory index 0 |
| 1 | Memory index follows |

For other instructions, hardcoded `0x00` bytes become memory indices.

## Example: Isolated Allocator

```wat
(module
  ;; Application memory
  (memory $app 1 100)

  ;; Allocator metadata (hidden from app)
  (memory $meta 1 1)

  ;; Free list pointer at meta[0]
  (global $free_ptr (mut i32) (i32.const 0))

  (func $alloc (param $size i32) (result i32)
    (local $ptr i32)

    ;; Read from metadata memory
    (local.set $ptr (i32.load $meta (i32.const 0)))

    ;; Update free pointer in metadata
    (i32.store $meta
      (i32.const 0)
      (i32.add (local.get $ptr) (local.get $size)))

    ;; Return pointer into app memory
    (local.get $ptr)
  )

  (func $write (param $ptr i32) (param $val i32)
    ;; Write to application memory
    (i32.store $app (local.get $ptr) (local.get $val))
  )

  (func $read (param $ptr i32) (result i32)
    ;; Read from application memory
    (i32.load $app (local.get $ptr))
  )
)
```

## Example: Copy Between Memories

```wat
(module
  (memory $src 1)
  (memory $dst 1)

  (func $copy_region
    (param $src_off i32)
    (param $dst_off i32)
    (param $len i32)

    (memory.copy $dst $src
      (local.get $dst_off)
      (local.get $src_off)
      (local.get $len))
  )
)
```

## Implementation Notes

- Engines already handle multiple memories conceptually
- Memory 0 can be optimized with register-based access
- Future: custom sections for optimization hints
