# WebAssembly Module Structure

## Core Definition

A WebAssembly module represents "the unit of deployment, loading, and compilation." It organizes definitions including types, tags, globals, memories, tables, and functions, along with imports, exports, initialization segments, and an optional start function.

## Index Spaces

The specification defines distinct zero-based index spaces for different definition classes:

| Index Type | Description |
|-----------|-------------|
| `typeidx` | Type definitions |
| `funcidx` | Functions (including imported functions) |
| `globalidx` | Global variables |
| `tableidx` | Tables |
| `memidx` | Linear memories |
| `tagidx` | Exception tags |
| `elemidx` | Element segments |
| `dataidx` | Data segments |
| `labelidx` | Control flow labels |
| `localidx` | Function parameters and local variables |
| `fieldidx` | Structure fields |

**Important:** Indices of imports precede the indices of other definitions in the same index space.

## Module Components

### Types
Type definitions establish the signatures for functions and other constructs:

```wat
(type $add (func (param i32 i32) (result i32)))
(type $point (struct (field $x f64) (field $y f64)))
```

### Functions
Each function references a function type by index, declares parameters and local variables, and contains an instruction sequence body:

```wat
(func $add (type $add) (param $a i32) (param $b i32) (result i32)
  (local $tmp i32)
  local.get $a
  local.get $b
  i32.add
)
```

### Tables
Arrays of opaque reference-typed values with configurable size limits:

```wat
(table $funcs 10 funcref)
(table $objects 0 100 externref)
```

### Memories
Linear byte arrays with minimum and maximum size constraints (measured in 64KB pages):

```wat
(memory $main 1 16)  ;; 1 page min, 16 pages max
(memory $large i64 1 1000)  ;; 64-bit addressed
```

### Globals
Single-value storage with mutability specification:

```wat
(global $counter (mut i32) (i32.const 0))
(global $pi f64 (f64.const 3.14159))
```

### Imports
Two-level module/item naming requiring matching external types during instantiation:

```wat
(import "env" "log" (func $log (param i32)))
(import "env" "memory" (memory 1))
(import "env" "table" (table 10 funcref))
(import "env" "global" (global i32))
```

### Exports
Provide unique names referencing definitions:

```wat
(export "add" (func $add))
(export "memory" (memory $main))
(export "table" (table $funcs))
```

### Data Segments
Initialize memory, supporting active and passive modes:

```wat
;; Active: initialized at instantiation
(data (i32.const 0) "Hello, World!")

;; Passive: available for runtime copying
(data $greeting "Hello!")
```

### Element Segments
Initialize tables with references:

```wat
;; Active segment
(elem (i32.const 0) $func1 $func2)

;; Passive segment
(elem $funcs funcref (ref.func $f1) (ref.func $f2))
```

### Start Function
Optional initialization function called after instantiation:

```wat
(start $init)
```

## Module Text Format Example

```wat
(module
  ;; Types
  (type $binary_op (func (param i32 i32) (result i32)))

  ;; Imports
  (import "console" "log" (func $log (param i32)))

  ;; Memory
  (memory (export "memory") 1)

  ;; Globals
  (global $result (mut i32) (i32.const 0))

  ;; Functions
  (func $add (export "add") (type $binary_op)
    (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.add
  )

  (func $sub (export "sub") (type $binary_op)
    (param $a i32) (param $b i32) (result i32)
    local.get $a
    local.get $b
    i32.sub
  )

  ;; Table with function references
  (table $ops 2 funcref)
  (elem (i32.const 0) $add $sub)

  ;; Start function
  (func $init
    i32.const 42
    global.set $result
  )
  (start $init)
)
```

## Binary Format Structure

The binary format consists of a header followed by sections:

1. **Header**: Magic number + version
2. **Custom Section** (optional, ID 0)
3. **Type Section** (ID 1)
4. **Import Section** (ID 2)
5. **Function Section** (ID 3)
6. **Table Section** (ID 4)
7. **Memory Section** (ID 5)
8. **Tag Section** (ID 13) - for exceptions
9. **Global Section** (ID 6)
10. **Export Section** (ID 7)
11. **Start Section** (ID 8)
12. **Element Section** (ID 9)
13. **Data Count Section** (ID 12)
14. **Code Section** (ID 10)
15. **Data Section** (ID 11)

Sections must appear in this order, though most are optional.
