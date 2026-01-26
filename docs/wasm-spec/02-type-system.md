# WebAssembly Type System Specification

## Overview

The WebAssembly type system classifies entities through validation, instantiation, and execution phases. Types are organized into several hierarchies.

## Core Type Categories

### Number Types
Represent numeric values:
- `i32` and `i64` for 32 and 64-bit integers (interpretation as signed/unsigned depends on operations)
- `f32` and `f64` for IEEE 754 single and double precision floating-point values
- These are "transparent" types—bit patterns are observable and storable in memory

### Vector Types
Process packed data via SIMD operations:
- `v128` represents a 128-bit vector interpreted as packed integers or floating-point values based on specific operations

## Reference Types

Reference types classify first-class pointers to runtime store objects. The syntax is `ref [null] heaptype`:
- Nullable references can be null or proper references
- Non-null references exclude null values
- These are "opaque"—bit patterns are unobservable and stored only in tables

## Heap Type Hierarchies

Three distinct hierarchies exist:

### Function Types
- Common supertype: `func`
- Subtype: `nofunc` (uninhabited)

### Aggregate Types
- Include `struct`, `array`, and `i31` (unboxed scalars)
- Supertypes: `any` (common supertype), `eq` (comparable types)
- Subtype: `none` (uninhabited)

### External Types
- Supertype: `extern`
- Subtype: `noextern` (uninhabited)

## Composite Types

### Function Types
Map parameters to results for function signatures and instruction I/O.

```wat
(func (param i32 i32) (result i32))
```

### Structure Types
Contain heterogeneous fields with optional mutability and packing (i8, i16 compressed storage).

```wat
(type $point (struct (field $x f64) (field $y f64) (field $z f64)))
```

### Array Types
Contain homogeneous elements with optional mutability and packing.

```wat
(type $vector (array (mut f64)))
```

### Recursive Types
Enable mutual recursion among composite types with optional subtyping declarations and finality modifiers.

## Supporting Type Constructs

### Value Types
Union of number, vector, and reference types—what WebAssembly code computes and variables accept.

### Result Types
Sequences of value types representing instruction or function outputs.

### Block Types
Input/output classifiers for control flow structures, given inline or via type index.

### Global Types
Hold mutable or immutable values.

### Memory Types
Linear memories with address type (i32/i64) and size limits measured in pages.

### Table Types
Hold reference types with address type and entry count limits.

### Limits
Define minimum and optional maximum sizes for resizable storage.

## Type Hierarchy Diagram

```
                    any
                     │
         ┌──────────┼──────────┐
         │          │          │
       func        eq       extern
         │       ┌──┴──┐       │
      nofunc  i31  struct   noextern
               array
                 │
               none
```
