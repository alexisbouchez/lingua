# WebAssembly Execution Semantics

## Stack Machine Model

WebAssembly executes instructions through a stack-based model. Values are pushed onto and popped from an implicit operand stack.

## Basic Operations

### Stack Manipulation
- **`drop`**: Removes a value from the stack
- **`select`**: Chooses between two values based on an i32 condition
  - If c ≠ 0, push val₁
  - If c = 0, push val₂

## Control Flow

### Blocks and Labels

The `block` instruction creates a structured control region with a label having specified arity. When execution completes normally, the label pops from the stack and control transfers to the continuation point.

```wat
(block $exit (result i32)
  i32.const 42
  br $exit        ;; exits block with 42 on stack
  i32.const 0     ;; never reached
)
```

### Loops

Unlike blocks, loops create labels whose continuation points to the loop's start, enabling backward jumps for iteration:

```wat
(loop $continue
  ;; loop body
  br $continue    ;; jumps back to start
)
```

### Conditionals

The `if` instruction consumes an i32 condition, then executes the true branch (if c ≠ 0) or else branch (if c = 0):

```wat
(if (result i32)
  (local.get $condition)
  (then
    i32.const 1   ;; true case
  )
  (else
    i32.const 0   ;; false case
  )
)
```

## Branching Mechanisms

### br (Branch)
Jumps to a label by index:
- When l = 0, targets the innermost label
- For l > 0, recursively pops enclosing labels until reaching the target
- Preserves the appropriate number of values based on target arity

### br_if (Conditional Branch)
```wat
(br_if $label (local.get $condition))
```

### br_table (Indexed Branch)
```wat
(br_table $case0 $case1 $case2 $default
  (local.get $index))
```

## Function Calls

### Call Frame Creation
1. Pop n parameter values from the operand stack
2. Create frame with locals initialized to parameters and defaults
3. Push frame activation with arity m
4. Enter instruction sequence

### Return Semantics
- Pop result values
- Pop call frame
- Push results to caller's stack

### Tail Calls
`return_call` and `return_call_indirect` unwind the current frame before calling, enabling constant-space recursion.

## Exception Handling

### try_table
Establishes exception handlers with catch clauses:

```wat
(try_table (result i32)
  (catch $my_exception $handler)
  ;; code that might throw
)
```

### throw
Creates and throws exceptions using a tag index:
```wat
(throw $error_tag (i32.const 42))
```

### Exception Propagation
1. Search for nearest enclosing try block
2. Try catch clauses sequentially
3. If matched, branch to handler with payload
4. If unmatched, propagate to outer scope

## Memory Access

### Load Operations
```wat
(i32.load offset=4 align=2 (local.get $ptr))
```

### Store Operations
```wat
(i32.store offset=0 (local.get $ptr) (local.get $value))
```

### Bounds Checking
All memory accesses are bounds-checked. Out-of-bounds access traps.

## Runtime Structure

### Store
The store contains all runtime state:
- Module instances
- Function instances
- Table instances
- Memory instances
- Global instances
- Exception tag instances

### Frame
Each call creates a frame containing:
- Return arity
- Local variables (params + locals)
- Module instance reference

### Stack
The stack contains:
- Values (operands)
- Labels (control flow targets)
- Frames (call contexts)

## Trapping

Traps abort execution when:
- Integer division by zero
- Integer overflow in division
- Invalid conversion to integer
- Out-of-bounds memory access
- Out-of-bounds table access
- Indirect call type mismatch
- Stack overflow
- `unreachable` instruction executed

## Instantiation

Module instantiation:
1. Validate module
2. Resolve imports from provided externals
3. Allocate module instance
4. Initialize globals with constant expressions
5. Initialize tables with element segments
6. Initialize memories with data segments
7. Call start function (if present)

## Determinism

WebAssembly execution is deterministic with specific exceptions:
- NaN bit patterns may vary across implementations
- Resource exhaustion behavior is implementation-defined
- Host function behavior is defined by the embedder
