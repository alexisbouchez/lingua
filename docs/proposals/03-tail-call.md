# WebAssembly Tail Call Proposal

## Status: Shipped (Phase 5)

The tail call extension enables proper tail call optimization, essential for functional programming languages and certain compilation patterns.

## Motivation

Without this proposal, "the Wasm design explicitly forbids tail call optimisations."

### Use Cases
- Languages requiring tail call elimination (Scheme, Erlang, etc.)
- Compilation of coroutines and continuations
- CPS (Continuation-Passing Style) transformations
- Finite state machines
- Dynamic recompilation techniques

## Instructions

### return_call
Direct tail call to a function:

```wat
(func $factorial (param $n i64) (param $acc i64) (result i64)
  (if (result i64) (i64.le_u (local.get $n) (i64.const 1))
    (then (local.get $acc))
    (else
      (return_call $factorial
        (i64.sub (local.get $n) (i64.const 1))
        (i64.mul (local.get $acc) (local.get $n))))))
```

### return_call_indirect
Indirect tail call through table:

```wat
(return_call_indirect (type $callback)
  (local.get $arg)
  (local.get $func_idx))
```

### return_call_ref
Tail call through function reference:

```wat
(return_call_ref $func_type (local.get $arg) (local.get $func_ref))
```

## Semantics

Tail calls:
1. Pop call operands from stack
2. Clear current stack frame (like `return`)
3. Perform the call operation
4. Callee returns directly to original caller

**Key Property:** Constant stack space for tail-recursive functions.

## Binary Encoding

| Instruction | Opcode |
|-------------|--------|
| `return_call` | 0x12 |
| `return_call_indirect` | 0x13 |
| `return_call_ref` | 0x14 |

## Type System

Tail calls are stack-polymorphic:
- Inherit typing from both `return` and `call`
- Caller and callee parameter types need not match exactly
- Result types must match caller's declared return type

## Example: State Machine

```wat
(module
  (type $state (func (param i32) (result i32)))

  (table $states 3 funcref)
  (elem (i32.const 0) $state_init $state_running $state_done)

  (func $state_init (param $input i32) (result i32)
    ;; Process and transition to running
    (return_call_indirect (type $state)
      (local.get $input)
      (i32.const 1)))  ;; state_running

  (func $state_running (param $input i32) (result i32)
    (if (result i32) (i32.eqz (local.get $input))
      (then
        ;; Transition to done
        (return_call_indirect (type $state)
          (local.get $input)
          (i32.const 2)))  ;; state_done
      (else
        ;; Stay in running
        (return_call_indirect (type $state)
          (i32.sub (local.get $input) (i32.const 1))
          (i32.const 1)))))  ;; state_running

  (func $state_done (param $input i32) (result i32)
    (local.get $input))

  (func (export "run") (param $n i32) (result i32)
    (return_call_indirect (type $state)
      (local.get $n)
      (i32.const 0)))  ;; Start at state_init
)
```

## Guarantees

- Tail calls across WebAssembly module boundaries: **Guaranteed**
- Tail calls to host functions: **Not guaranteed** (host-dependent)

## Implementation Notes

Engines may implement tail calls via:
- Stack frame reuse
- Trampoline mechanisms
- Continuation-passing transformations
