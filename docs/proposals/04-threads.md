# WebAssembly Threads Proposal

## Status: Phase 4 (Standardization)

The threads proposal introduces multi-threaded execution through shared linear memory and atomic operations.

## Core Concepts

### Agents and Clusters
- **Agent**: Execution context (equivalent to a thread)
- **Agent Cluster**: Group of agents sharing memory

Thread creation/joining is delegated to the embedder (e.g., Web Workers, pthreads).

## Shared Linear Memory

### Declaration
```wat
(memory $shared 1 100 shared)
```

### Properties
- Must specify maximum size
- Growth is sequentially consistent across agents
- All agents observe modifications

### Initialization Strategy
Place data segments in a separate module instantiated once, then share that memory:

```wat
;; data.wat - instantiated once
(module
  (memory (export "mem") 1 100 shared)
  (data (i32.const 0) "Hello"))

;; worker.wat - instantiated per thread
(module
  (import "env" "mem" (memory 1 100 shared))
  ;; Use shared memory
)
```

## Atomic Operations

### Atomic Load/Store

```wat
;; Atomic loads (sequentially consistent)
(i32.atomic.load (local.get $ptr))
(i64.atomic.load (local.get $ptr))
(i32.atomic.load8_u (local.get $ptr))
(i32.atomic.load16_u (local.get $ptr))
(i64.atomic.load8_u (local.get $ptr))
(i64.atomic.load16_u (local.get $ptr))
(i64.atomic.load32_u (local.get $ptr))

;; Atomic stores
(i32.atomic.store (local.get $ptr) (local.get $val))
(i64.atomic.store (local.get $ptr) (local.get $val))
(i32.atomic.store8 (local.get $ptr) (local.get $val))
(i32.atomic.store16 (local.get $ptr) (local.get $val))
(i64.atomic.store8 (local.get $ptr) (local.get $val))
(i64.atomic.store16 (local.get $ptr) (local.get $val))
(i64.atomic.store32 (local.get $ptr) (local.get $val))
```

### Read-Modify-Write (RMW)

All RMW operations return the **previous** value:

```wat
;; Add
(i32.atomic.rmw.add (local.get $ptr) (local.get $val))
(i64.atomic.rmw.add (local.get $ptr) (local.get $val))

;; Subtract
(i32.atomic.rmw.sub (local.get $ptr) (local.get $val))

;; Bitwise AND
(i32.atomic.rmw.and (local.get $ptr) (local.get $val))

;; Bitwise OR
(i32.atomic.rmw.or (local.get $ptr) (local.get $val))

;; Bitwise XOR
(i32.atomic.rmw.xor (local.get $ptr) (local.get $val))

;; Exchange (swap)
(i32.atomic.rmw.xchg (local.get $ptr) (local.get $val))
```

### Compare-Exchange (CAS)

```wat
;; If memory[ptr] == expected, store replacement
;; Returns previous value at ptr
(i32.atomic.rmw.cmpxchg
  (local.get $ptr)
  (local.get $expected)
  (local.get $replacement))

(i64.atomic.rmw.cmpxchg
  (local.get $ptr)
  (local.get $expected)
  (local.get $replacement))
```

## Wait and Notify

### memory.atomic.wait32 / wait64
Suspend agent if memory matches expected value:

```wat
;; Wait until memory[ptr] != expected or timeout
;; timeout in nanoseconds (-1 for infinite)
;; Returns: 0=ok, 1=not-equal, 2=timed-out
(memory.atomic.wait32
  (local.get $ptr)
  (local.get $expected)
  (local.get $timeout))

(memory.atomic.wait64
  (local.get $ptr)
  (local.get $expected)
  (local.get $timeout))
```

### memory.atomic.notify
Wake waiting agents:

```wat
;; Wake up to $count agents waiting at $ptr
;; Returns number of agents woken
(memory.atomic.notify
  (local.get $ptr)
  (local.get $count))
```

## Alignment Requirements

**Critical:** Misaligned atomic accesses trap!

| Operation | Required Alignment |
|-----------|-------------------|
| 8-bit | 1 |
| 16-bit | 2 |
| 32-bit | 4 |
| 64-bit | 8 |

## Mutex Example

```wat
(module
  (memory (export "mem") 1 1 shared)

  ;; Mutex at offset 0: 0 = unlocked, 1 = locked

  (func (export "lock")
    (local $old i32)
    (block $done
      (loop $spin
        ;; Try to acquire: CAS(0 -> 1)
        (local.set $old
          (i32.atomic.rmw.cmpxchg
            (i32.const 0)      ;; mutex address
            (i32.const 0)      ;; expected (unlocked)
            (i32.const 1)))    ;; replacement (locked)

        ;; If old was 0, we got the lock
        (br_if $done (i32.eqz (local.get $old)))

        ;; Wait for unlock notification
        (drop (memory.atomic.wait32
          (i32.const 0)        ;; mutex address
          (i32.const 1)        ;; expected (locked)
          (i64.const -1)))     ;; infinite timeout

        (br $spin)
      )
    )
  )

  (func (export "unlock")
    ;; Release lock
    (i32.atomic.store (i32.const 0) (i32.const 0))

    ;; Wake one waiter
    (drop (memory.atomic.notify
      (i32.const 0)   ;; mutex address
      (i32.const 1))) ;; wake 1 agent
  )
)
```

## Fence (atomic.fence)

Full memory barrier:

```wat
(atomic.fence)
```

Ensures all prior memory operations complete before subsequent ones.

## Trapping Conditions

| Operation | Traps When |
|-----------|------------|
| Atomic access | Misaligned |
| Atomic access | Out of bounds |
| wait32/wait64 | Unshared memory |
| wait32/wait64 | Misaligned |
| wait32/wait64 | Out of bounds |
| notify | Misaligned |
| notify | Out of bounds |

## Memory Model

WebAssembly uses a **sequentially consistent** memory model for atomics:
- All agents observe operations in a single total order
- Non-atomic accesses are NOT ordered with respect to atomics
- Data races on non-atomic accesses are undefined behavior
