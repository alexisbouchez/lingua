# WebAssembly Exception Handling Proposal

## Status: Shipped (Phase 5)

The exception handling proposal enables WebAssembly code to throw and catch exceptions, with interoperability between WebAssembly and host environments.

## Core Concepts

### Exception Tags
Distinguish different exception types, defined in a module's tag section:

```wat
(tag $my_error (param i32 i32))
(tag $io_error (param i32))
(tag $oom)  ;; no payload
```

### Exception References (exnref)
When caught, exceptions become `exnref` values enabling rethrow:

```wat
(local $caught exnref)
```

## Throwing Exceptions

### throw
Creates and throws an exception using a tag:

```wat
(throw $my_error
  (i32.const 404)
  (i32.const 0))
```

### throw_ref
Rethrows a caught exception:

```wat
(throw_ref (local.get $caught))
```

## Catching Exceptions

### try_table
Structured control flow with catch clauses:

```wat
(try_table (result i32)
  (catch $my_error $handler)      ;; catch specific tag
  (catch_all $fallback)           ;; catch any exception

  ;; code that might throw
  (call $risky_function)
)
```

### Catch Clause Forms

| Form | Description | Stack Effect |
|------|-------------|--------------|
| `catch tag label` | Catches specific tag | Pushes tag arguments |
| `catch_ref tag label` | Catches specific + ref | Pushes arguments + exnref |
| `catch_all label` | Catches any exception | Pushes nothing |
| `catch_all_ref label` | Catches any + ref | Pushes exnref |

## Exception Propagation

1. Exception thrown
2. Search for nearest enclosing try block
3. Try catch clauses sequentially until match
4. If matched: branch to handler with payload
5. If unmatched: propagate to outer scope
6. If uncaught: trap or propagate to host

## Complete Example

```wat
(module
  ;; Define exception tags
  (tag $div_by_zero)
  (tag $overflow (param i32))

  ;; Function that might throw
  (func $divide (param $a i32) (param $b i32) (result i32)
    (if (i32.eqz (local.get $b))
      (then (throw $div_by_zero)))
    (i32.div_s (local.get $a) (local.get $b))
  )

  ;; Function with exception handling
  (func $safe_divide (param $a i32) (param $b i32) (result i32)
    (block $handler (result i32)
      (try_table (result i32)
        (catch $div_by_zero $handler)

        (call $divide (local.get $a) (local.get $b))
      )
      (return)
    )
    ;; Handler: return -1 on division by zero
    (i32.const -1)
  )

  ;; Catch and rethrow
  (func $logged_divide (param $a i32) (param $b i32) (result i32)
    (local $ex exnref)
    (block $rethrow (result exnref)
      (try_table (result i32)
        (catch_all_ref $rethrow)

        (call $divide (local.get $a) (local.get $b))
      )
      (return)
    )
    ;; Log and rethrow
    (local.set $ex)
    (call $log_error)
    (throw_ref (local.get $ex))
  )
)
```

## Binary Encoding

### Value Types
- `exnref`: opcode `-0x17` (0x69)

### Tag Section
- Section code: `13`
- Position: between Memory and Global sections

### Tag Definition
```
tag ::= attr:u8 type:typeidx
attr ::= 0x00  ;; exception attribute
```

### Instructions
| Instruction | Opcode |
|-------------|--------|
| `try_table` | 0x1F |
| `throw` | 0x08 |
| `throw_ref` | 0x0A |

### Catch Clauses
| Clause | Code |
|--------|------|
| `catch` | 0x00 |
| `catch_ref` | 0x01 |
| `catch_all` | 0x02 |
| `catch_all_ref` | 0x03 |

## JavaScript API

### WebAssembly.Tag
```javascript
const myError = new WebAssembly.Tag({
  parameters: ['i32', 'i32']
});

// Check tag type
myError.type(); // { parameters: ['i32', 'i32'] }
```

### WebAssembly.Exception
```javascript
// Create exception
const ex = new WebAssembly.Exception(myError, [404, 0]);

// Check exception tag
ex.is(myError); // true

// Get argument
ex.getArg(myError, 0); // 404
ex.getArg(myError, 1); // 0
```

### Catching in JavaScript
```javascript
try {
  instance.exports.risky_function();
} catch (e) {
  if (e instanceof WebAssembly.Exception) {
    if (e.is(myError)) {
      console.log('Error code:', e.getArg(myError, 0));
    }
  }
}
```

## Import/Export

### Importing Tags
```wat
(import "errors" "io_error" (tag $io_error (param i32)))
```

### Exporting Tags
```wat
(export "my_error" (tag $my_error))
```

## Design Rationale

### Why try_table vs try-catch?
- Integrates with existing control flow
- Uses branch targets instead of inline handlers
- More flexible composition

### Why exnref?
- Enables rethrow without losing exception identity
- Supports logging/wrapping patterns
- Avoids duplicating exception data

### Stack Traces
- Implementation-defined
- JavaScript API may expose via `stack` property
- Not guaranteed across all hosts
