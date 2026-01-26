# WebAssembly Interface Types (WIT) Specification

## Core Purpose

WIT is an Interface Description Language designed to describe component imports/exports and enable sharing types across WebAssembly components. It provides a developer-friendly format for generating components in guest languages and consuming them in host environments.

## Package Structure

Packages are named using the format `namespace:package@version`:
```
wasi:clocks@1.2.0
wasi:filesystem@0.2.8
mycompany:mylib@1.0.0
```

A WIT package is a collection of WIT interfaces and worlds defined in files in the same directory that all use the file extension `.wit`.

## Interfaces

Collections of functions and types representing units of functionality:

```wit
interface my-interface {
    // Type definitions
    record point {
        x: f64,
        y: f64,
    }

    // Function definitions
    distance: func(a: point, b: point) -> f64;
}
```

## Worlds

Complete descriptions of both imports and exports for a component:

```wit
world my-world {
    // What we need from the host
    import wasi:clocks/wall-clock@0.2.8;
    import wasi:io/streams@0.2.8;

    // What we provide
    export run: func() -> result<_, string>;
}
```

### World Composition

Worlds can be combined using `include`:

```wit
world extended {
    include base-world;

    // Additional imports/exports
    export extra-function: func();
}
```

Name conflicts require resolution:
```wit
include other-world with { name as new-name };
```

## Use Statements

Enable sharing types between interfaces:

```wit
interface consumer {
    use producer.{my-type, other-type as renamed};

    process: func(input: my-type) -> renamed;
}
```

## Type System

### Primitive Types

| Type | Description |
|------|-------------|
| `bool` | Boolean (true/false) |
| `u8`, `u16`, `u32`, `u64` | Unsigned integers |
| `s8`, `s16`, `s32`, `s64` | Signed integers |
| `f32`, `f64` | Floating-point |
| `char` | Unicode scalar value |
| `string` | UTF-8 string |

### Records
Structured data with named fields:
```wit
record person {
    name: string,
    age: u32,
    email: option<string>,
}
```

### Variants
One value from a set of types (tagged union):
```wit
variant json-value {
    null,
    boolean(bool),
    number(f64),
    %string(string),
    array(list<json-value>),
    object(list<tuple<string, json-value>>),
}
```

### Enums
Variants without payloads:
```wit
enum color {
    red,
    green,
    blue,
}
```

### Flags
Collections of booleans:
```wit
flags permissions {
    read,
    write,
    execute,
}
```

### Options and Results
```wit
// Optional value
option<string>

// Result with success/error types
result<string, error-code>

// Result with no success value
result<_, error-code>

// Result with no error value
result<string>
```

### Lists and Tuples
```wit
// Homogeneous list
list<u8>

// Heterogeneous tuple
tuple<string, u32, bool>
```

### Resources
Reference types with lifetimes:
```wit
resource file-handle {
    constructor(path: string);
    read: func(len: u64) -> list<u8>;
    write: func(data: list<u8>);
    close: func();
}
```

### Handles
Capabilities to resources:
```wit
// Owned handle - caller is responsible for cleanup
own<file-handle>

// Borrowed handle - callee must not outlive caller
borrow<file-handle>
```

## Functions

### Basic Functions
```wit
add: func(a: s32, b: s32) -> s32;
```

### Functions with Multiple Returns
```wit
divmod: func(a: s32, b: s32) -> tuple<s32, s32>;
```

### Functions with Named Parameters
```wit
create-user: func(
    name: string,
    email: string,
    age: option<u32>,
) -> result<user-id, error>;
```

### Resource Methods
```wit
resource counter {
    constructor(initial: u32);

    // Instance method
    increment: func();

    // Instance method with return
    get: func() -> u32;

    // Static method
    create-pair: static func() -> tuple<counter, counter>;
}
```

## Versioning

Interfaces and worlds support semantic versioning:

```wit
package wasi:http@0.2.8;

@since(version = 0.2.0)
interface types {
    @since(version = 0.2.0)
    record request { ... }

    @since(version = 0.2.5)
    record enhanced-request { ... }
}
```

## Complete Example

```wit
package example:calculator@1.0.0;

interface types {
    enum operation {
        add,
        subtract,
        multiply,
        divide,
    }

    record calculation {
        op: operation,
        a: f64,
        b: f64,
    }

    variant calc-error {
        division-by-zero,
        overflow,
    }
}

interface calculator {
    use types.{calculation, calc-error};

    calculate: func(calc: calculation) -> result<f64, calc-error>;
}

world calculator-world {
    import wasi:cli/stdout@0.2.8;

    export calculator;
}
```
