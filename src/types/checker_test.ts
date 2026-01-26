/**
 * Type Checker Tests
 */

import { assertEquals } from "jsr:@std/assert";
import { describe, it } from "jsr:@std/testing/bdd";
import { Lexer } from "../lexer/lexer.ts";
import { Parser } from "../parser/parser.ts";
import { typeCheck, TypeChecker, TypeError } from "./checker.ts";
import { resetTypeVarCounter } from "./types.ts";

// Helper to parse and type check source code
function check(source: string): TypeError[] {
  resetTypeVarCounter();
  const lexer = new Lexer(source);
  const { tokens } = lexer.tokenize();
  const parser = new Parser(tokens);
  const { module } = parser.parse();
  const { errors } = typeCheck(module);
  return errors;
}

// Helper to check that code has no type errors
function expectNoErrors(source: string): void {
  const errors = check(source);
  if (errors.length > 0) {
    throw new Error(`Expected no errors, got: ${errors.map((e) => e.message).join(", ")}`);
  }
}

// Helper to check that code has an error containing a message
function expectError(source: string, messagePart: string): void {
  const errors = check(source);
  const hasMatch = errors.some((e) => e.message.includes(messagePart));
  if (!hasMatch) {
    throw new Error(
      `Expected error containing "${messagePart}", got: ${errors.map((e) => e.message).join(", ")}`,
    );
  }
}

// =============================================================================
// BASIC EXPRESSION TESTS
// =============================================================================

describe("Type Checker - Literals", () => {
  it("type checks integer literals", () => {
    expectNoErrors(`
      fn main() {
        let x = 42
      }
    `);
  });

  it("type checks float literals", () => {
    expectNoErrors(`
      fn main() {
        let x = 3.14
      }
    `);
  });

  it("type checks string literals", () => {
    expectNoErrors(`
      fn main() {
        let x = "hello"
      }
    `);
  });

  it("type checks boolean literals", () => {
    expectNoErrors(`
      fn main() {
        let x = true
        let y = false
      }
    `);
  });
});

// =============================================================================
// VARIABLE TESTS
// =============================================================================

describe("Type Checker - Variables", () => {
  it("type checks variable declarations with type annotations", () => {
    expectNoErrors(`
      fn main() {
        let x: i32 = 42
      }
    `);
  });

  it("infers variable types from initializers", () => {
    expectNoErrors(`
      fn main() {
        let x = 42
        let y = x
      }
    `);
  });

  it("errors on undefined variables", () => {
    expectError(
      `
      fn main() {
        let x = y
      }
    `,
      "Undefined variable: y",
    );
  });

  it("errors on type annotation mismatch", () => {
    expectError(
      `
      fn main() {
        let x: bool = 42
      }
    `,
      "type mismatch",
    );
  });
});

// =============================================================================
// ARITHMETIC TESTS
// =============================================================================

describe("Type Checker - Arithmetic", () => {
  it("type checks addition", () => {
    expectNoErrors(`
      fn main() {
        let x = 1 + 2
      }
    `);
  });

  it("type checks subtraction", () => {
    expectNoErrors(`
      fn main() {
        let x = 5 - 3
      }
    `);
  });

  it("type checks multiplication", () => {
    expectNoErrors(`
      fn main() {
        let x = 2 * 3
      }
    `);
  });

  it("type checks division", () => {
    expectNoErrors(`
      fn main() {
        let x = 10 / 2
      }
    `);
  });

  it("errors on arithmetic with non-numeric types", () => {
    expectError(
      `
      fn main() {
        let x = true + false
      }
    `,
      "numeric operands",
    );
  });

  it("errors on mismatched operand types", () => {
    expectError(
      `
      fn main() {
        let x: i32 = 1
        let y: i64 = 2
        let z = x + y
      }
    `,
      "same type",
    );
  });
});

// =============================================================================
// COMPARISON TESTS
// =============================================================================

describe("Type Checker - Comparisons", () => {
  it("type checks equality", () => {
    expectNoErrors(`
      fn main() {
        let x = 1 == 2
        let y = true == false
      }
    `);
  });

  it("type checks ordering comparisons", () => {
    expectNoErrors(`
      fn main() {
        let a = 1 < 2
        let b = 1 > 2
        let c = 1 <= 2
        let d = 1 >= 2
      }
    `);
  });

  it("comparison returns bool", () => {
    expectNoErrors(`
      fn main() {
        let x: bool = 1 < 2
      }
    `);
  });
});

// =============================================================================
// LOGICAL TESTS
// =============================================================================

describe("Type Checker - Logical", () => {
  it("type checks logical and", () => {
    expectNoErrors(`
      fn main() {
        let x = true && false
      }
    `);
  });

  it("type checks logical or", () => {
    expectNoErrors(`
      fn main() {
        let x = true || false
      }
    `);
  });

  it("errors on non-bool operands", () => {
    expectError(
      `
      fn main() {
        let x = 1 && 2
      }
    `,
      "bool",
    );
  });
});

// =============================================================================
// UNARY TESTS
// =============================================================================

describe("Type Checker - Unary", () => {
  it("type checks negation", () => {
    expectNoErrors(`
      fn main() {
        let x = -42
      }
    `);
  });

  it("type checks logical not", () => {
    expectNoErrors(`
      fn main() {
        let x = !true
      }
    `);
  });

  it("errors on negating non-numeric", () => {
    expectError(
      `
      fn main() {
        let x = -true
      }
    `,
      "numeric operand",
    );
  });
});

// =============================================================================
// FUNCTION TESTS
// =============================================================================

describe("Type Checker - Functions", () => {
  it("type checks function definitions", () => {
    expectNoErrors(`
      fn add(a: i32, b: i32) -> i32 {
        a + b
      }
    `);
  });

  it("type checks function calls", () => {
    expectNoErrors(`
      fn double(x: i32) -> i32 {
        x * 2
      }

      fn main() {
        let y = double(21)
      }
    `);
  });

  it("errors on wrong argument count", () => {
    expectError(
      `
      fn add(a: i32, b: i32) -> i32 {
        a + b
      }

      fn main() {
        let x = add(1)
      }
    `,
      "Expected 2 arguments",
    );
  });

  it("errors on wrong argument type", () => {
    expectError(
      `
      fn square(x: i32) -> i32 {
        x * x
      }

      fn main() {
        let x = square(true)
      }
    `,
      "type mismatch",
    );
  });

  it("errors on wrong return type", () => {
    expectError(
      `
      fn foo() -> i32 {
        true
      }
    `,
      "type mismatch",
    );
  });

  it("allows unit return type", () => {
    expectNoErrors(`
      fn foo() {
        let x = 42
      }
    `);
  });
});

// =============================================================================
// IF EXPRESSION TESTS
// =============================================================================

describe("Type Checker - If Expressions", () => {
  it("type checks if with else", () => {
    expectNoErrors(`
      fn main() {
        let x = if true { 1 } else { 2 }
      }
    `);
  });

  it("errors on non-bool condition", () => {
    expectError(
      `
      fn main() {
        let x = if 42 { 1 } else { 2 }
      }
    `,
      "must be bool",
    );
  });

  it("errors on branch type mismatch", () => {
    expectError(
      `
      fn main() {
        let x = if true { 1 } else { "hello" }
      }
    `,
      "same type",
    );
  });

  it("if without else must be unit", () => {
    expectNoErrors(`
      fn main() {
        if true { let x = 42 }
      }
    `);
  });
});

// =============================================================================
// BLOCK TESTS
// =============================================================================

describe("Type Checker - Blocks", () => {
  it("block type is trailing expression type", () => {
    expectNoErrors(`
      fn main() {
        let x: i32 = {
          let y = 1
          y + 1
        }
      }
    `);
  });

  it("block without trailing expr is unit", () => {
    expectNoErrors(`
      fn main() {
        let x: () = {
          let y = 1;
        }
      }
    `);
  });

  it("variables in block are scoped", () => {
    expectError(
      `
      fn main() {
        {
          let x = 42
        }
        let y = x
      }
    `,
      "Undefined variable: x",
    );
  });
});

// =============================================================================
// TUPLE TESTS
// =============================================================================

describe("Type Checker - Tuples", () => {
  it("type checks tuple expressions", () => {
    expectNoErrors(`
      fn main() {
        let x = (1, "hello", true)
      }
    `);
  });

  it("type checks tuple field access", () => {
    expectNoErrors(`
      fn main() {
        let t = (1, "hello")
        let x = t.0
        let y = t.1
      }
    `);
  });

  it("errors on out of bounds tuple access", () => {
    expectError(
      `
      fn main() {
        let t = (1, 2)
        let x = t.5
      }
    `,
      "out of bounds",
    );
  });

  it("empty tuple is unit", () => {
    expectNoErrors(`
      fn main() {
        let x: () = ()
      }
    `);
  });
});

// =============================================================================
// ARRAY TESTS
// =============================================================================

describe("Type Checker - Arrays", () => {
  it("type checks array literals", () => {
    expectNoErrors(`
      fn main() {
        let arr = [1, 2, 3]
      }
    `);
  });

  it("array elements must have same type", () => {
    expectError(
      `
      fn main() {
        let arr = [1, "hello", 3]
      }
    `,
      "type mismatch",
    );
  });

  it("type checks array indexing", () => {
    expectNoErrors(`
      fn main() {
        let arr = [1, 2, 3]
        let x = arr[0]
      }
    `);
  });

  it("array index must be integer", () => {
    expectError(
      `
      fn main() {
        let arr = [1, 2, 3]
        let x = arr[true]
      }
    `,
      "must be integer",
    );
  });
});

// =============================================================================
// STRUCT TESTS
// =============================================================================

describe("Type Checker - Structs", () => {
  it("type checks struct definitions", () => {
    expectNoErrors(`
      struct Point {
        x: i32,
        y: i32
      }
    `);
  });

  it("type checks struct literals", () => {
    expectNoErrors(`
      struct Point {
        x: i32,
        y: i32
      }

      fn main() {
        let p = Point { x: 1, y: 2 }
      }
    `);
  });

  it("type checks struct field access", () => {
    expectNoErrors(`
      struct Point {
        x: i32,
        y: i32
      }

      fn main() {
        let p = Point { x: 1, y: 2 }
        let x = p.x
      }
    `);
  });

  it("errors on unknown struct", () => {
    expectError(
      `
      fn main() {
        let p = Unknown { x: 1 }
      }
    `,
      "Unknown struct",
    );
  });

  it("errors on unknown field in literal", () => {
    expectError(
      `
      struct Point {
        x: i32,
        y: i32
      }

      fn main() {
        let p = Point { x: 1, z: 2 }
      }
    `,
      "no field 'z'",
    );
  });

  it("errors on missing field in literal", () => {
    expectError(
      `
      struct Point {
        x: i32,
        y: i32
      }

      fn main() {
        let p = Point { x: 1 }
      }
    `,
      "Missing field",
    );
  });

  it("errors on wrong field type", () => {
    expectError(
      `
      struct Point {
        x: i32,
        y: i32
      }

      fn main() {
        let p = Point { x: true, y: 2 }
      }
    `,
      "type mismatch",
    );
  });

  it("errors on accessing non-existent field", () => {
    expectError(
      `
      struct Point {
        x: i32,
        y: i32
      }

      fn main() {
        let p = Point { x: 1, y: 2 }
        let z = p.z
      }
    `,
      "no field 'z'",
    );
  });
});

// =============================================================================
// ENUM TESTS
// =============================================================================

describe("Type Checker - Enums", () => {
  it("type checks enum definitions", () => {
    expectNoErrors(`
      enum Option {
        None,
        Some(i32)
      }
    `);
  });

  it("type checks unit variant paths", () => {
    expectNoErrors(`
      enum Status {
        Ok,
        Error
      }

      fn main() {
        let s = Status::Ok
      }
    `);
  });
});

// =============================================================================
// CLOSURE TESTS
// =============================================================================

describe("Type Checker - Closures", () => {
  it("type checks closures with annotations", () => {
    expectNoErrors(`
      fn main() {
        let f = |x: i32| -> i32 { x * 2 }
      }
    `);
  });

  it("type checks closure calls", () => {
    expectNoErrors(`
      fn main() {
        let f = |x: i32| -> i32 { x + 1 }
        let y = f(10)
      }
    `);
  });
});

// =============================================================================
// RETURN TESTS
// =============================================================================

describe("Type Checker - Return", () => {
  it("type checks return statements", () => {
    expectNoErrors(`
      fn foo() -> i32 {
        return 42
      }
    `);
  });

  it("errors on return type mismatch", () => {
    expectError(
      `
      fn foo() -> i32 {
        return "hello"
      }
    `,
      "type mismatch",
    );
  });

  it("allows return without value in unit function", () => {
    expectNoErrors(`
      fn foo() {
        return
      }
    `);
  });
});

// =============================================================================
// ASSIGNMENT TESTS
// =============================================================================

describe("Type Checker - Assignment", () => {
  it("allows assignment to mutable variables", () => {
    expectNoErrors(`
      fn main() {
        let mut x = 1
        x = 2
      }
    `);
  });

  it("errors on assignment to immutable variables", () => {
    expectError(
      `
      fn main() {
        let x = 1
        x = 2
      }
    `,
      "Cannot assign to immutable",
    );
  });

  it("errors on assignment type mismatch", () => {
    expectError(
      `
      fn main() {
        let mut x = 1
        x = true
      }
    `,
      "type mismatch",
    );
  });
});

// =============================================================================
// WHILE LOOP TESTS
// =============================================================================

describe("Type Checker - While Loops", () => {
  it("type checks while loops", () => {
    expectNoErrors(`
      fn main() {
        let mut i = 0
        while i < 10 {
          i = i + 1
        }
      }
    `);
  });

  it("errors on non-bool condition", () => {
    expectError(
      `
      fn main() {
        while 42 {
          let x = 1
        }
      }
    `,
      "must be bool",
    );
  });
});

// =============================================================================
// MATCH TESTS
// =============================================================================

describe("Type Checker - Match", () => {
  it("type checks basic match", () => {
    expectNoErrors(`
      fn main() {
        let x = 1
        let y = match x {
          1 => "one",
          2 => "two",
          _ => "other"
        }
      }
    `);
  });

  it("match arms must have same type", () => {
    expectError(
      `
      fn main() {
        let x = 1
        let y = match x {
          1 => 1,
          _ => "other"
        }
      }
    `,
      "same type",
    );
  });
});

// =============================================================================
// PATTERN BINDING TESTS
// =============================================================================

describe("Type Checker - Pattern Binding", () => {
  it("binds variables in patterns", () => {
    expectNoErrors(`
      fn main() {
        let (a, b) = (1, 2)
        let c = a + b
      }
    `);
  });

  it("errors on tuple pattern length mismatch", () => {
    expectError(
      `
      fn main() {
        let (a, b, c) = (1, 2)
      }
    `,
      "Tuple pattern has 3 elements",
    );
  });
});
