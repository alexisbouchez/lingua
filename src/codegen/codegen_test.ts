/**
 * Code Generator Tests
 */

import { assertEquals } from "jsr:@std/assert";
import { describe, it } from "jsr:@std/testing/bdd";
import { Lexer } from "../lexer/lexer.ts";
import { Parser } from "../parser/parser.ts";
import { compile } from "./codegen.ts";
import { WASM_MAGIC, WASM_VERSION } from "./binary.ts";

// Helper to compile Lingua source to WASM bytes
function compileSource(source: string): Uint8Array {
  const lexer = new Lexer(source);
  const { tokens } = lexer.tokenize();
  const parser = new Parser(tokens);
  const { module } = parser.parse();
  return compile(module);
}

// Helper to check WASM module header
function isValidWasm(bytes: Uint8Array): boolean {
  if (bytes.length < 8) return false;
  for (let i = 0; i < 4; i++) {
    if (bytes[i] !== WASM_MAGIC[i]) return false;
  }
  for (let i = 0; i < 4; i++) {
    if (bytes[i + 4] !== WASM_VERSION[i]) return false;
  }
  return true;
}

describe("Code Generator", () => {
  describe("Basic Functions", () => {
    it("compiles empty function", () => {
      const bytes = compileSource(`
        pub fn main() {
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles function returning constant", () => {
      const bytes = compileSource(`
        pub fn answer() -> i32 {
          42
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles function with parameters", () => {
      const bytes = compileSource(`
        pub fn add(a: i32, b: i32) -> i32 {
          a + b
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles function with locals", () => {
      const bytes = compileSource(`
        pub fn square(x: i32) -> i32 {
          let result = x * x
          result
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });
  });

  describe("Arithmetic", () => {
    it("compiles addition", () => {
      const bytes = compileSource(`
        pub fn add(a: i32, b: i32) -> i32 {
          a + b
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles subtraction", () => {
      const bytes = compileSource(`
        pub fn sub(a: i32, b: i32) -> i32 {
          a - b
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles multiplication", () => {
      const bytes = compileSource(`
        pub fn mul(a: i32, b: i32) -> i32 {
          a * b
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles division", () => {
      const bytes = compileSource(`
        pub fn div(a: i32, b: i32) -> i32 {
          a / b
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles modulo", () => {
      const bytes = compileSource(`
        pub fn mod_(a: i32, b: i32) -> i32 {
          a % b
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles complex expression", () => {
      const bytes = compileSource(`
        pub fn calc(a: i32, b: i32, c: i32) -> i32 {
          (a + b) * c - a / b
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });
  });

  describe("Comparisons", () => {
    it("compiles equality", () => {
      const bytes = compileSource(`
        pub fn eq(a: i32, b: i32) -> i32 {
          if a == b { 1 } else { 0 }
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles inequality", () => {
      const bytes = compileSource(`
        pub fn ne(a: i32, b: i32) -> i32 {
          if a != b { 1 } else { 0 }
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles less than", () => {
      const bytes = compileSource(`
        pub fn lt(a: i32, b: i32) -> i32 {
          if a < b { 1 } else { 0 }
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles less than or equal", () => {
      const bytes = compileSource(`
        pub fn le(a: i32, b: i32) -> i32 {
          if a <= b { 1 } else { 0 }
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles greater than", () => {
      const bytes = compileSource(`
        pub fn gt(a: i32, b: i32) -> i32 {
          if a > b { 1 } else { 0 }
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles greater than or equal", () => {
      const bytes = compileSource(`
        pub fn ge(a: i32, b: i32) -> i32 {
          if a >= b { 1 } else { 0 }
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });
  });

  describe("Logical Operations", () => {
    it("compiles logical and", () => {
      const bytes = compileSource(`
        pub fn and_(a: i32, b: i32) -> i32 {
          if a != 0 && b != 0 { 1 } else { 0 }
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles logical or", () => {
      const bytes = compileSource(`
        pub fn or_(a: i32, b: i32) -> i32 {
          if a != 0 || b != 0 { 1 } else { 0 }
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles logical not", () => {
      const bytes = compileSource(`
        pub fn not_(a: i32) -> i32 {
          if !a { 1 } else { 0 }
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });
  });

  describe("Bitwise Operations", () => {
    it("compiles bitwise and", () => {
      const bytes = compileSource(`
        pub fn band(a: i32, b: i32) -> i32 {
          a & b
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles bitwise or", () => {
      const bytes = compileSource(`
        pub fn bor(a: i32, b: i32) -> i32 {
          a | b
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles bitwise xor", () => {
      const bytes = compileSource(`
        pub fn bxor(a: i32, b: i32) -> i32 {
          a ^ b
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles left shift", () => {
      const bytes = compileSource(`
        pub fn shl(a: i32, b: i32) -> i32 {
          a << b
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles right shift", () => {
      const bytes = compileSource(`
        pub fn shr(a: i32, b: i32) -> i32 {
          a >> b
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });
  });

  describe("Unary Operations", () => {
    it("compiles negation", () => {
      const bytes = compileSource(`
        pub fn neg(x: i32) -> i32 {
          -x
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });
  });

  describe("Control Flow", () => {
    it("compiles if expression", () => {
      const bytes = compileSource(`
        pub fn max(a: i32, b: i32) -> i32 {
          if a > b { a } else { b }
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles nested if", () => {
      const bytes = compileSource(`
        pub fn sign(x: i32) -> i32 {
          if x > 0 {
            1
          } else {
            if x < 0 { -1 } else { 0 }
          }
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles while loop", () => {
      const bytes = compileSource(`
        pub fn sum_to_n(n: i32) -> i32 {
          let mut sum = 0
          let mut i = 1
          while i <= n {
            sum = sum + i
            i = i + 1
          }
          sum
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles return statement", () => {
      const bytes = compileSource(`
        pub fn early_return(x: i32) -> i32 {
          if x > 10 {
            return 10
          }
          x
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });
  });

  describe("Function Calls", () => {
    it("compiles function call", () => {
      const bytes = compileSource(`
        fn helper(x: i32) -> i32 {
          x * 2
        }

        pub fn main(n: i32) -> i32 {
          helper(n)
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles recursive function", () => {
      const bytes = compileSource(`
        pub fn factorial(n: i32) -> i32 {
          if n <= 1 {
            1
          } else {
            n * factorial(n - 1)
          }
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles multiple functions", () => {
      const bytes = compileSource(`
        fn square(x: i32) -> i32 {
          x * x
        }

        fn cube(x: i32) -> i32 {
          x * square(x)
        }

        pub fn power4(x: i32) -> i32 {
          square(square(x))
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });
  });

  describe("Variables", () => {
    it("compiles mutable variable", () => {
      const bytes = compileSource(`
        pub fn increment(x: i32) -> i32 {
          let mut y = x
          y = y + 1
          y
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles compound assignment", () => {
      const bytes = compileSource(`
        pub fn double(x: i32) -> i32 {
          let mut y = x
          y += x
          y
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });

    it("compiles multiple locals", () => {
      const bytes = compileSource(`
        pub fn calc(a: i32, b: i32) -> i32 {
          let x = a + b
          let y = a - b
          let z = x * y
          z
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });
  });

  describe("Booleans", () => {
    it("compiles boolean literals", () => {
      const bytes = compileSource(`
        pub fn get_true() -> i32 {
          if true { 1 } else { 0 }
        }

        pub fn get_false() -> i32 {
          if false { 1 } else { 0 }
        }
      `);
      assertEquals(isValidWasm(bytes), true);
    });
  });
});
