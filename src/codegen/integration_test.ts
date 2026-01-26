/**
 * Integration Tests - Run actual compiled WASM modules
 */

import { assertEquals } from "jsr:@std/assert";
import { describe, it } from "jsr:@std/testing/bdd";
import { Lexer } from "../lexer/lexer.ts";
import { Parser } from "../parser/parser.ts";
import { compile } from "./codegen.ts";

// Capture printed values for testing
let printedValues: number[] = [];

// Helper to compile and instantiate WASM
async function compileAndRun(source: string): Promise<WebAssembly.Instance> {
  printedValues = [];

  const lexer = new Lexer(source);
  const { tokens } = lexer.tokenize();
  const parser = new Parser(tokens);
  const { module } = parser.parse();
  const wasm = compile(module);

  const wasmModule = await WebAssembly.compile(wasm.buffer as ArrayBuffer);

  // Provide imports for built-in functions
  const imports = {
    env: {
      print_i32: (value: number) => {
        printedValues.push(value);
        // console.log(value);  // Uncomment to see output during tests
      },
    },
  };

  const instance = await WebAssembly.instantiate(wasmModule, imports);
  return instance;
}

// Helper to get printed values after running
function getPrintedValues(): number[] {
  return [...printedValues];
}

// Helper to get exported function and call it
async function runFunc<T>(source: string, funcName: string, ...args: number[]): Promise<T> {
  const instance = await compileAndRun(source);
  const func = instance.exports[funcName] as (...args: number[]) => T;
  return func(...args);
}

describe("WASM Integration Tests", () => {
  describe("Basic Functions", () => {
    it("returns constant", async () => {
      const result = await runFunc<number>(`
        pub fn answer() -> i32 { 42 }
      `, "answer");
      assertEquals(result, 42);
    });

    it("returns parameter", async () => {
      const result = await runFunc<number>(`
        pub fn identity(x: i32) -> i32 { x }
      `, "identity", 123);
      assertEquals(result, 123);
    });
  });

  describe("Arithmetic", () => {
    it("adds two numbers", async () => {
      const result = await runFunc<number>(`
        pub fn add(a: i32, b: i32) -> i32 { a + b }
      `, "add", 3, 5);
      assertEquals(result, 8);
    });

    it("subtracts two numbers", async () => {
      const result = await runFunc<number>(`
        pub fn sub(a: i32, b: i32) -> i32 { a - b }
      `, "sub", 10, 4);
      assertEquals(result, 6);
    });

    it("multiplies two numbers", async () => {
      const result = await runFunc<number>(`
        pub fn mul(a: i32, b: i32) -> i32 { a * b }
      `, "mul", 7, 6);
      assertEquals(result, 42);
    });

    it("divides two numbers", async () => {
      const result = await runFunc<number>(`
        pub fn div(a: i32, b: i32) -> i32 { a / b }
      `, "div", 20, 4);
      assertEquals(result, 5);
    });

    it("computes modulo", async () => {
      const result = await runFunc<number>(`
        pub fn mod_(a: i32, b: i32) -> i32 { a % b }
      `, "mod_", 17, 5);
      assertEquals(result, 2);
    });

    it("handles complex expressions", async () => {
      const result = await runFunc<number>(`
        pub fn calc(a: i32, b: i32, c: i32) -> i32 {
          (a + b) * c
        }
      `, "calc", 2, 3, 4);
      assertEquals(result, 20);
    });

    it("handles negative numbers", async () => {
      const result = await runFunc<number>(`
        pub fn neg(x: i32) -> i32 { -x }
      `, "neg", 42);
      assertEquals(result, -42);
    });
  });

  describe("Comparisons", () => {
    it("compares equal", async () => {
      const result = await runFunc<number>(`
        pub fn eq(a: i32, b: i32) -> i32 {
          if a == b { 1 } else { 0 }
        }
      `, "eq", 5, 5);
      assertEquals(result, 1);
    });

    it("compares not equal", async () => {
      const result = await runFunc<number>(`
        pub fn ne(a: i32, b: i32) -> i32 {
          if a != b { 1 } else { 0 }
        }
      `, "ne", 5, 3);
      assertEquals(result, 1);
    });

    it("compares less than", async () => {
      const result = await runFunc<number>(`
        pub fn lt(a: i32, b: i32) -> i32 {
          if a < b { 1 } else { 0 }
        }
      `, "lt", 3, 5);
      assertEquals(result, 1);
    });

    it("compares greater than", async () => {
      const result = await runFunc<number>(`
        pub fn gt(a: i32, b: i32) -> i32 {
          if a > b { 1 } else { 0 }
        }
      `, "gt", 5, 3);
      assertEquals(result, 1);
    });
  });

  describe("Bitwise Operations", () => {
    it("performs bitwise AND", async () => {
      const result = await runFunc<number>(`
        pub fn band(a: i32, b: i32) -> i32 { a & b }
      `, "band", 0b1010, 0b1100);
      assertEquals(result, 0b1000);
    });

    it("performs bitwise OR", async () => {
      const result = await runFunc<number>(`
        pub fn bor(a: i32, b: i32) -> i32 { a | b }
      `, "bor", 0b1010, 0b1100);
      assertEquals(result, 0b1110);
    });

    it("performs bitwise XOR", async () => {
      const result = await runFunc<number>(`
        pub fn bxor(a: i32, b: i32) -> i32 { a ^ b }
      `, "bxor", 0b1010, 0b1100);
      assertEquals(result, 0b0110);
    });

    it("performs left shift", async () => {
      const result = await runFunc<number>(`
        pub fn shl(a: i32, b: i32) -> i32 { a << b }
      `, "shl", 1, 4);
      assertEquals(result, 16);
    });

    it("performs right shift", async () => {
      const result = await runFunc<number>(`
        pub fn shr(a: i32, b: i32) -> i32 { a >> b }
      `, "shr", 16, 2);
      assertEquals(result, 4);
    });
  });

  describe("Control Flow", () => {
    it("handles if/else", async () => {
      const max = await runFunc<number>(`
        pub fn max(a: i32, b: i32) -> i32 {
          if a > b { a } else { b }
        }
      `, "max", 5, 10);
      assertEquals(max, 10);

      const max2 = await runFunc<number>(`
        pub fn max(a: i32, b: i32) -> i32 {
          if a > b { a } else { b }
        }
      `, "max", 10, 5);
      assertEquals(max2, 10);
    });

    it("handles nested if", async () => {
      const result = await runFunc<number>(`
        pub fn sign(x: i32) -> i32 {
          if x > 0 {
            1
          } else {
            if x < 0 { -1 } else { 0 }
          }
        }
      `, "sign", -5);
      assertEquals(result, -1);
    });

    it("handles while loop", async () => {
      const result = await runFunc<number>(`
        pub fn sum_to(n: i32) -> i32 {
          let mut sum = 0
          let mut i = 1
          while i <= n {
            sum = sum + i
            i = i + 1
          }
          sum
        }
      `, "sum_to", 10);
      assertEquals(result, 55);
    });

    it("handles early return", async () => {
      const result = await runFunc<number>(`
        pub fn clamp(x: i32, max: i32) -> i32 {
          if x > max {
            return max
          }
          x
        }
      `, "clamp", 100, 50);
      assertEquals(result, 50);
    });
  });

  describe("Function Calls", () => {
    it("calls other functions", async () => {
      const result = await runFunc<number>(`
        fn double(x: i32) -> i32 { x * 2 }
        pub fn quadruple(x: i32) -> i32 { double(double(x)) }
      `, "quadruple", 5);
      assertEquals(result, 20);
    });

    it("handles recursion - factorial", async () => {
      const result = await runFunc<number>(`
        pub fn factorial(n: i32) -> i32 {
          if n <= 1 { 1 } else { n * factorial(n - 1) }
        }
      `, "factorial", 5);
      assertEquals(result, 120);
    });

    it("handles recursion - fibonacci", async () => {
      const result = await runFunc<number>(`
        pub fn fib(n: i32) -> i32 {
          if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }
        }
      `, "fib", 10);
      assertEquals(result, 55);
    });
  });

  describe("Variables", () => {
    it("uses local variables", async () => {
      const result = await runFunc<number>(`
        pub fn calc(a: i32, b: i32) -> i32 {
          let x = a + b
          let y = a - b
          x * y
        }
      `, "calc", 5, 3);
      assertEquals(result, 16);  // (5+3) * (5-3) = 8 * 2 = 16
    });

    it("handles mutable variables", async () => {
      const result = await runFunc<number>(`
        pub fn countdown(n: i32) -> i32 {
          let mut x = n
          let mut count = 0
          while x > 0 {
            x = x - 1
            count = count + 1
          }
          count
        }
      `, "countdown", 5);
      assertEquals(result, 5);
    });

    it("handles compound assignment", async () => {
      const result = await runFunc<number>(`
        pub fn accumulate(n: i32) -> i32 {
          let mut sum = 0
          let mut i = 0
          while i < n {
            sum += i
            i += 1
          }
          sum
        }
      `, "accumulate", 5);
      assertEquals(result, 10);  // 0 + 1 + 2 + 3 + 4 = 10
    });
  });

  describe("Real-World Examples", () => {
    it("computes GCD", async () => {
      const result = await runFunc<number>(`
        pub fn gcd(a: i32, b: i32) -> i32 {
          let mut x = a
          let mut y = b
          while y != 0 {
            let t = y
            y = x % y
            x = t
          }
          x
        }
      `, "gcd", 48, 18);
      assertEquals(result, 6);
    });

    it("checks if prime", async () => {
      const source = `
        pub fn is_prime(n: i32) -> i32 {
          if n <= 1 { return 0 }
          if n <= 3 { return 1 }
          if n % 2 == 0 { return 0 }
          let mut i = 3
          while i * i <= n {
            if n % i == 0 { return 0 }
            i = i + 2
          }
          1
        }
      `;
      assertEquals(await runFunc<number>(source, "is_prime", 2), 1);
      assertEquals(await runFunc<number>(source, "is_prime", 17), 1);
      assertEquals(await runFunc<number>(source, "is_prime", 18), 0);
      assertEquals(await runFunc<number>(source, "is_prime", 1), 0);
    });

    it("computes power", async () => {
      const result = await runFunc<number>(`
        pub fn power(base: i32, exp: i32) -> i32 {
          let mut result = 1
          let mut i = 0
          while i < exp {
            result = result * base
            i = i + 1
          }
          result
        }
      `, "power", 2, 10);
      assertEquals(result, 1024);
    });
  });

  describe("Structs", () => {
    it("creates struct and reads field", async () => {
      const result = await runFunc<number>(`
        struct Point {
          x: i32,
          y: i32,
        }

        pub fn get_x() -> i32 {
          let p = Point { x: 42, y: 100 }
          p.x
        }
      `, "get_x");
      assertEquals(result, 42);
    });

    it("reads multiple fields", async () => {
      const result = await runFunc<number>(`
        struct Point {
          x: i32,
          y: i32,
        }

        pub fn sum_coords() -> i32 {
          let p = Point { x: 10, y: 20 }
          p.x + p.y
        }
      `, "sum_coords");
      assertEquals(result, 30);
    });

    it("passes struct to function", async () => {
      const result = await runFunc<number>(`
        struct Point {
          x: i32,
          y: i32,
        }

        fn magnitude_squared(p: Point) -> i32 {
          p.x * p.x + p.y * p.y
        }

        pub fn test() -> i32 {
          let p = Point { x: 3, y: 4 }
          magnitude_squared(p)
        }
      `, "test");
      assertEquals(result, 25);
    });

    it("creates multiple structs", async () => {
      const result = await runFunc<number>(`
        struct Point {
          x: i32,
          y: i32,
        }

        pub fn add_points() -> i32 {
          let p1 = Point { x: 1, y: 2 }
          let p2 = Point { x: 10, y: 20 }
          p1.x + p1.y + p2.x + p2.y
        }
      `, "add_points");
      assertEquals(result, 33);
    });

    it("handles struct with three fields", async () => {
      const result = await runFunc<number>(`
        struct Vec3 {
          x: i32,
          y: i32,
          z: i32,
        }

        pub fn sum_vec3() -> i32 {
          let v = Vec3 { x: 1, y: 2, z: 3 }
          v.x + v.y + v.z
        }
      `, "sum_vec3");
      assertEquals(result, 6);
    });

    it("computes distance squared between points", async () => {
      const result = await runFunc<number>(`
        struct Point {
          x: i32,
          y: i32,
        }

        fn distance_squared(a: Point, b: Point) -> i32 {
          let dx = b.x - a.x
          let dy = b.y - a.y
          dx * dx + dy * dy
        }

        pub fn test() -> i32 {
          let p1 = Point { x: 0, y: 0 }
          let p2 = Point { x: 3, y: 4 }
          distance_squared(p1, p2)
        }
      `, "test");
      assertEquals(result, 25);
    });
  });

  describe("Enums", () => {
    it("creates unit enum variants", async () => {
      const result = await runFunc<number>(`
        enum Status {
          Ok,
          Error,
        }

        pub fn get_ok() -> i32 {
          let s = Status::Ok
          s
        }
      `, "get_ok");
      assertEquals(result, 0);
    });

    it("creates different enum variants with different tags", async () => {
      const result = await runFunc<number>(`
        enum Status {
          Ok,
          Error,
          Pending,
        }

        pub fn get_error() -> i32 {
          let s = Status::Error
          s
        }
      `, "get_error");
      assertEquals(result, 1);
    });

    it("compares enum variants", async () => {
      const result = await runFunc<number>(`
        enum Status {
          Ok,
          Error,
        }

        pub fn is_ok(s: i32) -> i32 {
          if s == 0 { 1 } else { 0 }
        }

        pub fn test() -> i32 {
          let s = Status::Ok
          is_ok(s)
        }
      `, "test");
      assertEquals(result, 1);
    });

    it("matches enum with simple patterns", async () => {
      const result = await runFunc<number>(`
        enum Status {
          Ok,
          Error,
        }

        pub fn status_code(s: i32) -> i32 {
          match s {
            0 => 200,
            1 => 500,
            _ => 0,
          }
        }

        pub fn test() -> i32 {
          let s = Status::Ok
          status_code(s)
        }
      `, "test");
      assertEquals(result, 200);
    });

    it("matches enum variants directly", async () => {
      const result = await runFunc<number>(`
        enum Color {
          Red,
          Green,
          Blue,
        }

        fn to_rgb(c: i32) -> i32 {
          match c {
            0 => 255,
            1 => 65280,
            2 => 16711680,
            _ => 0,
          }
        }

        pub fn test() -> i32 {
          let c = Color::Blue
          to_rgb(c)
        }
      `, "test");
      assertEquals(result, 16711680);
    });

    it("uses enum in conditional", async () => {
      const result = await runFunc<number>(`
        enum Direction {
          Up,
          Down,
          Left,
          Right,
        }

        pub fn move_y(dir: i32, y: i32) -> i32 {
          if dir == 0 {
            y + 1
          } else {
            if dir == 1 {
              y - 1
            } else {
              y
            }
          }
        }

        pub fn test() -> i32 {
          let d = Direction::Up
          move_y(d, 10)
        }
      `, "test");
      assertEquals(result, 11);
    });

    it("creates enum with tuple variant", async () => {
      const result = await runFunc<number>(`
        enum Option {
          None,
          Some(i32),
        }

        pub fn test() -> i32 {
          let x = Option::Some(42)
          42
        }
      `, "test");
      assertEquals(result, 42);
    });

    it("matches enum tuple variant and extracts value", async () => {
      const result = await runFunc<number>(`
        enum Option {
          None,
          Some(i32),
        }

        pub fn unwrap(opt: i32) -> i32 {
          match opt {
            None => 0,
            Some(x) => x,
          }
        }

        pub fn test() -> i32 {
          let x = Option::Some(100)
          unwrap(x)
        }
      `, "test");
      assertEquals(result, 100);
    });

    it("matches None variant", async () => {
      const result = await runFunc<number>(`
        enum Option {
          None,
          Some(i32),
        }

        pub fn is_some(opt: i32) -> i32 {
          match opt {
            None => 0,
            Some(_) => 1,
          }
        }

        pub fn test() -> i32 {
          let x = Option::None
          is_some(x)
        }
      `, "test");
      assertEquals(result, 0);
    });

    it("handles Option-like pattern with computation", async () => {
      const result = await runFunc<number>(`
        enum Option {
          None,
          Some(i32),
        }

        pub fn double_or_zero(opt: i32) -> i32 {
          match opt {
            None => 0,
            Some(x) => x * 2,
          }
        }

        pub fn test() -> i32 {
          let x = Option::Some(21)
          double_or_zero(x)
        }
      `, "test");
      assertEquals(result, 42);
    });
  });

  describe("Print Function", () => {
    it("prints a single value", async () => {
      await runFunc<void>(`
        pub fn test() {
          print(42)
        }
      `, "test");
      assertEquals(getPrintedValues(), [42]);
    });

    it("prints multiple values", async () => {
      await runFunc<void>(`
        pub fn test() {
          print(1)
          print(2)
          print(3)
        }
      `, "test");
      assertEquals(getPrintedValues(), [1, 2, 3]);
    });

    it("prints computed values", async () => {
      await runFunc<void>(`
        pub fn test() {
          let x = 10
          let y = 20
          print(x + y)
        }
      `, "test");
      assertEquals(getPrintedValues(), [30]);
    });

    it("prints in a loop", async () => {
      await runFunc<void>(`
        pub fn test() {
          let mut i = 0
          while i < 5 {
            print(i)
            i = i + 1
          }
        }
      `, "test");
      assertEquals(getPrintedValues(), [0, 1, 2, 3, 4]);
    });

    it("println works as alias for print", async () => {
      await runFunc<void>(`
        pub fn test() {
          println(100)
        }
      `, "test");
      assertEquals(getPrintedValues(), [100]);
    });
  });

  describe("Tuples", () => {
    it("creates tuple and reads first element", async () => {
      const result = await runFunc<number>(`
        pub fn get_first() -> i32 {
          let t = (42, 100)
          t.0
        }
      `, "get_first");
      assertEquals(result, 42);
    });

    it("creates tuple and reads second element", async () => {
      const result = await runFunc<number>(`
        pub fn get_second() -> i32 {
          let t = (10, 20)
          t.1
        }
      `, "get_second");
      assertEquals(result, 20);
    });

    it("creates tuple with three elements", async () => {
      const result = await runFunc<number>(`
        pub fn sum_tuple() -> i32 {
          let t = (1, 2, 3)
          t.0 + t.1 + t.2
        }
      `, "sum_tuple");
      assertEquals(result, 6);
    });

    it("creates multiple tuples", async () => {
      const result = await runFunc<number>(`
        pub fn add_tuples() -> i32 {
          let a = (1, 2)
          let b = (10, 20)
          a.0 + a.1 + b.0 + b.1
        }
      `, "add_tuples");
      assertEquals(result, 33);
    });

    it("uses tuple elements in computation", async () => {
      const result = await runFunc<number>(`
        pub fn compute() -> i32 {
          let point = (3, 4)
          point.0 * point.0 + point.1 * point.1
        }
      `, "compute");
      assertEquals(result, 25);
    });

    it("destructures tuple with let", async () => {
      const result = await runFunc<number>(`
        pub fn test() -> i32 {
          let (a, b) = (10, 20)
          a + b
        }
      `, "test");
      assertEquals(result, 30);
    });

    it("destructures three element tuple", async () => {
      const result = await runFunc<number>(`
        pub fn test() -> i32 {
          let (x, y, z) = (5, 10, 15)
          x + y + z
        }
      `, "test");
      assertEquals(result, 30);
    });

    it("destructures tuple with wildcard", async () => {
      const result = await runFunc<number>(`
        pub fn test() -> i32 {
          let (a, _) = (42, 100)
          a
        }
      `, "test");
      assertEquals(result, 42);
    });

    it("destructures tuple from variable", async () => {
      const result = await runFunc<number>(`
        pub fn test() -> i32 {
          let t = (3, 7)
          let (a, b) = t
          a * b
        }
      `, "test");
      assertEquals(result, 21);
    });

    it("destructures in function returning tuple", async () => {
      const result = await runFunc<number>(`
        fn make_pair(x: i32, y: i32) -> (i32, i32) {
          (x, y)
        }

        pub fn test() -> i32 {
          let (a, b) = make_pair(5, 8)
          a + b
        }
      `, "test");
      assertEquals(result, 13);
    });
  });

  describe("Arrays", () => {
    it("creates array and accesses first element", async () => {
      const result = await runFunc<number>(`
        pub fn test() -> i32 {
          let arr = [10, 20, 30]
          arr[0]
        }
      `, "test");
      assertEquals(result, 10);
    });

    it("creates array and accesses middle element", async () => {
      const result = await runFunc<number>(`
        pub fn test() -> i32 {
          let arr = [5, 15, 25]
          arr[1]
        }
      `, "test");
      assertEquals(result, 15);
    });

    it("creates array and accesses last element", async () => {
      const result = await runFunc<number>(`
        pub fn test() -> i32 {
          let arr = [100, 200, 300]
          arr[2]
        }
      `, "test");
      assertEquals(result, 300);
    });

    it("sums array elements", async () => {
      const result = await runFunc<number>(`
        pub fn test() -> i32 {
          let arr = [1, 2, 3, 4, 5]
          arr[0] + arr[1] + arr[2] + arr[3] + arr[4]
        }
      `, "test");
      assertEquals(result, 15);
    });

    it("uses computed index", async () => {
      const result = await runFunc<number>(`
        pub fn test() -> i32 {
          let arr = [10, 20, 30]
          let i = 1
          arr[i]
        }
      `, "test");
      assertEquals(result, 20);
    });

    it("passes array to function", async () => {
      const result = await runFunc<number>(`
        fn sum_first_two(arr: [i32; 3]) -> i32 {
          arr[0] + arr[1]
        }

        pub fn test() -> i32 {
          let arr = [7, 8, 9]
          sum_first_two(arr)
        }
      `, "test");
      assertEquals(result, 15);
    });

    it("returns array from function", async () => {
      const result = await runFunc<number>(`
        fn make_array() -> [i32; 3] {
          [5, 10, 15]
        }

        pub fn test() -> i32 {
          let arr = make_array()
          arr[0] + arr[1] + arr[2]
        }
      `, "test");
      assertEquals(result, 30);
    });
  });
});
