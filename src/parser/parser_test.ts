/**
 * Lingua Parser Tests
 */

import { assertEquals, assertExists } from "@std/assert";
import { lex } from "../lexer/mod.ts";
import { parse } from "./parser.ts";
import type { Expr, FnItem, StructItem, EnumItem, LetStmt, Module, Item } from "./ast.ts";

// Helper to parse source and return module
function parseSource(source: string): { module: Module; errors: { message: string }[] } {
  const { tokens, errors: lexErrors } = lex(source);
  if (lexErrors.length > 0) {
    throw new Error(`Lexer errors: ${lexErrors.map((e) => e.message).join(", ")}`);
  }
  return parse(tokens);
}

// Helper to get first item as specific type
function getFirstItem<T extends Item>(source: string): T {
  const { module, errors } = parseSource(source);
  assertEquals(errors.length, 0, `Parse errors: ${errors.map((e) => e.message).join(", ")}`);
  assertExists(module.items[0]);
  return module.items[0] as T;
}

// Helper to parse expression from a function body
function parseExpr(exprSource: string): Expr {
  const source = `fn test() { ${exprSource} }`;
  const fn = getFirstItem<FnItem>(source);
  assertExists(fn.body);
  assertExists(fn.body.expr);
  return fn.body.expr;
}

// =============================================================================
// FUNCTION DEFINITIONS
// =============================================================================

Deno.test("parser - simple function", () => {
  const fn = getFirstItem<FnItem>("fn add(a: i32, b: i32) -> i32 { a + b }");
  assertEquals(fn.kind, "FnItem");
  assertEquals(fn.name, "add");
  assertEquals(fn.params.length, 2);
  assertEquals(fn.params[0].pattern.kind, "IdentPattern");
  assertExists(fn.ret);
  assertEquals(fn.ret.kind, "NamedType");
});

Deno.test("parser - function without return type", () => {
  const fn = getFirstItem<FnItem>("fn greet() { }");
  assertEquals(fn.kind, "FnItem");
  assertEquals(fn.name, "greet");
  assertEquals(fn.ret, undefined);
});

Deno.test("parser - async function", () => {
  const fn = getFirstItem<FnItem>("async fn fetch() { }");
  assertEquals(fn.async, true);
});

Deno.test("parser - public function", () => {
  const fn = getFirstItem<FnItem>("pub fn api() { }");
  assertEquals(fn.vis, "public");
});

Deno.test("parser - generic function", () => {
  const fn = getFirstItem<FnItem>("fn identity<T>(x: T) -> T { x }");
  assertExists(fn.generics);
  assertEquals(fn.generics.params.length, 1);
  assertEquals(fn.generics.params[0].name, "T");
});

Deno.test("parser - generic with bounds", () => {
  const fn = getFirstItem<FnItem>("fn print<T: Display>(x: T) { }");
  assertExists(fn.generics);
  assertExists(fn.generics.params[0].bounds);
  assertEquals(fn.generics.params[0].bounds.length, 1);
});

// =============================================================================
// STRUCT DEFINITIONS
// =============================================================================

Deno.test("parser - simple struct", () => {
  const s = getFirstItem<StructItem>("struct Point { x: f64, y: f64 }");
  assertEquals(s.kind, "StructItem");
  assertEquals(s.name, "Point");
  assertEquals(s.fields.length, 2);
  assertEquals(s.fields[0].name, "x");
  assertEquals(s.fields[1].name, "y");
});

Deno.test("parser - public struct with public fields", () => {
  const s = getFirstItem<StructItem>("pub struct User { pub name: str, age: i32 }");
  assertEquals(s.vis, "public");
  assertEquals(s.fields[0].vis, "public");
  assertEquals(s.fields[1].vis, "private");
});

Deno.test("parser - generic struct", () => {
  const s = getFirstItem<StructItem>("struct Pair<A, B> { first: A, second: B }");
  assertExists(s.generics);
  assertEquals(s.generics.params.length, 2);
});

// =============================================================================
// ENUM DEFINITIONS
// =============================================================================

Deno.test("parser - simple enum", () => {
  const e = getFirstItem<EnumItem>("enum Color { Red, Green, Blue }");
  assertEquals(e.kind, "EnumItem");
  assertEquals(e.name, "Color");
  assertEquals(e.variants.length, 3);
  assertEquals(e.variants[0].name, "Red");
});

Deno.test("parser - enum with data", () => {
  const e = getFirstItem<EnumItem>("enum Option<T> { Some(T), None }");
  assertEquals(e.variants[0].name, "Some");
  assertExists(e.variants[0].fields);
  assertEquals(e.variants[0].fields.length, 1);
  assertEquals(e.variants[1].name, "None");
  assertEquals(e.variants[1].fields, undefined);
});

// =============================================================================
// EXPRESSIONS - LITERALS
// =============================================================================

Deno.test("parser - integer literal", () => {
  const expr = parseExpr("42");
  assertEquals(expr.kind, "IntLiteral");
  if (expr.kind === "IntLiteral") {
    assertEquals(expr.value, 42n);
  }
});

Deno.test("parser - float literal", () => {
  const expr = parseExpr("3.14");
  assertEquals(expr.kind, "FloatLiteral");
  if (expr.kind === "FloatLiteral") {
    assertEquals(expr.value, 3.14);
  }
});

Deno.test("parser - string literal", () => {
  const expr = parseExpr('"hello"');
  assertEquals(expr.kind, "StringLiteral");
  if (expr.kind === "StringLiteral") {
    assertEquals(expr.value, "hello");
  }
});

Deno.test("parser - boolean literals", () => {
  const t = parseExpr("true");
  assertEquals(t.kind, "BoolLiteral");
  if (t.kind === "BoolLiteral") {
    assertEquals(t.value, true);
  }

  const f = parseExpr("false");
  assertEquals(f.kind, "BoolLiteral");
  if (f.kind === "BoolLiteral") {
    assertEquals(f.value, false);
  }
});

// =============================================================================
// EXPRESSIONS - BINARY OPERATORS
// =============================================================================

Deno.test("parser - addition", () => {
  const expr = parseExpr("1 + 2");
  assertEquals(expr.kind, "BinaryExpr");
  if (expr.kind === "BinaryExpr") {
    assertEquals(expr.op, "+");
  }
});

Deno.test("parser - operator precedence", () => {
  const expr = parseExpr("1 + 2 * 3");
  assertEquals(expr.kind, "BinaryExpr");
  if (expr.kind === "BinaryExpr") {
    assertEquals(expr.op, "+");
    assertEquals(expr.right.kind, "BinaryExpr");
    if (expr.right.kind === "BinaryExpr") {
      assertEquals(expr.right.op, "*");
    }
  }
});

Deno.test("parser - comparison operators", () => {
  const expr = parseExpr("a < b && c > d");
  assertEquals(expr.kind, "BinaryExpr");
  if (expr.kind === "BinaryExpr") {
    assertEquals(expr.op, "&&");
  }
});

Deno.test("parser - logical operators", () => {
  const expr = parseExpr("a || b && c");
  assertEquals(expr.kind, "BinaryExpr");
  if (expr.kind === "BinaryExpr") {
    assertEquals(expr.op, "||");
    // && binds tighter than ||
    assertEquals(expr.right.kind, "BinaryExpr");
  }
});

// =============================================================================
// EXPRESSIONS - UNARY OPERATORS
// =============================================================================

Deno.test("parser - negation", () => {
  const expr = parseExpr("-x");
  assertEquals(expr.kind, "UnaryExpr");
  if (expr.kind === "UnaryExpr") {
    assertEquals(expr.op, "-");
  }
});

Deno.test("parser - logical not", () => {
  const expr = parseExpr("!flag");
  assertEquals(expr.kind, "UnaryExpr");
  if (expr.kind === "UnaryExpr") {
    assertEquals(expr.op, "!");
  }
});

// =============================================================================
// EXPRESSIONS - FUNCTION CALLS
// =============================================================================

Deno.test("parser - function call no args", () => {
  const expr = parseExpr("foo()");
  assertEquals(expr.kind, "CallExpr");
  if (expr.kind === "CallExpr") {
    assertEquals(expr.args.length, 0);
  }
});

Deno.test("parser - function call with args", () => {
  const expr = parseExpr("add(1, 2)");
  assertEquals(expr.kind, "CallExpr");
  if (expr.kind === "CallExpr") {
    assertEquals(expr.args.length, 2);
  }
});

Deno.test("parser - method call", () => {
  const expr = parseExpr("list.push(1)");
  assertEquals(expr.kind, "MethodCallExpr");
  if (expr.kind === "MethodCallExpr") {
    assertEquals(expr.method, "push");
    assertEquals(expr.args.length, 1);
  }
});

Deno.test("parser - chained method calls", () => {
  const expr = parseExpr("list.filter(f).map(g)");
  assertEquals(expr.kind, "MethodCallExpr");
  if (expr.kind === "MethodCallExpr") {
    assertEquals(expr.method, "map");
    assertEquals(expr.receiver.kind, "MethodCallExpr");
  }
});

// =============================================================================
// EXPRESSIONS - FIELD AND INDEX ACCESS
// =============================================================================

Deno.test("parser - field access", () => {
  const expr = parseExpr("point.x");
  assertEquals(expr.kind, "FieldExpr");
  if (expr.kind === "FieldExpr") {
    assertEquals(expr.field, "x");
  }
});

Deno.test("parser - index access", () => {
  const expr = parseExpr("arr[0]");
  assertEquals(expr.kind, "IndexExpr");
});

// =============================================================================
// EXPRESSIONS - TUPLES AND ARRAYS
// =============================================================================

Deno.test("parser - tuple", () => {
  const expr = parseExpr("(1, 2, 3)");
  assertEquals(expr.kind, "TupleExpr");
  if (expr.kind === "TupleExpr") {
    assertEquals(expr.elements.length, 3);
  }
});

Deno.test("parser - array", () => {
  const expr = parseExpr("[1, 2, 3]");
  assertEquals(expr.kind, "ArrayExpr");
  if (expr.kind === "ArrayExpr") {
    assertEquals(expr.elements.length, 3);
  }
});

// =============================================================================
// EXPRESSIONS - STRUCT LITERALS
// =============================================================================

Deno.test("parser - struct literal", () => {
  const expr = parseExpr("Point { x: 1, y: 2 }");
  assertEquals(expr.kind, "StructExpr");
  if (expr.kind === "StructExpr") {
    assertEquals(expr.name, "Point");
    assertEquals(expr.fields.length, 2);
  }
});

Deno.test("parser - struct literal with spread", () => {
  const expr = parseExpr("Point { x: 1, ..other }");
  assertEquals(expr.kind, "StructExpr");
  if (expr.kind === "StructExpr") {
    assertExists(expr.spread);
  }
});

// =============================================================================
// EXPRESSIONS - CONTROL FLOW
// =============================================================================

Deno.test("parser - if expression", () => {
  const expr = parseExpr("if x > 0 { 1 } else { 0 }");
  assertEquals(expr.kind, "IfExpr");
  if (expr.kind === "IfExpr") {
    assertExists(expr.else_);
  }
});

Deno.test("parser - if else if chain", () => {
  const expr = parseExpr("if a { 1 } else if b { 2 } else { 3 }");
  assertEquals(expr.kind, "IfExpr");
  if (expr.kind === "IfExpr") {
    assertEquals(expr.else_?.kind, "IfExpr");
  }
});

Deno.test("parser - match expression", () => {
  const expr = parseExpr("match x { 0 => zero, _ => other }");
  assertEquals(expr.kind, "MatchExpr");
  if (expr.kind === "MatchExpr") {
    assertEquals(expr.arms.length, 2);
  }
});

Deno.test("parser - match with guard", () => {
  const expr = parseExpr("match x { n if n > 0 => positive, _ => other }");
  assertEquals(expr.kind, "MatchExpr");
  if (expr.kind === "MatchExpr") {
    assertExists(expr.arms[0].guard);
  }
});

// =============================================================================
// EXPRESSIONS - LOOPS
// =============================================================================

Deno.test("parser - loop expression", () => {
  const expr = parseExpr("loop { break }");
  assertEquals(expr.kind, "LoopExpr");
});

Deno.test("parser - while expression", () => {
  const expr = parseExpr("while x > 0 { x = x - 1 }");
  assertEquals(expr.kind, "WhileExpr");
});

Deno.test("parser - for expression", () => {
  const expr = parseExpr("for x in items { }");
  assertEquals(expr.kind, "ForExpr");
  if (expr.kind === "ForExpr") {
    assertEquals(expr.pattern.kind, "IdentPattern");
  }
});

// =============================================================================
// EXPRESSIONS - CLOSURES
// =============================================================================

Deno.test("parser - closure no params", () => {
  const expr = parseExpr("|| 42");
  assertEquals(expr.kind, "ClosureExpr");
  if (expr.kind === "ClosureExpr") {
    assertEquals(expr.params.length, 0);
  }
});

Deno.test("parser - closure with params", () => {
  const expr = parseExpr("|x, y| x + y");
  assertEquals(expr.kind, "ClosureExpr");
  if (expr.kind === "ClosureExpr") {
    assertEquals(expr.params.length, 2);
  }
});

Deno.test("parser - closure with type annotation", () => {
  const expr = parseExpr("|x: i32| -> i32 { x * 2 }");
  assertEquals(expr.kind, "ClosureExpr");
  if (expr.kind === "ClosureExpr") {
    assertExists(expr.params[0].type);
    assertExists(expr.ret);
  }
});

// =============================================================================
// EXPRESSIONS - ERROR HANDLING
// =============================================================================

Deno.test("parser - try operator", () => {
  const expr = parseExpr("foo()?");
  assertEquals(expr.kind, "TryExpr");
});

// =============================================================================
// STATEMENTS
// =============================================================================

Deno.test("parser - let statement", () => {
  const source = "fn test() { let x = 42; }";
  const fn = getFirstItem<FnItem>(source);
  assertExists(fn.body);
  assertEquals(fn.body.stmts.length, 1);
  const stmt = fn.body.stmts[0] as LetStmt;
  assertEquals(stmt.kind, "LetStmt");
  assertEquals(stmt.mutable, false);
});

Deno.test("parser - let mut statement", () => {
  const source = "fn test() { let mut x = 0; }";
  const fn = getFirstItem<FnItem>(source);
  const stmt = fn.body?.stmts[0] as LetStmt;
  assertEquals(stmt.mutable, true);
});

Deno.test("parser - let with type annotation", () => {
  const source = "fn test() { let x: i32 = 0; }";
  const fn = getFirstItem<FnItem>(source);
  const stmt = fn.body?.stmts[0] as LetStmt;
  assertExists(stmt.type);
});

// =============================================================================
// TYPES
// =============================================================================

Deno.test("parser - named type", () => {
  const fn = getFirstItem<FnItem>("fn f(x: i32) { }");
  assertEquals(fn.params[0].type.kind, "NamedType");
});

Deno.test("parser - generic type", () => {
  const fn = getFirstItem<FnItem>("fn f(x: Vec<i32>) { }");
  assertEquals(fn.params[0].type.kind, "GenericType");
  if (fn.params[0].type.kind === "GenericType") {
    assertEquals(fn.params[0].type.name, "Vec");
    assertEquals(fn.params[0].type.args.length, 1);
  }
});

Deno.test("parser - tuple type", () => {
  const fn = getFirstItem<FnItem>("fn f(x: (i32, str)) { }");
  assertEquals(fn.params[0].type.kind, "TupleType");
});

Deno.test("parser - array type", () => {
  const fn = getFirstItem<FnItem>("fn f(x: [i32]) { }");
  assertEquals(fn.params[0].type.kind, "ArrayType");
});

// =============================================================================
// PATTERNS
// =============================================================================

Deno.test("parser - wildcard pattern", () => {
  const expr = parseExpr("match x { _ => 0 }");
  if (expr.kind === "MatchExpr") {
    assertEquals(expr.arms[0].pattern.kind, "WildcardPattern");
  }
});

Deno.test("parser - ident pattern", () => {
  const source = "fn test() { let x = 0; }";
  const fn = getFirstItem<FnItem>(source);
  const stmt = fn.body?.stmts[0] as LetStmt;
  assertEquals(stmt.pattern.kind, "IdentPattern");
  if (stmt.pattern.kind === "IdentPattern") {
    assertEquals(stmt.pattern.name, "x");
  }
});

Deno.test("parser - tuple pattern", () => {
  const source = "fn test() { let (a, b) = pair; }";
  const fn = getFirstItem<FnItem>(source);
  const stmt = fn.body?.stmts[0] as LetStmt;
  assertEquals(stmt.pattern.kind, "TuplePattern");
});

// =============================================================================
// COMPLETE PROGRAMS
// =============================================================================

Deno.test("parser - complete function", () => {
  const source = `
fn factorial(n: i32) -> i32 {
    if n <= 1 {
        1
    } else {
        n * factorial(n - 1)
    }
}
`;
  const fn = getFirstItem<FnItem>(source);
  assertEquals(fn.name, "factorial");
  assertExists(fn.body);
});

Deno.test("parser - multiple items", () => {
  const source = `
struct Point { x: f64, y: f64 }

fn distance(a: Point, b: Point) -> f64 {
    let dx = a.x - b.x;
    let dy = a.y - b.y;
    (dx * dx + dy * dy)
}
`;
  const { module, errors } = parseSource(source);
  assertEquals(errors.length, 0, `Parse errors: ${errors.map((e) => e.message).join(", ")}`);
  assertEquals(module.items.length, 2);
  assertEquals(module.items[0].kind, "StructItem");
  assertEquals(module.items[1].kind, "FnItem");
});

// =============================================================================
// ERROR HANDLING
// =============================================================================

Deno.test("parser - recovers from errors", () => {
  const source = `
fn broken( { }
fn good() { 42 }
`;
  const { module, errors } = parseSource(source);
  // Should have at least one error
  assertEquals(errors.length >= 1, true);
  // But should still parse the second function
  const goodFn = module.items.find(
    (i) => i.kind === "FnItem" && (i as FnItem).name === "good"
  );
  assertExists(goodFn);
});
