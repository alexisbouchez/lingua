/**
 * Lingua Lexer Tests
 *
 * TDD: All lexer functionality should be covered here.
 */

import { assertEquals, assertExists } from "@std/assert";
import { lex, Lexer } from "./lexer.ts";
import { TokenType } from "./token.ts";

// Helper to get token types from source
function tokenTypes(source: string): TokenType[] {
  const { tokens } = lex(source);
  return tokens.map((t) => t.type);
}

// Helper to get token values from source
function tokenValues(source: string): (string | number | boolean | undefined)[] {
  const { tokens } = lex(source);
  return tokens.map((t) => t.value);
}

// =============================================================================
// BASIC TOKENS
// =============================================================================

Deno.test("lexer - empty source", () => {
  const { tokens, errors } = lex("");
  assertEquals(errors.length, 0);
  assertEquals(tokens.length, 1);
  assertEquals(tokens[0].type, TokenType.Eof);
});

Deno.test("lexer - whitespace only", () => {
  const { tokens, errors } = lex("   \t\n  ");
  assertEquals(errors.length, 0);
  assertEquals(tokens.length, 1);
  assertEquals(tokens[0].type, TokenType.Eof);
});

// =============================================================================
// DELIMITERS
// =============================================================================

Deno.test("lexer - parentheses", () => {
  const types = tokenTypes("()");
  assertEquals(types, [TokenType.LParen, TokenType.RParen, TokenType.Eof]);
});

Deno.test("lexer - braces", () => {
  const types = tokenTypes("{}");
  assertEquals(types, [TokenType.LBrace, TokenType.RBrace, TokenType.Eof]);
});

Deno.test("lexer - brackets", () => {
  const types = tokenTypes("[]");
  assertEquals(types, [TokenType.LBracket, TokenType.RBracket, TokenType.Eof]);
});

Deno.test("lexer - punctuation", () => {
  const types = tokenTypes(", ; : :: . .. ..= -> => @ # ?");
  assertEquals(types, [
    TokenType.Comma,
    TokenType.Semicolon,
    TokenType.Colon,
    TokenType.DoubleColon,
    TokenType.Dot,
    TokenType.DotDot,
    TokenType.DotDotEq,
    TokenType.Arrow,
    TokenType.FatArrow,
    TokenType.At,
    TokenType.Hash,
    TokenType.Question,
    TokenType.Eof,
  ]);
});

// =============================================================================
// OPERATORS
// =============================================================================

Deno.test("lexer - arithmetic operators", () => {
  const types = tokenTypes("+ - * / %");
  assertEquals(types, [
    TokenType.Plus,
    TokenType.Minus,
    TokenType.Star,
    TokenType.Slash,
    TokenType.Percent,
    TokenType.Eof,
  ]);
});

Deno.test("lexer - comparison operators", () => {
  const types = tokenTypes("== != < > <= >=");
  assertEquals(types, [
    TokenType.Eq,
    TokenType.Ne,
    TokenType.Lt,
    TokenType.Gt,
    TokenType.Le,
    TokenType.Ge,
    TokenType.Eof,
  ]);
});

Deno.test("lexer - logical operators", () => {
  const types = tokenTypes("&& || !");
  assertEquals(types, [
    TokenType.And,
    TokenType.Or,
    TokenType.Not,
    TokenType.Eof,
  ]);
});

Deno.test("lexer - bitwise operators", () => {
  const types = tokenTypes("& | ^ << >>");
  assertEquals(types, [
    TokenType.BitAnd,
    TokenType.BitOr,
    TokenType.BitXor,
    TokenType.Shl,
    TokenType.Shr,
    TokenType.Eof,
  ]);
});

Deno.test("lexer - assignment operators", () => {
  const types = tokenTypes("= += -= *= /=");
  assertEquals(types, [
    TokenType.Assign,
    TokenType.PlusAssign,
    TokenType.MinusAssign,
    TokenType.StarAssign,
    TokenType.SlashAssign,
    TokenType.Eof,
  ]);
});

// =============================================================================
// LITERALS - INTEGERS
// =============================================================================

Deno.test("lexer - integer literal", () => {
  const { tokens } = lex("42");
  assertEquals(tokens[0].type, TokenType.Int);
  assertEquals(tokens[0].value, 42);
});

Deno.test("lexer - integer with underscores", () => {
  const { tokens } = lex("1_000_000");
  assertEquals(tokens[0].type, TokenType.Int);
  assertEquals(tokens[0].value, 1000000);
});

Deno.test("lexer - hex integer", () => {
  const { tokens } = lex("0xFF");
  assertEquals(tokens[0].type, TokenType.Int);
  assertEquals(tokens[0].value, 255);
});

Deno.test("lexer - hex integer with underscores", () => {
  const { tokens } = lex("0xFF_FF");
  assertEquals(tokens[0].type, TokenType.Int);
  assertEquals(tokens[0].value, 65535);
});

Deno.test("lexer - octal integer", () => {
  const { tokens } = lex("0o77");
  assertEquals(tokens[0].type, TokenType.Int);
  assertEquals(tokens[0].value, 63);
});

Deno.test("lexer - binary integer", () => {
  const { tokens } = lex("0b1010");
  assertEquals(tokens[0].type, TokenType.Int);
  assertEquals(tokens[0].value, 10);
});

Deno.test("lexer - binary with underscores", () => {
  const { tokens } = lex("0b1111_0000");
  assertEquals(tokens[0].type, TokenType.Int);
  assertEquals(tokens[0].value, 240);
});

// =============================================================================
// LITERALS - FLOATS
// =============================================================================

Deno.test("lexer - float literal", () => {
  const { tokens } = lex("3.14");
  assertEquals(tokens[0].type, TokenType.Float);
  assertEquals(tokens[0].value, 3.14);
});

Deno.test("lexer - float with underscores", () => {
  const { tokens } = lex("1_000.123_456");
  assertEquals(tokens[0].type, TokenType.Float);
  assertEquals(tokens[0].value, 1000.123456);
});

Deno.test("lexer - float with exponent", () => {
  const { tokens } = lex("1e10");
  assertEquals(tokens[0].type, TokenType.Float);
  assertEquals(tokens[0].value, 1e10);
});

Deno.test("lexer - float with negative exponent", () => {
  const { tokens } = lex("1.5e-3");
  assertEquals(tokens[0].type, TokenType.Float);
  assertEquals(tokens[0].value, 0.0015);
});

// =============================================================================
// LITERALS - STRINGS
// =============================================================================

Deno.test("lexer - simple string", () => {
  const { tokens } = lex('"hello"');
  assertEquals(tokens[0].type, TokenType.String);
  assertEquals(tokens[0].value, "hello");
});

Deno.test("lexer - empty string", () => {
  const { tokens } = lex('""');
  assertEquals(tokens[0].type, TokenType.String);
  assertEquals(tokens[0].value, "");
});

Deno.test("lexer - string with escapes", () => {
  const { tokens } = lex('"hello\\nworld"');
  assertEquals(tokens[0].type, TokenType.String);
  assertEquals(tokens[0].value, "hello\nworld");
});

Deno.test("lexer - string with all escapes", () => {
  const { tokens } = lex('"\\n\\r\\t\\\\\\"\\0"');
  assertEquals(tokens[0].type, TokenType.String);
  assertEquals(tokens[0].value, "\n\r\t\\\"\0");
});

Deno.test("lexer - triple-quoted string", () => {
  const { tokens } = lex('"""hello\nworld"""');
  assertEquals(tokens[0].type, TokenType.String);
  assertEquals(tokens[0].value, "hello\nworld");
});

// =============================================================================
// LITERALS - CHARACTERS
// =============================================================================

Deno.test("lexer - character literal", () => {
  const { tokens } = lex("'a'");
  assertEquals(tokens[0].type, TokenType.Char);
  assertEquals(tokens[0].value, "a");
});

Deno.test("lexer - character with escape", () => {
  const { tokens } = lex("'\\n'");
  assertEquals(tokens[0].type, TokenType.Char);
  assertEquals(tokens[0].value, "\n");
});

// =============================================================================
// LITERALS - BOOLEANS
// =============================================================================

Deno.test("lexer - true literal", () => {
  const { tokens } = lex("true");
  assertEquals(tokens[0].type, TokenType.True);
  assertEquals(tokens[0].value, true);
});

Deno.test("lexer - false literal", () => {
  const { tokens } = lex("false");
  assertEquals(tokens[0].type, TokenType.False);
  assertEquals(tokens[0].value, false);
});

// =============================================================================
// IDENTIFIERS
// =============================================================================

Deno.test("lexer - simple identifier", () => {
  const { tokens } = lex("foo");
  assertEquals(tokens[0].type, TokenType.Ident);
  assertEquals(tokens[0].lexeme, "foo");
});

Deno.test("lexer - identifier with underscore", () => {
  const { tokens } = lex("foo_bar");
  assertEquals(tokens[0].type, TokenType.Ident);
  assertEquals(tokens[0].lexeme, "foo_bar");
});

Deno.test("lexer - identifier starting with underscore", () => {
  const { tokens } = lex("_private");
  assertEquals(tokens[0].type, TokenType.Ident);
  assertEquals(tokens[0].lexeme, "_private");
});

Deno.test("lexer - identifier with numbers", () => {
  const { tokens } = lex("var1");
  assertEquals(tokens[0].type, TokenType.Ident);
  assertEquals(tokens[0].lexeme, "var1");
});

Deno.test("lexer - underscore alone", () => {
  const { tokens } = lex("_");
  assertEquals(tokens[0].type, TokenType.Underscore);
});

// =============================================================================
// KEYWORDS
// =============================================================================

Deno.test("lexer - all keywords", () => {
  const source = `
    fn let mut if else match for while loop break continue return
    struct enum trait impl use pub mod async await self Self where as in type
  `;
  const types = tokenTypes(source);

  const expectedKeywords = [
    TokenType.Fn,
    TokenType.Let,
    TokenType.Mut,
    TokenType.If,
    TokenType.Else,
    TokenType.Match,
    TokenType.For,
    TokenType.While,
    TokenType.Loop,
    TokenType.Break,
    TokenType.Continue,
    TokenType.Return,
    TokenType.Struct,
    TokenType.Enum,
    TokenType.Trait,
    TokenType.Impl,
    TokenType.Use,
    TokenType.Pub,
    TokenType.Mod,
    TokenType.Async,
    TokenType.Await,
    TokenType.Self,
    TokenType.SelfType,
    TokenType.Where,
    TokenType.As,
    TokenType.In,
    TokenType.Type,
    TokenType.Eof,
  ];

  assertEquals(types, expectedKeywords);
});

// =============================================================================
// COMMENTS
// =============================================================================

Deno.test("lexer - line comment", () => {
  const { tokens } = lex("// this is a comment\n42");
  assertEquals(tokens[0].type, TokenType.Int);
  assertEquals(tokens[0].value, 42);
});

Deno.test("lexer - line comment at end", () => {
  const { tokens } = lex("42 // comment");
  assertEquals(tokens[0].type, TokenType.Int);
  assertEquals(tokens[0].value, 42);
  assertEquals(tokens[1].type, TokenType.Eof);
});

Deno.test("lexer - block comment", () => {
  const { tokens } = lex("/* comment */ 42");
  assertEquals(tokens[0].type, TokenType.Int);
});

Deno.test("lexer - nested block comment", () => {
  const { tokens } = lex("/* outer /* inner */ outer */ 42");
  assertEquals(tokens[0].type, TokenType.Int);
});

// =============================================================================
// SOURCE LOCATIONS
// =============================================================================

Deno.test("lexer - source location tracking", () => {
  const { tokens } = lex("let x = 42");
  assertEquals(tokens[0].span.start.line, 1);
  assertEquals(tokens[0].span.start.column, 1);
});

Deno.test("lexer - multiline source location", () => {
  const { tokens } = lex("let\nx");
  assertEquals(tokens[0].span.start.line, 1);
  assertEquals(tokens[1].span.start.line, 2);
  assertEquals(tokens[1].span.start.column, 1);
});

// =============================================================================
// COMPLEX EXPRESSIONS
// =============================================================================

Deno.test("lexer - function definition", () => {
  const source = "fn add(a: i32, b: i32) -> i32 { a + b }";
  const types = tokenTypes(source);
  assertEquals(types, [
    TokenType.Fn,
    TokenType.Ident, // add
    TokenType.LParen,
    TokenType.Ident, // a
    TokenType.Colon,
    TokenType.Ident, // i32
    TokenType.Comma,
    TokenType.Ident, // b
    TokenType.Colon,
    TokenType.Ident, // i32
    TokenType.RParen,
    TokenType.Arrow,
    TokenType.Ident, // i32
    TokenType.LBrace,
    TokenType.Ident, // a
    TokenType.Plus,
    TokenType.Ident, // b
    TokenType.RBrace,
    TokenType.Eof,
  ]);
});

Deno.test("lexer - struct definition", () => {
  const source = "struct Point { x: f64, y: f64 }";
  const types = tokenTypes(source);
  assertEquals(types, [
    TokenType.Struct,
    TokenType.Ident, // Point
    TokenType.LBrace,
    TokenType.Ident, // x
    TokenType.Colon,
    TokenType.Ident, // f64
    TokenType.Comma,
    TokenType.Ident, // y
    TokenType.Colon,
    TokenType.Ident, // f64
    TokenType.RBrace,
    TokenType.Eof,
  ]);
});

Deno.test("lexer - match expression", () => {
  const source = "match x { 0 => zero, _ => other }";
  const types = tokenTypes(source);
  assertEquals(types, [
    TokenType.Match,
    TokenType.Ident, // x
    TokenType.LBrace,
    TokenType.Int, // 0
    TokenType.FatArrow,
    TokenType.Ident, // zero
    TokenType.Comma,
    TokenType.Underscore,
    TokenType.FatArrow,
    TokenType.Ident, // other
    TokenType.RBrace,
    TokenType.Eof,
  ]);
});

Deno.test("lexer - generic type", () => {
  const source = "Option<T>";
  const types = tokenTypes(source);
  assertEquals(types, [
    TokenType.Ident, // Option
    TokenType.Lt,
    TokenType.Ident, // T
    TokenType.Gt,
    TokenType.Eof,
  ]);
});

Deno.test("lexer - async await", () => {
  const source = "async fn fetch() { data.await }";
  const types = tokenTypes(source);
  assertEquals(types, [
    TokenType.Async,
    TokenType.Fn,
    TokenType.Ident, // fetch
    TokenType.LParen,
    TokenType.RParen,
    TokenType.LBrace,
    TokenType.Ident, // data
    TokenType.Dot,
    TokenType.Await,
    TokenType.RBrace,
    TokenType.Eof,
  ]);
});

Deno.test("lexer - range expressions", () => {
  const types = tokenTypes("0..10 0..=10");
  assertEquals(types, [
    TokenType.Int,
    TokenType.DotDot,
    TokenType.Int,
    TokenType.Int,
    TokenType.DotDotEq,
    TokenType.Int,
    TokenType.Eof,
  ]);
});

// =============================================================================
// ERROR HANDLING
// =============================================================================

Deno.test("lexer - unterminated string error", () => {
  const { errors } = lex('"hello');
  assertEquals(errors.length, 1);
  assertEquals(errors[0].message, "Unterminated string literal");
});

Deno.test("lexer - unterminated char error", () => {
  const { errors } = lex("'a");
  assertEquals(errors.length, 1);
});

Deno.test("lexer - invalid escape in string", () => {
  const { errors } = lex('"\\q"');
  // First error for invalid escape, second for leftover quote parsed as unterminated
  assertEquals(errors.length >= 1, true);
  assertEquals(errors[0].message, "Invalid escape sequence: \\q");
});

Deno.test("lexer - unexpected character", () => {
  const { errors } = lex("$");
  assertEquals(errors.length, 1);
});

// =============================================================================
// INTEGRATION TESTS
// =============================================================================

Deno.test("lexer - full program", () => {
  const source = `
// A simple program
fn main() {
    let x = 42
    let y = 3.14
    let name = "Lingua"

    if x > 0 {
        print(name)
    } else {
        print("negative")
    }
}
`;
  const { tokens, errors } = lex(source);
  assertEquals(errors.length, 0);
  // Just verify it tokenizes without errors
  assertExists(tokens.find((t) => t.type === TokenType.Fn));
  assertExists(tokens.find((t) => t.type === TokenType.Let));
  assertExists(tokens.find((t) => t.type === TokenType.If));
  assertExists(tokens.find((t) => t.type === TokenType.Else));
});
