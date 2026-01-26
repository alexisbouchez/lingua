/**
 * Lingua Token Definitions
 *
 * Tokens are the atomic units produced by the lexer.
 * Each token carries its type, lexeme, and source location.
 */

// Source location for error reporting
export interface SourceLocation {
  line: number;
  column: number;
  offset: number;
}

export interface Span {
  start: SourceLocation;
  end: SourceLocation;
  file?: string;
}

// Token types enumeration
export enum TokenType {
  // Literals
  Int = "Int",
  Float = "Float",
  String = "String",
  Char = "Char",
  True = "True",
  False = "False",

  // Identifier
  Ident = "Ident",

  // Keywords
  Fn = "Fn",
  Let = "Let",
  Mut = "Mut",
  If = "If",
  Else = "Else",
  Match = "Match",
  For = "For",
  While = "While",
  Loop = "Loop",
  Break = "Break",
  Continue = "Continue",
  Return = "Return",
  Struct = "Struct",
  Enum = "Enum",
  Trait = "Trait",
  Impl = "Impl",
  Use = "Use",
  Pub = "Pub",
  Mod = "Mod",
  Async = "Async",
  Await = "Await",
  Self = "Self",
  SelfType = "SelfType", // Self (the type)
  Where = "Where",
  As = "As",
  In = "In",
  Type = "Type",

  // Operators - Arithmetic
  Plus = "Plus", // +
  Minus = "Minus", // -
  Star = "Star", // *
  Slash = "Slash", // /
  Percent = "Percent", // %

  // Operators - Comparison
  Eq = "Eq", // ==
  Ne = "Ne", // !=
  Lt = "Lt", // <
  Gt = "Gt", // >
  Le = "Le", // <=
  Ge = "Ge", // >=

  // Operators - Logical
  And = "And", // &&
  Or = "Or", // ||
  Not = "Not", // !

  // Operators - Bitwise
  BitAnd = "BitAnd", // &
  BitOr = "BitOr", // |
  BitXor = "BitXor", // ^
  Shl = "Shl", // <<
  Shr = "Shr", // >>

  // Operators - Assignment
  Assign = "Assign", // =
  PlusAssign = "PlusAssign", // +=
  MinusAssign = "MinusAssign", // -=
  StarAssign = "StarAssign", // *=
  SlashAssign = "SlashAssign", // /=

  // Delimiters
  LParen = "LParen", // (
  RParen = "RParen", // )
  LBrace = "LBrace", // {
  RBrace = "RBrace", // }
  LBracket = "LBracket", // [
  RBracket = "RBracket", // ]

  // Punctuation
  Comma = "Comma", // ,
  Dot = "Dot", // .
  Colon = "Colon", // :
  Semicolon = "Semicolon", // ;
  Arrow = "Arrow", // ->
  FatArrow = "FatArrow", // =>
  DoubleColon = "DoubleColon", // ::
  Question = "Question", // ?
  At = "At", // @
  Hash = "Hash", // #
  DotDot = "DotDot", // ..
  DotDotEq = "DotDotEq", // ..=
  Underscore = "Underscore", // _

  // Special
  Newline = "Newline",
  Eof = "Eof",
  Error = "Error",
}

// The token structure
export interface Token {
  type: TokenType;
  lexeme: string;
  span: Span;
  // For literals, store the parsed value
  value?: number | string | boolean;
}

// Keywords lookup table
export const KEYWORDS: Record<string, TokenType> = {
  fn: TokenType.Fn,
  let: TokenType.Let,
  mut: TokenType.Mut,
  if: TokenType.If,
  else: TokenType.Else,
  match: TokenType.Match,
  for: TokenType.For,
  while: TokenType.While,
  loop: TokenType.Loop,
  break: TokenType.Break,
  continue: TokenType.Continue,
  return: TokenType.Return,
  struct: TokenType.Struct,
  enum: TokenType.Enum,
  trait: TokenType.Trait,
  impl: TokenType.Impl,
  use: TokenType.Use,
  pub: TokenType.Pub,
  mod: TokenType.Mod,
  async: TokenType.Async,
  await: TokenType.Await,
  self: TokenType.Self,
  Self: TokenType.SelfType,
  where: TokenType.Where,
  as: TokenType.As,
  in: TokenType.In,
  type: TokenType.Type,
  true: TokenType.True,
  false: TokenType.False,
};

// Helper to create a token
export function token(
  type: TokenType,
  lexeme: string,
  span: Span,
  value?: number | string | boolean,
): Token {
  return { type, lexeme, span, value };
}

// Helper to create a simple span for testing
export function span(
  startLine: number,
  startCol: number,
  endLine: number,
  endCol: number,
): Span {
  return {
    start: { line: startLine, column: startCol, offset: 0 },
    end: { line: endLine, column: endCol, offset: 0 },
  };
}

// Format a token for debugging
export function formatToken(tok: Token): string {
  if (tok.value !== undefined) {
    return `${tok.type}(${tok.lexeme}) = ${tok.value}`;
  }
  return `${tok.type}(${tok.lexeme})`;
}
