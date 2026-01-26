/**
 * Lingua Abstract Syntax Tree
 *
 * Defines all AST node types for the Lingua language.
 */

import type { Span } from "../lexer/token.ts";

// =============================================================================
// BASE TYPES
// =============================================================================

export interface Node {
  span: Span;
}

// =============================================================================
// TYPES (Type Annotations)
// =============================================================================

export type Type =
  | NamedType
  | GenericType
  | TupleType
  | ArrayType
  | FunctionType
  | RefType
  | InferredType;

export interface NamedType extends Node {
  kind: "NamedType";
  name: string;
}

export interface GenericType extends Node {
  kind: "GenericType";
  name: string;
  args: Type[];
}

export interface TupleType extends Node {
  kind: "TupleType";
  elements: Type[];
}

export interface ArrayType extends Node {
  kind: "ArrayType";
  element: Type;
  size?: Expr; // Fixed size if present
}

export interface FunctionType extends Node {
  kind: "FunctionType";
  params: Type[];
  ret: Type;
}

export interface RefType extends Node {
  kind: "RefType";
  inner: Type;
  mutable: boolean;
}

export interface InferredType extends Node {
  kind: "InferredType";
}

// =============================================================================
// PATTERNS
// =============================================================================

export type Pattern =
  | WildcardPattern
  | IdentPattern
  | LiteralPattern
  | TuplePattern
  | StructPattern
  | EnumPattern
  | RangePattern
  | OrPattern;

export interface WildcardPattern extends Node {
  kind: "WildcardPattern";
}

export interface IdentPattern extends Node {
  kind: "IdentPattern";
  name: string;
  mutable: boolean;
}

export interface LiteralPattern extends Node {
  kind: "LiteralPattern";
  value: Expr;
}

export interface TuplePattern extends Node {
  kind: "TuplePattern";
  elements: Pattern[];
}

export interface StructPattern extends Node {
  kind: "StructPattern";
  name: string;
  fields: { name: string; pattern: Pattern }[];
  rest: boolean; // Has ..
}

export interface EnumPattern extends Node {
  kind: "EnumPattern";
  name: string;
  variant: string;
  fields?: Pattern[];
}

export interface RangePattern extends Node {
  kind: "RangePattern";
  start?: Expr;
  end?: Expr;
  inclusive: boolean;
}

export interface OrPattern extends Node {
  kind: "OrPattern";
  patterns: Pattern[];
}

// =============================================================================
// EXPRESSIONS
// =============================================================================

export type Expr =
  | IntLiteral
  | FloatLiteral
  | StringLiteral
  | CharLiteral
  | BoolLiteral
  | Identifier
  | BinaryExpr
  | UnaryExpr
  | CallExpr
  | MethodCallExpr
  | FieldExpr
  | IndexExpr
  | TupleExpr
  | ArrayExpr
  | StructExpr
  | IfExpr
  | MatchExpr
  | LoopExpr
  | WhileExpr
  | ForExpr
  | BlockExpr
  | ClosureExpr
  | ReturnExpr
  | BreakExpr
  | ContinueExpr
  | RangeExpr
  | TryExpr
  | AwaitExpr
  | AssignExpr
  | PathExpr;

// Literals
export interface IntLiteral extends Node {
  kind: "IntLiteral";
  value: bigint;
}

export interface FloatLiteral extends Node {
  kind: "FloatLiteral";
  value: number;
}

export interface StringLiteral extends Node {
  kind: "StringLiteral";
  value: string;
}

export interface CharLiteral extends Node {
  kind: "CharLiteral";
  value: string;
}

export interface BoolLiteral extends Node {
  kind: "BoolLiteral";
  value: boolean;
}

// Identifiers and Paths
export interface Identifier extends Node {
  kind: "Identifier";
  name: string;
}

export interface PathExpr extends Node {
  kind: "PathExpr";
  segments: string[];
}

// Operators
export type BinaryOp =
  | "+" | "-" | "*" | "/" | "%"
  | "==" | "!=" | "<" | ">" | "<=" | ">="
  | "&&" | "||"
  | "&" | "|" | "^" | "<<" | ">>";

export type UnaryOp = "-" | "!" | "&" | "*";

export interface BinaryExpr extends Node {
  kind: "BinaryExpr";
  op: BinaryOp;
  left: Expr;
  right: Expr;
}

export interface UnaryExpr extends Node {
  kind: "UnaryExpr";
  op: UnaryOp;
  expr: Expr;
}

// Calls
export interface CallExpr extends Node {
  kind: "CallExpr";
  callee: Expr;
  args: Expr[];
}

export interface MethodCallExpr extends Node {
  kind: "MethodCallExpr";
  receiver: Expr;
  method: string;
  args: Expr[];
}

// Access
export interface FieldExpr extends Node {
  kind: "FieldExpr";
  expr: Expr;
  field: string;
}

export interface IndexExpr extends Node {
  kind: "IndexExpr";
  expr: Expr;
  index: Expr;
}

// Compound Literals
export interface TupleExpr extends Node {
  kind: "TupleExpr";
  elements: Expr[];
}

export interface ArrayExpr extends Node {
  kind: "ArrayExpr";
  elements: Expr[];
}

export interface StructExpr extends Node {
  kind: "StructExpr";
  name: string;
  fields: { name: string; value: Expr }[];
  spread?: Expr;
}

// Control Flow
export interface IfExpr extends Node {
  kind: "IfExpr";
  condition: Expr;
  then: BlockExpr;
  else_?: Expr; // BlockExpr or another IfExpr
}

export interface MatchArm {
  pattern: Pattern;
  guard?: Expr;
  body: Expr;
}

export interface MatchExpr extends Node {
  kind: "MatchExpr";
  expr: Expr;
  arms: MatchArm[];
}

export interface LoopExpr extends Node {
  kind: "LoopExpr";
  label?: string;
  body: BlockExpr;
}

export interface WhileExpr extends Node {
  kind: "WhileExpr";
  label?: string;
  condition: Expr;
  body: BlockExpr;
}

export interface ForExpr extends Node {
  kind: "ForExpr";
  label?: string;
  pattern: Pattern;
  iter: Expr;
  body: BlockExpr;
}

// Blocks
export interface BlockExpr extends Node {
  kind: "BlockExpr";
  stmts: Stmt[];
  expr?: Expr; // Trailing expression (the value)
}

// Closures
export interface ClosureParam {
  pattern: Pattern;
  type?: Type;
}

export interface ClosureExpr extends Node {
  kind: "ClosureExpr";
  params: ClosureParam[];
  ret?: Type;
  body: Expr;
}

// Control
export interface ReturnExpr extends Node {
  kind: "ReturnExpr";
  value?: Expr;
}

export interface BreakExpr extends Node {
  kind: "BreakExpr";
  label?: string;
  value?: Expr;
}

export interface ContinueExpr extends Node {
  kind: "ContinueExpr";
  label?: string;
}

// Range
export interface RangeExpr extends Node {
  kind: "RangeExpr";
  start?: Expr;
  end?: Expr;
  inclusive: boolean;
}

// Error handling
export interface TryExpr extends Node {
  kind: "TryExpr";
  expr: Expr;
}

// Async
export interface AwaitExpr extends Node {
  kind: "AwaitExpr";
  expr: Expr;
}

// Assignment
export interface AssignExpr extends Node {
  kind: "AssignExpr";
  target: Expr;
  value: Expr;
  op?: BinaryOp; // For compound assignment (+=, -=, etc.)
}

// =============================================================================
// STATEMENTS
// =============================================================================

export type Stmt = LetStmt | ExprStmt | Item;

export interface LetStmt extends Node {
  kind: "LetStmt";
  pattern: Pattern;
  type?: Type;
  init?: Expr;
  mutable: boolean;
}

export interface ExprStmt extends Node {
  kind: "ExprStmt";
  expr: Expr;
  semi: boolean; // Whether it ends with semicolon
}

// =============================================================================
// ITEMS (Top-level declarations)
// =============================================================================

export type Item =
  | FnItem
  | StructItem
  | EnumItem
  | TraitItem
  | ImplItem
  | UseItem
  | ModItem
  | TypeItem;

// Function parameters
export interface FnParam {
  pattern: Pattern;
  type: Type;
}

export interface FnItem extends Node {
  kind: "FnItem";
  name: string;
  vis: Visibility;
  async: boolean;
  generics?: GenericParams;
  params: FnParam[];
  ret?: Type;
  body?: BlockExpr;
}

export interface StructItem extends Node {
  kind: "StructItem";
  name: string;
  vis: Visibility;
  generics?: GenericParams;
  fields: StructField[];
}

export interface StructField {
  name: string;
  vis: Visibility;
  type: Type;
}

export interface EnumItem extends Node {
  kind: "EnumItem";
  name: string;
  vis: Visibility;
  generics?: GenericParams;
  variants: EnumVariant[];
}

export interface EnumVariant {
  name: string;
  fields?: Type[]; // Tuple variant
  named?: StructField[]; // Struct variant
}

export interface TraitItem extends Node {
  kind: "TraitItem";
  name: string;
  vis: Visibility;
  generics?: GenericParams;
  supertraits?: Type[];
  items: (FnItem | TypeItem)[];
}

export interface ImplItem extends Node {
  kind: "ImplItem";
  generics?: GenericParams;
  trait?: Type;
  type: Type;
  items: FnItem[];
}

export interface UseItem extends Node {
  kind: "UseItem";
  vis: Visibility;
  path: string[];
  alias?: string;
  glob: boolean; // use foo::*
  names?: { name: string; alias?: string }[]; // use foo::{a, b as c}
}

export interface ModItem extends Node {
  kind: "ModItem";
  name: string;
  vis: Visibility;
  items?: Item[]; // Inline module
}

export interface TypeItem extends Node {
  kind: "TypeItem";
  name: string;
  vis: Visibility;
  generics?: GenericParams;
  type?: Type;
}

// =============================================================================
// GENERICS
// =============================================================================

export interface GenericParam {
  name: string;
  bounds?: Type[];
  default?: Type;
}

export interface GenericParams {
  params: GenericParam[];
  where_?: WhereClause[];
}

export interface WhereClause {
  type: Type;
  bounds: Type[];
}

// =============================================================================
// VISIBILITY
// =============================================================================

export type Visibility = "private" | "public";

// =============================================================================
// MODULE (Top-level)
// =============================================================================

export interface Module extends Node {
  kind: "Module";
  items: Item[];
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

export function createSpan(start: number, end: number): Span {
  return {
    start: { line: 0, column: 0, offset: start },
    end: { line: 0, column: 0, offset: end },
  };
}

// Factory functions for common nodes
export function intLit(value: number | bigint, span: Span): IntLiteral {
  return { kind: "IntLiteral", value: BigInt(value), span };
}

export function floatLit(value: number, span: Span): FloatLiteral {
  return { kind: "FloatLiteral", value, span };
}

export function stringLit(value: string, span: Span): StringLiteral {
  return { kind: "StringLiteral", value, span };
}

export function boolLit(value: boolean, span: Span): BoolLiteral {
  return { kind: "BoolLiteral", value, span };
}

export function ident(name: string, span: Span): Identifier {
  return { kind: "Identifier", name, span };
}

export function binary(
  op: BinaryOp,
  left: Expr,
  right: Expr,
  span: Span,
): BinaryExpr {
  return { kind: "BinaryExpr", op, left, right, span };
}

export function unary(op: UnaryOp, expr: Expr, span: Span): UnaryExpr {
  return { kind: "UnaryExpr", op, expr, span };
}

export function call(callee: Expr, args: Expr[], span: Span): CallExpr {
  return { kind: "CallExpr", callee, args, span };
}

export function block(
  stmts: Stmt[],
  expr: Expr | undefined,
  span: Span,
): BlockExpr {
  return { kind: "BlockExpr", stmts, expr, span };
}
