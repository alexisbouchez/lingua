/**
 * Lingua Parser
 *
 * Recursive descent parser that transforms tokens into an AST.
 */

import { type Token, TokenType, type Span } from "../lexer/mod.ts";
import type {
  BinaryOp,
  BlockExpr,
  Expr,
  FnItem,
  FnParam,
  GenericParams,
  Identifier,
  Item,
  LetStmt,
  Module,
  Pattern,
  Stmt,
  StructField,
  StructItem,
  Type,
  UnaryOp,
  Visibility,
  EnumItem,
  EnumVariant,
} from "./ast.ts";

export interface ParseError {
  message: string;
  span: Span;
}

export class Parser {
  private tokens: Token[];
  private current = 0;
  private errors: ParseError[] = [];
  private allowStructLiteral = true;

  constructor(tokens: Token[]) {
    this.tokens = tokens;
  }

  // ==========================================================================
  // PUBLIC API
  // ==========================================================================

  parse(): { module: Module; errors: ParseError[] } {
    const items: Item[] = [];
    const start = this.peek().span;

    while (!this.isAtEnd()) {
      try {
        const item = this.parseItem();
        if (item) items.push(item);
      } catch (e) {
        this.synchronize();
      }
    }

    const end = this.previous().span;
    return {
      module: {
        kind: "Module",
        items,
        span: this.mergeSpans(start, end),
      },
      errors: this.errors,
    };
  }

  // ==========================================================================
  // ITEMS
  // ==========================================================================

  private parseItem(): Item | null {
    const vis = this.parseVisibility();

    if (this.check(TokenType.Fn) || this.check(TokenType.Async)) {
      return this.parseFnItem(vis);
    }
    if (this.check(TokenType.Struct)) {
      return this.parseStructItem(vis);
    }
    if (this.check(TokenType.Enum)) {
      return this.parseEnumItem(vis);
    }
    if (this.check(TokenType.Use)) {
      return this.parseUseItem(vis);
    }

    this.error("Expected item (fn, struct, enum, use, ...)");
    return null;
  }

  private parseVisibility(): Visibility {
    if (this.match(TokenType.Pub)) {
      return "public";
    }
    return "private";
  }

  private parseFnItem(vis: Visibility): FnItem {
    const start = this.peek().span;
    const isAsync = this.match(TokenType.Async);
    this.expect(TokenType.Fn, "Expected 'fn'");
    const name = this.expectIdent("Expected function name");

    const generics = this.parseOptionalGenerics();
    this.expect(TokenType.LParen, "Expected '('");
    const params = this.parseFnParams();
    this.expect(TokenType.RParen, "Expected ')'");

    let ret: Type | undefined;
    if (this.match(TokenType.Arrow)) {
      ret = this.parseType();
    }

    let body: BlockExpr | undefined;
    if (this.check(TokenType.LBrace)) {
      body = this.parseBlock();
    }

    return {
      kind: "FnItem",
      name,
      vis,
      async: isAsync,
      generics,
      params,
      ret,
      body,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  private parseFnParams(): FnParam[] {
    const params: FnParam[] = [];

    if (!this.check(TokenType.RParen)) {
      do {
        const pattern = this.parsePattern();
        this.expect(TokenType.Colon, "Expected ':'");
        const type = this.parseType();
        params.push({ pattern, type });
      } while (this.match(TokenType.Comma));
    }

    return params;
  }

  private parseStructItem(vis: Visibility): StructItem {
    const start = this.peek().span;
    this.expect(TokenType.Struct, "Expected 'struct'");
    const name = this.expectIdent("Expected struct name");
    const generics = this.parseOptionalGenerics();

    this.expect(TokenType.LBrace, "Expected '{'");
    const fields = this.parseStructFields();
    this.expect(TokenType.RBrace, "Expected '}'");

    return {
      kind: "StructItem",
      name,
      vis,
      generics,
      fields,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  private parseStructFields(): StructField[] {
    const fields: StructField[] = [];

    while (!this.check(TokenType.RBrace) && !this.isAtEnd()) {
      const fieldVis = this.parseVisibility();
      const name = this.expectIdent("Expected field name");
      this.expect(TokenType.Colon, "Expected ':'");
      const type = this.parseType();
      fields.push({ name, vis: fieldVis, type });

      if (!this.match(TokenType.Comma)) break;
    }

    return fields;
  }

  private parseEnumItem(vis: Visibility): EnumItem {
    const start = this.peek().span;
    this.expect(TokenType.Enum, "Expected 'enum'");
    const name = this.expectIdent("Expected enum name");
    const generics = this.parseOptionalGenerics();

    this.expect(TokenType.LBrace, "Expected '{'");
    const variants = this.parseEnumVariants();
    this.expect(TokenType.RBrace, "Expected '}'");

    return {
      kind: "EnumItem",
      name,
      vis,
      generics,
      variants,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  private parseEnumVariants(): EnumVariant[] {
    const variants: EnumVariant[] = [];

    while (!this.check(TokenType.RBrace) && !this.isAtEnd()) {
      const name = this.expectIdent("Expected variant name");
      let fields: Type[] | undefined;

      if (this.match(TokenType.LParen)) {
        fields = [];
        if (!this.check(TokenType.RParen)) {
          do {
            fields.push(this.parseType());
          } while (this.match(TokenType.Comma));
        }
        this.expect(TokenType.RParen, "Expected ')'");
      }

      variants.push({ name, fields });

      if (!this.match(TokenType.Comma)) break;
    }

    return variants;
  }

  private parseUseItem(vis: Visibility): Item {
    const start = this.peek().span;
    this.expect(TokenType.Use, "Expected 'use'");

    const path: string[] = [];
    path.push(this.expectIdent("Expected module name"));

    while (this.match(TokenType.DoubleColon)) {
      if (this.match(TokenType.Star)) {
        return {
          kind: "UseItem",
          vis,
          path,
          glob: true,
          span: this.mergeSpans(start, this.previous().span),
        };
      }
      path.push(this.expectIdent("Expected module name"));
    }

    return {
      kind: "UseItem",
      vis,
      path,
      glob: false,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  // ==========================================================================
  // GENERICS
  // ==========================================================================

  private parseOptionalGenerics(): GenericParams | undefined {
    if (!this.match(TokenType.Lt)) return undefined;

    const params: GenericParams["params"] = [];

    do {
      const name = this.expectIdent("Expected type parameter name");
      let bounds: Type[] | undefined;

      if (this.match(TokenType.Colon)) {
        bounds = [];
        do {
          bounds.push(this.parseType());
        } while (this.match(TokenType.Plus));
      }

      params.push({ name, bounds });
    } while (this.match(TokenType.Comma));

    this.expect(TokenType.Gt, "Expected '>'");

    return { params };
  }

  // ==========================================================================
  // TYPES
  // ==========================================================================

  private parseType(): Type {
    const start = this.peek().span;

    // Tuple type
    if (this.match(TokenType.LParen)) {
      const elements: Type[] = [];
      if (!this.check(TokenType.RParen)) {
        do {
          elements.push(this.parseType());
        } while (this.match(TokenType.Comma));
      }
      this.expect(TokenType.RParen, "Expected ')'");
      return { kind: "TupleType", elements, span: this.mergeSpans(start, this.previous().span) };
    }

    // Array type
    if (this.match(TokenType.LBracket)) {
      const element = this.parseType();
      let size: Expr | undefined;
      if (this.match(TokenType.Semicolon)) {
        size = this.parseExpr();
      }
      this.expect(TokenType.RBracket, "Expected ']'");
      return { kind: "ArrayType", element, size, span: this.mergeSpans(start, this.previous().span) };
    }

    // Named or generic type
    const name = this.expectIdent("Expected type name");

    if (this.match(TokenType.Lt)) {
      const args: Type[] = [];
      do {
        args.push(this.parseType());
      } while (this.match(TokenType.Comma));
      this.expect(TokenType.Gt, "Expected '>'");
      return { kind: "GenericType", name, args, span: this.mergeSpans(start, this.previous().span) };
    }

    return { kind: "NamedType", name, span: this.mergeSpans(start, this.previous().span) };
  }

  // ==========================================================================
  // STATEMENTS
  // ==========================================================================

  private parseStmt(): Stmt {
    if (this.check(TokenType.Let)) {
      return this.parseLetStmt();
    }
    return this.parseExprStmt();
  }

  private parseLetStmt(): LetStmt {
    const start = this.peek().span;
    this.expect(TokenType.Let, "Expected 'let'");
    const mutable = this.match(TokenType.Mut);
    const pattern = this.parsePattern();

    let type: Type | undefined;
    if (this.match(TokenType.Colon)) {
      type = this.parseType();
    }

    let init: Expr | undefined;
    if (this.match(TokenType.Assign)) {
      init = this.parseExpr();
    }

    // Consume optional semicolon (let statements typically end with ;)
    this.match(TokenType.Semicolon);

    return {
      kind: "LetStmt",
      pattern,
      type,
      init,
      mutable,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  private parseExprStmt(): Stmt {
    const start = this.peek().span;
    const expr = this.parseExpr();
    const semi = this.match(TokenType.Semicolon);

    return {
      kind: "ExprStmt",
      expr,
      semi,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  // ==========================================================================
  // EXPRESSIONS - Precedence climbing
  // ==========================================================================

  private parseExpr(allowStructLiteral = true): Expr {
    const prev = this.allowStructLiteral;
    this.allowStructLiteral = allowStructLiteral;
    const result = this.parseAssignment();
    this.allowStructLiteral = prev;
    return result;
  }

  private parseAssignment(): Expr {
    const expr = this.parseOr();

    if (
      this.match(TokenType.Assign) ||
      this.match(TokenType.PlusAssign) ||
      this.match(TokenType.MinusAssign) ||
      this.match(TokenType.StarAssign) ||
      this.match(TokenType.SlashAssign)
    ) {
      const op = this.previous();
      const value = this.parseAssignment();
      const opStr = this.tokenToAssignOp(op.type);

      return {
        kind: "AssignExpr",
        target: expr,
        value,
        op: opStr,
        span: this.mergeSpans(expr.span, value.span),
      };
    }

    return expr;
  }

  private tokenToAssignOp(type: TokenType): BinaryOp | undefined {
    switch (type) {
      case TokenType.PlusAssign: return "+";
      case TokenType.MinusAssign: return "-";
      case TokenType.StarAssign: return "*";
      case TokenType.SlashAssign: return "/";
      default: return undefined;
    }
  }

  private parseOr(): Expr {
    let left = this.parseAnd();

    while (this.match(TokenType.Or)) {
      const right = this.parseAnd();
      left = {
        kind: "BinaryExpr",
        op: "||",
        left,
        right,
        span: this.mergeSpans(left.span, right.span),
      };
    }

    return left;
  }

  private parseAnd(): Expr {
    let left = this.parseBitwiseOr();

    while (this.match(TokenType.And)) {
      const right = this.parseBitwiseOr();
      left = {
        kind: "BinaryExpr",
        op: "&&",
        left,
        right,
        span: this.mergeSpans(left.span, right.span),
      };
    }

    return left;
  }

  private parseBitwiseOr(): Expr {
    let left = this.parseBitwiseXor();

    while (this.match(TokenType.BitOr)) {
      const right = this.parseBitwiseXor();
      left = {
        kind: "BinaryExpr",
        op: "|",
        left,
        right,
        span: this.mergeSpans(left.span, right.span),
      };
    }

    return left;
  }

  private parseBitwiseXor(): Expr {
    let left = this.parseBitwiseAnd();

    while (this.match(TokenType.BitXor)) {
      const right = this.parseBitwiseAnd();
      left = {
        kind: "BinaryExpr",
        op: "^",
        left,
        right,
        span: this.mergeSpans(left.span, right.span),
      };
    }

    return left;
  }

  private parseBitwiseAnd(): Expr {
    let left = this.parseEquality();

    while (this.match(TokenType.BitAnd)) {
      const right = this.parseEquality();
      left = {
        kind: "BinaryExpr",
        op: "&",
        left,
        right,
        span: this.mergeSpans(left.span, right.span),
      };
    }

    return left;
  }

  private parseEquality(): Expr {
    let left = this.parseComparison();

    while (this.match(TokenType.Eq) || this.match(TokenType.Ne)) {
      const op: BinaryOp = this.previous().type === TokenType.Eq ? "==" : "!=";
      const right = this.parseComparison();
      left = {
        kind: "BinaryExpr",
        op,
        left,
        right,
        span: this.mergeSpans(left.span, right.span),
      };
    }

    return left;
  }

  private parseComparison(): Expr {
    let left = this.parseShift();

    while (
      this.match(TokenType.Lt) ||
      this.match(TokenType.Gt) ||
      this.match(TokenType.Le) ||
      this.match(TokenType.Ge)
    ) {
      const op = this.tokenToBinaryOp(this.previous().type);
      const right = this.parseShift();
      left = {
        kind: "BinaryExpr",
        op,
        left,
        right,
        span: this.mergeSpans(left.span, right.span),
      };
    }

    return left;
  }

  private parseShift(): Expr {
    let left = this.parseTerm();

    while (this.match(TokenType.Shl) || this.match(TokenType.Shr)) {
      const op: BinaryOp = this.previous().type === TokenType.Shl ? "<<" : ">>";
      const right = this.parseTerm();
      left = {
        kind: "BinaryExpr",
        op,
        left,
        right,
        span: this.mergeSpans(left.span, right.span),
      };
    }

    return left;
  }

  private parseTerm(): Expr {
    let left = this.parseFactor();

    while (this.match(TokenType.Plus) || this.match(TokenType.Minus)) {
      const op: BinaryOp = this.previous().type === TokenType.Plus ? "+" : "-";
      const right = this.parseFactor();
      left = {
        kind: "BinaryExpr",
        op,
        left,
        right,
        span: this.mergeSpans(left.span, right.span),
      };
    }

    return left;
  }

  private parseFactor(): Expr {
    let left = this.parseUnary();

    while (
      this.match(TokenType.Star) ||
      this.match(TokenType.Slash) ||
      this.match(TokenType.Percent)
    ) {
      const op = this.tokenToBinaryOp(this.previous().type);
      const right = this.parseUnary();
      left = {
        kind: "BinaryExpr",
        op,
        left,
        right,
        span: this.mergeSpans(left.span, right.span),
      };
    }

    return left;
  }

  private parseUnary(): Expr {
    if (this.match(TokenType.Not) || this.match(TokenType.Minus)) {
      const op: UnaryOp = this.previous().type === TokenType.Not ? "!" : "-";
      const expr = this.parseUnary();
      return {
        kind: "UnaryExpr",
        op,
        expr,
        span: this.mergeSpans(this.previous().span, expr.span),
      };
    }

    return this.parsePostfix();
  }

  private parsePostfix(): Expr {
    let expr = this.parsePrimary();

    while (true) {
      if (this.match(TokenType.LParen)) {
        // Function call
        const args: Expr[] = [];
        if (!this.check(TokenType.RParen)) {
          do {
            args.push(this.parseExpr());
          } while (this.match(TokenType.Comma));
        }
        this.expect(TokenType.RParen, "Expected ')'");
        expr = {
          kind: "CallExpr",
          callee: expr,
          args,
          span: this.mergeSpans(expr.span, this.previous().span),
        };
      } else if (this.match(TokenType.Dot)) {
        // Field access or method call
        // Allow both identifiers and integers (for tuple access like t.0)
        let field: string;
        if (this.check(TokenType.Ident)) {
          field = this.advance().lexeme;
        } else if (this.check(TokenType.Int)) {
          field = this.advance().lexeme;
        } else {
          this.error("Expected field name");
          field = "<error>";
        }
        if (this.match(TokenType.LParen)) {
          const args: Expr[] = [];
          if (!this.check(TokenType.RParen)) {
            do {
              args.push(this.parseExpr());
            } while (this.match(TokenType.Comma));
          }
          this.expect(TokenType.RParen, "Expected ')'");
          expr = {
            kind: "MethodCallExpr",
            receiver: expr,
            method: field,
            args,
            span: this.mergeSpans(expr.span, this.previous().span),
          };
        } else {
          expr = {
            kind: "FieldExpr",
            expr,
            field,
            span: this.mergeSpans(expr.span, this.previous().span),
          };
        }
      } else if (this.match(TokenType.LBracket)) {
        // Index access
        const index = this.parseExpr();
        this.expect(TokenType.RBracket, "Expected ']'");
        expr = {
          kind: "IndexExpr",
          expr,
          index,
          span: this.mergeSpans(expr.span, this.previous().span),
        };
      } else if (this.match(TokenType.Question)) {
        // Try operator
        expr = {
          kind: "TryExpr",
          expr,
          span: this.mergeSpans(expr.span, this.previous().span),
        };
      } else {
        break;
      }
    }

    return expr;
  }

  private parsePrimary(): Expr {
    const tok = this.peek();

    // Literals
    if (this.match(TokenType.Int)) {
      return {
        kind: "IntLiteral",
        value: BigInt(this.previous().value as number),
        span: this.previous().span,
      };
    }
    if (this.match(TokenType.Float)) {
      return {
        kind: "FloatLiteral",
        value: this.previous().value as number,
        span: this.previous().span,
      };
    }
    if (this.match(TokenType.String)) {
      return {
        kind: "StringLiteral",
        value: this.previous().value as string,
        span: this.previous().span,
      };
    }
    if (this.match(TokenType.Char)) {
      return {
        kind: "CharLiteral",
        value: this.previous().value as string,
        span: this.previous().span,
      };
    }
    if (this.match(TokenType.True) || this.match(TokenType.False)) {
      return {
        kind: "BoolLiteral",
        value: this.previous().type === TokenType.True,
        span: this.previous().span,
      };
    }

    // Identifier or Path
    if (this.match(TokenType.Ident)) {
      const start = this.previous().span;
      const segments = [this.previous().lexeme];

      // Check for path (e.g., Status::Ok)
      while (this.match(TokenType.DoubleColon)) {
        if (this.check(TokenType.Ident)) {
          segments.push(this.advance().lexeme);
        } else {
          this.error("Expected identifier after '::'");
          break;
        }
      }

      // If it's a multi-segment path, return PathExpr
      if (segments.length > 1) {
        return {
          kind: "PathExpr",
          segments,
          span: this.mergeSpans(start, this.previous().span),
        };
      }

      const name = segments[0];

      // Check for struct literal (only if allowed in current context)
      if (this.allowStructLiteral && this.check(TokenType.LBrace)) {
        return this.parseStructExpr(name, start);
      }

      return {
        kind: "Identifier",
        name,
        span: start,
      };
    }

    // Grouped expression or tuple
    if (this.match(TokenType.LParen)) {
      const start = this.previous().span;
      if (this.match(TokenType.RParen)) {
        // Empty tuple / unit
        return {
          kind: "TupleExpr",
          elements: [],
          span: this.mergeSpans(start, this.previous().span),
        };
      }

      const first = this.parseExpr();

      if (this.match(TokenType.Comma)) {
        // Tuple
        const elements = [first];
        if (!this.check(TokenType.RParen)) {
          do {
            elements.push(this.parseExpr());
          } while (this.match(TokenType.Comma));
        }
        this.expect(TokenType.RParen, "Expected ')'");
        return {
          kind: "TupleExpr",
          elements,
          span: this.mergeSpans(start, this.previous().span),
        };
      }

      this.expect(TokenType.RParen, "Expected ')'");
      return first; // Grouped expression
    }

    // Array literal
    if (this.match(TokenType.LBracket)) {
      const start = this.previous().span;
      const elements: Expr[] = [];
      if (!this.check(TokenType.RBracket)) {
        do {
          elements.push(this.parseExpr());
        } while (this.match(TokenType.Comma));
      }
      this.expect(TokenType.RBracket, "Expected ']'");
      return {
        kind: "ArrayExpr",
        elements,
        span: this.mergeSpans(start, this.previous().span),
      };
    }

    // Block expression
    if (this.check(TokenType.LBrace)) {
      return this.parseBlock();
    }

    // If expression
    if (this.match(TokenType.If)) {
      return this.parseIfExpr();
    }

    // Match expression
    if (this.match(TokenType.Match)) {
      return this.parseMatchExpr();
    }

    // Loop expressions
    if (this.match(TokenType.Loop)) {
      return this.parseLoopExpr();
    }
    if (this.match(TokenType.While)) {
      return this.parseWhileExpr();
    }
    if (this.match(TokenType.For)) {
      return this.parseForExpr();
    }

    // Return
    if (this.match(TokenType.Return)) {
      const start = this.previous().span;
      let value: Expr | undefined;
      if (!this.check(TokenType.Semicolon) && !this.check(TokenType.RBrace)) {
        value = this.parseExpr();
      }
      return {
        kind: "ReturnExpr",
        value,
        span: this.mergeSpans(start, this.previous().span),
      };
    }

    // Break
    if (this.match(TokenType.Break)) {
      const start = this.previous().span;
      return {
        kind: "BreakExpr",
        span: this.mergeSpans(start, this.previous().span),
      };
    }

    // Continue
    if (this.match(TokenType.Continue)) {
      return {
        kind: "ContinueExpr",
        span: this.previous().span,
      };
    }

    // Closure: |params| body or || body (no params)
    if (this.match(TokenType.BitOr)) {
      return this.parseClosureExpr();
    }
    // Handle || as empty param closure
    if (this.match(TokenType.Or)) {
      return this.parseClosureExprNoParams();
    }

    this.error(`Unexpected token: ${tok.type}`);
    this.advance();
    return {
      kind: "Identifier",
      name: "<error>",
      span: tok.span,
    };
  }

  private parseStructExpr(name: string, start: Span): Expr {
    this.expect(TokenType.LBrace, "Expected '{'");
    const fields: { name: string; value: Expr }[] = [];
    let spread: Expr | undefined;

    while (!this.check(TokenType.RBrace) && !this.isAtEnd()) {
      if (this.match(TokenType.DotDot)) {
        spread = this.parseExpr();
        break;
      }

      const fieldName = this.expectIdent("Expected field name");
      let value: Expr;

      if (this.match(TokenType.Colon)) {
        value = this.parseExpr();
      } else {
        // Shorthand: Point { x } means Point { x: x }
        value = { kind: "Identifier", name: fieldName, span: this.previous().span };
      }

      fields.push({ name: fieldName, value });

      if (!this.match(TokenType.Comma)) break;
    }

    this.expect(TokenType.RBrace, "Expected '}'");
    return {
      kind: "StructExpr",
      name,
      fields,
      spread,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  private parseBlock(): BlockExpr {
    const start = this.peek().span;
    this.expect(TokenType.LBrace, "Expected '{'");

    const stmts: Stmt[] = [];
    let expr: Expr | undefined;

    while (!this.check(TokenType.RBrace) && !this.isAtEnd()) {
      const stmt = this.parseStmt();

      // Check if this is a trailing expression
      if (stmt.kind === "ExprStmt" && !stmt.semi && this.check(TokenType.RBrace)) {
        expr = stmt.expr;
        break;
      }

      stmts.push(stmt);
    }

    this.expect(TokenType.RBrace, "Expected '}'");
    return {
      kind: "BlockExpr",
      stmts,
      expr,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  private parseIfExpr(): Expr {
    const start = this.previous().span;
    const condition = this.parseExpr(false); // No struct literals in condition
    const then = this.parseBlock();

    let else_: Expr | undefined;
    if (this.match(TokenType.Else)) {
      if (this.match(TokenType.If)) {
        else_ = this.parseIfExpr();
      } else {
        else_ = this.parseBlock();
      }
    }

    return {
      kind: "IfExpr",
      condition,
      then,
      else_,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  private parseMatchExpr(): Expr {
    const start = this.previous().span;
    const expr = this.parseExpr(false); // No struct literals in scrutinee
    this.expect(TokenType.LBrace, "Expected '{'");

    const arms: { pattern: Pattern; guard?: Expr; body: Expr }[] = [];

    while (!this.check(TokenType.RBrace) && !this.isAtEnd()) {
      const pattern = this.parsePattern();
      let guard: Expr | undefined;

      if (this.match(TokenType.If)) {
        guard = this.parseExpr(false); // No struct literals in guard
      }

      this.expect(TokenType.FatArrow, "Expected '=>'");
      const body = this.parseExpr();
      arms.push({ pattern, guard, body });

      if (!this.match(TokenType.Comma)) break;
    }

    this.expect(TokenType.RBrace, "Expected '}'");
    return {
      kind: "MatchExpr",
      expr,
      arms,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  private parseLoopExpr(): Expr {
    const start = this.previous().span;
    const body = this.parseBlock();
    return {
      kind: "LoopExpr",
      body,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  private parseWhileExpr(): Expr {
    const start = this.previous().span;
    const condition = this.parseExpr(false); // No struct literals in condition
    const body = this.parseBlock();
    return {
      kind: "WhileExpr",
      condition,
      body,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  private parseForExpr(): Expr {
    const start = this.previous().span;
    const pattern = this.parsePattern();
    this.expect(TokenType.In, "Expected 'in'");
    const iter = this.parseExpr(false); // No struct literals in iterator
    const body = this.parseBlock();
    return {
      kind: "ForExpr",
      pattern,
      iter,
      body,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  private parseClosureExpr(): Expr {
    const start = this.previous().span;
    const params: { pattern: Pattern; type?: Type }[] = [];

    if (!this.check(TokenType.BitOr)) {
      do {
        const pattern = this.parsePattern();
        let type: Type | undefined;
        if (this.match(TokenType.Colon)) {
          type = this.parseType();
        }
        params.push({ pattern, type });
      } while (this.match(TokenType.Comma));
    }

    this.expect(TokenType.BitOr, "Expected '|'");

    let ret: Type | undefined;
    if (this.match(TokenType.Arrow)) {
      ret = this.parseType();
    }

    const body = this.check(TokenType.LBrace) ? this.parseBlock() : this.parseExpr();

    return {
      kind: "ClosureExpr",
      params,
      ret,
      body,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  // Handle || as closure with no params (|| is lexed as single Or token)
  private parseClosureExprNoParams(): Expr {
    const start = this.previous().span;

    let ret: Type | undefined;
    if (this.match(TokenType.Arrow)) {
      ret = this.parseType();
    }

    const body = this.check(TokenType.LBrace) ? this.parseBlock() : this.parseExpr();

    return {
      kind: "ClosureExpr",
      params: [],
      ret,
      body,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  // ==========================================================================
  // PATTERNS
  // ==========================================================================

  private parsePattern(): Pattern {
    const start = this.peek().span;

    // Wildcard
    if (this.match(TokenType.Underscore)) {
      return { kind: "WildcardPattern", span: this.previous().span };
    }

    // Literal patterns
    if (this.match(TokenType.Int)) {
      return {
        kind: "LiteralPattern",
        value: {
          kind: "IntLiteral",
          value: BigInt(this.previous().value as number),
          span: this.previous().span,
        },
        span: this.previous().span,
      };
    }
    if (this.match(TokenType.True) || this.match(TokenType.False)) {
      return {
        kind: "LiteralPattern",
        value: {
          kind: "BoolLiteral",
          value: this.previous().type === TokenType.True,
          span: this.previous().span,
        },
        span: this.previous().span,
      };
    }
    if (this.match(TokenType.String)) {
      return {
        kind: "LiteralPattern",
        value: {
          kind: "StringLiteral",
          value: this.previous().value as string,
          span: this.previous().span,
        },
        span: this.previous().span,
      };
    }

    // Tuple pattern
    if (this.match(TokenType.LParen)) {
      const elements: Pattern[] = [];
      if (!this.check(TokenType.RParen)) {
        do {
          elements.push(this.parsePattern());
        } while (this.match(TokenType.Comma));
      }
      this.expect(TokenType.RParen, "Expected ')'");
      return {
        kind: "TuplePattern",
        elements,
        span: this.mergeSpans(start, this.previous().span),
      };
    }

    // Identifier pattern (or struct/enum pattern)
    if (this.match(TokenType.Mut)) {
      const name = this.expectIdent("Expected identifier");
      return { kind: "IdentPattern", name, mutable: true, span: this.mergeSpans(start, this.previous().span) };
    }

    if (this.match(TokenType.Ident)) {
      const name = this.previous().lexeme;

      // Check for struct pattern
      if (this.check(TokenType.LBrace)) {
        return this.parseStructPattern(name, start);
      }

      // Check for enum variant
      if (this.check(TokenType.LParen)) {
        this.advance();
        const fields: Pattern[] = [];
        if (!this.check(TokenType.RParen)) {
          do {
            fields.push(this.parsePattern());
          } while (this.match(TokenType.Comma));
        }
        this.expect(TokenType.RParen, "Expected ')'");
        return {
          kind: "EnumPattern",
          name: "",
          variant: name,
          fields,
          span: this.mergeSpans(start, this.previous().span),
        };
      }

      return { kind: "IdentPattern", name, mutable: false, span: this.previous().span };
    }

    this.error("Expected pattern");
    return { kind: "WildcardPattern", span: this.peek().span };
  }

  private parseStructPattern(name: string, start: Span): Pattern {
    this.expect(TokenType.LBrace, "Expected '{'");
    const fields: { name: string; pattern: Pattern }[] = [];
    let rest = false;

    while (!this.check(TokenType.RBrace) && !this.isAtEnd()) {
      if (this.match(TokenType.DotDot)) {
        rest = true;
        break;
      }

      const fieldName = this.expectIdent("Expected field name");
      let pattern: Pattern;

      if (this.match(TokenType.Colon)) {
        pattern = this.parsePattern();
      } else {
        pattern = { kind: "IdentPattern", name: fieldName, mutable: false, span: this.previous().span };
      }

      fields.push({ name: fieldName, pattern });

      if (!this.match(TokenType.Comma)) break;
    }

    this.expect(TokenType.RBrace, "Expected '}'");
    return {
      kind: "StructPattern",
      name,
      fields,
      rest,
      span: this.mergeSpans(start, this.previous().span),
    };
  }

  // ==========================================================================
  // HELPERS
  // ==========================================================================

  private tokenToBinaryOp(type: TokenType): BinaryOp {
    switch (type) {
      case TokenType.Plus: return "+";
      case TokenType.Minus: return "-";
      case TokenType.Star: return "*";
      case TokenType.Slash: return "/";
      case TokenType.Percent: return "%";
      case TokenType.Eq: return "==";
      case TokenType.Ne: return "!=";
      case TokenType.Lt: return "<";
      case TokenType.Gt: return ">";
      case TokenType.Le: return "<=";
      case TokenType.Ge: return ">=";
      case TokenType.And: return "&&";
      case TokenType.Or: return "||";
      case TokenType.BitAnd: return "&";
      case TokenType.BitOr: return "|";
      case TokenType.BitXor: return "^";
      case TokenType.Shl: return "<<";
      case TokenType.Shr: return ">>";
      default:
        throw new Error(`Unknown binary operator: ${type}`);
    }
  }

  private check(type: TokenType): boolean {
    if (this.isAtEnd()) return false;
    return this.peek().type === type;
  }

  private match(...types: TokenType[]): boolean {
    for (const type of types) {
      if (this.check(type)) {
        this.advance();
        return true;
      }
    }
    return false;
  }

  private advance(): Token {
    if (!this.isAtEnd()) this.current++;
    return this.previous();
  }

  private isAtEnd(): boolean {
    return this.peek().type === TokenType.Eof;
  }

  private peek(): Token {
    return this.tokens[this.current];
  }

  private previous(): Token {
    return this.tokens[this.current - 1];
  }

  private expect(type: TokenType, message: string): Token {
    if (this.check(type)) return this.advance();
    this.error(message);
    return this.peek();
  }

  private expectIdent(message: string): string {
    if (this.check(TokenType.Ident)) {
      return this.advance().lexeme;
    }
    this.error(message);
    return "<error>";
  }

  private error(message: string): void {
    this.errors.push({
      message,
      span: this.peek().span,
    });
  }

  private synchronize(): void {
    this.advance();

    while (!this.isAtEnd()) {
      // Stop at statement boundaries
      if (this.previous().type === TokenType.Semicolon) return;
      if (this.previous().type === TokenType.RBrace) return;

      // Stop at item keywords
      switch (this.peek().type) {
        case TokenType.Fn:
        case TokenType.Struct:
        case TokenType.Enum:
        case TokenType.Trait:
        case TokenType.Impl:
        case TokenType.Use:
        case TokenType.Pub:
        case TokenType.Mod:
          return;
      }

      this.advance();
    }
  }

  private mergeSpans(start: Span, end: Span): Span {
    return {
      start: start.start,
      end: end.end,
      file: start.file,
    };
  }
}

/**
 * Convenience function to parse source code
 */
export function parse(tokens: Token[]): { module: Module; errors: ParseError[] } {
  const parser = new Parser(tokens);
  return parser.parse();
}
