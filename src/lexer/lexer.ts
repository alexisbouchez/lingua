/**
 * Lingua Lexer
 *
 * Transforms source code into a stream of tokens.
 * Handles:
 * - Keywords and identifiers
 * - Integer and float literals
 * - String literals with escape sequences
 * - Character literals
 * - Operators (single and multi-character)
 * - Comments (line and block)
 * - Source location tracking
 */

import {
  KEYWORDS,
  type SourceLocation,
  type Span,
  type Token,
  token,
  TokenType,
} from "./token.ts";

export interface LexerError {
  message: string;
  span: Span;
}

export class Lexer {
  private source: string;
  private tokens: Token[] = [];
  private errors: LexerError[] = [];

  // Current position tracking
  private start = 0; // Start of current token
  private current = 0; // Current character
  private line = 1;
  private column = 1;
  private startLine = 1;
  private startColumn = 1;

  private file?: string;

  constructor(source: string, file?: string) {
    this.source = source;
    this.file = file;
  }

  /**
   * Tokenize the entire source code
   */
  tokenize(): { tokens: Token[]; errors: LexerError[] } {
    while (!this.isAtEnd()) {
      this.start = this.current;
      this.startLine = this.line;
      this.startColumn = this.column;
      this.scanToken();
    }

    // Add EOF token
    this.tokens.push(
      token(TokenType.Eof, "", this.makeSpan()),
    );

    return { tokens: this.tokens, errors: this.errors };
  }

  private scanToken(): void {
    const c = this.advance();

    switch (c) {
      // Single-character tokens
      case "(":
        this.addToken(TokenType.LParen);
        break;
      case ")":
        this.addToken(TokenType.RParen);
        break;
      case "{":
        this.addToken(TokenType.LBrace);
        break;
      case "}":
        this.addToken(TokenType.RBrace);
        break;
      case "[":
        this.addToken(TokenType.LBracket);
        break;
      case "]":
        this.addToken(TokenType.RBracket);
        break;
      case ",":
        this.addToken(TokenType.Comma);
        break;
      case ";":
        this.addToken(TokenType.Semicolon);
        break;
      case "@":
        this.addToken(TokenType.At);
        break;
      case "#":
        this.addToken(TokenType.Hash);
        break;
      case "?":
        this.addToken(TokenType.Question);
        break;
      case "_":
        // Could be underscore or start of identifier
        if (this.isAlphaNumeric(this.peek())) {
          this.identifier();
        } else {
          this.addToken(TokenType.Underscore);
        }
        break;

      // Potentially multi-character tokens
      case "+":
        this.addToken(this.match("=") ? TokenType.PlusAssign : TokenType.Plus);
        break;
      case "-":
        if (this.match(">")) {
          this.addToken(TokenType.Arrow);
        } else if (this.match("=")) {
          this.addToken(TokenType.MinusAssign);
        } else {
          this.addToken(TokenType.Minus);
        }
        break;
      case "*":
        this.addToken(this.match("=") ? TokenType.StarAssign : TokenType.Star);
        break;
      case "%":
        this.addToken(TokenType.Percent);
        break;
      case "!":
        this.addToken(this.match("=") ? TokenType.Ne : TokenType.Not);
        break;
      case "=":
        if (this.match("=")) {
          this.addToken(TokenType.Eq);
        } else if (this.match(">")) {
          this.addToken(TokenType.FatArrow);
        } else {
          this.addToken(TokenType.Assign);
        }
        break;
      case "<":
        if (this.match("=")) {
          this.addToken(TokenType.Le);
        } else if (this.match("<")) {
          this.addToken(TokenType.Shl);
        } else {
          this.addToken(TokenType.Lt);
        }
        break;
      case ">":
        if (this.match("=")) {
          this.addToken(TokenType.Ge);
        } else if (this.match(">")) {
          this.addToken(TokenType.Shr);
        } else {
          this.addToken(TokenType.Gt);
        }
        break;
      case "&":
        this.addToken(this.match("&") ? TokenType.And : TokenType.BitAnd);
        break;
      case "|":
        this.addToken(this.match("|") ? TokenType.Or : TokenType.BitOr);
        break;
      case "^":
        this.addToken(TokenType.BitXor);
        break;
      case ".":
        if (this.match(".")) {
          if (this.match("=")) {
            this.addToken(TokenType.DotDotEq);
          } else {
            this.addToken(TokenType.DotDot);
          }
        } else {
          this.addToken(TokenType.Dot);
        }
        break;
      case ":":
        this.addToken(this.match(":") ? TokenType.DoubleColon : TokenType.Colon);
        break;

      // Slash: division or comment
      case "/":
        if (this.match("/")) {
          // Line comment
          while (this.peek() !== "\n" && !this.isAtEnd()) {
            this.advance();
          }
        } else if (this.match("*")) {
          // Block comment
          this.blockComment();
        } else if (this.match("=")) {
          this.addToken(TokenType.SlashAssign);
        } else {
          this.addToken(TokenType.Slash);
        }
        break;

      // Whitespace
      case " ":
      case "\r":
      case "\t":
        // Ignore whitespace
        break;
      case "\n":
        this.line++;
        this.column = 1;
        break;

      // String literal
      case '"':
        this.string();
        break;

      // Character literal
      case "'":
        this.char();
        break;

      default:
        if (this.isDigit(c)) {
          this.number();
        } else if (this.isAlpha(c)) {
          this.identifier();
        } else {
          this.error(`Unexpected character: '${c}'`);
        }
    }
  }

  private string(): void {
    let value = "";

    // Check for triple-quoted string (peek ahead without consuming)
    const isTriple = this.peek() === '"' && this.peekNext() === '"';
    if (isTriple) {
      this.advance(); // consume second "
      this.advance(); // consume third "
    }

    while (!this.isAtEnd()) {
      if (isTriple) {
        if (this.peek() === '"' && this.peekNext() === '"' && this.peekAt(2) === '"') {
          this.advance();
          this.advance();
          this.advance();
          this.addToken(TokenType.String, value);
          return;
        }
        // In triple-quoted strings, handle newlines
        if (this.peek() === "\n") {
          this.line++;
          this.column = 1;
          value += this.advance();
          continue;
        }
      } else {
        if (this.peek() === '"') {
          this.advance();
          this.addToken(TokenType.String, value);
          return;
        }
        if (this.peek() === "\n") {
          this.error("Unterminated string literal");
          return;
        }
      }

      if (this.peek() === "\\") {
        this.advance(); // consume backslash
        if (this.isAtEnd()) {
          this.error("Unterminated string literal");
          return;
        }
        const escaped = this.advance();
        switch (escaped) {
          case "n":
            value += "\n";
            break;
          case "r":
            value += "\r";
            break;
          case "t":
            value += "\t";
            break;
          case "\\":
            value += "\\";
            break;
          case '"':
            value += '"';
            break;
          case "0":
            value += "\0";
            break;
          default:
            this.error(`Invalid escape sequence: \\${escaped}`);
            return;
        }
      } else {
        value += this.advance();
      }
    }

    this.error("Unterminated string literal");
  }

  private char(): void {
    if (this.isAtEnd()) {
      this.error("Unterminated character literal");
      return;
    }

    let value: string;
    if (this.peek() === "\\") {
      this.advance(); // consume backslash
      const escaped = this.advance();
      switch (escaped) {
        case "n":
          value = "\n";
          break;
        case "r":
          value = "\r";
          break;
        case "t":
          value = "\t";
          break;
        case "\\":
          value = "\\";
          break;
        case "'":
          value = "'";
          break;
        case "0":
          value = "\0";
          break;
        default:
          this.error(`Invalid escape sequence: \\${escaped}`);
          return;
      }
    } else {
      value = this.advance();
    }

    if (this.peek() !== "'") {
      this.error("Unterminated character literal");
      return;
    }
    this.advance(); // consume closing quote

    this.addToken(TokenType.Char, value);
  }

  private number(): void {
    // Check for hex, octal, or binary
    if (this.source[this.start] === "0") {
      if (this.match("x") || this.match("X")) {
        this.hexNumber();
        return;
      }
      if (this.match("o") || this.match("O")) {
        this.octalNumber();
        return;
      }
      if (this.match("b") || this.match("B")) {
        this.binaryNumber();
        return;
      }
    }

    // Decimal number
    while (this.isDigit(this.peek()) || this.peek() === "_") {
      this.advance();
    }

    let isFloat = false;

    // Check for decimal point
    if (this.peek() === "." && this.isDigit(this.peekNext())) {
      isFloat = true;
      this.advance(); // consume '.'
      while (this.isDigit(this.peek()) || this.peek() === "_") {
        this.advance();
      }
    }

    // Check for exponent (can appear with or without decimal point)
    if (this.peek() === "e" || this.peek() === "E") {
      isFloat = true;
      this.advance();
      if (this.peek() === "+" || this.peek() === "-") {
        this.advance();
      }
      while (this.isDigit(this.peek())) {
        this.advance();
      }
    }

    const text = this.source.slice(this.start, this.current).replace(/_/g, "");
    if (isFloat) {
      const value = parseFloat(text);
      this.addToken(TokenType.Float, value);
    } else {
      const value = parseInt(text, 10);
      this.addToken(TokenType.Int, value);
    }
  }

  private hexNumber(): void {
    while (this.isHexDigit(this.peek()) || this.peek() === "_") {
      this.advance();
    }
    const text = this.source.slice(this.start + 2, this.current).replace(/_/g, "");
    const value = parseInt(text, 16);
    this.addToken(TokenType.Int, value);
  }

  private octalNumber(): void {
    while (this.isOctalDigit(this.peek()) || this.peek() === "_") {
      this.advance();
    }
    const text = this.source.slice(this.start + 2, this.current).replace(/_/g, "");
    const value = parseInt(text, 8);
    this.addToken(TokenType.Int, value);
  }

  private binaryNumber(): void {
    while (this.peek() === "0" || this.peek() === "1" || this.peek() === "_") {
      this.advance();
    }
    const text = this.source.slice(this.start + 2, this.current).replace(/_/g, "");
    const value = parseInt(text, 2);
    this.addToken(TokenType.Int, value);
  }

  private identifier(): void {
    while (this.isAlphaNumeric(this.peek())) {
      this.advance();
    }

    const text = this.source.slice(this.start, this.current);
    const type = KEYWORDS[text] ?? TokenType.Ident;

    if (type === TokenType.True) {
      this.addToken(type, true);
    } else if (type === TokenType.False) {
      this.addToken(type, false);
    } else {
      this.addToken(type);
    }
  }

  private blockComment(): void {
    let depth = 1;
    while (depth > 0 && !this.isAtEnd()) {
      if (this.peek() === "/" && this.peekNext() === "*") {
        this.advance();
        this.advance();
        depth++;
      } else if (this.peek() === "*" && this.peekNext() === "/") {
        this.advance();
        this.advance();
        depth--;
      } else {
        if (this.peek() === "\n") {
          this.line++;
          this.column = 1;
        }
        this.advance();
      }
    }

    if (depth > 0) {
      this.error("Unterminated block comment");
    }
  }

  // Helper methods

  private isAtEnd(): boolean {
    return this.current >= this.source.length;
  }

  private advance(): string {
    const c = this.source[this.current];
    this.current++;
    this.column++;
    return c;
  }

  private peek(): string {
    if (this.isAtEnd()) return "\0";
    return this.source[this.current];
  }

  private peekNext(): string {
    if (this.current + 1 >= this.source.length) return "\0";
    return this.source[this.current + 1];
  }

  private peekAt(offset: number): string {
    if (this.current + offset >= this.source.length) return "\0";
    return this.source[this.current + offset];
  }

  private match(expected: string): boolean {
    if (this.isAtEnd()) return false;
    if (this.source[this.current] !== expected) return false;
    this.current++;
    this.column++;
    return true;
  }

  private isDigit(c: string): boolean {
    return c >= "0" && c <= "9";
  }

  private isHexDigit(c: string): boolean {
    return (c >= "0" && c <= "9") ||
      (c >= "a" && c <= "f") ||
      (c >= "A" && c <= "F");
  }

  private isOctalDigit(c: string): boolean {
    return c >= "0" && c <= "7";
  }

  private isAlpha(c: string): boolean {
    return (c >= "a" && c <= "z") ||
      (c >= "A" && c <= "Z") ||
      c === "_";
  }

  private isAlphaNumeric(c: string): boolean {
    return this.isAlpha(c) || this.isDigit(c);
  }

  private makeSpan(): Span {
    return {
      start: {
        line: this.startLine,
        column: this.startColumn,
        offset: this.start,
      },
      end: {
        line: this.line,
        column: this.column,
        offset: this.current,
      },
      file: this.file,
    };
  }

  private addToken(type: TokenType, value?: number | string | boolean): void {
    const lexeme = this.source.slice(this.start, this.current);
    this.tokens.push(token(type, lexeme, this.makeSpan(), value));
  }

  private error(message: string): void {
    this.errors.push({
      message,
      span: this.makeSpan(),
    });
    this.addToken(TokenType.Error);
  }
}

/**
 * Convenience function to tokenize source code
 */
export function lex(source: string, file?: string): { tokens: Token[]; errors: LexerError[] } {
  const lexer = new Lexer(source, file);
  return lexer.tokenize();
}
