import {
  createConnection,
  TextDocuments,
  ProposedFeatures,
  InitializeResult,
  TextDocumentSyncKind,
  Diagnostic,
  DiagnosticSeverity,
  CompletionItem,
  CompletionItemKind,
  InsertTextFormat,
  Hover,
} from "vscode-languageserver/node";
import { TextDocument } from "vscode-languageserver-textdocument";

// --- Lexer (port of src/lexer.c) ---

enum TokenType {
  Ident,
  String,
  Int,
  Float,
  Bool,
  LParen,
  RParen,
  LBrace,
  RBrace,
  Semicolon,
  Equals,
  Colon,
  Comma,
  Arrow,
  // Comparison operators
  EqEq,
  Ne,
  Gt,
  Ge,
  Lt,
  Le,
  // Arithmetic operators
  Plus,
  Minus,
  Star,
  Slash,
  Percent,
  // Logical
  And,
  Or,
  // Keywords
  For,
  If,
  Else,
  Match,
  Break,
  Continue,
  Import,
  From,
  Pub,
  Dot,
  FatArrow,
  // Bitwise operators
  Amp,
  Pipe,
  Caret,
  Tilde,
  Shl,
  Shr,
  // Brackets
  LBracket,
  RBracket,
  // Compound assignment operators
  PlusEq,
  MinusEq,
  StarEq,
  SlashEq,
  PercentEq,
  AmpEq,
  PipeEq,
  CaretEq,
  ShlEq,
  ShrEq,
  // Increment/Decrement
  PlusPlus,
  MinusMinus,
  EOF,
  Error,
}

interface Token {
  type: TokenType;
  text: string;
  offset: number;
  length: number;
}

function tokenize(source: string): Token[] {
  const tokens: Token[] = [];
  let pos = 0;

  function skipWhitespace() {
    for (;;) {
      while (pos < source.length && /\s/.test(source[pos])) pos++;
      // Line comment
      if (pos < source.length - 1 && source[pos] === "/" && source[pos + 1] === "/") {
        pos += 2;
        while (pos < source.length && source[pos] !== "\n") pos++;
        continue;
      }
      // Block comment
      if (pos < source.length - 1 && source[pos] === "/" && source[pos + 1] === "*") {
        pos += 2;
        while (pos < source.length) {
          if (source[pos] === "*" && pos + 1 < source.length && source[pos + 1] === "/") {
            pos += 2;
            break;
          }
          pos++;
        }
        continue;
      }
      break;
    }
  }

  while (true) {
    skipWhitespace();

    if (pos >= source.length) {
      tokens.push({ type: TokenType.EOF, text: "", offset: pos, length: 0 });
      break;
    }

    const c = source[pos];

    if (c === "(") {
      tokens.push({ type: TokenType.LParen, text: "(", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === ")") {
      tokens.push({ type: TokenType.RParen, text: ")", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "{") {
      tokens.push({ type: TokenType.LBrace, text: "{", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "}") {
      tokens.push({ type: TokenType.RBrace, text: "}", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === ";") {
      tokens.push({ type: TokenType.Semicolon, text: ";", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === ":") {
      tokens.push({ type: TokenType.Colon, text: ":", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === ",") {
      tokens.push({ type: TokenType.Comma, text: ",", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "=" && pos + 1 < source.length && source[pos + 1] === ">") {
      tokens.push({ type: TokenType.FatArrow, text: "=>", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "=" && pos + 1 < source.length && source[pos + 1] === "=") {
      tokens.push({ type: TokenType.EqEq, text: "==", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "=") {
      tokens.push({ type: TokenType.Equals, text: "=", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "!" && pos + 1 < source.length && source[pos + 1] === "=") {
      tokens.push({ type: TokenType.Ne, text: "!=", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === ">" && pos + 1 < source.length && source[pos + 1] === ">") {
      if (pos + 2 < source.length && source[pos + 2] === "=") {
        tokens.push({ type: TokenType.ShrEq, text: ">>=", offset: pos, length: 3 });
        pos += 3;
      } else {
        tokens.push({ type: TokenType.Shr, text: ">>", offset: pos, length: 2 });
        pos += 2;
      }
      continue;
    }

    if (c === ">" && pos + 1 < source.length && source[pos + 1] === "=") {
      tokens.push({ type: TokenType.Ge, text: ">=", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === ">") {
      tokens.push({ type: TokenType.Gt, text: ">", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "<" && pos + 1 < source.length && source[pos + 1] === "<") {
      if (pos + 2 < source.length && source[pos + 2] === "=") {
        tokens.push({ type: TokenType.ShlEq, text: "<<=", offset: pos, length: 3 });
        pos += 3;
      } else {
        tokens.push({ type: TokenType.Shl, text: "<<", offset: pos, length: 2 });
        pos += 2;
      }
      continue;
    }

    if (c === "<" && pos + 1 < source.length && source[pos + 1] === "=") {
      tokens.push({ type: TokenType.Le, text: "<=", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "<") {
      tokens.push({ type: TokenType.Lt, text: "<", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "-" && pos + 1 < source.length && source[pos + 1] === ">") {
      tokens.push({ type: TokenType.Arrow, text: "->", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "-" && pos + 1 < source.length && source[pos + 1] === "-") {
      tokens.push({ type: TokenType.MinusMinus, text: "--", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "-" && pos + 1 < source.length && source[pos + 1] === "=") {
      tokens.push({ type: TokenType.MinusEq, text: "-=", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "-") {
      tokens.push({ type: TokenType.Minus, text: "-", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "+" && pos + 1 < source.length && source[pos + 1] === "+") {
      tokens.push({ type: TokenType.PlusPlus, text: "++", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "+" && pos + 1 < source.length && source[pos + 1] === "=") {
      tokens.push({ type: TokenType.PlusEq, text: "+=", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "+") {
      tokens.push({ type: TokenType.Plus, text: "+", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "*" && pos + 1 < source.length && source[pos + 1] === "=") {
      tokens.push({ type: TokenType.StarEq, text: "*=", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "*") {
      tokens.push({ type: TokenType.Star, text: "*", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "/" && pos + 1 < source.length && source[pos + 1] === "=") {
      tokens.push({ type: TokenType.SlashEq, text: "/=", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "/") {
      tokens.push({ type: TokenType.Slash, text: "/", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "%" && pos + 1 < source.length && source[pos + 1] === "=") {
      tokens.push({ type: TokenType.PercentEq, text: "%=", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "%") {
      tokens.push({ type: TokenType.Percent, text: "%", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "&" && pos + 1 < source.length && source[pos + 1] === "=") {
      tokens.push({ type: TokenType.AmpEq, text: "&=", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "&") {
      tokens.push({ type: TokenType.Amp, text: "&", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "|" && pos + 1 < source.length && source[pos + 1] === "=") {
      tokens.push({ type: TokenType.PipeEq, text: "|=", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "|") {
      tokens.push({ type: TokenType.Pipe, text: "|", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "^" && pos + 1 < source.length && source[pos + 1] === "=") {
      tokens.push({ type: TokenType.CaretEq, text: "^=", offset: pos, length: 2 });
      pos += 2;
      continue;
    }

    if (c === "^") {
      tokens.push({ type: TokenType.Caret, text: "^", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "~") {
      tokens.push({ type: TokenType.Tilde, text: "~", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "[") {
      tokens.push({ type: TokenType.LBracket, text: "[", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === "]") {
      tokens.push({ type: TokenType.RBracket, text: "]", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === ".") {
      tokens.push({ type: TokenType.Dot, text: ".", offset: pos, length: 1 });
      pos++;
      continue;
    }

    if (c === '"') {
      const start = pos;
      pos++; // skip opening quote
      while (pos < source.length && source[pos] !== '"') {
        if (source[pos] === '\\') pos++; // skip escaped char
        pos++;
      }
      if (pos >= source.length) {
        tokens.push({
          type: TokenType.Error,
          text: source.slice(start),
          offset: start,
          length: pos - start,
        });
        continue;
      }
      pos++; // skip closing quote
      tokens.push({
        type: TokenType.String,
        text: source.slice(start, pos),
        offset: start,
        length: pos - start,
      });
      continue;
    }

    if (/\d/.test(c)) {
      const start = pos;
      // Hex literal: 0x...
      if (c === "0" && pos + 1 < source.length && (source[pos + 1] === "x" || source[pos + 1] === "X")) {
        pos += 2; // skip '0x'
        while (pos < source.length && /[0-9a-fA-F]/.test(source[pos])) pos++;
        tokens.push({ type: TokenType.Int, text: source.slice(start, pos), offset: start, length: pos - start });
      } else {
        while (pos < source.length && /\d/.test(source[pos])) pos++;
        if (pos < source.length && source[pos] === "." && pos + 1 < source.length && /\d/.test(source[pos + 1])) {
          pos++; // skip '.'
          while (pos < source.length && /\d/.test(source[pos])) pos++;
          tokens.push({ type: TokenType.Float, text: source.slice(start, pos), offset: start, length: pos - start });
        } else {
          tokens.push({ type: TokenType.Int, text: source.slice(start, pos), offset: start, length: pos - start });
        }
      }
      continue;
    }

    if (/[a-zA-Z_]/.test(c)) {
      const start = pos;
      while (pos < source.length && /[a-zA-Z0-9_]/.test(source[pos])) pos++;
      const text = source.slice(start, pos);
      let type = TokenType.Ident;
      if (text === "true" || text === "false") type = TokenType.Bool;
      else if (text === "and") type = TokenType.And;
      else if (text === "or") type = TokenType.Or;
      else if (text === "for") type = TokenType.For;
      else if (text === "if") type = TokenType.If;
      else if (text === "else") type = TokenType.Else;
      else if (text === "match") type = TokenType.Match;
      else if (text === "break") type = TokenType.Break;
      else if (text === "continue") type = TokenType.Continue;
      else if (text === "import") type = TokenType.Import;
      else if (text === "from") type = TokenType.From;
      else if (text === "pub") type = TokenType.Pub;
      tokens.push({ type, text, offset: start, length: pos - start });
      continue;
    }

    // Unknown character
    tokens.push({
      type: TokenType.Error,
      text: source[pos],
      offset: pos,
      length: 1,
    });
    pos++;
  }

  return tokens;
}

// --- Validator (port of src/parser.c with error recovery) ---

function validate(doc: TextDocument): Diagnostic[] {
  const text = doc.getText();
  const tokens = tokenize(text);
  const diagnostics: Diagnostic[] = [];
  const declaredVars = new Map<string, "const" | "var">();
  let i = 0;

  function current(): Token {
    return tokens[i] ?? { type: TokenType.EOF, text: "", offset: text.length, length: 0 };
  }

  function advance(): Token {
    const tok = current();
    if (tok.type !== TokenType.EOF) i++;
    return tok;
  }

  function makeDiag(offset: number, length: number, message: string, severity: DiagnosticSeverity = DiagnosticSeverity.Error): Diagnostic {
    const startPos = doc.positionAt(offset);
    const endPos = doc.positionAt(offset + Math.max(length, 1));
    return {
      severity,
      range: { start: startPos, end: endPos },
      message,
      source: "lingua",
    };
  }

  // Panic-mode recovery: skip to next semicolon or EOF
  function recover() {
    while (current().type !== TokenType.Semicolon && current().type !== TokenType.EOF) {
      advance();
    }
    if (current().type === TokenType.Semicolon) advance();
  }

  // Check escape sequences inside a string token
  function checkEscapes(tok: Token) {
    const inner = tok.text.slice(1, -1); // strip quotes
    for (let j = 0; j < inner.length; j++) {
      if (inner[j] === '\\') {
        if (j + 1 >= inner.length) {
          diagnostics.push(makeDiag(tok.offset + 1 + j, 1, "Incomplete escape sequence at end of string", DiagnosticSeverity.Error));
        } else {
          const ch = inner[j + 1];
          if (!"ntr\\\"0{}".includes(ch)) {
            diagnostics.push(makeDiag(tok.offset + 1 + j, 2, `Unknown escape sequence '\\${ch}'`, DiagnosticSeverity.Warning));
          }
          j++; // skip the escaped char
        }
      }
    }
  }

  // Skip an expression (tokens until ';' or ')' at nesting level 0)
  function skipExpression() {
    let depth = 0;
    while (current().type !== TokenType.EOF) {
      const t = current().type;
      if (t === TokenType.LParen) depth++;
      else if (t === TokenType.RParen) {
        if (depth === 0) break;
        depth--;
      }
      else if (t === TokenType.Semicolon && depth === 0) break;

      // Check string escapes as we go
      if (t === TokenType.String) checkEscapes(current());
      if (t === TokenType.Error && current().text.startsWith('"')) {
        diagnostics.push(makeDiag(current().offset, current().length, "Unterminated string literal"));
      }
      advance();
    }
  }

  while (current().type !== TokenType.EOF) {
    const tok = advance();

    if (tok.type === TokenType.Error) {
      if (tok.text.startsWith('"')) {
        diagnostics.push(makeDiag(tok.offset, tok.length, "Unterminated string literal"));
      } else {
        diagnostics.push(makeDiag(tok.offset, tok.length, `Unexpected character '${tok.text}'`));
      }
      recover();
      continue;
    }

    // --- pub ---
    if (tok.type === TokenType.Pub) {
      // pub just modifies the next declaration — continue to let it be parsed
      continue;
    }

    // --- import ---
    if (tok.type === TokenType.Import) {
      // import { name1, name2 } from "path";
      if (current().type === TokenType.LBrace) {
        advance(); // skip '{'
        while (current().type !== TokenType.EOF && current().type !== TokenType.RBrace) {
          advance();
        }
        if (current().type === TokenType.RBrace) advance();
      }
      if (current().type === TokenType.From) advance();
      if (current().type === TokenType.String) advance();
      if (current().type === TokenType.Semicolon) advance();
      continue;
    }

    // --- for ---
    if (tok.type === TokenType.For) {
      // Skip for loop header: for (...)
      if (current().type === TokenType.LParen) {
        advance();
        let parenDepth = 1;
        while (current().type !== TokenType.EOF && parenDepth > 0) {
          if (current().type === TokenType.LParen) parenDepth++;
          else if (current().type === TokenType.RParen) parenDepth--;
          advance();
        }
      }
      // Skip body: either { ... } or single statement ending with ;
      if (current().type === TokenType.LBrace) {
        advance();
        let depth = 1;
        while (current().type !== TokenType.EOF && depth > 0) {
          if (current().type === TokenType.LBrace) depth++;
          else if (current().type === TokenType.RBrace) depth--;
          advance();
        }
      } else {
        // Braceless: skip to semicolon
        while (current().type !== TokenType.EOF && current().type !== TokenType.Semicolon) advance();
        if (current().type === TokenType.Semicolon) advance();
      }
      continue;
    }

    // --- if/else if/else ---
    if (tok.type === TokenType.If) {
      // Skip if/else if/else chain, supporting both block and braceless bodies
      const skipIfChain = () => {
        // Skip condition expression (until '{' or a statement token)
        while (current().type !== TokenType.EOF && current().type !== TokenType.LBrace && current().type !== TokenType.Semicolon) {
          // If we hit a keyword that starts a statement, it's a braceless body
          if (current().type === TokenType.Ident || current().type === TokenType.If || current().type === TokenType.For || current().type === TokenType.Match) break;
          advance();
        }
        // Skip body
        if (current().type === TokenType.LBrace) {
          advance();
          let depth = 1;
          while (current().type !== TokenType.EOF && depth > 0) {
            if (current().type === TokenType.LBrace) depth++;
            else if (current().type === TokenType.RBrace) depth--;
            advance();
          }
        } else {
          // Braceless: skip to semicolon
          while (current().type !== TokenType.EOF && current().type !== TokenType.Semicolon) advance();
          if (current().type === TokenType.Semicolon) advance();
        }
        // Check for else/else if continuation
        if (current().type === TokenType.Else) {
          advance(); // consume 'else'
          if (current().type === TokenType.If) {
            advance(); // consume 'if'
            skipIfChain(); // recurse
          } else if (current().type === TokenType.LBrace) {
            advance();
            let depth = 1;
            while (current().type !== TokenType.EOF && depth > 0) {
              if (current().type === TokenType.LBrace) depth++;
              else if (current().type === TokenType.RBrace) depth--;
              advance();
            }
          } else {
            // Braceless else
            while (current().type !== TokenType.EOF && current().type !== TokenType.Semicolon) advance();
            if (current().type === TokenType.Semicolon) advance();
          }
        }
      };
      skipIfChain();
      continue;
    }

    // --- match ---
    if (tok.type === TokenType.Match) {
      // Skip entire match block: match <expr> { ... }
      let depth = 0;
      let seenBrace = false;
      while (current().type !== TokenType.EOF) {
        if (current().type === TokenType.LBrace) { depth++; seenBrace = true; }
        else if (current().type === TokenType.RBrace) {
          depth--;
          if (depth === 0 && seenBrace) { advance(); break; }
        }
        advance();
      }
      continue;
    }

    // --- break / continue ---
    if (tok.type === TokenType.Break || tok.type === TokenType.Continue) {
      if (current().type === TokenType.Semicolon) advance();
      continue;
    }

    if (tok.type !== TokenType.Ident) {
      diagnostics.push(makeDiag(tok.offset, tok.length, `Expected statement, got '${tok.text}'`));
      recover();
      continue;
    }

    // --- class ---
    if (tok.text === "class") {
      // Skip class declaration: class Name [extends Parent] { fields; methods }
      if (current().type === TokenType.Ident) advance(); // skip class name
      // Optional: extends Parent
      if (current().type === TokenType.Ident && current().text === "extends") {
        advance(); // skip 'extends'
        if (current().type === TokenType.Ident) advance(); // skip parent name
      }
      if (current().type === TokenType.LBrace) {
        advance();
        let depth = 1;
        while (current().type !== TokenType.EOF && depth > 0) {
          if (current().type === TokenType.LBrace) depth++;
          else if (current().type === TokenType.RBrace) depth--;
          advance();
        }
      }
      continue;
    }

    // --- fn ---
    if (tok.text === "fn") {
      // Skip function declaration: either block body or shorthand `return <expr>;`
      // First skip to past the closing paren of params
      while (current().type !== TokenType.EOF && current().type !== TokenType.RParen) advance();
      if (current().type === TokenType.RParen) advance();
      // Skip optional return type: -> type
      if (current().type === TokenType.Arrow) {
        advance(); // skip ->
        if (current().type === TokenType.Ident) advance(); // skip type
      }
      // Now either { body } or `return <expr>;`
      if (current().type === TokenType.LBrace) {
        advance();
        let depth = 1;
        while (current().type !== TokenType.EOF && depth > 0) {
          if (current().type === TokenType.LBrace) depth++;
          else if (current().type === TokenType.RBrace) depth--;
          advance();
        }
      } else if (current().type === TokenType.Ident && current().text === "return") {
        // Shorthand: return <expr>;
        advance(); // skip 'return'
        while (current().type !== TokenType.EOF && current().type !== TokenType.Semicolon) advance();
        if (current().type === TokenType.Semicolon) advance();
      }
      continue;
    }

    // --- return ---
    if (tok.text === "return") {
      skipExpression();
      if (current().type === TokenType.Semicolon) advance();
      continue;
    }

    // --- const/var <ident> [: type] = <expr> ; ---
    if (tok.text === "const" || tok.text === "var") {
      const keyword = tok.text;
      const name = current();
      if (name.type !== TokenType.Ident) {
        diagnostics.push(makeDiag(name.offset, Math.max(name.length, 1), `Expected variable name after '${keyword}'`));
        recover();
        continue;
      }
      advance();

      // Optional type annotation
      if (current().type === TokenType.Colon) {
        advance(); // skip ':'
        if (current().type !== TokenType.Ident) {
          diagnostics.push(makeDiag(current().offset, Math.max(current().length, 1), "Expected type name"));
          recover();
          continue;
        }
        advance(); // skip type name
      }

      const eq = current();
      if (eq.type !== TokenType.Equals) {
        diagnostics.push(makeDiag(eq.offset, Math.max(eq.length, 1), "Expected '=' after variable name"));
        recover();
        continue;
      }
      advance();

      skipExpression();

      const semi = current();
      if (semi.type !== TokenType.Semicolon) {
        diagnostics.push(makeDiag(current().offset, 0, "Expected ';'"));
        recover();
        continue;
      }
      advance();

      declaredVars.set(name.text, keyword as "const" | "var");
      continue;
    }

    // --- <ident>.<member>(...) or <ident>.<field> = <expr> ---
    if (tok.text !== "print" && current().type === TokenType.Dot) {
      advance(); // skip '.'
      if (current().type === TokenType.Ident) advance(); // skip member name
      if (current().type === TokenType.LParen) {
        // Method call: obj.method(args);
        advance(); // skip '('
        let depth = 1;
        while (current().type !== TokenType.EOF && depth > 0) {
          if (current().type === TokenType.LParen) depth++;
          else if (current().type === TokenType.RParen) depth--;
          if (depth > 0) advance();
        }
        if (current().type === TokenType.RParen) advance();
        if (current().type === TokenType.Semicolon) advance();
      } else if (current().type === TokenType.Equals) {
        // Field assignment: obj.field = expr;
        advance(); // skip '='
        skipExpression();
        if (current().type === TokenType.Semicolon) advance();
      }
      continue;
    }

    // --- <ident>++ or <ident>-- ; (increment/decrement) ---
    if (tok.text !== "print" && (current().type === TokenType.PlusPlus || current().type === TokenType.MinusMinus)) {
      advance(); // skip ++ or --
      if (current().type === TokenType.Semicolon) advance();
      continue;
    }

    // --- <ident> += / -= / *= / /= / %= / &= / |= / ^= / <<= / >>= <expr> ; (compound assignment) ---
    if (tok.text !== "print" && (
      current().type === TokenType.PlusEq ||
      current().type === TokenType.MinusEq ||
      current().type === TokenType.StarEq ||
      current().type === TokenType.SlashEq ||
      current().type === TokenType.PercentEq ||
      current().type === TokenType.AmpEq ||
      current().type === TokenType.PipeEq ||
      current().type === TokenType.CaretEq ||
      current().type === TokenType.ShlEq ||
      current().type === TokenType.ShrEq
    )) {
      advance(); // skip compound operator
      skipExpression();
      if (current().type === TokenType.Semicolon) advance();

      if (!declaredVars.has(tok.text)) {
        diagnostics.push(makeDiag(tok.offset, tok.length, `Undefined variable '${tok.text}'`, DiagnosticSeverity.Warning));
      } else if (declaredVars.get(tok.text) === "const") {
        diagnostics.push(makeDiag(tok.offset, tok.length, `Cannot reassign const variable '${tok.text}'`));
      }
      continue;
    }

    // --- <ident> = <expr> ; (assignment) ---
    if (tok.text !== "print" && current().type === TokenType.Equals) {
      advance(); // skip =

      skipExpression();

      const semi = current();
      if (semi.type !== TokenType.Semicolon) {
        diagnostics.push(makeDiag(current().offset, 0, "Expected ';'"));
        recover();
        continue;
      }
      advance();

      if (!declaredVars.has(tok.text)) {
        diagnostics.push(makeDiag(tok.offset, tok.length, `Undefined variable '${tok.text}'`, DiagnosticSeverity.Warning));
      } else if (declaredVars.get(tok.text) === "const") {
        diagnostics.push(makeDiag(tok.offset, tok.length, `Cannot reassign const variable '${tok.text}'`));
      }
      continue;
    }

    // --- <ident>(...) ; (standalone function call) ---
    if (tok.text !== "print" && current().type === TokenType.LParen) {
      advance(); // skip '('
      // Skip args
      let depth = 1;
      while (current().type !== TokenType.EOF && depth > 0) {
        if (current().type === TokenType.LParen) depth++;
        else if (current().type === TokenType.RParen) depth--;
        if (depth > 0) advance();
      }
      if (current().type === TokenType.RParen) advance();
      if (current().type === TokenType.Semicolon) advance();
      continue;
    }

    // --- print ( <expr> ) ; ---
    if (tok.text === "print") {
      const lp = current();
      if (lp.type !== TokenType.LParen) {
        diagnostics.push(makeDiag(tok.offset + tok.length, 0, "Expected '(' after 'print'"));
        recover();
        continue;
      }
      advance();

      skipExpression();

      const rp = current();
      if (rp.type !== TokenType.RParen) {
        diagnostics.push(makeDiag(rp.offset, Math.max(rp.length, 1), "Expected ')' after argument"));
        recover();
        continue;
      }
      advance();

      const semi = current();
      if (semi.type !== TokenType.Semicolon) {
        diagnostics.push(makeDiag(rp.offset + rp.length, 0, "Expected ';' after ')'"));
        recover();
        continue;
      }
      advance();
      continue;
    }

    // Unknown identifier
    diagnostics.push(makeDiag(tok.offset, tok.length, `Unknown statement '${tok.text}'`));
    recover();
  }

  return diagnostics;
}

// --- LSP Server setup ---

const connection = createConnection(ProposedFeatures.all);
const documents = new TextDocuments(TextDocument);

connection.onInitialize((): InitializeResult => {
  return {
    capabilities: {
      textDocumentSync: TextDocumentSyncKind.Incremental,
      completionProvider: { triggerCharacters: ["p", "c", "v", "f", "i", "m", "n", "b", "l", "t", "r", "s", "e", "."] },
      hoverProvider: true,
    },
  };
});

documents.onDidChangeContent((change) => {
  const diagnostics = validate(change.document);
  connection.sendDiagnostics({ uri: change.document.uri, diagnostics });
});

connection.onCompletion((): CompletionItem[] => {
  return [
    {
      label: "print",
      kind: CompletionItemKind.Function,
      detail: "Print a value to stdout",
      insertText: 'print($1);',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "const",
      kind: CompletionItemKind.Keyword,
      detail: "Declare an immutable variable",
      insertText: 'const $1 = $2;',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "var",
      kind: CompletionItemKind.Keyword,
      detail: "Declare a mutable variable",
      insertText: 'var $1 = $2;',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "fn",
      kind: CompletionItemKind.Keyword,
      detail: "Declare a function",
      insertText: 'fn $1($2) {\n\t$3\n}',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "for",
      kind: CompletionItemKind.Keyword,
      detail: "For loop",
      insertText: 'for (var $1 = $2; $3; $4) {\n\t$5\n}',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "if",
      kind: CompletionItemKind.Keyword,
      detail: "Conditional statement",
      insertText: 'if ($1) {\n\t$2\n}',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "match",
      kind: CompletionItemKind.Keyword,
      detail: "Pattern matching statement",
      insertText: 'match ($1) {\n\t$2 => {\n\t\t$3\n\t}\n\t_ => {\n\t\t$4\n\t}\n}',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "class",
      kind: CompletionItemKind.Keyword,
      detail: "Declare a class",
      insertText: 'class $1 {\n\t$2: $3;\n}',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "new",
      kind: CompletionItemKind.Keyword,
      detail: "Create a new object instance",
      insertText: 'new $1($2)',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "break",
      kind: CompletionItemKind.Keyword,
      detail: "Exit the nearest for loop",
      insertText: 'break;',
      insertTextFormat: InsertTextFormat.PlainText,
    },
    {
      label: "continue",
      kind: CompletionItemKind.Keyword,
      detail: "Skip to the next iteration of the nearest for loop",
      insertText: 'continue;',
      insertTextFormat: InsertTextFormat.PlainText,
    },
    {
      label: "import",
      kind: CompletionItemKind.Keyword,
      detail: "Import symbols from another module",
      insertText: 'import { $1 } from "$2";',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "pub",
      kind: CompletionItemKind.Keyword,
      detail: "Make a declaration public (importable)",
      insertText: 'pub ',
      insertTextFormat: InsertTextFormat.PlainText,
    },
    {
      label: "len",
      kind: CompletionItemKind.Function,
      detail: "Returns the length of a string",
      insertText: 'len($1)',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "trim",
      kind: CompletionItemKind.Function,
      detail: "Removes leading and trailing whitespace from a string",
      insertText: 'trim($1)',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "contains",
      kind: CompletionItemKind.Function,
      detail: "Checks if a string contains a substring",
      insertText: 'contains($1, $2)',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "replace",
      kind: CompletionItemKind.Function,
      detail: "Replaces occurrences of a substring in a string",
      insertText: 'replace($1, $2, $3)',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "to_upper",
      kind: CompletionItemKind.Function,
      detail: "Converts a string to uppercase",
      insertText: 'to_upper($1)',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "to_lower",
      kind: CompletionItemKind.Function,
      detail: "Converts a string to lowercase",
      insertText: 'to_lower($1)',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "starts_with",
      kind: CompletionItemKind.Function,
      detail: "Checks if a string starts with a prefix",
      insertText: 'starts_with($1, $2)',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "ends_with",
      kind: CompletionItemKind.Function,
      detail: "Checks if a string ends with a suffix",
      insertText: 'ends_with($1, $2)',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "index_of",
      kind: CompletionItemKind.Function,
      detail: "Returns the index of a substring in a string, or -1 if not found",
      insertText: 'index_of($1, $2)',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "char_at",
      kind: CompletionItemKind.Function,
      detail: "Returns the character at a given index in a string",
      insertText: 'char_at($1, $2)',
      insertTextFormat: InsertTextFormat.Snippet,
    },
    {
      label: "substr",
      kind: CompletionItemKind.Function,
      detail: "Returns a substring from start index with given length",
      insertText: 'substr($1, $2, $3)',
      insertTextFormat: InsertTextFormat.Snippet,
    },
  ];
});

connection.onHover((params): Hover | null => {
  const doc = documents.get(params.textDocument.uri);
  if (!doc) return null;

  const offset = doc.offsetAt(params.position);
  const text = doc.getText();

  // Find the word under cursor
  let start = offset;
  let end = offset;
  while (start > 0 && /[a-zA-Z_]/.test(text[start - 1])) start--;
  while (end < text.length && /[a-zA-Z0-9_]/.test(text[end])) end++;
  const word = text.slice(start, end);

  if (word === "print") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nprint(expr)\nprint(expr, newline: false)\n```\nPrints a value to standard output. Appends `\\n` by default. Pass `newline: false` to suppress.",
      },
    };
  }

  if (word === "const") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nconst name = expr;\n```\nDeclares an immutable variable.",
      },
    };
  }

  if (word === "var") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nvar name = expr;\n```\nDeclares a mutable variable.",
      },
    };
  }

  if (word === "fn") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nfn name(params) -> type { body }\n```\nDeclares a function.",
      },
    };
  }

  if (word === "for") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nfor (var i = 0; i < n; i = i + 1) { body }\n```\nC-style for loop. Unrolled at compile time.",
      },
    };
  }

  if (word === "if") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nif (condition) { body }\nif (condition) { body } else { body }\nif (condition) { body } else if (condition) { body } else { body }\n```\nConditional statement. Parentheses required. Condition must be a `bool`. Evaluated at compile time.",
      },
    };
  }

  if (word === "match") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nmatch (expr) {\n    1 => { ... }\n    _ => { ... }\n}\n```\nPattern matching statement. Parentheses required. Arms are checked top-to-bottom. `_` is the wildcard/default arm. Evaluated at compile time.",
      },
    };
  }

  if (word === "else") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\n} else {\n} else if (condition) {\n```\nAlternative branch of an `if` statement.",
      },
    };
  }

  if (word === "class") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nclass Name {\n    field: type;\n    fn method(params) -> type { body }\n}\nclass Child extends Parent { ... }\n```\nDeclares a class with fields and methods. Supports single inheritance with `extends`.",
      },
    };
  }

  if (word === "new") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nnew ClassName(field: value, ...)\n```\nCreates a new instance of a class. Arguments are matched to fields by name or position.",
      },
    };
  }

  if (word === "extends") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nclass Child extends Parent { ... }\n```\nInherits fields and methods from the parent class. Child class can add new fields and override methods.",
      },
    };
  }

  if (word === "break") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nbreak;\n```\nExits the nearest enclosing `for` loop immediately.",
      },
    };
  }

  if (word === "continue") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\ncontinue;\n```\nSkips the rest of the current iteration and jumps to the next iteration of the nearest `for` loop.",
      },
    };
  }

  if (word === "import") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nimport { add, PI } from \"./math\";\n```\nImports public symbols from another `.lingua` module. `./` = relative to current file, otherwise relative to project root.",
      },
    };
  }

  if (word === "from") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nimport { name } from \"path\";\n```\nSpecifies the module path in an import statement. `.lingua` extension is auto-appended.",
      },
    };
  }

  if (word === "pub") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\npub fn add(a: int, b: int) -> int { ... }\npub const PI = 3;\npub class Point { ... }\n```\nMakes a declaration public so it can be imported by other modules. Only top-level declarations can be `pub`.",
      },
    };
  }

  if (word === "len") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nlen(s: string) -> int\n```\nReturns the length of a string.",
      },
    };
  }

  if (word === "trim") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\ntrim(s: string) -> string\n```\nRemoves leading and trailing whitespace from a string.",
      },
    };
  }

  if (word === "contains") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\ncontains(s: string, substr: string) -> bool\n```\nReturns `true` if `s` contains `substr`.",
      },
    };
  }

  if (word === "replace") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nreplace(s: string, old: string, new: string) -> string\n```\nReplaces all occurrences of `old` with `new` in `s`.",
      },
    };
  }

  if (word === "to_upper") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nto_upper(s: string) -> string\n```\nConverts all characters in the string to uppercase.",
      },
    };
  }

  if (word === "to_lower") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nto_lower(s: string) -> string\n```\nConverts all characters in the string to lowercase.",
      },
    };
  }

  if (word === "starts_with") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nstarts_with(s: string, prefix: string) -> bool\n```\nReturns `true` if `s` starts with `prefix`.",
      },
    };
  }

  if (word === "ends_with") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nends_with(s: string, suffix: string) -> bool\n```\nReturns `true` if `s` ends with `suffix`.",
      },
    };
  }

  if (word === "index_of") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nindex_of(s: string, substr: string) -> int\n```\nReturns the index of the first occurrence of `substr` in `s`, or `-1` if not found.",
      },
    };
  }

  if (word === "char_at") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nchar_at(s: string, index: int) -> string\n```\nReturns the character at the given index in the string as a single-character string.",
      },
    };
  }

  if (word === "substr") {
    return {
      contents: {
        kind: "markdown",
        value: "```lingua\nsubstr(s: string, start: int, length: int) -> string\n```\nReturns a substring starting at `start` with the given `length`.",
      },
    };
  }

  return null;
});

documents.listen(connection);
connection.listen();
